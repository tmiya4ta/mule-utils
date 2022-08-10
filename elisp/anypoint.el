;; This library aims to operate Anypoint Platform via the management API.
;; Please vist https://anypoint.mulesoft.com/exchange/portals/anypoint-platform/
;; to check more


;; -*- lexical-binding: t -*-

(require 'request)
(require 'dash)
(require 'parseedn)
(require 'dom)
(require 'esxml-query)
(require 'subr-x)
(require 'promise)

;; http-post-simple require 'cl package (require 'cl) in order to handle mulipart/form

(require 'http-post-simple)

(defvar anypoint-context '() "alist for system context")
(defvar anypoint-credential '() "alist for user credential (token)")

(define-derived-mode anypoint-mode special-mode "Anypoint"
  :group 'anypoint)

(defun set-log-level (level)
  (interactive
   (list (completing-read "level: " '(info debug))))
  (setq request-message-level (intern level)))

(defun anypoint-show (obj &optional print-mode)
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (switch-to-buffer (get-buffer-create "*anypoint-show*"))
  (lisp-mode)
  (quittable-output-mode)
  (prin1 print-mode)
  (erase-buffer)
  (cond
   ((eq print-mode :raw) (cl-prettyprint obj))
   ((eq print-mode :json) (progn
			   (insert obj)
			   (json-pretty-print-buffer)))
   ((or (null print-mode) (eq print-mode :object)) (progn
						     (insert (json-encode obj))
						     (json-pretty-print-buffer))))
  (goto-char (point-min)))

(defun get-in (keys alist)
  (if (null (car keys))
      alist
    (get-in (cdr keys) (if (numberp (car keys))
			   (aref alist (car keys))
			 (assoc-default (car keys) alist)))))


(defun make-default-header (&optional ctx)
  (interactive)
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Bearer " (get-in '(token access_token) anypoint-credential)))))

(cl-defun http-get (uri &rest settings &key (headers nil) (data nil) (callback nil) (sync nil))
  (request uri
	   :type "GET"
	   :headers (or headers (make-default-header))
	   :parser 'json-read
	   :data data
	   :sync sync
	   :error (cl-function (lambda (&rest args &key data error-thrown &allow-other-keys)
				 (message "error: %s %s"
					  (assoc-default 'status data )
					  (assoc-default 'message data))
				 data))

	   :success (cl-function
		     (lambda (&key data &allow-other-keys)
		       (if callback
			   (funcall callback data)
			 (message "Success"))))))



(defun anypoint-login (user password)
  (interactive "suser: \nspassword: ")
  ;; Required to delete cookie file since request.el does not provide how to disable
  (delete-file (expand-file-name "~/.emacs.d/request/curl-cookie-jar"))
  (let ((result nil))
    (request "https://anypoint.mulesoft.com/accounts/login"
	     :type "POST"
	     :data (json-encode `(("username" . ,user)
				  ("password" . ,password)))
	     :headers '(("Content-Type" . "application/json"))
	     :parser 'json-read
	     :sync t
	     :complete (cl-function
			(lambda (&key data &allow-other-keys)
			  (setq result `((token . ,data))))))
    result))



(defun anypoint-login-with-profile (profile-name)
  (interactive
   (list (completing-read "profile: " (json-read-file "~/.anypoint/credentials"))))
  ;; Required to delete cookie file since request.el does not provide how to disable
  (delete-file (expand-file-name "~/.emacs.d/request/curl-cookie-jar"))
  (let* ((profile (json-read-file "~/.anypoint/credentials"))
	 (user (get-in `(,(intern profile-name) username) profile))
	 (password (get-in `(,(intern profile-name) password) profile))
	 (client-id (get-in `(,(intern profile-name) client-id) profile))
	 (client-secret (get-in `(,(intern profile-name) client-secret) profile))
	 (result nil))
    (cond 
     ((and user password) (progn
			    (message "Logging in")
			    (request "https://anypoint.mulesoft.com/accounts/login"
			      :type "POST"
			      :data (json-encode `(("username" . ,user)
						   ("password" . ,password)))
			      :headers '(("Content-Type" . "application/json"))
			      :parser 'json-read
			      :sync t
			      :success (cl-function
					(lambda (&key data &allow-other-keys)
					  (setq result `((token . ,data)))
					  (message "Logged in succesfully")))))) 
     ((and client-id client-secret) (progn
				      (message "Logging in")
				      (request "https://anypoint.mulesoft.com/accounts/api/v2/oauth2/token"
					:type "POST"
					:data (json-encode `(("client_id" . ,client-id)
							     ("client_secret" . ,client-secret)
							     ("grant_type" . "client_credentials")))
					:headers '(("Content-Type" . "application/json"))
					:parser 'json-read
					:sync t
					:success (cl-function
						  (lambda (&key data &allow-other-keys)
						    (setq result `((token . ,data)))
						    (message "Logged in succesfully"))))))
      (message "No user nor password" ))
    result))


(defun anypoint-get-regions ()
  (interactive)
  ;; Required to delete cookie file since request.el does not provide how to disable
  (delete-file (expand-file-name "~/.emacs.d/request/curl-cookie-jar"))
  (let ((result nil))
    (http-get (format "https://anypoint.mulesoft.com/cloudhub/api/regions")
	      :sync t	      
	      :callback (lambda (data)
			  (setq result data)))

    (if (called-interactively-p 'any)
	(anypoint-show result)
      result)))


(defun anypoint-me (&optional cred)
  (interactive)
  (let ((result nil))
    (message "Retrieving basic context")
    (request "https://anypoint.mulesoft.com/accounts/api/me"
	     :type "GET"
	     :headers (make-default-header)
	     :parser 'json-read
	     :sync t
	     :success (cl-function
		       (lambda (&key data &allow-other-keys)
			 (setq result data)
			 ;;(setq result (append (assq-delete-all 'user anypoint-context) data))
			 (message "Retrieved successfull"))))
    result))



(defun anypoint-exchange-get-asset (asset-str)
  ;; asset-str: group-id:asset-id:version
  (interactive 
   (let* ((assets (->> (anypoint-exchange-graphql-query "")
		       (get-in '(data assets))
		       (-map (lambda (x)
			       (let ((org (car (rassoc (assoc-default 'groupId x)
						       (anypoint-summary-organizations)))))
				 (list
				  org
				  (assoc-default 'assetId x)
				  (assoc-default 'version x)
				  (string-width (symbol-name org))))))))
	  (max-width (-max (-map (lambda (x) (cadddr x)) assets))))

     (list
      (completing-read "asset: " (->> assets
				      (-map (lambda (x)
					      (format (concat "%" (number-to-string (- (+ 5 max-width))) "s"  "%-50s %s")
						      (car x) (cadr x) (caddr x))))
				      (-sort (lambda (x y)
					       (< (string-to-char (substring  x 0 1))
						  (string-to-char (substring  y 0 1))))))))))
  (let ((orgs (anypoint-summary-organizations)))
    (pcase-let ((`(,g ,a ,v) (split-string asset-str "[ :]+")) )
      (prin1 (format "https://anypoint.mulesoft.com/exchange/api/v2/assets/%s/%s/asset"
		     (assoc-default (intern g) orgs)
		     a))
      (http-get (format "https://anypoint.mulesoft.com/exchange/api/v2/assets/%s/%s/asset"
			(assoc-default (intern g) orgs)
			a)
		:headers `(("Content-Type" . "application/json")
			   ("Authorization" . ,(concat "Bearer " (get-in '(token access_token) anypoint-credential))))
		:sync t	      
		:callback (lambda (data)
			    (anypoint-show  data))))))



(defun anypoint-applications ()
  (interactive)
  (let* ((env-id (assoc-default 'id env)))
    (http-get "https://anypoint.mulesoft.com/cloudhub/api/v2/applications"
	      :headers `(("Content-Type" . "application/json")
			 ("Authorization" . ,(concat "Bearer " (get-in '(token access_token) ctx)))
			 ("x-anypnt-env-id" . ,env-id))
	      :sync t	      
	      :callback (lambda (data)
			  (setq anypoint-context
				(append (assq-delete-all 'applications anypoint-context)
					`((applications . ,data))))))))

(defun anypoint-collect ()
  (interactive)
  (let* ((orgs (anypoint-summary-organizations)))
    (list `(envs . ,(seq-mapcat  (lambda (org)
				   (mapcar (lambda (env)
					     (append env (list (cons 'org org))))
					   (anypoint-get-environments org)))
				 (mapcar 'car orgs))))))

(defun anypoint-async-collect ()
  (interactive)
  (setq anypoint-context (append anypoint-context '((envs . ()))))
  (let* ((orgs (anypoint-summary-organizations)))
    (->> (-map 'car orgs)
	 (-map (lambda (o)
		 (anypoint-async-get-environments o (lambda (x)
						      (let* ((es (->> (assoc-default 'data x)
								     ;; envs (colllection)
								     (-map (lambda (e)
									     (cons (cons 'org o) e)))))
							     (cs (cdr (assoc 'envs anypoint-context))))
							(if (null es)
							    (message "No env: %s" o)
							    (setf (cdr (assoc 'envs anypoint-context))
								  (append es cs)))))))))
    (message "started retrieving asynchronously")))


(defun anypoint-init! (user password)
  (interactive (list (read-string "user: " )
		     (read-passwd "password: ")))
  (setq anypoint-context nil)

  (message "Logging in ...")
  ;; Make basic context  (auth token, and basic info)
  (->> (anypoint-login user password)
       (anypoint-me)
       (setq anypoint-context))
  (message "Logged in")  

  ;; Add environments
  (setq anypoint-context (append anypoint-context (anypoint-collect)))
  anypoint-context)


(defun anypoint-init-with-profile! (profile-name)
  (interactive
   (list (completing-read "profile: " (json-read-file "~/.anypoint/credentials"))))
  (message "Initializing")
  (setq anypoint-credential (anypoint-login-with-profile profile-name))
  (setq anypoint-context (anypoint-me))
  ;; (setq anypoint-context (append anypoint-context (anypoint-collect)))
  (anypoint-async-collect)
  (message "Retrieved")
  anypoint-context)


(defun anypoint-describe-variable-in-json (sexp)
  (interactive "svar: ")
  (let ()
    (switch-to-buffer-other-window  "*json-output*" t)
    (quittable-output-mode)
    (insert (json-encode (symbol-value (intern "anypoint-context"))))
    (json-pretty-print-buffer)
    (beginning-of-buffer)))

(defun delete-last-slash (str)
  (interactive)
  (replace-regexp-in-string "/$" "" str))

(defun find-mvn-root-path (dirname)
  (interactive "sPath: ")
  (when dirname
    (let ((dirname (delete-last-slash
		    (or (file-truename dirname)
			(file-name-directory (file-truename (buffer-file-name)))))))
      (cond ((member "pom.xml" (directory-files dirname)) dirname)
	    ((string-match "^/$" dirname) nil)
	    (t (find-mvn-root-path (file-name-directory (delete-last-slash dirname))))))))


(defun find-file-in-target-dir (file-extension dir)
  (interactive "sDir: ")
  (let ((pom-dir (find-mvn-root-path dir)))
    (when pom-dir
      (mapcar (lambda (x) (concat pom-dir "/target/" x))
	      (directory-files (concat pom-dir "/target") nil (concat "." file-extension))))))

(defun anypoint-open-shell-at-root-mvn-root-path ()
  (interactive)
  (let ((dir (find-mvn-root-path (file-name-directory (buffer-file-name)))))
    (if dir
	(let ((default-directory dir))
	  (split-window-vertically)
	  (windmove-down)
	  (switch-to-buffer (get-buffer-create "*shell*"))
	  (shell ))
      (message "Not found pom.xml"))))

(defun anypoint-create-init-structure (project-name)
  (interactive "sProject: ")
  (shell-command "mvn org.mule.extensions:mule-extensions-archetype-maven-plugin:1.2.0:generate"))


(defun anypoint-mvn-init-plugin ()
  (interactive)
  (let ((default-directory (expand-file-name "~/AnypointStudio/studio-workspace")))
    (async-shell-command
     "mvn org.mule.extensions:mule-extensions-archetype-maven-plugin:1.2.0:generate")))

(defun anypoint-mvn-install-plugin (dir)
  (interactive
   (list (completing-read
	  "Choose plugin: "
	  (seq-filter (lambda (d)
			(and (file-directory-p  d)
			     (string-match-p "^[^.]" (file-name-base d))))
		      (directory-files "~/AnypointStudio/studio-workspace" t)))))
  (let ((default-directory (expand-file-name dir)))
    (async-shell-command
     "mvn clean install -DskipTests=true")))


(defun anypoint-mvn-package ()
  (interactive)
  (let ((root-dir (find-mvn-root-path ".")))
    (if root-dir
	(async-shell-command (concat "mvn -U clean package -DskipTests=true -B -f " root-dir "/pom.xml"))
      (message "Not found pom.xml"))))

(defun anypoint-mvn-deploy ()
  (interactive)
  (let ((root-dir (find-mvn-root-path ".")))
    (if root-dir
	(async-shell-command (concat "mvn clean package deploy -DmuleDeploy -DskipTests=true -B -f " root-dir "/pom.xml"))
      (message "Not found pom.xml"))))


(defun anypoint-mvn-create-app (app)
  (interactive "sapp: ")
  (let ((default-directory (expand-file-name (concat "~/AnypointStudio/studio-workspace/" app))))
    (dired-create-directory default-directory)
    (async-shell-command
     (concat "mvn mule-project-archetype:create -DartifactId=" app  " -DmuleVersion=3.9.0"))))



(defun query-element (xpath xml-tree-alist-or-file-path)
  (if (stringp xml-tree-alist-or-file-path)
      (with-temp-buffer
	(insert-file-contents xml-tree-alist-or-file-path)
	(car (dom-children (esxml-query xpath (libxml-parse-xml-region (point-min) (point-max))))))
    (car (dom-children (esxml-query xpath (libxml-parse-xml-region (point-min) (point-max)))))))

(defun anypoint-exchange-delete-asset (asset)
  (interactive
   (let* ((str (read-string "search: " ))
	  (assets (anypoint-exchange-graphql-query str)))
     (list (completing-read "asset:" (->> (get-in '(data assets) assets)
					  (mapcar (lambda (a)
						    (let* ((org (car (rassoc (get-in '(groupId) a)
									     (anypoint-summary-organizations)))))
						      (when org
							(concat
							 (symbol-name org)
							 "/"
							 (get-in '(assetId) a)
							 ":"
							 (get-in '(version) a)
							 ":"
							 (get-in '(type) a))))))
					  (-filter (-compose #'not #'null)))))))
  (string-match "^\\(.*\\)/\\(.*\\):\\(.*\\):\\(.*\\)" asset)
  (let* ((org-id (get-in `(,(intern (match-string 1 asset))) (anypoint-summary-organizations)))
	 (asset-name (match-string 2 asset))
	 (version (match-string 3 asset))
	 (uri (format "https://anypoint.mulesoft.com/exchange/api/v2/assets/%s/%s/%s" org-id asset-name version)))
    (request uri
      :type "DELETE"
      :headers `(("Authorization" . ,(concat "Bearer " (get-in '(token access_token) anypoint-credential)))
		 ("X-Delete-Type" . "hard-delete"))
      :complete (cl-function
		 (lambda (&key data response &allow-other-keys)
		   (message "Completed: %d" (request-response-status-code response)))))))




(defun anypoint-exchange-publish-asset (org asset-type asset-name version)
  (interactive
   (list (completing-read "org: " (anypoint-summary-organizations))
	 (completing-read "type: " (list "raml"))
	 (read-string "name: " (progn
				 (string-match ".*/\\(.*\\)\\..*$" (buffer-file-name ))
				 (match-string 1 (buffer-file-name))))
	 (read-string "version: " "1.0.0")))
  
  (with-temp-buffer
    (let* ((org-id (get-in `(,(intern org)) (anypoint-summary-organizations)))
	   (raml-file-name (concat asset-name ".raml"))
	   (uri (concat "https://anypoint.mulesoft.com/exchange/api/v2/organizations/"
			org-id "/"
			"assets/"
			org-id "/"
			asset-name "/"
			version)))

      (info-insert-file-contents raml-file-name)
      
      (let* ((data `((name . ,asset-name)
		     (classifier . "raml")
		     (keywords . "raml, test")
		     ;; TO BE FIXED with reg exp
		     (properties.apiVersion . ,(progn
						 (string-match "^[0-9]+\\..*" version)
						 (concat "v" (match-string 1 version))))
		     (properties.mainFile . ,raml-file-name)))
	     (files `((files.raml.raml ,raml-file-name "application/octet-stream" ,(buffer-string))))
	     (multipart-data (http-post-encode-multipart-data data files 'utf-8)))
	
	(request uri
	  :type "POST"
	  :headers `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" (http-post-multipart-boundary)))
		     ("Authorization" . ,(concat "Bearer " (get-in '(token access_token) anypoint-credential)))
		     ("x-sync-publication" . true)
		     ("x-allowed-api-spec-formats" . "")
		     ("x-strict-package" . false))
	  :parser 'json-read
	  :data multipart-data)))))

(defun anypoint-exchange-publish-application (jar-path pom-path asset-type)
  (interactive
   (let* ((profiles (json-read-file "~/.anypoint/credentials"))
	  (pom-root-dir (find-mvn-root-path "."))
	  (regions (mapcar (lambda (x) (assoc-default 'id x)) (anypoint-get-regions))))
     (list (completing-read "jar: " (find-file-in-target-dir "jar" "."))
	   (read-string "pom: " (list (when pom-root-dir
					(concat pom-root-dir "/pom.xml"))))
	   (completing-read "type: " (list "application"
					   "application-example"
					   "application-template")))))
  
  (let* ((cred anypoint-credential)
	 (name (query-element "project>name" pom-path))
	 (type asset-type)
	 (desc "Published by Exchange API")
	 (org-id (query-element "project>groupId" pom-path))
	 (group-id org-id)
	 (asset-id (query-element "project>artifactId" pom-path))
	 (version (query-element "project>version" pom-path))
	 (uri (concat
	       "https://anypoint.mulesoft.com/exchange/api/v2/organizations/"
	       org-id
	       "/assets"
	       "/"  group-id
	       "/" asset-id
	       "/" version)))
    

    (message "Publishing: %s %s %s" group-id asset-id version)
    (request uri
       :type "POST"
       :headers `(("Content-Type" . "multipart/form-data")
		  ("Authorization" . ,(concat "Bearer " (get-in '(token access_token) cred)))
		  ("x-sync-publication" . true))
       :files `(("files.pom" . ("pom.xml" :file ,pom-path))
		;; This key is the value to choose the TYPE.
		;; Even though type is configured in pom.xml, NOTHING HAPPENS !
		(,(concat "files.mule-" asset-type ".jar") . ("file.jar" :file ,jar-path)))
       :parser 'json-read
       :error (cl-function (lambda (&rest args &key data error-thrown &allow-other-keys)
			     (message "error: %s %s"
				      (assoc-default 'status data )
				      (assoc-default 'message data))
			     data))
       :success (cl-function
		 (lambda (&key data &allow-other-keys)
		   (if (called-interactively-p)
		       (progn
			 (message "Published: %s %s %s" group-id asset-id version)
			 (anypoint-show data))
		     data))))))

(defun anypoint-select-org-env-interactively ()
  (interactive)
  (let* ((org (completing-read "org: " (mapcar 'car (anypoint-summary-organizations))))
	 (env (completing-read "env: " (assoc-default (intern org) (anypoint-summary-environments)))))
    (list (intern org) (intern env))))



(defun anypoint-change-application-state (org env app state)
  (let* ((cred anypoint-credential)
	 (env-id (get-in `(,org ,env) (anypoint-summary-environments))))
    (message "Starting: %s/%s:  %s:  %s" org env app state)
    (request (format "https://anypoint.mulesoft.com/cloudhub/api/applications/%s/status" app)
      :type "POST"
      :headers `(("Content-Type" . "application/json")
		 ("Authorization" . ,(concat "Bearer " (get-in '(token access_token) cred)))
		 ("x-anypnt-env-id" . ,env-id))
      :data (json-encode `(("status" . ,state)))
      :success (cl-function
		(lambda (&key response &allow-other-keys)
		  (message "Applied: %s" (request-response-status-code response)))))))

(defun anypoint-start-application (org env app)
  (interactive
   (let* ((org-env (anypoint-select-org-env-interactively))
	  (o (car org-env))
	  (e (cadr org-env))
	  (a (let ((as (->> (anypoint-get-applications o e)
			    (seq-filter (lambda (x)
					  (not (string= "STARTED"
							(assoc-default 'status x)))))
			    (-map (lambda (x)
				    (assoc-default 'domain x))))))
	       (if (= 0 (length as))
		   (error "No stopped app")
		   (completing-read "app: " as)))))
     
     `(,o ,e ,a)))
  (anypoint-change-application-state org env app "start"))

(defun anypoint-stop-application (org env app)
  (interactive
   (let* ((org-env (anypoint-select-org-env-interactively))
	  (o (car org-env))
	  (e (cadr org-env))
	  (a (let ((as (->> (anypoint-get-applications o e)
			    (seq-filter (lambda (x)
					  (string= "STARTED"
						   (assoc-default 'status x))))
			    (-map (lambda (x)
				    (assoc-default 'domain x))))))
	       (if (= 0 (length as))
		   (error "No running app")
		   (completing-read "app: " as)))))
     `(,o ,e ,a)))
  (anypoint-change-application-state org env app "stop"))


(defun anypoint-deploy-application (profile org env jar-path app-name region workers type)
  ;; This function assumes to be able to read .anypoint/credentials formatted as below.
  ;; In order upload, client id and secret of org are required.
  ;;
  ;;  {"some-profilename": {
  ;;     "org-client-id": "sample-178b6f42068f437e160a070eb5ab",
  ;;     "org-client-secret": "sample-3D5b3a4f64B14F37DEea419462ed"
  ;;    }
  ;;  }
  
  (interactive
   (let* ((profiles (json-read-file "~/.anypoint/credentials"))
	  (regions (mapcar (lambda (x) (assoc-default 'id x)) (anypoint-get-regions)))
	  (pom-root-dir (find-mvn-root-path "."))
	  (pom-path (concat pom-root-dir "/pom.xml"))
	  (org-env (anypoint-select-org-env-interactively)))
     (list (completing-read "credential: " (mapcar 'car profiles))
	   (car org-env)
	   (cadr org-env)
	   (completing-read "jar: " (find-file-in-target-dir "jar" "."))
	   (read-string "app name: " (concat "app-" (query-element "project>name" pom-path)))
	   (completing-read "region: " regions)
y	   (completing-read "workers: " '(("1") ("2") ("3") ("4")))
	   (completing-read "type: " '(("Micro") ("Medium"))))))


  (message "org: %s env: %s" org env)
  (let* ((profile (assoc-default (intern profile) (json-read-file "~/.anypoint/credentials")))
	 (cred anypoint-credential)
	 (env-id (get-in `(,org ,env) (anypoint-summary-environments)))
	 (file-name (file-name-nondirectory jar-path))
	 (project-path (find-mvn-root-path "."))
	 (client-id (assoc-default 'org-client-id profile))
	 (client-secret (assoc-default 'org-client-secret profile)))

    (if (and env-id client-id client-secret project-path app-name region workers type) 
	(progn
	  (message "Start uploading:  %s" `(,env-id ,client-id ,client-secret ,project-path ,app-name ,region ,workers ,type))
	  (message "Uploading...")
	  (with-temp-buffer
	    (insert (json-encode `(("persistentQueues" . :json-false)
				   ("objectStoreV1" . :json-false)
				   ("region" . ,region)
				   ("properties" . (("anypoint.platform.client_id" . ,client-id)
						    ("anypoint.platform.client_secret" . ,client-secret)
						    ("app.file" . ,file-name)))
				   ("domain" . ,app-name)
				   ("workers" . (("amount" . ,(string-to-number workers))
						 ("type" . (("name" . ,type))))))))
	    (request "https://anypoint.mulesoft.com/cloudhub/api/v2/applications"
	     :type "POST"
	     :headers `(("Content-Type" . "multipart/form-data")
			("Authorization" . ,(concat "Bearer " (get-in '(token access_token) cred)))
			("x-anypnt-env-id" . ,env-id))
	     :files `(("file" . (,file-name :file ,jar-path))
		      ;; request.el does not support multiple :data/:buffer entries.
		      ;; request--curl-command-args: request--curl-command-args: only one buffer or data entry permitted
		      ;; ("autoStart" . ("autoStart" :data :json-true))
		      ("appInfoJson" . ("appInfoJson" :buffer ,(current-buffer))))
	     :parser 'json-read
	     :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
				   (message "error %S" (assoc-default 'message args))))
	     :success (cl-function
		       (lambda (&key data &allow-other-keys)
			 (message "Uploaded successfully: %s" (assoc-default 'fullDomain data))
			 (anypoint-start-application org env app-name)
			 (message "Start %s" app-name))))))
      (progn
	(message "Current: %s\nParameter missing: env: %s c-id: %s c-sec: %s project: %s" (-map 'car (anypoint-summary-organizations)) env-id client-id client-secret project-path)))))

(defun anypoint-easy-deploy-application (profile-name)
  (interactive
   (list (completing-read "org: " (-map 'car (json-read-file "~/.anypoint/credentials")))))
  
  (let* ((profiles (json-read-file "~/.anypoint/credentials"))
	 (pom-root-dir (find-mvn-root-path "."))
	 (pom-path (concat pom-root-dir "/pom.xml"))
	 (app-name (concat (string (+ #x61 (random 10)) (+ #x61 (random 10)))
			   "-"
			   (query-element "project>name" pom-path)))
	 (jar-path (car (find-file-in-target-dir "jar" ".")))
	 (org-default (symbol-name (car (rassoc (query-element "project>groupId" pom-path) (anypoint-summary-organizations)))))
	 
	 (profile (assoc-default (intern profile-name) profiles))
	 (org (intern (assoc-default 'organization profile)))
	 (env (intern (assoc-default 'environment profile)))
	 (client-id (assoc-default 'org-client-id profile))
	 (client-secret (assoc-default 'org-client-secret profile))
	 (logging-p (assoc-default org (anypoint-summary-organizations))))

    (message (format "Use application name: %s" app-name))
    (if (and logging-p jar-path client-id client-secret)
	(anypoint-deploy-application profile-name
				     (or org org-default)
				     (or env "Sandbox")
				     jar-path
				     app-name
				     "ap-northeast-1"
				     "1"
				     "Micro")
      (message "Current: %s\njar-path: %s\nid/secret: %s"
	       (-map 'car (anypoint-summary-organizations))
	       jar-path
	       (if (or (null client-id) (null client-secret))
		   "No id or secret"
		 "found")))))


(defun anypoint-exchange-graphql-query (query-string)

  ;; Here is the document for graphql API
  ;; https://docs.mulesoft.com/jp/exchange/to-search-with-graph-api
  ;;
  ;; 
  ;; files {externalLink}
  ;;
  ;; Use (get-in '(data assets 0 files) assets) to get externallink to download

  (interactive "sQuery exchange: ")
  (string-match "^\\(\\+?\\)\\(.*\\)" (or query-string ""))
  
  (let* ((additional-attrs (if (string= "+" (match-string 1 query-string))
			       ", files {externalLink}"
			     ""))
	 (q (format "{ assets (query: {searchTerm: \"%s\" , organizationIds: [%s] }) { name, groupId, assetId, version, type %s }}"
		    (match-string 2 query-string)
		    (string-join (--map (concat "\"" (cdr it) "\"")
					(anypoint-summary-organizations)) ",")
		    additional-attrs))
	 (ci-p (called-interactively-p 'any))
	 (result-data nil))

    (request "https://anypoint.mulesoft.com/graph/api/v1/graphql"
      :type "POST"
      :headers `(("Content-Type" . "application/json")
		 ("Authorization" . ,(concat "Bearer " (get-in '(token access_token) anypoint-credential))))
      :parser 'json-read
      :data (json-encode `(("query" . ,q)))
      :error (cl-function (lambda (&rest args &key response data error-thrown response symbol-status
					 &allow-other-keys)
			    
			    ))
      :sync t
      :success (cl-function
		(lambda (&key response &allow-other-keys)
		  ;; (message "Done: %s" (request-response-status-code response))
		  (if ci-p
		      (anypoint-show (get-in '(data assets) (request-response-data response)))
		    (setq result-data (request-response-data response))))))
    result-data))


(defun anypoint-delete-application (org env)
  (interactive (anypoint-select-org-env-interactively))
  (let* ((app (completing-read "app: " (mapcar (lambda (x) (assoc-default 'domain x))
					       (anypoint-get-applications org env))))
	 (cred anypoint-credential)
	 (env-id (get-in `(,org ,env) (anypoint-summary-environments))))

    (message "Deleting: %s/%s:  %s" org env app)
    
    (request (format "https://anypoint.mulesoft.com/cloudhub/api/applications/%s" app)
      :type "DELETE"
      :headers `(("Content-Type" . "application/json")
		 ("Authorization" . ,(concat "Bearer " (get-in '(token access_token) cred)))
		 ("x-anypnt-env-id" . ,env-id))
      :data (json-encode '(("status" . "start")))
      :success (cl-function
		(lambda (&key response &allow-other-keys)
		  (message "Done: %s" (request-response-status-code response)))))))


(define-minor-mode anypoint-minor-mode
  "Minor mode to open functions for Anypoint Platform admin"
  :init-value nil
  :lighter " Anypoint"
  :keymap '(;; Info
	    ("\C-c\C-l" . anypoint-init-with-profile!)
	    ("\C-c\C-e" . anypoint-summary-environments)
	    ("\C-c\C-a" . anypoint-summary-applications)
	    ("\C-c\C-v" . anypoint-show-context)
	    ("\C-c\C-o" . anypoint-summary-organizations)

	    ;; Develop
	    ("\C-cmp" . anypoint-mvn-package)

	    ;; Deploy
	    ("\C-cD" . anypoint-easy-deploy-application)
	    ("\C-c\C-u" . anypoint-deploy-application)
	    
	    ;; Exchange
	    ("\C-cs" . anypoint-exchange-graphql-query)
	    ("\C-cep" . anypoint-exchange-publish-application)

	    ;; Utils
	    ("\C-hj" . anypoint-describe-variable-in-json))
  :group 'anypoint)

(defun quittable-kill-window ()
  (interactive)
  (kill-this-buffer)
  (delete-window (get-buffer-window (current-buffer))))

(define-minor-mode quittable-output-mode
  "Minor mode to "
  :init-value nil
  :lighter nil
  :keymap '(("q" . quittable-kill-window))
  :group 'quittable)

;; For REPL
(defun anypoint-summary-organizations ()
  (interactive)
  (let ((orgs (mapcar (lambda (x)
			(cons (intern (get-in '(name) x)) (get-in '(id) x)))
		      (get-in '(user memberOfOrganizations) anypoint-context))))
    (if (called-interactively-p 'any)
	(anypoint-show orgs)
      orgs)))



;; Get environemtns
(defun anypoint-get-environments (org-symbol)
  (interactive)
  (let* ((org-id (assoc-default org-symbol (anypoint-summary-organizations)))
	 (result nil))
    (message "Retrieving %s" org-symbol)
    (if org-id
	(progn
	  (http-get (format "https://anypoint.mulesoft.com/accounts/api/organizations/%s/environments" org-id)
		   :sync t	      
		   :callback (lambda (data)
			       (setq result (assoc-default 'data data))			       ))
	  (message "Retrieved %s" org-symbol)
	  result)
      (message "Invalid org id: %s" org-id))))

(defun anypoint-async-get-environments (org-symbol callback-fn)
  (let* ((org-id (assoc-default org-symbol (anypoint-summary-organizations))))
    (message "Retrieving %s" org-symbol)
    (if org-id
	(progn
	  (http-get (format "https://anypoint.mulesoft.com/accounts/api/organizations/%s/environments" org-id)
		   :callback callback-fn))
      (message "Invalid org id: %s" org-id))))


;; Get applications
;;   - org: 'Y1
;;   - env: 'Design

(defun anypoint-get-applications (org env)
  (interactive (anypoint-select-org-env-interactively))
  (let* ((ctx anypoint-context)
	 (result nil)
	 (env-id (get-in `(,org ,env)
				(anypoint-summary-environments))))
    
    (if env-id
	(progn
	  (http-get "https://anypoint.mulesoft.com/cloudhub/api/v2/applications"
		    :headers (append (make-default-header) `(("x-anypnt-env-id" . ,env-id)))
		    :sync t
		    :callback (lambda (data)
				(setq result data)))
	  (if (called-interactively-p 'any)
	      (anypoint-show result)
	    result))
      (message "No env id"))))


(defun anypoint-show-context ()
  (interactive)
  (anypoint-show anypoint-context))


(defun anypoint-summary-environments (&optional formatter)
  (interactive)
  (let* ((orgs (anypoint-summary-organizations))
	 (result (->> (assoc-default 'envs anypoint-context)
		      (mapcar (lambda (x)
				(cons `(,(assoc-default 'org x) ,(intern (assoc-default 'name x)))
				      (assoc-default 'id x))))
		      (mapcar '-cons-to-list)
		      (-group-by (lambda (x) (caar x)))
		      (-map (lambda (xss)
			      (let ((g (car xss)))
				(cons g (->> (cdr xss)
					     (-map (lambda (xs)
						     (let ((e (cadar xs)))
						       (cons  e  (cadr xs))))))))))
		      ;; (-map (lambda (xs)
		      ;; 	      (let ((sym (car xs)))
		      ;; 		(cons sym (cons (cons 'Org (assoc-default sym orgs)) (cdr xs) )))))
		      )))
    (if (called-interactively-p 'any)
	(anypoint-show  result :object)
      result)))



(defun anypoint-summary-applications (org env)
  (interactive (anypoint-select-org-env-interactively))
  
  (anypoint-get-applications org env)
  (if (called-interactively-p 'any)
      (->> (anypoint-get-applications org env)
	   (mapcar (lambda (app)
		     (list (assoc-default 'domain app)
			   (assoc 'fullDomain app)
			   (assoc 'status app)
			   (assoc 'fileName app))))
	   (anypoint-show))
    (anypoint-get-applications org env)))


(anypoint-minor-mode 1)
(add-hook 'java-mode-hook 'anypoint-minor-mode)

(provide 'anypoint)

