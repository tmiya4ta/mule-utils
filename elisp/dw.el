;;; dw.el --- Dataweave major mode
;;
;; GPL2
;; https://www.gnu.org/licenses/old-licenses/gpl-2.0.txt

;; Author: Tsutomu Miyashita <myst3m@gmail.com>
;; Maintainer: Tsutomu Miyashita <myst3m@gmail.com>
;; Created: 16 Feb 2022
;; Keywords: Dataweave
;; Package-Version: 20220216.0001
;; Package-Commit: SNAPSHOT

;; This file is not part of GNU Emacs.


(require 'url)

;; Use only keymap
(require 'paredit)

(defconst dw-output-buffer "*dw-output*")

(defconst dw-version-regexp
  "\\(%dw[ \t][0-9.]+\\)")

(defface dw-version-face
  '((t (:inherit font-lock-warning-face)))
  "Face for version"
  :group 'dw-faces)



(defconst dw-special-variable-regexp
  "\\(payload\\|vars\\|attributes\\|\\$\\)")

(defface dw-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for variables"
  :group 'dw-faces)

(defconst dw-reserved-regexp
  "\\(if\\|else\\)")

(defface dw-reserved-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for reserved"
  :group 'dw-faces)

(defconst dw-comment-regexp
  "\\(//.*\\)")

(defface dw-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for Comments"
  :group 'dw-faces)


(defconst dw-keywords-regexp
  "\\(fun\\|input\\|output\\|import\\|from\\|var\\)")

(defface dw-keywords-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for Keywords"
  :group 'dw-faces)

(defconst dw-mime-regexp
  "\\(csv\\|xml\\|json\\)")

(defface dw-mime-face
  '((t (:inherit font-lock-type-face)))
  "Face for Keywords"
  :group 'dw-faces)

(defconst dw-defun-regexp
  "fun[ \t]+\\([^:=]+\\)(.*)[ \t]*:?[ \t]*")

(defface dw-defun-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for defun"
  :group 'dw-faces)


(defconst dw-defvar-regexp
  "var[ \t]+\\([^=: \t]+\\)")

(defface dw-defvar-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for defun"
  :group 'dw-faces)


(defconst dw-map-key-regexp
  "\\([a-zA-Z0-9_-]+\\):")

(defface dw-map-key-face
  '((t (:inherit font-lock-type-face)))
  "Face for defun"
  :group 'dw-faces)

(defconst dw-function-regexp
  "\\(\\w+\\)[ \t]*(.*)")

(defface dw-function-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for Function"
  :group 'dw-faces)

(defconst dw-class-regexp
  "[ :]\\([A-Z]\\w*\\)")

(defconst dw-mode-keywords
  (list 
   (list dw-keywords-regexp '(1 'dw-keywords-face))
   (list dw-mime-regexp '(1 'dw-mime-face))
   (list dw-version-regexp '(1 'dw-version-face))
   (list dw-special-variable-regexp '(1 'dw-variable-face))
   (list dw-defun-regexp '(1 'dw-defun-face))
   (list dw-defvar-regexp '(1 'dw-defvar-face))
   (list dw-map-key-regexp '(1 'dw-map-key-face))
   (list dw-reserved-regexp '(1 'dw-reserved-face))
   (list dw-function-regexp '(1 'dw-function-face))
   (list dw-class-regexp '(1 'dw-mime-face))))


(defvar dw-cli-version "1.0.24"
  "Dataweve CLI version")

(defvar dw-cli-download-path (concat "https://github.com/mulesoft-labs/data-weave-cli/releases/download/v" dw-cli-version "/")
  "Dataweave CLI download URL")

(defvar dw-cli-download-file (concat
			      dw-cli-download-path
			      "dw-" dw-cli-version
			      (cond 
			       ((string-match-p "linux" (symbol-name system-type))
				"-Linux")
			       ((string-match-p "darwin" (symbol-name system-type))
				"-macOS")
			       ((string-match-p "windows" (symbol-name system-type))
				".exe")
			       ((string-match-p "cygwin" (symbol-name system-type))
				".exe"))))

(defun dw-download-cli ()
  (interactive)
  (url-retrieve
   dw-cli-download-file
   (lambda (s)
     (prin1 s)
     (let* ((target-dir (expand-file-name "~/.dw"))
	    (tmp-file-path (if (string-match "\\.exe$" dw-cli-download-file)
			       (concat target-dir "/dw.exe" )
			     (concat target-dir "/dw.zip"))))
       (prin1 tmp-file-path)
       (shell-command (concat "rm -f" tmp-file-path))
       (ignore-errors
	 (make-directory target-dir)
	 (write-region (point) (point-max) tmp-file-path))
       (re-search-forward "\r?\n\r?\n")
       (when (string-match "\\.zip$" tmp-file-path)
	 (shell-command (concat "unzip " "-b " "-o " tmp-file-path " -d " target-dir)))))))

(setq max-mini-window-height 0)

(defvar dw-cli-path "~/.dw/bin/dw"
  "Install path of CLI")

(defconst dw-request-regexp "^//![ *]\\(.*\\)$")
(defconst dw-only-http-request-regexp "^//\\*\\*\\*")
(defconst dw-source-regexp "\\(GET\\|POST\\|FILE\\)[ *]\\(.*\\)")
(defconst dw-header-regexp
  "\\([^](),/:;@[\\{}= \t\n]+\\): \\([^}\r\n]+\\)$")

(defun dw-only-http-request-p ()
  (save-excursion
    (ignore-errors
      (beginning-of-buffer)
    (re-search-forward dw-only-http-request-regexp))))

(defun dw-parse-request ()
  (interactive)
  (goto-char (point-min))
  (let ((start 0)
	(request ""))
    (while (re-search-forward dw-request-regexp nil t)
      (setq request (concat request (match-string-no-properties 1) "\n"))
      (setq start (match-end 0)))
    request))

(defun dw-parse-source (string)
  (string-match dw-source-regexp string)
  (cons (match-string-no-properties 1 string)
	(match-string-no-properties 2 string)))


(defun dw-parse-headers-body (string)
  (interactive)
  (let* ((start 0)
         (headers '())
	 ;; Skip the first line.
	 ;; Assume the first line is source.
	 (header-list (cdr (split-string string "\n")))
	 (body "")) 
    (while header-list
      (if (string-match dw-header-regexp (car header-list))
	  (progn
	    (let ((header (cons (match-string-no-properties 1 (car header-list))
				(match-string-no-properties 2 (car header-list)))))
	      (setq headers (cons header headers)
		    header-list (cdr header-list))))
	(progn
	  (setq body (string-join header-list "\n")
		header-list '()))))
    (cons headers body)))


(defconst dw-default-output-code
  (prin1-to-string "%dw 2.0 output json --- payload"))

(defun dw-build-command ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((request (dw-parse-request))
	   (src (dw-parse-source request))
	   (method (car src))
	   (path (cdr src))
	   (hb (dw-parse-headers-body request))
	   (headers (car hb))
	   (data (cdr hb)))
      (cond
       ((or (string= method "GET") (string= method "POST"))
	;; dynamic bind
	(let* ((url-request-method method) 
	       (url-request-extra-headers headers)
	       (url-request-data data)
	       (code (if (dw-only-http-request-p)
			 dw-default-output-code
			 (prin1-to-string
			  (buffer-substring-no-properties (point-min) (point-max)))))
	       (buf (url-retrieve-synchronously path)))
	  (save-window-excursion
	    (switch-to-buffer buf)
	    (goto-char (point-min))
	    (re-search-forward "\r?\n\r?\n")
	    (let ((payload (buffer-substring-no-properties (point) (point-max))))
	      (format "echo %s | %s %s"
		      (prin1-to-string payload)
		      (expand-file-name dw-cli-path)
		      code)))))
       ((string= method "FILE")
	(format "cat %s | %s %s"
		(string-trim path)
		(expand-file-name dw-cli-path)
		(prin1-to-string
		 (buffer-substring-no-properties (point-min) (point-max)))))
       ((null method)
	(format "%s %s"
		(expand-file-name dw-cli-path)
		(prin1-to-string
		 (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun dw-in-string-p ()
  (interactive)
  (let ((point (point)))
    (beginning-of-defun)
    (and (nth 3 (parse-partial-sexp (point) point))
	 t)))

(defun dw-skip-map-item ()
  (interactive)
  (re-search-forward ","))

(defun dw-point-char (&optional p)
  (interactive)
  (string-to-char (buffer-substring-no-properties (or p (point)) (+ 1 (or p (point))))))

(defun dw-grab-map ()
  (interactive)
  (save-excursion
    (let ((pos (point)))
      (re-search-backward "{")
      (let ((open-brace-pos (point))
	    (close-brace-pos (ignore-errors
			       (forward-sexp)
			       (point))))
	(cons open-brace-pos close-brace-pos)))))

(defun dw-braces-on-same-line-p ()
  (interactive)
  (let ((m (dw-grab-map)))
    (not (and (string-match-p "\n" (buffer-substring-no-properties (car m) (cdr m)))
	      t))))

(defun dw-in-map-p ()
  (interactive)
  (let* ((pos (point))
	 (m (dw-grab-map))
	 (open-brace-pos (car m))
	 (close-brace-pos (cdr m))
	 (most-recent-open-brace-candidate (nth 1 (parse-partial-sexp open-brace-pos pos))))
    (and pos open-brace-pos close-brace-pos
	 (eql most-recent-open-brace-candidate open-brace-pos)
	 (<= open-brace-pos pos close-brace-pos))))

(defun dw-parse-map ()
  (interactive)
  (let ((p (point)))
    (re-search-backward "{")
    (let* ((state (parse-partial-sexp (point) p))
	   (brace-pos (nth 1 state)))
      (goto-char brace-pos)
      (goto-char (+ 1 brace-pos))
      (newline)
      (goto-char brace-pos)
      (mark-sexp)
      (replace-string-in-region "," ",\n")
      (forward-sexp)
      (indent-region brace-pos (point))
      (goto-char (- (point) 1))
      (newline-and-indent)
      (previous-line)
      (end-of-line)
      (indent-for-tab-command))))


(defun dw-new-line ()
  (interactive)
  (cond
   ((dw-in-string-p)
    (newline))
   ((and (dw-in-map-p) (dw-braces-on-same-line-p))
    (dw-parse-map))
   (t (progn
	(newline-and-indent)
	(beginning-of-line)
	(indent-for-tab-command)))))

(defvar dw-use-paredit-keymap t)

(defvar dw-mode-map
  (let ((map (if dw-use-paredit-keymap
		 (copy-keymap paredit-mode-map)
	       (make-keymap))))
    (bind-keys :map map
	       ((kbd "C-c C-c") . dw-run-with-input-file)
	       ((kbd "C-j") . dw-new-line)
	       ((kbd "{") . (lambda ()
			      (interactive)
			      (insert "{}")
			      (backward-char))))
    map))


(defun dw-run-with-input-file ()
  (interactive)
  (save-excursion
    (shell-command (dw-build-command) dw-output-buffer)
    (save-current-buffer
      (switch-to-buffer-other-window dw-output-buffer t)
      (dw-output-mode))))




(require 'smie)

(defconst bnf-json
  '((json-id)
    (null-value ("null"))
    (boolean-value ("true") ("false"))
    (string-value (json-id))
    (number-value (json-id))
    (value (null-value) (boolean-value) (string-value) (number-value) (object-value) (array-value))
    (object-value ("{" "}") ("{" member-list "}"))
    (member (json-id ":" value))
    (member-list (member) (member-list "," member))
    (array-value ("[" "]") ("[" element-list "]"))
    (element-list (value) (element-list "," value)))
  "BNF JSON grammer. Just sample")

(defconst smie-dataweave-grammer
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (document (header "---" body))
      (header (id))
      (body (id)))))
  "Dataweave Grammer")


(defun dataweave-smie-rules (kind token)
  (pcase (cons kind token)
    ;; base indent step
    ('(:list-intro . "") 0)
    ('(:elem . basic) 4)))


;; (defun dataweave-smie-forward-token ()
;;   (forward-comment (point-max))
;;   (buffer-substring-no-properties
;;    (point)
;;    (progn
;;      (skip-syntax-forward "w")
;;      (point))))


;; (defun dataweave-smie-backward-token ()
;;   (forward-comment (- (point)))
;;   (buffer-substring-no-properties
;;    (point)
;;    (progn
;;      (skip-syntax-backward "w")
;;      (point))))

(defun dw-kill-this-window ()
  (interactive)
  (quit-window (get-buffer-window (current-buffer))))

(defconst dw-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode dw-mode prog-mode "Dataweave"
  "Major mode for Dataweave"
  (set (make-local-variable 'font-lock-defaults) '(dw-mode-keywords))
  (set-syntax-table dw-mode-syntax-table)
  (setq comment-start "//")
  (smie-setup smie-dataweave-grammer
	      #'dataweave-smie-rules
	      ;; :forward-token #'dataweave-smie-forward-token
	      ;; :backward-token  #'dataweave-smie-backward-token
	      )
  :group 'dw)

(define-minor-mode dw-output-mode
  "Minor mode to allow additional keybindings in dataweave output buffer."
  :init-value nil
  :lighter nil
  :keymap '(("q" . dw-kill-this-window))
  :group 'dw)

(add-to-list 'auto-mode-alist '("\\(\\.dwl\\|\\.dw\\)" . dw-mode))

(define-keymap :keymap dw-mode-map)


(provide 'dw)
