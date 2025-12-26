;;; rg-edit.el --- Invoke ripgrep-edit
;;; SPDX-License-Identifier: GPL-3.0-or-later OR AGPL-3.0-or-later

;; Copyright (C) 2025  Red Hat Inc.

;;; Commentary:

;; This module provides helper functions to invoke the `rg-edit` executable
;; with the current region or word as the search pattern. It requires the `rg-edit`
;; binary to be installed and accessible in the system PATH or configured via
;; `rg-edit-executable`.

;;; Example configuration:

;; (add-to-list 'load-path "~/.../ripgrep-edit/emacs/")
;; (use-package rg-edit
;;   :ensure nil
;;   :config
;;   (setq-default rg-edit-auto-mark-whole-buffer t)
;;   (setq-default rg-edit-executable "~/.../ripgrep-edit/target/release/rg-edit"))

;;; Code:

(defcustom rg-edit-executable "rg-edit"
  "The rg-edit executable to use."
  :type 'string
  :group 'rg-edit)

(defcustom rg-edit-gptel-system-message
  (concat "You are a careful programmer. Rewrite cross-file snippets.\n"
	  "Rewrite everything exactly the same except: the required change.\n"
	  "Keep the filenames at the start of the files.\n"
	  "Keep the separators at the end of the snippets.\n"
	  "Do not delete the filenames and the separators.\n"
	  "Do not add markdown fences.\n"
	  "Do not ask clarification.")
  "The rg-edit gptel-system-message to use when in a rg-edit buffer."
  :type 'string
  :group 'rg-edit)

(defcustom rg-edit-auto-mark-whole-buffer nil
  "If non-nil, mark the whole buffer when entering rg-edit-mode.
This prepares the buffer for AI rewriting by selecting all content."
  :type 'boolean
  :group 'rg-edit)

(defvar rg-edit--history-regexp nil "History for rg-edit regexp patterns.")
(defvar rg-edit--history-extra-args nil "History for rg-edit extra-args.")

(define-key global-map (kbd "C-c r") #'rg-edit-git)

(defun rg-edit--check-server ()
  "Check if the Emacs server is running and signal an error if not."
  (unless (server-running-p)
    (user-error "Emacs server is not running. Please start it with 'M-x server-start'")))

(defun rg-edit--collect-extra-args (extra-args)
  "Collect extra arguments for rg-edit command."
  (let ((extra-args (read-string "Extra args: " extra-args 'rg-edit--history-extra-args)))
    (unless (string-empty-p extra-args)
      extra-args)))

(defun rg-edit--terminate ()
  "Terminate all outstanding rg-edit processes by closing the *rg-edit* buffer."
  (interactive)
  (kill-buffer))

(defun rg-edit--setup-buffer (rg-buffer)
  "Setup the *rg-edit* buffer before running rg-edit."
  (switch-to-buffer rg-buffer)
  (read-only-mode -1)
  (erase-buffer)
  (goto-char (point-min))
  (unless (current-local-map)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd "C-c C-k") #'rg-edit--terminate)
      (use-local-map map)))
  (insert "C-c C-k on this buffer will terminate any outstanding rg-edit\n")
  (when (and (fboundp 'gptel--model-capable-p)
	     (gptel--model-capable-p 'gbnf))
    (insert "GBNF Grammar Enabled\n")))

(defun rg-edit--run-command (regexp path extra-args)
  "Run rg-edit with REGEXP, PATH, and EXTRA-ARGS."
  (let* ((path-dir (directory-file-name path))
	 (default-directory (file-name-directory path-dir))
	 (dir-name (file-name-nondirectory path-dir))
	 (rg-buffer (get-buffer-create "*rg-edit*" t)))
    (save-some-buffers
     nil (lambda () (string-prefix-p (file-truename dir-name)
				     (file-truename (buffer-file-name)))))
    (rg-edit--setup-buffer rg-buffer)
    (apply #'start-process "rg-edit"
	   rg-buffer
	   rg-edit-executable
	   "-e" regexp
	   "-E" "emacsclient"
	   "--dump-on-error"
	   (if (and (fboundp 'gptel--model-capable-p)
		      (gptel--model-capable-p 'gbnf))
	       "--gbnf" "")
	   (shell-quote-argument dir-name)
	   (when extra-args
	     (split-string-shell-command extra-args)))))

(defun rg-edit--get-path (path buffer-file)
  "Get the search path for rg-edit, using PATH or buffer file location."
  (or path
      (if buffer-file
	  (file-name-directory buffer-file)
	default-directory)))

(defun rg-edit--get-git-path (path buffer-file)
  "Get the git repository root as search path for rg-edit, using PATH or buffer file location."
  (or path
      (if buffer-file
	  (let ((git-root (locate-dominating-file buffer-file ".git")))
	    (if git-root
		(file-name-directory git-root)
	      (file-name-directory buffer-file)))
	default-directory)))

(defun rg-edit--invoke (regexp path extra-args get-path-fn refine-path)
  "Invoke rg-edit with REGEXP, PATH, EXTRA-ARGS using GET-PATH-FN to determine the path."
  (rg-edit--check-server)
  (rg-edit--warn-if-auto-revert-disabled)
  (let ((search-regexp (or regexp
			   (if (use-region-p)
			       (buffer-substring-no-properties (region-beginning) (region-end))
			     (or (current-word) "")))))
    (let ((search-regexp (read-string "Regexp: " search-regexp 'rg-edit--history-regexp))
	  (buffer-file (buffer-file-name)))
      (unless (and (stringp search-regexp)
		   (> (length search-regexp) 0))
	(user-error "No regexp provided"))
      (let ((search-path (funcall get-path-fn path buffer-file)))
	(let ((search-path (expand-file-name (if (or refine-path (null buffer-file))
						 (read-directory-name "Path: " search-path)
					       search-path)))
	      (extra-args (rg-edit--collect-extra-args extra-args)))
	  (rg-edit--run-command search-regexp search-path extra-args))))))

(defun rg-edit ()
  "Invoke rg-edit with the path in the current directory."
  (interactive)
  (rg-edit--invoke nil nil nil #'rg-edit--get-path t))

(defun rg-edit-git-conflicts ()
  "Invoke rg-edit-git with regex and extra-args preset to edit git conflicts."
  (interactive)
  (rg-edit--invoke "(?s)^<<<<<<<+ .*?^>>>>>>>+ " nil "-U" #'rg-edit--get-git-path nil))

(defun rg-edit-git (&optional arg)
  "Invoke rg-edit-git with the search path set in the git root.

With a C-u prefix argument invoke rg-edit-git-conflicts instead."
  (interactive "p")
  (cond
   ((= arg 1) (rg-edit--invoke nil nil nil #'rg-edit--get-git-path nil))
   ((= arg 4) (rg-edit-git-conflicts))))

(defun rg-edit--warn-if-auto-revert-disabled ()
  "Warn if automatic file revert is not enabled."
  (unless (bound-and-true-p global-auto-revert-mode)
    (display-warning 'rg-edit
		     "Automatic file revert is not enabled. Consider enabling it with `M-x global-auto-revert-mode`."
		    :warning)))

(defun rg-edit--gptel-rewrite ()
  (when (and (boundp 'gptel-version)
             (string-match-p "\\.rg.edit\\'" (buffer-name)))
    rg-edit-gptel-system-message))
(add-hook 'gptel-rewrite-directives-hook #'rg-edit--gptel-rewrite)

(defun rg-edit--setup-gptel-directives ()
  "Set up gptel directives for rg-edit."
  (when (boundp 'gptel-directives)
    (setf (alist-get 'rg-edit gptel-directives)
          rg-edit-gptel-system-message)))

(defun rg-edit--setup-gptel--rewrite-directive ()
  ;; workaround for gptel issue in pull request #1095
  ;; fixed in 129032fca88f29c20343e973c98fa17db3000405 and
  ;; the following commit f4344b8a7950fd6b969b32f84f0fe427a9bc925b
  ;; introduced gptel-version
  (unless (boundp 'gptel-version)
    (setq-local gptel--rewrite-directive
		rg-edit-gptel-system-message)))

(defun rg-edit--kill-buffer-hook ()
  "Interrupt any in-flight gptel-send request when the buffer is closed."
  (when (fboundp 'gptel-abort)
    (gptel-abort (current-buffer))))

(defun rg-edit--gbnf ()
  "Read the .gbnf file and inject its contents into the JSON as the 'gbnf' field."
  (when (and (fboundp 'gptel--model-capable-p)
	     (gptel--model-capable-p 'gbnf))
    (when-let* ((buffer-file (buffer-file-name))
                (gbnf-path (concat buffer-file ".gbnf"))
                (gbnf-content (when (file-exists-p gbnf-path)
                                (with-temp-buffer
                                  (insert-file-contents gbnf-path)
                                  (buffer-string)))))
      (when gbnf-content
        (setq-local gptel--request-params
                    (plist-put gptel--request-params :grammar gbnf-content))))))

(defun rg-edit--commit ()
  "Commit changes made in the rg-edit buffer."
  (interactive)
  (save-buffer)
  (kill-buffer))

(defun rg-edit--abort ()
  "Abort changes made in the rg-edit buffer."
  (interactive)
  (erase-buffer)
  (rg-edit--commit))

(defun rg-edit-mode ()
  (let ((rg-buffer (get-buffer "*rg-edit*")))
    (with-current-buffer rg-buffer
      (bury-buffer)))
  (prog-mode)
  (rg-edit--setup-gptel-directives)
  (rg-edit--setup-gptel--rewrite-directive)
  (add-hook 'kill-buffer-hook #'rg-edit--kill-buffer-hook nil t)
  (rg-edit--gbnf)
  (when rg-edit-auto-mark-whole-buffer
    (mark-whole-buffer))
  (define-key (current-local-map) (kbd "C-c C-c") #'rg-edit--commit)
  (define-key (current-local-map) (kbd "C-c C-k") #'rg-edit--abort)
  (setq-local server-client-instructions nil)
  (message "When done C-c C-c to commit, C-c C-k to abort or C-x # to manually close the session"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rg-edit\\'" . rg-edit-mode))

(provide 'rg-edit)

;;; rg-edit.el ends here
