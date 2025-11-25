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

(defcustom rg-edit-workaround-gptel-rewrite-directives-hook t
  "Enable workaround for gptel-rewrite-directives-hook regression

Due to a regression introduced in gptel commit
89d1b4768d9ab1d2624cb68d826f2134cb4c067e, the
gptel-rewrite-directives-hook dynamic prompt appears functional (TAB
after gptel-rewrite shows the expected prompt), but the generic prompt
is silently used instead. This can be confirmed using Inspect (`I` key
with gptel-expert-commands enabled). rg-edit relies on a precise
prompt format, and this regression may falsely suggest that the LLM
cannot handle the rg-edit structure when in reality it's a gptel bug.

The issue originated in gptel v0.9.8.5 and persists. A tentative fix
was submitted in pull request #1095.

The rg-edit-workaround-gptel-rewrite-directives-hook setting disables
gptel-rewrite-directives-hook and instead directly sets
gptel--rewrite-directive in the local rg-edit buffer.

Although gptel--rewrite-directive is documented as internal-only, it
is functional, while the intended hook mechanism fails. Enabling this
workaround ensures that rg-edit is processed with the correct system
prompt, preserving functionality until the upstream issue is
resolved."
  :type 'boolean
  :group 'rg-edit)

(define-key global-map (kbd "C-c r") #'rg-edit-git)

(defun rg-edit--check-server ()
  "Check if the Emacs server is running and signal an error if not."
  (unless (server-running-p)
    (user-error "Emacs server is not running. Please start it with 'M-x server-start'")))

(defun rg-edit--collect-extra-args (extra-args)
  "Collect extra arguments for rg-edit command."
  (let ((extra-args (read-string "Extra args: " extra-args)))
    (unless (string-empty-p extra-args)
      extra-args)))

(defun rg-edit--cleanup-buffer (buffer)
  "Clean up the *rg-edit* buffer before running rg-edit."
  (with-current-buffer buffer
    (erase-buffer)
    (goto-char (point-min))))

(defun rg-edit--run-command (regexp path extra-args)
  "Run rg-edit with REGEXP, PATH, and EXTRA-ARGS."
  (let* ((path-dir (directory-file-name path))
	 (default-directory (file-name-directory path-dir))
	 (dir-name (file-name-nondirectory path-dir))
	 (process-buffer (get-buffer-create "*rg-edit*")))
    (save-some-buffers
     nil (lambda () (string-prefix-p (file-truename dir-name)
				     (file-truename (buffer-file-name)))))
    (rg-edit--cleanup-buffer process-buffer)
    (apply #'start-process "rg-edit"
	   process-buffer
	   rg-edit-executable
	   "-e" regexp
	   "-E" "emacsclient"
	   "--dump-on-error"
	   dir-name
	   (when extra-args
	     (split-string extra-args)))))

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

(defun rg-edit--invoke (regexp path extra-args get-path-fn)
  "Invoke rg-edit with REGEXP, PATH, EXTRA-ARGS using GET-PATH-FN to determine the path."
  (rg-edit--check-server)
  (rg-edit--warn-if-auto-revert-disabled)
  (let ((search-regexp (or regexp
			   (if (use-region-p)
			       (buffer-substring-no-properties (region-beginning) (region-end))
			     (or (current-word) "")))))
    (unless search-regexp
      (user-error "No regexp provided or region/word to use"))
    (let ((search-regexp (read-string "Regexp: " search-regexp))
	  (buffer-file (buffer-file-name)))
      (let ((search-path (funcall get-path-fn path buffer-file)))
	(let ((search-path (expand-file-name (read-directory-name "Path: " search-path)))
	      (extra-args (rg-edit--collect-extra-args extra-args)))
	  (rg-edit--run-command search-regexp search-path extra-args))))))

(defun rg-edit ()
  "Invoke rg-edit with the path in the current directory."
  (interactive)
  (rg-edit--invoke nil nil nil #'rg-edit--get-path))

(defun rg-edit-git-conflicts ()
  "Invoke rg-edit-git with regex and extra-args preset to edit git conflicts."
  (interactive)
  (rg-edit--invoke "(?s)^<<<<<<<+ .*?^>>>>>>>+ " nil "-U" #'rg-edit--get-git-path))

(defun rg-edit-git (&optional arg)
  "Invoke rg-edit-git with the search path set in the git root.

With a C-u prefix argument invoke rg-edit-git-conflicts instead."
  (interactive "p")
  (cond
   ((= arg 1) (rg-edit--invoke nil nil nil #'rg-edit--get-git-path))
   ((= arg 4) (rg-edit-git-conflicts))))

(defun rg-edit--warn-if-auto-revert-disabled ()
  "Warn if automatic file revert is not enabled."
  (unless (bound-and-true-p global-auto-revert-mode)
    (display-warning 'rg-edit
		     "Automatic file revert is not enabled. Consider enabling it with `M-x global-auto-revert-mode`."
		    :warning)))

(defun rg-edit--gptel-rewrite ()
  (when (and (not rg-edit-workaround-gptel-rewrite-directives-hook)
             (string-match-p "\\.rg.edit\\'" (buffer-name)))
    rg-edit-gptel-system-message))
(add-hook 'gptel-rewrite-directives-hook #'rg-edit--gptel-rewrite)

(defun rg-edit--setup-gptel-directives ()
  "Set up gptel directives for rg-edit."
  (when (boundp 'gptel-directives)
    (setf (alist-get 'rg-edit gptel-directives)
          rg-edit-gptel-system-message)))

(defun rg-edit--setup-gptel--rewrite-directive ()
  (when rg-edit-workaround-gptel-rewrite-directives-hook
    ;; workaround for gptel issue in pull request #1095
    (setq-local gptel--rewrite-directive
		rg-edit-gptel-system-message)))

(defun rg-edit-mode ()
  (prog-mode)
  (rg-edit--setup-gptel-directives)
  (rg-edit--setup-gptel--rewrite-directive)
  (when rg-edit-auto-mark-whole-buffer
    (mark-whole-buffer)))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rg-edit\\'" . rg-edit-mode))

(provide 'rg-edit)

;;; rg-edit.el ends here
