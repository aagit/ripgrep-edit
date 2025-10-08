;;; ripgrep-edit.el --- Invoke ripgrep-edit
;;; SPDX-License-Identifier: GPL-3.0-or-later OR AGPL-3.0-or-later

;; Copyright (C) 2025  Red Hat Inc.

;;; Commentary:

;; This module provides a function `ripgrep-edit` that invokes the
;; ripgrep-edit executable with the current region or word as the regexp
;; and the current buffer's directory as the path.
;; It requires ripgrep-edit to be installed and in PATH.

;;; Code:

(defcustom ripgrep-edit-executable "ripgrep-edit"
  "The ripgrep-edit executable to use."
  :type 'string
  :group 'ripgrep-edit)

(define-key global-map (kbd "C-x C-g") #'ripgrep-edit-git)

(defun ripgrep-edit--check-server ()
  "Check if the Emacs server is running and signal an error if not."
  (unless (server-running-p)
    (user-error "Emacs server is not running. Please start it with 'M-x server-start'")))

(defun ripgrep-edit--collect-extra-args ()
  "Collect extra arguments for ripgrep-edit command."
  (let ((extra-args (read-string "Extra args: ")))
    (unless (string-empty-p extra-args)
      extra-args)))

(defun ripgrep-edit--run-command (regexp path extra-args)
  "Run ripgrep-edit with REGEXP, PATH, and EXTRA-ARGS."
  (apply #'start-process "ripgrep-edit"
	 (get-buffer-create "*ripgrep-edit*")
	 ripgrep-edit-executable
	 "-e" regexp
	 "-p" path
	 "-E" "emacsclient"
	 (when extra-args
	   (split-string extra-args))))

(defun ripgrep-edit--get-path (path buffer-file)
  "Get the search path for ripgrep-edit, using PATH or buffer file location."
  (or path
      (if buffer-file
	  (file-name-directory buffer-file)
	default-directory)))

(defun ripgrep-edit--get-git-path (path buffer-file)
  "Get the git repository root as search path for ripgrep-edit, using PATH or buffer file location."
  (or path
      (if buffer-file
	  (let ((git-root (locate-dominating-file buffer-file ".git")))
	    (if git-root
		(file-name-directory git-root)
	      (file-name-directory buffer-file)))
	default-directory)))

(defun ripgrep-edit--invoke (regexp path extra-args get-path-fn)
  "Invoke ripgrep-edit with REGEXP, PATH, EXTRA-ARGS using GET-PATH-FN to determine the path."
  (ripgrep-edit--check-server)
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
	      (extra-args (ripgrep-edit--collect-extra-args)))
	  (ripgrep-edit--run-command search-regexp search-path extra-args))))))

(defun ripgrep-edit (&optional regexp path extra-args)
  "Invoke ripgrep-edit with REGEXP, PATH and EXTRA-ARGS."
  (interactive)
  (ripgrep-edit--invoke regexp path extra-args #'ripgrep-edit--get-path))

(defun ripgrep-edit-git (&optional regexp path extra-args)
  "Invoke ripgrep-edit-git with REGEXP, PATH and EXTRA-ARGS."
  (interactive)
  (ripgrep-edit--invoke regexp path extra-args #'ripgrep-edit--get-git-path))

(defun gptel-rewrite-ripgrep-edit ()
  (when (string-match-p "\.ripgrep-edit" (buffer-name))
    (concat "This is a cross-file edit:\n"
	    "- Rewrite everything exactly the same, "
	    "except: the required change\n"
	    "- Keep the filename at the start of each file\n"
	    "- Keep the separator at the end of each file section\n"
	    "- Do not add markdown fences\n"
	    "- Do not ask clarification")))
(add-hook 'gptel-rewrite-directives-hook #'gptel-rewrite-ripgrep-edit)

(provide 'ripgrep-edit)

;;; ripgrep-edit.el ends here
