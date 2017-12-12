;; Python

(require 'python)
;; (defun py-my-indent-region (&optional min max)
;;   "Stupidly clamp indentation to the closest multiple of 4 spaces."
;;   (interactive)
;;   (save-excursion
;;     (let ((top (or min (point-min)))
;; 	  (bottom (or max (point-max)))
;; 	  (line-move-visual nil))
;;       (goto-char top)
;;       (while (<= (point) bottom)
;; 	(indent-line-to
;; 	 (* 4 (round (/ (float (current-indentation)) 4))))
;; 	(next-line) (end-of-line)))))


(add-to-list 'auto-mode-alist '("\\.djhtml$" . web-mode))
(defun my/python-mode-hook ()
  (define-key python-mode-map (kbd "C-c C-t")
    'fd-python-jump-between-test-and-implementation)
  (define-key python-mode-map (kbd "C-c M-t") 'fd-python-run-tests)
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (define-key python-mode-map (kbd "M-q") 'python-smart-fill-paragraph)
  (local-unset-key (kbd "<backtab>"))
  (setq jedi:setup-function nil
        jedi:mode-function nil)
  (jedi:setup)
  (add-to-list 'company-backends 'company-jedi)
  ;; Fix docstring paragraph filling
  (setq paragraph-start (concat paragraph-start "\\|\\s-*\"\"\".*$")
        python-fill-docstring-style 'pep-257))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq-default python-shell-virtualenv-path "~/bin/py")
(setq-default python-shell-virtualenv-root "~/bin/py")

(defun fd-venv-path (path)
  (interactive (list (read-directory-name "Virtualenv root: ")))
  (add-dir-local-variable 'python-mode 'python-shell-virtualenv-root path)
  (add-dir-local-variable 'python-mode 'python-shell-virtualenv-path path))

(defun fd-python-project-root (&optional path)
  (locate-dominating-file (or path (buffer-file-name)) "setup.py"))

(defun fd-python-project-tests-dir (&optional root-dir)
  (concat (file-name-as-directory (or root-dir (fd-python-project-root)))
	  "tests"))

(defun fd-python-jump-to-test ()
  "Assume that we are in the same filename only with the test_
prefix."
  (find-file (format "%stest_%s.py"
		     (file-name-as-directory
		      (fd-python-project-tests-dir))
		     (file-name-base))))

(defun fd-python-source-dir (&optional root-dir)
  (format "%s/%s"
	  (or root-dir (fd-python-project-root))
	  (downcase
	   (file-name-nondirectory
	    (directory-file-name
	     (or root-dir (fd-python-project-root)))))))

(defun fd-python-jump-to-implementation ()
  "I assume I am in a test."
  (find-file (format "%s/%s.py"
		     (fd-python-source-dir)
		     (replace-regexp-in-string "^test_" "" (file-name-base)))))

(defun fd-python-in-test-p ()
  "If the file is called 'test_<name>.py' we are in."
  (string-match-p "/test_[^/]*\\.py$" (buffer-file-name)))

(defun fd-python-jump-between-test-and-implementation ()
  (interactive)
  (if (fd-python-in-test-p)
      (fd-python-jump-to-implementation)
    (fd-python-jump-to-test)))

(defun fd-python-command (&optional venv)
  "Calculate the string used to execute the inferior Python process."
  (if venv (format "%s/bin/python" venv)
    (let ((process-environment (python-shell-calculate-process-environment))
	  (exec-path (python-shell-calculate-exec-path)))
      (executable-find python-shell-interpreter))))

(defun fd-python-current-class-path ()
  (save-excursion
    (let ((cls (progn (re-search-backward "^class *\\(Test.*\\)(" nil t)
		      (match-string-no-properties 1)))
	  (module (replace-regexp-in-string
		   "/" "."
		   (file-relative-name
		    (concat (file-name-directory (buffer-file-name))
			    (file-name-base (buffer-file-name)))
		    (fd-python-project-root)))))
      (if cls
	  (concat module "." cls)
	""))))

(defun modtime (f)
  (nth 5 (file-attributes f)))

(defun fd-python-in-project-p (path &optional prj-path)
  "Non nil if PATH is in project defined by PRJ_PATH. If PRJ_PATH
is nil check if path in any project."
  (or
   (file-exists-p (concat (file-name-as-directory path) "setup.py"))
   (if prj-path
       (let ((prj-root (fd-python-project-root prj-path)))
	 (when prj-root
	   (s-starts-with-p (expand-file-name prj-root)
			    (expand-file-name path))))
     (fd-python-project-root path))))

(defun fd-python-open-project-internal (dir)
  (dolist  (f (sort
	       (append (directory-files
			(fd-python-project-tests-dir dir) t ".*\.py$")
		       (directory-files
			(fd-python-source-dir dir) t ".*\.py$"))
	       (lambda (f1 f2) (time-less-p (modtime f1) (modtime f2)))))
    (find-file f)))

(defvar fd-python-projects-directory "~/Projects/CSAIL/Python/"
  "The directory where you have most of your python projects.")

(defun fd-python-open-project (dir)
  (interactive (list (read-directory-name
		      "Python project root to open: "
		      fd-python-projects-directory)))
  (when (not (fd-python-in-project-p dir))
    (error "Not a python project"))

  (if (fd-python-in-project-p dir)
      (save-excursion
	(fd-python-open-project-internal dir))
    (fd-python-open-project-internal dir)))

(defun fd-python-close-project (dir)
  (interactive (list (read-directory-name
		      "Python project root to close: ")))
  (when (not (fd-python-in-project-p dir))
    (error "Could not close project. Not a project directory"))

  (dolist (b (buffer-list))
    (when (and (buffer-file-name b)
	       (fd-python-in-project-p (buffer-file-name b)
				       dir))
      (kill-buffer b))))

(defvar fd-setup-test-cmd "test"
  "This might even 'nosetests'.")

(defvar fd-test-venv nil
  "If non-nil use this for tests.")

(defvar fd-python-pre-test-fn nil
  "Run this before you run a test, useful if you need to commit
  or sth.")

(defun fd-flake8-cmd ()
  (format "%s %s setup.py %s %s"
	  (fd-python-command fd-test-venv "flake8")
	  fd-setup-test-cmd
	  (if (fd-python-in-test-p)
	      (save-excursion
		(end-of-buffer)
		(format "-s %s" (fd-python-current-class-path)))
	    "")))

(defun fd-test-cmd (&optional module)
  (format "%s %s setup.py %s %s"
	  (fd-python-command fd-test-venv)
	  (or module "")
	  fd-setup-test-cmd
	  (if (fd-python-in-test-p)
	      (save-excursion
		(end-of-buffer)
		(format "-s %s" (fd-python-current-class-path)))
	    "")))

(defun fd-python-run-tests (&optional module pre-cmd post-cmd)
  "Run tests of current project."
  (interactive)
  ;;   (and fd-python-pre-test-fn (funcall fd-python-pre-test-fn))

  (save-excursion
    (let (compilation-directory
	  compile-command
	  (default-directory (fd-python-project-root)))
      (compile (format "%s %s %s"
		       (or pre-cmd "")
		       (fd-test-cmd module)
		       (or post-cmd "")) t))))

(defun fd-python-profile-tests ()
  "Run tests of current project."
  (interactive)
  ;;   (and fd-python-pre-test-fn (funcall fd-python-pre-test-fn))
  (fd-python-run-tests
   "-m cProfile -o ./tests.prof"
   nil
   "&& echo 'Find your profile at ./tests.prof'; pyprof2calltree -k -i ./tests.prof"))

(defun fd-gud-pdb-test (&optional cmd)
  (interactive
   (list
    (read-shell-command "Command to run tests: "
			(fd-test-cmd "-m pdb"))))

  (let ((default-directory (fd-python-project-root)))
    (when (null default-directory) (error "Not in a python project."))
    (when (get-buffer "*gud-pdb*") (kill-buffer-ask (get-buffer "*gud-pdb*")))
    (pdb cmd)))

(defun pep8-diff ()
  "Starts an ediff session between the FILE and its specified revision.
REVISION should not include the filename, e.g. \"HEAD:\". If
BEFORE-EDIFF-HOOK is specified, it is executed as an ediff setup
hook. If AFTER-EDIFF-HOOK is specified, it is executed as an
ediff quit hook. Both hooks run in the ediff context, i.e. with
valid ediff-buffer-A and B variables, among others. If the
versions are identical, error out without executing either type
of hook."
  (interactive)
  (let* ((buf1 (current-buffer))
	 (fname (buffer-file-name))
	 (buf2 (switch-to-buffer "*pep8*"))
	 (config (current-window-configuration))
	 (autopep8-path (concat python-shell-virtualenv-path "/bin/autopep8"))
	 (autopep8-args (concat "-a " fname)))

    ;; build buf2
    (shell-command
     (concat autopep8-path " " autopep8-args)
     "*pep8*")
    (python-mode)

    (when (eq 0 (compare-buffer-substrings buf1 nil nil buf2 nil nil))
      (kill-buffer buf2)
      (error "Pep8 conformant"))

    (set-buffer
     (ediff-buffers buf1 buf2))

    (lexical-let ((config config)
		  (buf2 buf2))
      (add-hook 'ediff-quit-hook
		(lambda ()
		  (ediff-cleanup-mess)
		  (kill-buffer buf2)
		  (set-window-configuration config))
                nil t))))


(defun my-python-send-buffer ()
  (interactive)
  (python-shell-send-string
   "__package__ = '%s';import '%s'; from ppring import pprint as pp")
  (call-interactively (python-shell-send-buffer)))


(defun fd-ein:hook ()
  (define-key ein:notebook-mode-map (kbd "C-c r") 'ein:worksheet-execute-all-cell))

(defun indent-lines-to (beg end column)
  (save-excursion
    (goto-char beg)
    (forward-line)
    (while (< (point) end)
      (delete-region (progn (beginning-of-line) (point)) (progn (back-to-indentation) (point)))
      (indent-to column)
      (forward-line))))

(defun python-smart-fill-paragraph (&optional justify region)
  (interactive)
  (if (null (python-info-docstring-p))
      (fill-paragraph justify region)
    (let ((paragraph-start "\\(^\\(?2:\s+\\)\\(?3:\\**[[:alnum:]_]+\\):\\|\n\s*\n\\|^\s*- \\)"))
      (fill-paragraph justify region)
      (save-match-data
        (save-excursion
          (when (and (re-search-backward paragraph-start nil t)
                     (save-match-data (looking-at "^\s+[[:alnum:]_]+:")))
            (let ((spaces (match-string 2))
                  (word (match-string 3)))
              ;; Return and Raises are special cases
              (when (or (string= word "Returns")
                        (string= word "Raises")
                        (string= word "Yields"))
                (search-forward ":" nil t)
                (insert "\n") (backward-char))
              (let ((rbeg (save-excursion (end-of-line) (point)))
                    (rend (save-excursion (end-of-paragraph-text) (point))))
                (indent-lines-to rbeg rend (+ 2 (length spaces)))
                (fill-region rbeg rend)))))))))

(defun fd-python-indent-line-function (pyfun)
  (if (and (python-info-docstring-p) (not (looking-at "\"\"\"")))
      (if (looking-back "^\s*\\(Returns\\|Raises\\|Args\\|Yields\\):\s*\n\s*")
          (progn (indent-relative-maybe) (insert "  "))
        (indent-relative))
    (funcall pyfun)))
(advice-add 'python-indent-line-function
            :around #'fd-python-indent-line-function)

(add-hook 'ein:notebook-mode-hook 'fd-ein:hook)

(defun python-path-of (dir-path)
  (let ((path (concat (file-name-as-directory dir-path)))
        (ret))
    (while (and (not (string= path "/"))
                (not (string= path ""))
                (file-exists-p (concat path "__init__.py")))
      (add-to-list 'ret (file-name-base (directory-file-name path)))
      (setq path (file-name-directory (directory-file-name path))))
    (mapconcat 'identity ret ".")))

(defun region-to-module (start end &optional filename should-append)
  "Try moving region to module."
  (save-excursion
    (save-match-data
      (goto-char start)
      (unless (re-search-forward "^class \\([A-Z][[:alnum:]]*\\)" end t)
        (error (format "No class in region (%d %d)" start end)))
      (let* ((class-name (match-string-no-properties 1))
             (class-start (match-beginning 0))
             (class-end (progn (end-of-defun) (point)))
             (class-body (buffer-substring class-start class-end))
             (class-filename (or filename (format "./%s.py" (downcase class-name))))
             (pypath (python-path-of (file-name-directory (buffer-file-name))))
             (orig-buf (current-buffer)))
        ;; Stop if anything starts from beginning of line
        (when (re-search-forward "^[^[:space:]]" class-end t)
          (error (format "Malformed class %s" class-name)))
        (find-file-other-window class-filename)
        (when (and (not should-append) (file-exists-p class-filename))
          (error (format "File %s already exists." class-filename)))
        (goto-char (point-max))
        (insert (concat "\n\n" class-body))
        (with-current-buffer orig-buf
          (delete-region class-start class-end)
          ;; Import the new file
          (goto-char
           (or (re-search-backward "^\\(import\\|from\\) .*\n" nil t) (point-min)))
          (insert (format "from %s.%s import %s\n" pypath (file-name-base class-filename) class-name)))
        (message "Remember to update the build file.")))))

;; (require 'ein)
;; (require 'ein-loaddefs)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)
(provide 'fd-python)
