(require 'js2-mode)
(require 'nodejs-repl)

(defun my-js-newline-and-indent ()
  "Append a newline first if the cursor is between { and }."
  (interactive)
  (when (and (not (nth 8 (syntax-ppss)))
	     (looking-back "{\s*")
	     (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (newline-and-indent))

(defun fd-js-mode-hook ()
  "Hooks for all clojure."
  (setq js2-basic-offset 2)
  (rainbow-delimiters-mode 1)
  (define-key js2-mode-map (kbd "C-j") 'my-js-newline-and-indent)
  (define-key js2-mode-map (kbd "C-c C-e") 'nodejs-repl-eval-buffer)
  (define-key js2-mode-map (kbd "C-M-x") 'nodejs-repl-eval-function)
  (define-key js2-mode-map (kbd "C-c C-s") 'nodejs-repl-eval-dwim)
  (define-key js2-mode-map (kbd "C-c C-t") 'js2-toggle-source-test)
  (define-key js2-mode-map (kbd "C-c s s") 'js2-set-self)
  (define-key js2-mode-map (kbd "C-c s d") 'js2-set-default)
  (define-key js2-mode-map (kbd "C-c M-t") 'js2-test-this-file)

  (setq-local untabify-on-save t))
(require 'compile)
(defvar js-compilation-error-regex-alist
  ;; REGEX FILE-GROUP LINE-GROUP COLUMN-GROUP ERROR-TYPE LINK-GROUP
  '(
    ;; When selenium runs pipe it through `sed
    ;; 's_http://localhost:8080_$(CURDIR)_g'`
    (my-selenium
     "^BrowserLog: \\(\\(.*\.js\\) \\([0-9]*\\):\\([0-9]*\\)\\) "
     2 3 4 '(1) 1)

    (nodejs
     "^[ \\t]*at.* (?\\(file://\\)?\\(\\(.*?\\):\\([0-9]*\\)\\(:\\([0-9]*\\)\\)?\\))?$"
     3 4 6 nil 2)
    (mocha
     "^\\(\\(/.*\\):\\([0-9]*\\)\\)$"
     2 3 nil nil  1)
    (jasmine
     "^[ \\t]*\\(file://\\(.*\\):\\([0-9]*\\)\\)$"
     2 3 nil nil  1)
    (browserify
     "Error: .*?\\(\\(/.*\\): Line \\([0-9]*\\)\\)"
     2 3 nil nil 1)))

(setq compilation-error-regexp-alist
      (append (mapcar 'car js-compilation-error-regex-alist)
	      (or compilation-error-regexp-alist nil)))

(setq compilation-error-regexp-alist-alist
      (append js-compilation-error-regex-alist
	      compilation-error-regexp-alist-alist))

(add-hook 'js2-mode-hook 'fd-js-mode-hook)

(defvar html-beatify-cmd "js-beautify --type html -s 2 -a -p -f -")
(defun html-beautify-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end)
			   html-beatify-cmd nil t))

(defvar js2-beatify-cmd "js-beautify -s 2 -a -p -f -")
(defun js2-beautify-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end)
			   js2-beatify-cmd nil t))

(defun top-level-functions ()
  (when (search-forward-regexp "^function\s+\\([a-zA-Z0-9_]+\\)" nil t)
    (cons (match-string 1) (top-level-functions))))

(defun js2-export-top-functions ()
  (interactive)
  (mapcar (lambda (f) (insert (format "module.exports.%s = %s;\n" f f)))
          (save-excursion
            (beginning-of-buffer)
            (top-level-functions))))

(require 'ffap)
(defun js2-relative-path (&optional decorate-string)
  "Change the path of the file at point to relative"
  (interactive)
  (let* ((fname (ffap-string-at-point))
         (rg ffap-string-at-point-region)
         (start (car rg))
         (end (cadr rg)))
    (when (> end start)
      (delete-region start end)
      (insert (funcall (or decorate-string 'identity)
                       (file-relative-name fname)))
      (goto-char start))))

(defun js2-require-abs-file ()
  "Require the filename at point."
  (interactive)
  (js2-relative-path
   (lambda (rel-fname)
     (format "require('%s');" rel-fname))))

(defun js2-beginning-of-outermost-defun ()
  (interactive)
  (if (called-interactively-p) (push-mark))
  (while (js2-mode-function-at-point)
    (js2-beginning-of-defun)
    (backward-char))
  (search-forward "{"))

(defun js2-set-self ()
  (interactive)
  (save-excursion
    (js2-beginning-of-outermost-defun)
    (my-js-newline-and-indent)
    (insert "var self = this;")))

(defun js2-set-default ()
  "Set default value for the selected variable or the variable
that is the line."
  (interactive)
  (let* ((var (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (save-excursion
                 (buffer-substring
                  (progn (back-to-indentation) (point))
                  (progn (end-of-line) (point))))))
        (before (format " = (typeof %s === 'undefined' ? " var))
        (after (format " : %s)" var)))
    (insert before)
    (save-excursion (insert after))))

(setq inferior-lisp-program "/usr/local/bin/swank-js")
(setq slime-contribs '(slime-fancy))

;; Variable names
(delete-if (lambda (c) (member c '("self" "Event"))) js2-browser-externs)
(setq js2-browser-externs (append js2-browser-externs '("require" "global")))

(provide 'fd-javascript)
