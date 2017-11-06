;; YASnippet

(defun fd-prev-match (rx alt &optional grp)
  "Get the last occurance or alt. GRP tells us which rx group to
keep, defaults to 1."
  (or
   (progn (re-search-backward rx nil t) (match-string-no-properties (or grp 1)))
   alt))

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (my-expand-path "my-snippets/"))
(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "C-c y n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-h y") 'yas-describe-tables)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand) ; S-Tab
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Helper functions for snippet definitions
(defun fd-snippet-python-arguments (text)
  (let* ((targs (replace-regexp-in-string "\\*[^,]" "" text))
	 (args (split-string targs
			     "[[:space:]]*\\(=.*\\)?[[:space:]]*,[[:space:]]*" t)))
    args))


(defun re-find-all (regex string &optional group start)
  (save-match-data
    (when (string-match regex string start)
      (cons (match-string (or group 0) string)
	    (re-find-all regex string group (match-end (or group 0)))))))

(flet ((arg (x) (format "\\(?:[[:space:]\n]*%s[[:space:]]*\\(?:,\\|$\\)\\)" x)))
  (setq python-symbol-rx "\\(?1:\\**[a-zA-Z_][a-zA-Z0-9_]*\\)"
        python-args-rx (arg python-symbol-rx)
        python-kw-rx (arg (format "\\(?:%s[[:space:]]*=[[:space:]]*\\(?2:.*?\\|(.*)\\)\\)"
                                  python-symbol-rx))
        python-variant-args-rx (arg (format "\\*%s" python-args-rx))
        python-variant-kw-rx (arg (format "\\*%s" python-variant-args-rx))
        python-func-rx
        (format
         "^[[:space:]]*def[[:space:]]+%s[[:space:]]*(\\(\\(?:.\\|,\n\\)*\\)):"
         python-symbol-rx)))

(defun fd-yas-python-methodargs ()
  "This dumb functions sees the arguments of the python function
we are in and returns a list of arguments and kwargs (:args (arg1
arg2) :kwargs ((kw1 . val1))). It does not parse python code, it
just matches regexes expect things to go south if default values
are lists, sets, strings with parens and commas or anything that
looks like argument list delim but is context sensitive."
  (save-match-data
    (save-excursion
      (let* ((sargs (if (re-search-backward
                         python-func-rx
                         (save-excursion (beginning-of-defun) (point)) t)
                        (match-string-no-properties 2)
                      (error "Function not found")))
             (args) (kwargs) (start))
        (save-match-data
          (while (string-match (format "\\(?:\\(?3:%s\\)\\|\\(?4:%s\\)\\)"
                                       python-kw-rx python-args-rx)
                               sargs start)
            (setq start (match-end 0))
            (if (match-string 4 sargs)
                (add-to-list 'args (match-string 1 sargs))
              (if (match-string 3 sargs)
                  (add-to-list 'kwargs (cons (match-string 1 sargs) (match-string 2 sargs)))
                (error "Unreached")))))
        (list :args (reverse (remove-if (lambda (x) (string= x "self")) args))
              :kwargs (reverse kwargs))))))

(defun python-default-variable-doc (name &optional value)
  "Make a bad doc based on the NAME and the default VALUE."
  (let ((human-name
         (replace-regexp-in-string
          "_" " " (string-remove-prefix "*" (string-remove-prefix "*" name)))))
    (concat
     (cond ((or (string-prefix-p "use_" name)
                (string-prefix-p "make_" name))
            human-name)
           ((or (string-prefix-p "is_" name)
                (string-prefix-p "has_" name))
            (concat "Whether " human-name))
           ((string-suffix-p "ed" name) (concat "If need be " human-name))
           (t (concat "The " human-name)))
     (if value (format " (default: %s)" value) ""))))

(defun python-docstring-type ()
  (save-match-data
    (save-excursion
      (if (re-search-backward "^\s*\\(class\\|def\\) " nil t)
          (pcase (match-string-no-properties 1)
            ('"class" 'class)
            ('"def" 'def)
            (_ (error "Unreached")))
        'module))))

(defun python-function-raises ()
  "Return all the raised types."
  (save-excursion
    (save-restriction
      (save-match-data
        (narrow-to-defun)
        (goto-char (point-min))
        (let ((ret))
          (while (re-search-forward "^\s*raise\s+\\([[:alnum:]]+\\)" nil t)
            (add-to-list 'ret (match-string 1)))
          (reverse ret))))))

(defun python-function-returns ()
  (save-excursion
    (save-restriction
      (save-match-data
        (narrow-to-defun)
        (goto-char (point-min))
        (re-search-forward "^\s*return\s+." nil t)))))

(defun python-function-yields ()
  (save-excursion
    (save-restriction
      (save-match-data
        (narrow-to-defun)
        (goto-char (point-min))
        (re-search-forward "^\s*yield\s+." nil t)))))

(defun  yas-python-docstring-def (fnargs-plist &optional offset)
  "Gets a plust like the one fd-yas-python-methodargs and maybe
an offset for pspaces and returns a sphinx readable template for
sphinx describing the arguments."
  (let* ((spaces (make-string (or offset 0) (string-to-char " ")))
         (spaces+2 (make-string (+ 2 (or offset 0)) (string-to-char " ")))
         (args (mapcar (lambda (p) (format "%s%s: %s\n"
                                      spaces+2 p (python-default-variable-doc p)))
                       (plist-get fnargs-plist :args)))
         (kwargs (mapcar (lambda (p) (format "%s%s: (Optional) %s\n"
                                        spaces+2 (car p)
                                        (python-default-variable-doc
                                         (car p) (cdr p))))
                         (plist-get fnargs-plist :kwargs)))
         (all-args (append args kwargs)))
    (concat
     ;; Add an Args: section
     (if all-args
         (format "%sArgs:\n%s" spaces (apply 'concat all-args))
       "")
     ;; Add a Returns: section
     (if (python-function-returns) (format "\nReturns:\n%s\n" spaces+2) "")
     (if (python-function-yields) (format "\nYields:\n%s\n" spaces+2) "")
     ;; Add a Raises section
     (if-let* ((errs (python-function-raises)))
         (format "\nRaises:\n%s\n"
                 (mapconcat (lambda (x) (concat spaces+2 x ":")) errs "\n"))
       ""))))
(defvar python-default-licence "")
(defun yas-python-docstring (type)
  (pcase type
    ('module (concat python-default-licence ""))
    ('class "")
    ('def (concat "\n\n" (yas-python-docstring-def (fd-yas-python-methodargs))))))

(defvar fd-debug-message-title "DRNINJABATAN"
  "This is set as the toplevel tag in the snippet of debug
  messages (printk)")

(defvar fd-debug-message-function "printk"
  "The default debug message emmiter.")

(defun last-syms-before (regex &optional until-rx)
  (save-excursion
    (let ((until-rx (or until-rx regex)))
      (re-find-all
       (format "\\([a-zA-Z0-9_]+\\)[[:space:]]*%s" regex)
       (buffer-substring-no-properties
        (point)
        (if (and until-rx (re-search-forward until-rx nil t))
            (match-end 0) point-max))
       1))))

(defun last-sym-before (regex)
  (car (last-syms-before regex)))

(setq yas-wrap-around-region t)

(defun fd-yas:slug-to-title ()
  (let ((title (split-string (file-name-base) "-")))
    (mapconcat 'identity (cons (capitalize (car title)) (cdr title)) " ")))

(defun fd-yas:today ()
  (format-time-string "%Y-%m-%d %R"))

;; (defun yas--save-backquotes ()
;;   "Save all the \"`(lisp-expression)`\"-style expressions
;; with their evaluated value into `yas--backquote-markers-and-strings'."
;;   (while (re-search-forward yas--backquote-lisp-expression-regexp nil t)
;;     (let ((current-string (match-string-no-properties 1)) transformed)
;;       (save-match-data                  ;XXX: here is my addition
;;         (save-restriction (widen)
;;                           (delete-region (match-beginning 0) (match-end 0))))
;;       (setq transformed (yas--eval-lisp (yas--read-lisp (yas--restore-escapes current-string '(?`)))))
;;       (goto-char (match-beginning 0))
;;       (when transformed
;;         (let ((marker (make-marker)))
;;           (save-restriction
;;             (widen)
;;             (insert "Y") ;; quite horrendous, I love it :)
;;             (set-marker marker (point))
;;             (insert "Y"))
;;           (push (cons marker transformed) yas--backquote-markers-and-strings))))))

(provide 'fd-yasnippet)
