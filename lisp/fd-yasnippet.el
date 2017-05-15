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

(defun fd-yas-python-methodargs ()
  "This dumb functions sees the arguments of the python function
we are in and returns a list of arguments and kwargs (:args (arg1
arg2) :kwargs ((kw1 . val1))). It does not parse python code, it
just matches regexes expect things to go south if default values
are lists, sets, strings with parens and commas or anything that
looks like argument list delim but is context sensitive."
  (save-match-data
    (save-excursion
      (let* ((kwargs-rx "[,(][[:space:]]*\\([a-zA-Z0-9_]+\\)[[:space:]]*=[[:space:]]*\\(.*?\\)[,)]")
	     (args-rx "[,(][[:space:]]*\\([a-zA-Z0-9_]+\\)[[:space:]]*[,)]")
	     (sargs (if (re-search-backward
			 "def .*\\((\\(self[[:space:]]*,\\)?[[:space:]]*.*)\\):" nil t)
			(match-string-no-properties 1)
		      (error "Function not found.")))
	     (args (re-find-all args-rx sargs 1))
	     (kwargs (mapcar* 'cons
			      (re-find-all kwargs-rx sargs 1)
			      (re-find-all kwargs-rx sargs 2))))
	(list :args (remove-if (lambda (x) (string= x "self")) args) :kwargs kwargs)))))

(defun  yas-sphinx-docstring (fnargs-plist &optional offset)
  "Gets a plust like the one fd-yas-python-methodargs and maybe
an offset for pspaces and returns a sphinx readable template for
sphinx describing the arguments."
  (let* ((spaces (make-string (or offset 0) (string-to-char " ")))
	 (args (mapconcat (lambda (p) (format "%s:param %s: " spaces p))
			  (plist-get fnargs-plist :args) "\n"))
	 (kwargs (mapconcat (lambda (p) (format "%s:param %s: "
						spaces (car p)))
			    (plist-get fnargs-plist :kwargs) "\n")))
    (format "%s%s:returns: "
	    (if (string= args "") "" (concat args "\n"))
	    (if (string= kwargs "") "" (concat kwargs "\n")))))

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
	(if (and until-rx (re-search-forward until-rx))
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
