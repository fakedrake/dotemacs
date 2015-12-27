(require 'gh)

                                        ; (setq fd-github-api (gh-issues-api "emacs-stuff"))
(defvar fd-github-api  nil)

(defmethod gh-issues-issue-list-all ((api gh-issues-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api issue-cls)) "GET"
   (format "/repos/%s/%s/issues?state=all" user repo)))

(defun branch-issue-alist (user repo)
  (let ((lst (gh-issues-issue-list-all fd-github-api user repo)))
    (mapcar (lambda (issue)
              (cons (format "issue_%d" (oref issue number)) (oref issue title)))
            (oref lst data))))

(defun github-user-repos ()
  (mapcar (lambda (url)
            (cdr (s-match "git@github.com:\\(.+\\)/\\(.+\\)" url)))
          (remove-duplicates
           (delete-if-not
            (apply-partially 's-starts-with? "git@github.com")
            (s-split "\\s-+" (git--exec-string-no-error "remote" "-v")))
           :test 'equal)))
(when (boundp 'git-branch-annotator-functions)
  (defun annotate-issue-names (branch-list)
    (let ((user (car (github-user-repos))))
      (when user (branch-issue-alist (car user) (cadr user)))))

  (add-to-list 'git-branch-annotator-functions 'annotate-issue-names))

(global-magit-file-mode t)
(define-key magit-file-mode-map (kbd "C-x g") 'magit-file-popup)
(define-key magit-file-mode-map (kbd "C-x G") 'magit-status)

;; Define faces.

(defface my/mode:vc-added
  `(
    (  ((class color))
       (:background "#FFAA55"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that have just been added to
version-control."
  :group 'MY/mode)

(defface my/mode:vc-edited
  `(
    (  ((class color))
       (:background "#F05B80"  :foreground "black")  )   ; "#F04040" maybe?
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are under version control
but which have been edited."
  :group 'MY/mode)

(defface my/mode:vc-in-sync
  `(
    (  ((class color))
       (:background "#60CC60"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are under version control
and which are in sync with the respository."
  :group 'MY/mode)

(defface my/mode:vc-none
  `(
    (  ((class color))
       (:background "#70A0D0"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are not under version
control"
  :group 'MY/mode)

(defface my/mode:vc-unknown
  `(
    (  ((class color))
       (:background "#FF0000"  :foreground "white")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files whose version-control status
cannot be determined."
  :group 'MY/mode)

(defvar my-vc-mode-attrs
  '((""  . (" NoVC "  my/mode:vc-none))
    ("-" . (" VC = "  my/mode:vc-in-sync))
    (":" . (" VC > "  my/mode:vc-edited))
    ("@" . (" VC + "  my/mode:vc-added))
    ("?" . (" ?VC? "  my/mode:vc-unknown))
    )
  "Lookup table to translate vc-mode character into another string/face."
  )


;; This function helps me understand the version-control status.
(defun my-mode-line-vc-info ()
  "Return version-control status information about the file in
the current buffer, as a fontified string.

The mode-line variable `vc-mode' is nil if the file is not under
version control, and displays a hyphen or a colon depending on whether
the file has been modified since check-in.  I can never keep those
straight.

This function returns \"NoVC\" if the file is not under version
control.  It displays a string with an = sign if the file is in sync
with its version control, and a string with a > sign if the file has
been modified since its last check-in."
  (let* ((class
          (cond
           ;; If not under version-control
           ((not vc-mode)
            "")

           ;; If under version-control decode the -:@ character
           ((string-match "\\` ?\\(?:CVS\\|Git\\)\\([-:@]\\)\\([^^:~ \x00-\x1F\\\\/]+\\)?" vc-mode)
            (match-string-no-properties 1 vc-mode))

           ;; Otherwise, indicate confusion
           (t
            "?")
           ))

         (branch
          (if (-any-p (lambda (x) (string= x class)) '("-" ":" "@"))
              (concat " " (match-string-no-properties 2 vc-mode))
            ""))

         ;; Fetch properties list for the class character above
         (props (cdr (assoc class my-vc-mode-attrs)))
         )

    (concat (propertize (car props) 'face (cadr props))
            branch)))



(provide 'fd-git)
