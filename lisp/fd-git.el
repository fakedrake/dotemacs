(require 'gh)
(defvar fd-github-api (gh-issues-api "emacs-stuff"))

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

(defun annotate-issue-names (branch-list)
  (let ((user (car (github-user-repos))))
    (when user (branch-issue-alist (car user) (cadr user)))))

(add-to-list 'git-branch-annotator-functions 'annotate-issue-names)

(provide 'fd-git)
