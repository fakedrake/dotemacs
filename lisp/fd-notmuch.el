(setq notmuch-saved-searches
      '((:name "unread"
               :query "tag:unread"
               :key "u"
               :sort-order newest-first
               :search-type tree)
        (:name "inbox"
               :key "i"
               :query "tag:inbox and date:2M..today"
               :sort-order newest-first
               :notmuch-search-type t)
        (:name "flagged"
               :key "f"
               :query "tag:flagged"
               :sort-order newest-first)
        (:name "drafts"
               :key "d"
               :query "tag:drafts"
               :sort-order newest-first)
        (:name "sent"
               :key "s"
               :query "tag:sent"
               :sort-order newest-first)
        (:name "travis"
               :key "t"
               :query "tag:travis and tag:unread"
               :sort-order newest-first)
        (:name "e-food"
               :key "f"
               :query "tag:efood and tag:unread"
               :sort-order newest-first)
        (:name "eurobank"
               :key "e"
               :query "tag:eurobank and tag:unread"
               :sort-order newest-first)))


(defun nm ()
  (interactive)
  (call-interactively 'notmuch)
  (notmuch-poll-and-refresh-this-buffer))


(defun fd-notmuch-show-hook ()
  (define-key notmuch-show-mode-map (kbd "RET") 'browse-url))
(add-hook 'notmuch-show-hook 'fd-notmuch-show-hook)

;;; Change all occurances of my email and name with "Me"
(defvar fd-notmuch-mail-name-alist '(("cperivol@csail.mit.edu" . "Me")))
(defun mapping-notmuch-clean-address (orig-fn &rest args)
  "Use `fd-notmuch-mail-name-alist' to change names for specific
mail addresses. This function is not used in `notmuch-searh'
buffers. Instead `notmuch-insert-authors' is used."
  (let* ((res (apply orig-fn args))
         (new-name (assoc (car res) fd-notmuch-mail-name-alist)))
    (if new-name (cons (car res) (cdr new-name)) res)))

(defvar fd-notmuch-author-alias-alist '(("Chris Perivolaropoulos" . "Me")))
(defun alias-authors (authors &optional res)
  "Replace the authors in the list with their alias according to
`fd-notmuch-author-alias-alist'"
  (if (not authors) res
    (cons
     (let ((alias-cons (assoc (car authors) fd-notmuch-author-alias-alist)))
       (if alias-cons (cdr alias-cons) (car authors)))
     (alias-authors (cdr authors)))))

(defun alias-authors-str (authors)
  "Hardcoded replacements for authors."
  (replace-regexp-in-string "Chris Perivolaropoulos" "Me" authors))

(defun alias-authors-notmuch-insert-authors (orig-fn format-string authors)
  "Use the `alias-authors-str' function to override the authors
string."
  (apply orig-fn format-string (alias-authors-str authors) nil))

(advice-add 'notmuch-search-insert-authors :around #'alias-authors-notmuch-insert-authors)
(advice-add 'notmuch-clean-address :around #'mapping-notmuch-clean-address)

;;; Make sure emails are visible.
(setq shr-color-visible-luminance-min 70)

;; For smtp errors try using the csail server defined in fd-mail.el

(provide 'fd-notmuch)
