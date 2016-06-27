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

(provide 'fd-notmuch)
