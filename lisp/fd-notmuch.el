(setq notmuch-saved-searches '(
                              (:name "unread"
                                     :query "tag:unread"
                                     :sort-order newest-first
                                     :key "u")
                              (:name "inbox"
                                     :query "tag:inbox and date:6M..today"
                                     :sort-order newest-first)
                              (:name "flagged"
                                     :query "tag:flagged"
                                     :sort-order newest-first)
                              (:name "drafts"
                                     :query "tag:drafts"
                                     :sort-order newest-first)
                              (:name "sent"
                                     :query "tag:sent"
                                     :sort-order newest-first)))

(defun nm ()
  (interactive)
  (call-interactively 'notmuch)
  (notmuch-poll-and-refresh-this-buffer))

(provide 'fd-notmuch)
