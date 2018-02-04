(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("outgoing.csail.mit.edu" 587 nil nil))
      smtpmail-auth-credentials '(("outgoing.csail.mit.edu" 587
				   "cperivol@csail.mit.edu" nil))
      smtpmail-default-smtp-server "outgoing.csail.mit.edu"
      smtpmail-smtp-server "outgoing.csail.mit.edu"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(defvar-local local-getmail-running nil)
(defun getmail-running ()
  (and (get-buffer "*getmail*")
       (with-current-buffer "*getmail*" local-getmail-running)))

(defun getmail-internal (cmd args callback)
  "An internal process for running an external process towards
complting the getmail process."
  ;; Throw it if we are trying to run a new process on buffer
  (with-current-buffer "*getmail*"
    ;; Increment the getmail id
    (insert (concat "# " cmd " " (string-join args " ") "\n"))
    (lexical-let ((callback callback))
      (set-process-sentinel
       (apply 'start-process "getmail" "*getmail*" cmd args)
       (lambda (p e) (funcall callback))))))

(defun getmail-refresh-notmuch ()
  "Refresha all notmuch buffers."
  (save-excursion
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (memq major-mode '(notmuch-hello-mode notmuch-show-mode))
          (notmuch-refresh-this-buffer))))))

(defun getmail ()
  "Asynchronously get mail and update the buffers. Without
blocking on external calls. If this a new call to getmail is
issued before the previous one has finished correctly it will be
blocked."
  (interactive)
  (if (getmail-running)
      (progn
        (switch-to-buffer "*getmail*")
        (error (concat "Already running getmail. Kill *getmail* buffer to abort.")))

    (switch-to-buffer "*getmail*")
    ;; Set running state
    (setq-local local-getmail-running t)
    (kill-region (point-min) (point-max))
    ;; First get ifcp mail
    (getmail-internal
     "mbsync" '("icfp")
     (lambda ()
       ;; Then update notmuch and when it's done call nm
       (getmail-internal "notmuch" '("new") (lambda () (nm)))
       ;; And samultaneously get gmail. Gmail is called second because
       ;; it is much slower
       (getmail-internal
        "mbsync" '("gmail")
        ;; And finally update notmuch again
        (lambda ()
          (getmail-internal
           "notmuch" '("new")
           (lambda ()
             (with-current-buffer "*getmail*"
               ;; Record that the call to getmail finished
               (setq-local local-getmail-running nil))
             ;; Refresh notmuch
             'getmail-refresh-notmuch))))))))

(add-hook 'notmuch-message-mode-hook #'fd-notmuch-message)
(add-hook 'notmuch-show-hook #'fd-notmuch-show)

(defun fd-lang-change ()
  "Change language and dictionary,"
  (interactive)
  (toggle-input-method)
  (ispell-change-dictionary (or current-input-method "english")))

(defun fd-notmuch-message ()
  (flyspell-mode)
  (define-key notmuch-message-mode-map (kbd "M-\\") 'fd-lang-change)
  (define-key notmuch-message-mode-map (kbd "C-\\") 'fd-lang-change))

(defun fd-notmuch-show ()
  (define-key notmuch-show-mode-map "r" 'notmuch-show-reply))

(setq message-signature "Chris Perivolaropoulos
University of Edinburgh
School of Informatics - IF-2.05

Web: fakedrake.github.io
Mail: c.perivol@ed.ac.uk
")

(add-to-list 'notmuch-saved-searches `(:name "remember" :query "tag:remember" :key ,(kbd "r")))
(setq mm-inlined-types (remove "application/zip" mm-inlined-types))
(provide 'fd-mail)
