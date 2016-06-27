(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("outgoing.csail.mit.edu" 587 nil nil))
      smtpmail-auth-credentials '(("outgoing.csail.mit.edu" 587
				   "cperivol@csail.mit.edu" nil))
      smtpmail-default-smtp-server "outgoing.csail.mit.edu"
      smtpmail-smtp-server "outgoing.csail.mit.edu"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
