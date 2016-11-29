;; ERC
;; check channels
(require 'erc)
(require 'erc-goodies)

(defcustom erc-modules '(netsplit fill button match track completion
			 networks ring autojoin
			 stamp menu list)
  "A list of modules which ERC should enable.
If you set the value of this without using `customize' remember to call
\(erc-update-modules) after you change it.  When using `customize', modules
removed from the list will be disabled."
  :get (lambda (sym)
	 ;; replace outdated names with their newer equivalents
	 (erc-migrate-modules (symbol-value sym)))
  :set (lambda (sym val)
	 ;; disable modules which have just been removed
	 (when (and (boundp 'erc-modules) erc-modules val)
	   (dolist (module erc-modules)
	     (unless (member module val)
	       (let ((f (intern-soft (format "erc-%s-mode" module))))
		 (when (and (fboundp f) (boundp f) (symbol-value f))
		   (message "Disabling `erc-%s'" module)
		   (funcall f 0))))))
	 (set sym val)
	 ;; this test is for the case where erc hasn't been loaded yet
	 (when (fboundp 'erc-update-modules)
	   (erc-update-modules)))
  :type
  '(set
    :greedy t
    (const :tag "autoaway: Set away status automatically" autoaway)
    (const :tag "autojoin: Join channels automatically" autojoin)
    (const :tag "bbdb: Integrate with Big Brother Database" bbdb)
    (const :tag "button: Buttonize URLs, nicknames, and other text" button)
    (const :tag "capab: Mark unidentified users on servers supporting CAPAB"
	   capab-identify)
    (const :tag "completion: Complete nicknames and commands (programmable)"
	   completion)
    (const :tag "hecomplete: Complete nicknames and commands (old)" hecomplete)
    (const :tag "dcc: Provide Direct Client-to-Client support" dcc)
    (const :tag "fill: Wrap long lines" fill)
    (const :tag "identd: Launch an identd server on port 8113" identd)
    (const :tag "irccontrols: Highlight or remove IRC control characters"
	   irccontrols)
    (const :tag "keep-place: Leave point above un-viewed text" keep-place)
    (const :tag "list: List channels in a separate buffer" list)
    (const :tag "list-old: List channels in a separate buffer (old)" list-old)
    (const :tag "log: Save buffers in logs" log)
    (const :tag "match: Highlight pals, fools, and other keywords" match)
    (const :tag "menu: Display a menu in ERC buffers" menu)
    (const :tag "move-to-prompt: Move to the prompt when typing text"
	   move-to-prompt)
    (const :tag "netsplit: Detect netsplits" netsplit)
    (const :tag "networks: Provide data about IRC networks" networks)
    (const :tag "noncommands: Don't display non-IRC commands after evaluation"
	   noncommands)
    (const :tag
	   "notify: Notify when the online status of certain users changes"
	   notify)
    (const :tag "page: Process CTCP PAGE requests from IRC" page)
    (const :tag "readonly: Make displayed lines read-only" readonly)
    (const :tag "replace: Replace text in messages" replace)
    (const :tag "ring: Enable an input history" ring)
    (const :tag "scrolltobottom: Scroll to the bottom of the buffer"
	   scrolltobottom)
    (const :tag "services: Identify to Nickserv (IRC Services) automatically"
	   services)
    (const :tag "smiley: Convert smileys to pretty icons" smiley)
    (const :tag "sound: Play sounds when you receive CTCP SOUND requests"
	   sound)
    (const :tag "stamp: Add timestamps to messages" stamp)
    (const :tag "spelling: Check spelling" spelling)
    (const :tag "track: Track channel activity in the mode-line" track)
    (const :tag "truncate: Truncate buffers to a certain size" truncate)
    (const :tag "unmorse: Translate morse code in messages" unmorse)
    (const :tag "xdcc: Act as an XDCC file-server" xdcc)
    (repeat :tag "Others" :inline t symbol))
  :group 'erc)

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; joining && autojoing

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-use-znc-marxistutopia t)

(defun fakedrake-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  ;; (select-frame (make-frame '((name . "Emacs IRC")
  ;; 			      (minibuffer . t))))
  ;; (setq erc-autojoin-channels-alist
  ;;       '((".*\\.freenode.net" "#node.js")
  ;;         (".*\\.freenode.net" "#p-space")
  ;;         (".*\\.freenode.net" "#codebender.cc")))
  (if (get-erc-buffer (buffer-list)) ;; ERC already active?
      (fd-digup-erc)
    (if erc-use-znc-marxistutopia
	(erc :server "marxistutopia"
	     :port 5000
	     :nick my-znc-nick
	     :password (format "%s:%s" my-znc-nick my-znc-password))
      (erc :server "irc.freenode.net"
	   :port 6667
	   :nick my-freenode-nick
	   :full-name my-freenode-fullname
	   :password my-freenode-password))))

(defun get-erc-buffer (buffers)
  (if (or (null (car buffers))
	   (eq (with-current-buffer (car buffers) major-mode) 'erc-mode))
      (car buffers)
    (get-erc-buffer (cdr buffers))))

(defun my-destroy-erc ()
  "Kill all erc buffers!!"
  (interactive)
  (message "Killing all ERC buffers!")
  (save-excursion
    (dolist (i (buffer-list))
      (with-current-buffer i
	(cond
	 ((eq major-mode 'erc-mode) (kill-buffer (current-buffer))))))))

(defun fd-digup-erc ()
  "Kill all erc buffers!!"
  (interactive)
  (message "Digging up ERC buffers...")
  (dolist (i (reverse (buffer-list)))
    (with-current-buffer i
      (when (eq major-mode 'erc-mode)
	(switch-to-buffer i)))))

(defun fd-bury-erc ()
  "Kill all erc buffers!!"
  (interactive)
  (message "Burying ERC buffers...")
  (dolist (i (buffer-list))
    (with-current-buffer i
      (when (eq major-mode 'erc-mode)
	(bury-buffer)))))

;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e s") 'fakedrake-erc-start-or-switch) ;; ERC
(global-set-key (kbd "C-c e b") 'fd-bury-erc)
(global-set-key (kbd "C-c e k") 'my-destroy-erc)

(add-hook 'erc-mode-hook '(lambda() (set (make-local-variable 'global-hl-line-mode) nil)))

(add-hook 'erc-mode-hook 'fd-erc-hook)

(defun fd-erc-hook ()
  (define-key erc-mode-map (kbd "M-m") 'erc-bol))

(erc-notifications-mode t)

;; (require 'erc-image)
;; (add-to-list 'erc-modules 'image)
(erc-update-modules)
(setq erc-image-inline-rescale 5)

(ignore-errors (fakedrake-erc-start-or-switch))
(provide 'fd-erc)
