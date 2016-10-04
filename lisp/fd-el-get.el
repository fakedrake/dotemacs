;; EL-GET
(add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))
(setq el-get-dir (concat dotemacs-dir "el-get"))
(require 'package)
(package-initialize)

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq my:el-get-packages
      '(;; Dependencies from who-kows-where
        rainbow-delimiters
        google-translate
        dired-hacks
        notmuch
	queue
	s
	pkg-info
	request
        auctex
        idris-mode

        ;; flycheck
	;; ;; Elisp hepers
	f

	;; Python
	python
	; django-mode ; snippets get fucked up
	jedi
	;; ein

	;; Auto Complete packages
	yasnippet
        ; auto-yasnippet
	; auto-complete
        company-mode

	;; themes I like
	naquadah-theme

	;; ido
	ido-mode-el
	ido-speed-hack
	ido-better-flex
	ido-ubiquitous
	smex

	;; erc
	erc-highlight-nicknames
	; erc-image

	;; Misc
        ;; slime
	;; helm
	;; nxhtml ; this is obsolete crap, stay away.
	;; realgud ; Can't install test-simple
	; twiki-mode ; problem with dependency
	lua-mode
	textile-mode
	haskell-mode
        ;; elm-mode
	js2-mode
	coffee-mode
        ;; flymake-coffee-load
        flymake-jslint
	json-mode
	graphviz-dot-mode
	cider
	; ac-nrepl
	clojure-mode
        nodejs-repl
        js-doc
	;; cscope
	cmake-mode
	multi-term
	yaml-mode
	vimperator-mode
	c-eldoc
	;; sort
	;; hide-region
	gist
	org-mode
        org-trello
        cdlatex-mode
	markdown-mode
	ggtags
	zencoding-mode
	;git-emacs
	magit
	bm
	compilation-setup
        google-c-style
        web-mode
        dtrace-script-mode
        proof-general))

(setq
 el-get-sources
 '(el-get
   zencoding-mode
   python-pep8

   (:name vimperator-mode
	  :description "Edit vimperator files"
	  :type github
	  :pkgname "xcezx/vimperator-mode")

   (:name undo-tree
	  :description "Visualize undo history as a tree"
	  :type github
	  :pkgname "emacsmirror/undo-tree")

   ;; This is temporary until the pull request is dealt with in upstream
   (:name compilation-setup
	  :description "Compilation that makes sense"
	  :type github
	  :pkgname "fakedrake/compilation-setup.el")

   (:name ido-better-flex
	  :description "Better flex matching for ido"
	  :type github
	  :pkgname "orfelyus/ido-better-flex"
	  :compile "ido-better-flex.el")

   (:name ido-mode-el
	  :description "Better flex matching for ido"
	  :type github
	  :pkgname "orfelyus/ido-mode-el"
	  :compile "ido.el")

   (:name ido-speed-hack
	  :description "Better flex matching for ido"
	  :type github
	  :pkgname "orfelyus/ido-speed-hack"
	  :compile "ido-speed-hack.el")

   (:name bm
	  :description "Simple bookmark manager"
	  :type github
	  :pkgname "joodland/bm")

   (:name twiki-mode
	  :description "Major mode for editing Twiki wiki files
	  for emacs, plus 'twikish' command line tool to retrieve
	  and save twiki pages from text files."
	  :type github
	  :pkgname "christopherjwhite/emacs-twiki-mode")

   (:name smex				; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file (my-expand-path ".smex-items"))
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name swank-js
	  :description "SLIME REPL and other development tools for in-browser JavaScript and Node.JS"
	  :type github
	  :pkgname "swank-js/swank-js"
	  :depends (js2-mode slime)
	  :after (let* ((slime-dir (el-get-package-directory "slime"))
			(swank-js-dir (el-get-package-directory "swank-js"))
			(slime-link (concat slime-dir "/contrib/slime-js.el"))
			(swank-el (concat swank-js-dir "/slime-js.el"))
			(npm-installed? (equal 0 (call-process-shell-command
						  "npm list -g swank-js")))
			(npm-install-cmd (format "npm install -g %s" swank-js-dir)))
		   ;; Make sur the file is there.
		   (unless (file-exists-p slime-link)
		     (make-symbolic-link swank-el slime-link))
		   (message "Installing swank-js: %s" npm-install-cmd)
		   (unless (or npm-installed?
			       (equal (call-process-shell-command npm-install-cmd) 0))
		     (error "Error during swank-js install to npm. (is prefix = .... in your ~/.npmrc?)")))
	  :features nil)

   (:name ido-ubiquitous
	  :description "Ido everywhere."
	  :type github
	  :pkgname "DarwinAwardWinner/ido-ubiquitous")

   (:name web-beautify
	  :description "Beautify CSS, HTML and JS/JSON."
	  :depends (js-beautify)
	  :type github
	  :pkgname "yasuyk/web-beautify")

   (:name js-doc
	  :description " Insert JsDoc style comment easily in Emacs."
	  :type github
	  :pkgname "mooz/js-doc")

   (:name elm-mode
	  :description "Elm mode for emacs"
          :depends (f let-alist s)
	  :type github
	  :pkgname "jcollard/elm-mode")

   (:name flymake-jslint
	  :description "Emacs flymake syntax-checker for javascript using jslint."
          :depends (flymake-easy)
	  :type github
	  :pkgname "purcell/flymake-jslint")

   (:name dtrace-script-mode
	  :description "DTrace script mode for Emacs "
	  :type github
	  :pkgname "dotemacs/dtrace-script-mode")

   (:name jslint
	  :description "Linter for javascript."
	  :type npm
	  :pkgname "jslint")

   (:name js-beautify
	  :description "Beautify on node"
	  :type npm
	  :pkgname "js-beautify")


   ;; (:name erc-image
   ;;        :description "Image previews in erc."
   ;;        :type github
   ;;        :pkgname "kidd/erc-image.el")
   ;; (:name ein
   ;;     :description "IPython notebook client in Emacs"
   ;;     :type github
   ;;     :pkgname "millejoh/emacs-ipython-notebook"
   ;;     :depends (websocket request auto-complete)
   ;;     :load-path ("lisp")
   ;;     :submodule nil
   ;;     :features ein)

   (:name notmuch
          :description "Run notmuch(thread-based email index, search and tagging) within emacs"
          :type git
          :depends (company-mode)
          :url "git://notmuchmail.org/git/notmuch"
          :load-path ("./emacs"))

 (:name nevermore
        :description "Notmuch based mail client"
        :type github
        :depends (company-mode notmuch)
        :pkgname "tjim/nevermore")
 (:name gazelle
        :description "Lisp to js"
        :type github
        :depends (shadchen-el)
        :pkgname "VincentToups/gazelle")
 (:name shadchen-el
        :description "Pattern matching for j"
        :type github
        :pkgname "VincentToups/shadchen-el")
 ))


(when (el-get-executable-find "svn")
  (loop for p in '(psvn    		; M-x svn-status
		   yasnippet		; powerful snippet mode
		   )
	do (add-to-list 'el-get-sources p)))

(el-get 'sync my:el-get-packages)

(provide 'fd-el-get)
