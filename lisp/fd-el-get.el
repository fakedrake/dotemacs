;; EL-GET
(add-to-list 'load-path (concat dotemacs-dir "el-get/el-get"))
(add-to-list 'load-path (concat dotemacs-dir "el-get/el-get/queue"))
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
        flycheck
	memoize
        ; imaxima
        org-caldav
        oauth2
        irony-mode
        google-translate
        ; rtags
        sx
        tree-mode
        dictionary
        go-mode
        rainbow-delimiters
        dired-hacks
        notmuch
	s
        popup
	pkg-info
	request
        auctex
        idris-mode
        scala-mode

        ;; flycheck
	;; ;; Elisp hepers
	f
        slime

	;; Python
	python
	company-jedi

	;; Auto Complete packages
	yasnippet
        company-mode

	;; themes I like
	naquadah-theme

	;; erc
	erc-highlight-nicknames

	;; Misc
	lua-mode
	textile-mode
	haskell-mode
        ghc-mod
        hs-lint
	js2-mode
	coffee-mode
        flymake-jslint
	json-mode
	graphviz-dot-mode
        queue
	cider
	clojure-mode
        nodejs-repl
	cmake-mode
	multi-term
	yaml-mode
	vimperator-mode
	c-eldoc
	gist
	org-mode
        org-ref
        pdf-tools
        cdlatex-mode
	markdown-mode
	ggtags
	magit
	bm
	compilation-setup
        google-c-style
        web-mode
        dtrace-script-mode
        proof-general
        skewer-mode
        ein
	))

(setq
 el-get-sources
 '(el-get
   zencoding-mode
   python-pep8

   (:name vimperator-mode
	  :description "Edit vimperator files"
	  :type github
	  :pkgname "xcezx/vimperator-mode")
   (:name org-mode
	  :description "Just my fork of org mode"
	  :type github
	  :pkgname "fakedrake/org-mode"
          :info "doc"
          :build/berkeley-unix `,(mapcar
                                  (lambda (target)
                                    (list "gmake" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                                  '("oldorg"))
          :build `,(mapcar
                    (lambda (target)
                      (list "make" target (concat "EMACS=" (shell-quote-argument el-get-emacs))))
                    '("oldorg"))
          :load-path ("." "contrib/lisp" "lisp")
          :load ("lisp/org-loaddefs.el"))

   (:name undo-tree
	  :description "Visualize undo history as a tree"
	  :type github
	  :pkgname "emacsmirror/undo-tree")

   ;; This is temporary until the pull request is dealt with in upstream
   (:name compilation-setup
	  :description "Compilation that makes sense"
	  :type github
	  :pkgname "fakedrake/compilation-setup.el")

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

 (:name gazelle
        :description "Lisp to js"
        :type github
        :depends (shadchen-el)
        :pkgname "VincentToups/gazelle")
 (:name shadchen-el
        :description "Pattern matching for j"
        :type github
        :pkgname "VincentToups/shadchen-el")
 (:name elfeed-org
        :description "Configure the Elfeed RSS reader with an Orgmode file"
        :type github
        :pkgname "remyhonig/elfeed-org")
 (:name reddit-mode
        :description "Reddit mode for Emacs"
        :type github
        :depends (tree-mode)
        :pkgname "death/reddit-mode")
 (:name tree-mode
        :description "Tree widgets"
        :type github
        :pkgname "emacsmirror/tree-mode")

 (:name queue
        :description "Override the shitty melpa mirror"
        :type github
        :pkgname "emacsmirror/queue")
 (:name emacs-w3m
       :description "A simple Emacs interface to w3m"
       :type cvs
       :website "http://emacs-w3m.namazu.org/"
       :module "emacs-w3m"
       :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot"
       :build `(("autoconf") ("./configure" ,(format "--with-emacs=%s" el-get-emacs) ) ("make"))
       :build/windows-nt (("sh" "./autogen.sh")
                          ("sh" "./configure")
                          ("make"))
       :info "doc")

 ))


(when (el-get-executable-find "svn")
  (loop for p in '(psvn    		; M-x svn-status
		   yasnippet		; powerful snippet mode
		   )
	do (add-to-list 'el-get-sources p)))

(el-get 'sync my:el-get-packages)

(require 'memoize)
(provide 'fd-el-get)
