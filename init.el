;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'cl)

(load-file "~/.emacs.d/lisp/fd-perliminaries.el")
(require 'fd-personal)
(when (eq system-type 'darwin) ;; mac specific settings
  (require 'fd-macosx))
(require 'fd-el-get)
(require 'fd-misc)
(require 'fd-misc-programming)
(require 'fd-automode)
(require 'fd-javascript)
(require 'fd-clipboard)
(require 'fd-ido)
(require 'fd-yasnippet)
; (require 'fd-autocomplete)
(require 'fd-python)
(require 'fd-undotree)
(require 'fd-recentfiles)
(require 'fd-erc)
;; (require 'fd-desktop)
(require 'fd-bookmarks)
(require 'fd-tags)
(require 'fd-lisp)
(require 'fd-cc-mode)
(require 'fd-prolog)
(require 'fd-vimperator)
(require 'fd-org)
(require 'fd-midnight)
(require 'fd-dired)
(require 'fd-term)
(require 'fd-compilation)
(require 'fd-imenu)
(require 'fd-codebender)
(require 'fd-sql)
(require 'fd-agenda)
(require 'fd-notify)
(require 'fd-projects)
(require 'fd-haskell)
(require 'fd-jstest)
(require 'fd-image)
(require 'fd-git)
(require 'fd-eshell)
(require 'fd-visual)
; (require 'fd-coq)
(require 'fd-notmuch)
(require 'fd-company)
(require 'fd-aspell)

(setq enable-local-variables :all)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(message "Welcome to emacs!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(safe-local-variable-values
   (quote
    ((compile-root . "/Users/drninjabatman/bin/")
     (haskell-process-log t)
     (haskell-process-args-ghci "repl")
     (haskell-process-path-ghci "cabal")
     (compile-root . "/Users/drninjabatman/Projects/Codebendercc/BFSuite/")
     (compile-root))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-page 'disabled nil)
