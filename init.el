;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'cl)

(load-file "~/.emacs.d/lisp/fd-perliminaries.el")

(if (not (file-exists-p "~/.emacs.d/lisp/fd-personal.el"))
    (error "Make lisp/fd-personal.el or we will be missig: my-znc-nick, my-znc-password, my-znc-fullname")
  (require 'fd-personal))
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
(require 'fd-mail)
(require 'fd-company)
(require 'fd-aspell)
(require 'fd-idris)
; (require 'fd-emms)

(setq enable-local-variables :all)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(message "Welcome to emacs!")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background nil :underline t :weight bold)))))
(put 'narrow-to-page 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (edit-indirect)))
 '(safe-local-variable-values
   (quote
    ((compile-root . "/Users/drninjabatman/Projects/UoE/fluidb/resources/include/")
     (compile-root . "/Users/drninjabatman/Scratch/tpch_2_17_0/dbgen/")
     (compile-root . "/home/drninjabatman/Projects/FluiDB/resources/")
     (compile-root . "/Users/drninjabatman/Projects/UoE/fluidb/resources/")
     (compile-root . "/Users/drninjabatman/Projects/CV-for-me/")
     (compile-root . "/home/drninjabatman/Projects/FluiDB/tests/")
     (compile-root . "/Users/drninjabatman/Downloads/")
     (compile-root . "/Users/drninjabatman/Projects/UoE/fluidb/")
     (compile-root . "/Users/drninjabatman/Documents/Inf2A_Prac1_Marking/")
     (compile-root . "/Users/drninjabatman/Projects/advent/")))))
