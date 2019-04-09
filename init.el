;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun timed-require (sym)
  (progn
    (require sym)
    (message (format-time-string
              (concat (symbol-name sym)
                      " finished loading at:\t%H:%M:%S.%3N")))))

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "/usr/local/bin")

(require 'cl)

(load-file "~/.emacs.d/lisp/fd-perliminaries.el")

(if (not (file-exists-p "~/.emacs.d/lisp/fd-personal.el"))
    (error "Make lisp/fd-personal.el or we will be missig: my-znc-nick, my-znc-password, my-znc-fullname")
  (timed-require 'fd-personal))
(when (eq system-type 'darwin) ;; mac specific settings
  (timed-require 'fd-macosx))
(timed-require 'fd-el-get)
(timed-require 'fd-misc)
(timed-require 'fd-fluidb)
(timed-require 'fd-misc-programming)
(timed-require 'fd-automode)
(timed-require 'fd-javascript)
(timed-require 'fd-clipboard)
; (timed-require 'fd-ido)
(timed-require 'fd-helm)
(timed-require 'fd-yasnippet)
; (timed-require 'fd-autocomplete)
(timed-require 'fd-python)
(timed-require 'fd-undotree)
(timed-require 'fd-recentfiles)
;; (timed-require 'fd-erc)
;; (timed-require 'fd-desktop)
(timed-require 'fd-bookmarks)
(timed-require 'fd-tags)
(timed-require 'fd-lisp)
(timed-require 'fd-cc-mode)
(timed-require 'fd-opengl)
(timed-require 'fd-prolog)
(timed-require 'fd-vimperator)
(timed-require 'fd-org)
(timed-require 'fd-midnight)
(timed-require 'fd-dired)
; (timed-require 'fd-term)
(timed-require 'fd-compilation)
(timed-require 'fd-imenu)
(timed-require 'fd-codebender)
(timed-require 'fd-sql)
(timed-require 'fd-agenda)
(timed-require 'fd-notify)
(timed-require 'fd-projects)
(timed-require 'fd-haskell)
(timed-require 'fd-jstest)
(timed-require 'fd-image)
(timed-require 'fd-git)
(timed-require 'fd-eshell)
(timed-require 'fd-visual)
; (timed-require 'fd-coq)
(timed-require 'fd-notmuch)
(timed-require 'fd-mail)
(timed-require 'fd-company)
(timed-require 'fd-aspell)
(timed-require 'fd-idris)
; (timed-require 'fd-emms)

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
 '(org-agenda-files
   (quote
    ("~/Projects/UoE/fluidb/caching.org" "~/.track.org")))
 '(package-selected-packages (quote (edit-indirect)))
 '(safe-local-variable-values
   (quote
    ((compile-root . "/Users/drninjabatman/.emacs.d/el-get/haskell-mode/")
     (compile-root . "/Users/drninjabatman/.emacs.d/")
     (compile-root . "/home/drninjabatman/Projects/cv-for-me/")
     (compile-root . "/Users/drninjabatman/Projects/UoE/fluidb/resources/include/")
     (compile-root . "/Users/drninjabatman/Scratch/tpch_2_17_0/dbgen/")
     (compile-root . "/home/drninjabatman/Projects/FluiDB/resources/")
     (compile-root . "/Users/drninjabatman/Projects/UoE/fluidb/resources/")
     (compile-root . "/Users/drninjabatman/Projects/CV-for-me/")
     (compile-root . "/home/drninjabatman/Projects/FluiDB/tests/")
     (compile-root . "/Users/drninjabatman/Downloads/")
     (compile-root . "/Users/drninjabatman/Projects/UoE/fluidb/")
     (compile-root . "/Users/drninjabatman/Documents/Inf2A_Prac1_Marking/")
     (compile-root . "/Users/drninjabatman/Projects/advent/"))))
 '(tramp-syntax (quote default) nil (tramp)))
