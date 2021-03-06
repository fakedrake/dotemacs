;; Lisp
;; Elisp
(define-key emacs-lisp-mode-map "\C-c\C-e" 'eval-buffer)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(defun fd-elisp-hooks ()
  (rainbow-delimiters-mode 1)
  (define-key emacs-lisp-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key emacs-lisp-mode-map (kbd "C-M-d") 'edebug-defun))
(require 'slime-autoloads)
(add-hook 'emacs-lisp-mode-hook 'fd-elisp-hooks)
(setq slime-contribs '(slime-fancy))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; ;; Slime
;; (require 'clojure-mode)
;; (require 'cider)
;; (defvar fd-clojure-etags-gen-command "find . \! -name '.*' -name '*.clj' \
;;     | xargs etags \
;;     --regex='/[ \t\(]*def[a-zA-Z!$%&*+\-.\/:<=>?@^_~]*[ \n\t]+\(^{[^}]*}[ \n\t]+\|\)\([a-zA-Z!$%&*+\-.\/:<=>?@^_~]+\)/\2/s' \
;;     --regex='/[ \t\(]*ns \([a-z.]+\)/\1/'"
;;   "Found see here http://stackoverflow.com/questions/1481842/clojure-emacs-etags#6726061")

;; (defun fd-clojure-mode-hook ()
;;   "Hooks for all clojure."
;;   (setq-local etags-table-create-table-command fd-clojure-etags-gen-command))

;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; (add-hook 'clojure-mode-hook 'fd-clojure-mode-hook)

;; (require 'ac-nrepl)
;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)


;; XXX: Automatically run sbcl
; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (require 'slime)
;; (slime-setup '(slime-fancy slime-tramp slime-asdf))
;; (slime-require :swank-listener-hooks)
;; (setq scheme-program-name "chez")

(provide 'fd-lisp)
