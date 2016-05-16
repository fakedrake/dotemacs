(when nil
  ;; Auto Complete
  (require 'auto-complete)
  (require 'auto-complete-config)
  ;; (require 'auto-complete+)
  ;; (require 'auto-complete-extension)
  ;; (require 'auto-complete-yasnippet)
  ;; (require 'auto-complete-etags)
  ;; (require 'ac-python)
  ;; (require 'auto-complete-emacs-lisp)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories (expand-file-name "dictionaries"))
  (add-to-list 'ac-modes 'org-mode)
  (setq ac-use-fuzzy t)
  (set-default 'ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-filename ac-source-words-in-same-mode-buffers))
  (setq ac-ignore-case nil)

  ;; Fixed some weird error with emacs 24.3
  (defalias 'cl-defsubst-expand 'cl--defsubst-expand))

(provide 'fd-autocomplete)
