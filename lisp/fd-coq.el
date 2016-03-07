(setq coq-symbols
      '(("forall" 8704)
        ("->" 8594)
        ("exists" 8707)
        ("=>" 8658)
        ("False" 8869)
        ("True" 8868)))

(add-hook 'coq-mode-hook
          (lambda () (setq prettify-symbols-alist coq-symbols)))

(provide 'fd-coq)
