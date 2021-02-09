(setq coq-prettify-symbols-alist
      '(("[[" . #x2726)
        ("]]" . #x27e7)
        ("->" . ?→)
        ("exists" . ?∃)
        ("=>" . ?⇒)
        ("forall" . ?∀)
        (">>" . ?»)
        ("|-" . ?⊦)
        ("||-" . ?⊩)
        ("|=" . ?⊧)
        ("|<=" . ?⊑)
        ("\\/" . ?∨)
        ("/\\" . ?∧)
        ("{{" . #x2983)
        ("}}" . #x2984)))


(defun my-coq-hook ()
  (define-key coq-mode-map (kbd "C-c C-l") 'proof-process-buffer))
(add-hook 'coq-mode-hook 'my-coq-hook)

(provide 'fd-coq)
