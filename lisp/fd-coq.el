;; (setq my-coq-symbols
;;       '(("forall" . 8704)
;;         ("->" . 8594)
;;         ("exists" . 8707)
;;         ("=>" . 8658)
;;         ("False" . 8869)
;;         ("True" . 8868)))
(defun my-coq-hook ()
  (define-key coq-mode-map (kbd "C-c C-l") 'proof-process-buffer))
(add-hook 'coq-mode-hook 'my-coq-hook)

(provide 'fd-coq)
