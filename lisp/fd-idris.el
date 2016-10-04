(defun idris-prover-hook ()
  (company-mode -1))

(add-hook 'idris-prover-script-mode-hook 'idris-prover-hook)
(provide 'fd-idris)
