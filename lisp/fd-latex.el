(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-parse-self t)

(add-to-list 'LaTeX-verbatim-environments "lstlisting")
(defun fd-latex-mode-hook ()
  (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)
  (LaTeX-add-environments "align" "align*"))
(add-hook 'LaTeX-mode-hook 'fd-latex-mode-hook)
(provide 'fd-latex)
