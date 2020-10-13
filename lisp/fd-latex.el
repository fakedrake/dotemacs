(load "auctex.el")
(load "preview-latex.el")
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-parse-self t)

(add-to-list 'LaTeX-verbatim-environments "lstlisting")
(defun fd-latex-mode-hook ()
  (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)
  (LaTeX-add-environments "align" "align*")
  (flycheck-mode))
(add-hook 'LaTeX-mode-hook 'fd-latex-mode-hook)

(flycheck-define-checker tex-textidote
  "A LaTeX grammar/spelling checker using textidote.

  See https://github.com/sylvainhalle/textidote"
  :modes (LaTeX-mode plain-tex-mode)
  :command ("java" "-jar"
            (eval
             (expand-file-name "/Users/drninjabatman/Scratch/textidote/textidote.jar")) "--read-all"
            "--check en"
            "--no-color" source-inplace)
  :error-patterns (
                   (warning line-start "* L" line "C" column "-" (one-or-more alphanumeric) " "
                            (message (one-or-more (not (any "]"))) "]"))))



(provide 'fd-latex)
