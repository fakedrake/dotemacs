(load-file (if (executable-find "agda-mode")
               (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate"))
             "/usr/share/emacs/site-lisp/agda/agda2.el"))

(provide 'fd-agda)
