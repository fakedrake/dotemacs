(if (executable-find "agda-mode")
    (load-file
     (let ((coding-system-for-read 'utf-8))
       (shell-command-to-string "agda-mode locate")))
  (warn "Couln't find agda2-mode executable."))

(provide 'fd-agda)
