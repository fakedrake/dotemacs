(when (boundp 'global-auto-complete-mode)
  (setq global-auto-complete-mode -1))

(add-hook 'after-init-hook 'global-company-mode)
; (global-set-key "\t" 'company-complete-common)
(setq company-idle-delay 0)

(provide 'fd-company)
