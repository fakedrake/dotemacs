
;; Prolog
(defun fd-prolog-hook ()
  (setq-local prolog-system 'swi))

(add-hook 'prolog-mode-hook 'fd-prolog-hook)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))
(provide 'fd-prolog)
