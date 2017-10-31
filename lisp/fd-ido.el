
;; Ido mode
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file (my-expand-path ".ido.last"))
(setq ido-auto-merge-work-directories-length -1)
(ido-everywhere t)
;; This is mainly for just swapped letters. It sometimes doesnt catch
;; entire words
(setq ido-file-extensions-order '(".js" ".c" ".cpp" ".h" ".py" ".org" ".el" ".clj" ".org"))
(setq org-completion-use-ido t)

(setq yas-prompt-functions '(yas-ido-prompt))

(provide 'fd-ido)
