;; GGtags
;;
;; Add new directories with
;; $ export GTAGSLIBPATH=/usr/src/lib:/usr/src/sys
;; Or put that in .globalrc at the root of the project.
;;
;; Use only specific files with
;; $ find */myproject -type f -print >gtags.files
;; $ gtags

(require 'ggtags)
(defun fd-tagging-hook ()
  (setq-local eldoc-documentation-function 'ggtags-eldoc-function)
  (setq-local imenu-create-index-function 'ggtags-build-imenu-index)
  (ggtags-mode 1)
  (setq ggtags-mode-map-alist nil))


;; (require 'rtags)
;; (defun fd-tagging-hook ())

(dolist (m '(c-mode-common-hook
	     python-mode
	     js2-mode))
  (add-hook m 'fd-tagging-hook))

;; (global-set-key (kbd "M-.") 'ggtags-find-tag-dwim)

(setq tags-revert-without-query t)
(provide 'fd-tags)
