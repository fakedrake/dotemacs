;; key bindings
  ;; Fn screws thing up in macs
  (global-set-key (kbd "<S-backspace>") 'backward-kill-word)
  (global-set-key (kbd "<end>") 'forward-word)
  (global-set-key (kbd "<home>") 'backward-word)

; (osx-key-mode -1)  ; no Mac-specific key bindings

; Frame and window management:

;(tabbar-mode -1)		     ; no tabbar
;(one-buffer-one-frame-mode -1)       ; no one-buffer-per-frame
'(setq special-display-regexps nil)   ; do not open certain buffers in special windows/frames
; (smart-frame-positioning-mode -1)  ; do not place frames behind the Dock or outside of screen boundaries

(tool-bar-mode 0) ; turn off toolbar
(scroll-bar-mode -1)  ; no scrollbars

;; Appearance
; (aquamacs-autoface-mode -1)                                ; no mode-specific faces, everything in Monaco
; (set-face-attribute 'mode-line nil :inherit 'unspecified) ; show modeline in Monaco
; (set-face-attribute 'echo-area nil :family 'unspecified)  ; show echo area in Monaco


 ;; Editing

; (global-smart-spacing-mode -1)  ; not on by default
(remove-hook 'text-mode-hook 'smart-spacing-mode)   ; do not use smart spacing in text modes
(global-visual-line-mode -1)  ; turn off Emacs 23 visual line
(cua-mode nil)
; (transient-mark-mode nil)  ; (must switch off CUA mode as well for this to work)

(setq mac-option-modifier 'meta
      mac-command-modifier 'meta
      mac-function-modifier 'control
)
(setq ns-option-modifier 'meta
      ns-command-modifier 'meta
      ns-function-modifier 'control)

(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
(set-face-font 'default "-*-Ubuntu Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

(require 'mailcap)
(setcdr (assoc 'viewer
               (assoc "pdf" (assoc "application" mailcap-mime-data))) "open %s")

(set-keyboard-coding-system 'mac-roman)
(set-selection-coding-system 'mac-roman)
(setq el-get-emacs "/Applications/Emacs.app/Contents/MacOS/Emacs")
(setenv "EMACS" "/Applications/Emacs.app/Contents/MacOS/Emacs")

(provide 'fd-macosx)
