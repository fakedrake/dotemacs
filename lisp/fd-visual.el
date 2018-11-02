;; Visual Settings
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                                          ("%b - Dir:  " default-directory)))))))

;; Extra keywords
(setq my:elisp-extra-keywords '("and" "interactive" "or" "cons" "list" "setq-default" "setq" "setf" "set"))

(font-lock-add-keywords 'emacs-lisp-mode (list (cons (regexp-opt my:elisp-extra-keywords 'symbols) font-lock-keyword-face)))

(line-number-mode 1)	; have line numbers and
(column-number-mode 1)	; column numbers in the mode line
(mouse-avoidance-mode 'banish)
(tool-bar-mode -1)	; no tool bar with icons
(menu-bar-mode -1)
(scroll-bar-mode -1)	; no scroll bars
(add-hook 'find-file-hook (lambda () (setq show-trailing-whitespace t)))
(global-linum-mode 1)	; add line numbers on the left
(show-paren-mode t)

(add-hook 'term-mode-hook '(lambda() (set (make-local-variable 'global-hl-line-mode) nil)))

;; full screen
(defun fullscreen ()
  "Set the fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(set-face-attribute 'default nil :height 140)

;; Zoom
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute
   'default nil :height
   (+ (face-attribute 'default :height)
      (* n 5)))
  (message (format "Font size: %d"
		   (face-attribute 'default :height))))

(global-set-key (kbd "M-+")      #'(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "M--")      #'(lambda nil (interactive) (djcb-zoom -2)))

;; XXX: Have this be the last thing you do. Apparently things get
;; overriden if it is at the beginning of the file.
(defun set-theme ()
  (interactive)
  (require 'naquadah-theme)
  (if window-system
      (let ((comment "IndianRed2"))
        (global-hl-line-mode t)
        (load-theme 'naquadah t)
        (custom-theme-set-faces
         'naquadah
         ; `(default ((t (:family "Ubuntu Mono" :background "#262B2C"))))
         `(default ((t (:family "Fira Code" :background "#262B2C"))))
         `(mode-line ((t (:height 1.1 :background "gray30"))))
         `(highlight ((t (:background nil :underline t :weight bold))))
         `(minibuffer-prompt ((t (:foreground "orange1"))))
         `(region ((t (:background "gray35"))))
         `(hl-line ((t (:background "gray25"))))
         `(ido-only-match ((t (:foreground "dark green" :bold nil))))
         `(linum ((t (:inherit default :background "#0c191C" :foreground "gray50"))))
         `(haskell-debug-newline-face ((t (:inherit linum))))
         `(haskell-debug-trace-number-face ((t (:inherit linum))))
         `(comint-highlight-prompt ((t (:inherit font-lock-function-name-face))))

         ;; Development
         `(font-lock-comment-face ((t (:foreground ,comment))))
         `(font-lock-function-name-face ((t (:foreground "orange1" :bold t))))
         `(font-lock-doc-face ((t (:foreground ,comment))))
         `(font-lock-doc-string-face ((t (:foreground ,comment))))
         `(link ((t (:foreground  "#729fcf" :underline t))))

         ;; ERC
         `(erc-prompt-face ((t (:background "#f57900" :bold t :foreground "gray10"))))))))
(set-theme)

(defun fd-new-buf-extension (dir &optional default path-len)
  "Return either the last two directories, or default"
  (if dir
      (let* ((dirs ))
	(mapconcat
         (lambda (s) (format "%s/" s))
         (reverse (seq-take (reverse (split-string dir "/" t)) 2))
         nil))
    default))

(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (when buffer-file-name
    (add-to-list 'mode-line-buffer-identification
		 '(:eval (fd-new-buf-extension default-directory "unlinked: ")))))
(add-hook 'find-file-hook 'add-mode-line-dirtrack)


;; (setq display-time-day-and-date t
;;       display-time-24hr-format t)
;; (display-time)

;; Remove which func from modeline
(setq mode-line-misc-info (cdr mode-line-misc-info))

;; Put which-func earlier in modeline
(setq-default mode-line-format
      '((:eval (my-mode-line-vc-info))
        mode-line-front-space
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        mode-line-frame-identification
        mode-line-buffer-identification
        (which-func-mode
         (":" which-funce-format " "))
        "   "
        mode-line-position
        "  " mode-line-modes
        mode-line-misc-info
        mode-line-end-spaces))

(setq-default cursor-type 'box)

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*" display-buffer-reuse-window
               ((reusable-frames . t))))

(setq initial-frame-alist nil)

(provide 'fd-visual)
