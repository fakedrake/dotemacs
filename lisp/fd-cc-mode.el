;; CC-MODE
;; Requires: c-eldoc
(require 'flycheck)
(setq flycheck-clang-language-standard "c++20")
(defun my-cc-newline-and-indent ()
  "Append a newline first if the cursor is between { and }."
  (interactive)
  (when (and (not (nth 8 (syntax-ppss)))
	     (looking-back "{\s*")
	     (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (newline-and-indent))

(defun c++-to-headers-mode ()
  "Change the mode of a c++ header file to c++-mode if there is
at least one .cpp file in the same directory."
  (when (and (s-ends-with? ".h" (buffer-file-name))
             (eq major-mode 'c-mode)
             (delete-if-not
              (lambda (f) (or (s-ends-with? ".cc" f) (s-ends-with? ".cpp" f)))
              (directory-files default-directory)))
    (c++-mode)))

(setq google-like-c-style

  '("google"
    (c-offsets-alist (access-label . [1]) (innamespace . [0]))))

(defun rtags-eldoc-function ()
  (let* ((symbol (rtags-symbol-info-internal
                  :location (or (rtags-target-declaration-first)
                                (rtags-current-location))
                  :silent t))
         (symbol-text (cdr (assoc 'contents
                                  (rtags-get-file-contents
                                   :info symbol
                                   :maxlines (or 1 5))))))
    (when symbol symbol-text)))

; (add-to-list 'flycheck-clang-warnings "pedantic")

(defun fakedrake-c/c++-mode-init ()
  (irony-mode)
  (c++-to-headers-mode))

(defun fakedrake-cc-mode-init ()
  "Just some initializations I need for C"
  (rainbow-delimiters-mode)
  ; (setq eldoc-documentation-function 'rtags-eldoc-function)
  (flycheck-mode)
  (mapc
   (function
    (lambda (sym)
      (put sym 'delete-selection nil)	; for delsel (Emacs)
      (put sym 'pending-delete nil)))	; for pending-del (XEmacs)
   '(c-electric-pound
     c-electric-brace
     c-electric-slash
     c-electric-star
     c-electric-semi&comma
     c-electric-lt-gt
     c-electric-colon
     c-electric-paren))
  (define-key c-mode-base-map (kbd "C-M-n") 'c-end-of-defun)
  (define-key c-mode-base-map (kbd "C-M-p") 'c-beginning-of-defun)
  (define-key c-mode-base-map (kbd "M-n") 'c-end-of-statement)
  (define-key c-mode-base-map (kbd "M-p") 'c-beginning-of-statement)
  (define-key c-mode-base-map (kbd "C-j") 'my-cc-newline-and-indent)
  (define-key c-mode-base-map (kbd "C-x <SPC>") 'gud-break)
  ; (setq-local flycheck-clang-language-standard "gnu++17")
  (c-add-style "google-like" google-like-c-style)
  (setq c-default-style "google-like" c-basic-offset 4))

(setq compilation-scroll-output t)
(add-hook 'c-mode-hook 'fakedrake-cc-mode-init)
(add-hook 'c++-mode-hook 'fakedrake-cc-mode-init)
(add-hook 'java-mode-hook 'fakedrake-cc-mode-init)


(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
; (add-to-list 'ido-ignore-buffers ".*-preprocessed\*")

;; ;; Sometimes I dont want emacs to indent comments at all.
;; (setq fd-c-disable-comments-lineup nil)
;; (defadvice c-lineup-C-comments (around c-lineup-C-comments-handle-doxygen activate)
;;   (let ((looking-at (if fd-c-disable-comments-lineup (lambda (s) nil) (symbol-function 'looking-at))))
;;     ad-do-it))
(setq c-macro-prompt-flag t)

(defun c-comment-includes ()
  (save-excursion
    (goto-char (point-min))
    ;; preserve the char count
    (replace-string "#include" "// clude")))

(add-hook 'c-macro-expansion-pre-hook 'c-comment-includes)
(defadvice c-macro-expansion (around c-macro-expansion-hooks activate)
  (let ((contents (buffer-string)))
    (with-temp-buffer
      (insert contents)
      (run-hooks 'c-macro-expansion-pre-hook)
      ad-do-it)))

(defun disable-ggtags-company-on-tramp-files (orig-func command &rest args)
  "When dealing with remote files gtags-company will make tramp
open a remote connection with every key stroke. This advice
function will stop ggtags completion for ssh remote files."
  (if (and (buffer-file-name)
           (string-prefix-p "/ssh:" (buffer-file-name))
           (eq command 'prefix))
      'stop (apply orig-func command args)))
(advice-add 'company-gtags :around #'disable-ggtags-company-on-tramp-files)
(advice-add 'company-clang :around #'disable-ggtags-company-on-tramp-files)

(provide 'fd-cc-mode)
