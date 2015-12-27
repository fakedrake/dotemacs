;; Cache lines in compilation mode so that emacs doesn't freeze due to
;; font-locking when huge lines are outputted.

(defvar commint-max-line-lenght 300)
(defvar commint-line-prefix-len 35)

(defun unfold-string (&optional pos)
  "Unfold folded-string in buffer"
  (interactive)
  (let ((str (get-char-property (or pos (point)) 'folded-string)))
    (if (null str)
        (message "No folded string under point.")
      (pop-to-buffer " *folded-string*")
      (insert str)
      (beginning-of-buffer))))

(defun fd-compilation-hook ()
  (message "Compilation!!"))

(defun shorten (str)
  (if (< commint-max-line-lenght (length str))
      (let ((prefix-str (substring str nil commint-line-prefix-len))
            (postfix-str
             (substring str (- (length str) commint-line-prefix-len) nil))
            (mid " [...folded...] "))
        (concat prefix-str (propertize mid 'face 'bold-italic 'folded-string str)
                postfix-str))
    str))

(defun shorten-lines (str)
  (let ((reverse-all-lines (reverse (split-string
                                     (concat commint--line-buffer str) "\n"))))
    (setq commint--line-buffer (car reverse-all-lines))
    (mapconcat 'shorten
               (reverse (cons "" (cdr reverse-all-lines)))
               "\n")))


;;;###autoload
(define-minor-mode cache-line-mode
  "Get your foos in the right places."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'unfold-string)
            map)
  :lighter " LineCache"
  (add-hook 'comint-preoutput-filter-functions 'shorten-lines)
  (make-variable-buffer-local (defvar commint--line-buffer "")))

;;;###autoload
(add-hook 'compilation-shell-minor-mode-hook 'cache-line-mode)

(require 'fd-cache-line-mode)
