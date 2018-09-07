(defun haskell-collect-imports (&optional buffer)
  (let ((buf (or buffer (current-buffer))) (ret nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^import .*$" nil t)
        (setq ret (cons (buffer-substring (match-beginning 0) (match-end 0)) ret))))
    ret))

(defun haskell-send-imports-internal (process imports done)
  (if (null imports) (and done (funcall done))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list (car imports) process (cdr imports) done)
      :go (lambda (state)
            (message (concat "Importing: " (car state)))
            (haskell-process-send-string (cadr state) (car state)))
      :live (lambda (state response) nil)
      :complete (lambda (state response)
                  (apply 'haskell-send-imports-internal (cdr state)))))))

(defun haskell-process-load-file-and-then-imports ()
  (interactive)
  (call-interactively 'haskell-process-load-file)
  (haskell-send-imports (haskell-process)))

(defun haskell-send-imports (process &optional buffer done)
  (haskell-send-imports-internal process (haskell-collect-imports buffer) done))

(defun fd-haskell-load-region ()
  (interactive)
  (if (not (region-active-p)) (call-interactively haskell-process-load-file)
    (let* ((tmp (concat (make-temp-file "haskell-region") ".hs"))
           (haskell-session (haskell-session))
           (cmd (format "load \"%s\"" (replace-regexp-in-string
                                       "\""
                                       "\\\\\""
                                       tmp))))
      (write-region (region-beginning) (region-end) tmp)
      (haskell-process-file-loadish cmd nil buf)
      (delete-file tmp))))

(provide 'fd-haskell-interactive)
