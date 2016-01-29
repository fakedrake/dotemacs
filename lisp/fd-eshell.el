;; XXX: Turn this into a macro for any mode.

(defun checked-symbol (sym-name) (intern sym-name))
;; (let (sym (intern sym-name))
;;   (if (boundp sym)
;;       (error "Already defined " (sybol-name sym))
;;     sym)))

(defmacro fd-def-mode-management (name mode prefix start-fn)
  "Make bury/digup/kill/start buffer of MODE and globally bind:
\"PREFIX s\" - > start or unburry
"

  (let ((fd-destroy (checked-symbol (concat "fd-managed-destroy-" name)))
        (fd-bury (checked-symbol (concat "fd-managed-bury-" name)))
        (fd-digup (checked-symbol (concat "fd-managed-digup-" name)))
        (fd-digup-or-start (checked-symbol (concat "fd-managed-digup-or-start-" name)))
        (fd-get-buffer (checked-symbol (concat "fd-managed-get-buffer-" name)))
        (mode-name (symbol-name mode)))

    (unless (boundp (quote mode)) (error (format "No such mode as %s" mode-name)))
    `(progn

       ;; Destroy all buffers of our type
       (defun ,fd-destroy ()
         ,(format "Kill all %s buffers!!" mode-name)
         (interactive)
         (when (y-or-n-p ,(format "Really kill all %s buffers?" mode-name))
           (message ,(format "Killing all %s buffers!" mode-name))
           (save-excursion
             (dolist (i (buffer-list))
               (with-current-buffer i
                 (cond
                  ((eq major-mode (quote ,mode)) (kill-buffer (current-buffer)))))))))

       ;; Find a buffer of our type
       (defun ,fd-get-buffer (buffers)
         (if (or (null (car buffers))
                 (eq (with-current-buffer (car buffers) major-mode) (quote ,mode)))
             (car buffers)
           (,fd-get-buffer (cdr buffers))))

       ;; If there is no buffer of mode, run the start
       ;; function. Otherwise dig them up
       (defun ,fd-digup-or-start ()
         ,(format "Try to digup all %s buffers. If that fails start them."
                  mode-name)
         (interactive)
         (if (,fd-get-buffer (buffer-list))
             (,fd-digup)
           (funcall ,start-fn)))

       ;; Unbury buffers of our type
       (defun ,fd-digup ()
         ,(format "Dig up all %s buffers!!" mode-name)
         (interactive)
         (message (format "Digging up %s buffers..." mode-name))
         (dolist (i (reverse (buffer-list)))
           (with-current-buffer i
             (when (eq major-mode (quote ,mode))
               (switch-to-buffer i)))))

       ;; Bury all buffers of our type.
       (defun ,fd-bury ()
         ,(format "Bury all %s buffers!!" mode-name)
         (interactive)
         (message (format "Burying %s buffers..." mode-name))
         (dolist (i (buffer-list))
           (with-current-buffer i
             (when (eq major-mode (quote ,mode))
               (bury-buffer)))))


       ;; switch to ESHELL with Ctrl+c e
       (global-set-key (kbd (concat ,prefix " s")) (quote ,fd-digup-or-start))
       (global-set-key (kbd (concat ,prefix " b")) (quote ,fd-bury))
       (global-set-key (kbd (concat ,prefix " k")) (quote ,fd-destroy)))))

(defun new-eshell ()
  (interactive)
  (eshell t))

(defun fd-eshell-update-bufname ()
  (rename-buffer
   (format "*eshell:%s:%s*" default-directory
           (if (and
                eshell-last-command-name
                (string-match "#<function eshell/\\(.*\\)>" eshell-last-command-name))
               (match-string 1 eshell-last-command-name)
             eshell-last-command-name))))

(defun fd-eshell-dirchange ()
  (fd-eshell-update-bufname))

(defun fd-eshell-pre-command ()
  (fd-eshell-update-bufname))

(defun fd-eshell-post-command ()
  (fd-eshell-update-bufname))


(defun fd-eshell-hook ()
  ;; Aliases
  (eshell/alias "d" "dired $1")
  (eshell/alias "e" "find-file $1")

  ;; Keys
  (define-key eshell-mode-map (kbd "M-m") 'eshell-bol)
  (define-key eshell-mode-map (kbd "C-c s n") 'new-eshell)
  (fd-eshell-update-bufname))

(add-hook 'eshell-mode-hook 'fd-eshell-hook)
(add-hook 'eshell-directory-change-hook 'fd-eshell-dirchange)
(add-hook 'eshell-pre-command-hook 'fd-eshell-pre-command)
(add-hook 'eshell-post-command-hook 'fd-eshell-post-command)

(fd-def-mode-management "eshell" eshell-mode "C-c s"
                        (lambda () (message "Starting eshell") (eshell)))


(provide 'fd-eshell)
