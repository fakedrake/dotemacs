
;; Ido mode
(require 'ido)
(require 'ido-speed-hack)
(require 'ido-better-flex)
(require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-save-directory-list-file (my-expand-path ".ido.last"))
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
;; (ido-everywhere t)
;; This is mainly for just swapped letters. It sometimes doesnt catch
;; entire words
(ido-better-flex/enable)
(setq ido-file-extensions-order '(".c" ".cpp" ".h" ".py" ".org" ".el" ".clj"))
(setq org-completion-use-ido t)

;; TODO: if you actually are at the buffer at point, rotate.
(defun ido-for-mode (prompt the-mode &optional not-found-fn keyword-fn)
  "Switch to buffer of the-mode using prompt use keyword-fn to
for a defun that will proide alternative names and not-found-fn
will get the user input and the alist of (displayed-name
. buffer) that we were completing on if the search fails so you
can create a buffer and switch to it or match in a stranger way."
  (let* ((keyword-cons (if keyword-fn
			   (lambda (b) (cons (funcall keyword-fn b) b))
			 (lambda (b) (cons b b))))
	 (key-buf-alist (mapcar keyword-cons
				(fd-mode-buffers the-mode)))
	 (input (ido-completing-read prompt key-buf-alist))
	 (target-buffer (cdr (assoc input key-buf-alist))))
    (if target-buffer
	(switch-to-buffer target-buffer)
      (when not-found-fn
	(funcall not-found-fn input key-buf-alist)))))

(defun fd-mode-buffers (the-mode)
  "List of buffers of mode THE-MODE."
  (save-excursion
    (delq
     nil
     (mapcar (lambda (buf)
	       (when (and (buffer-live-p buf) (not (eq (current-buffer) buf)))
		 (with-current-buffer buf
		   (and (eq major-mode the-mode)
			(buffer-name buf)))))
	     (buffer-list)))))

(setq yas-prompt-functions '(yas-ido-prompt))

(provide 'fd-ido)
