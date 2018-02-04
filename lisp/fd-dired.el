;; Dired related.

(defun dired-sort-size ()
  "Dired sort by size."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S")))

(defun dired-sort-extension ()
  "Dired sort by extension."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X")))

(defun dired-sort-ctime ()
  "Dired sort by create time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ct")))

(defun dired-sort-utime ()
  "Dired sort by access time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "ut")))

(defun dired-sort-time ()
  "Dired sort by time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))

(defun dired-sort-name ()
  "Dired sort by name."
  (interactive)
  (dired-sort-other (concat dired-listing-switches "")))

(setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))

(defun dired-source-find-file ()
  "When in a dired buffer find the file under cursor and set the
`dired-source-buffer' local variable so that
`dired-source-next-file' and `dired-source-prev-file' can jump
directly up and down the list"
  (interactive)
  (let ((dbuf (current-buffer)))
    (dired-find-file)
    (setq-local dired-source-buffer dbuf)))

(defun dired-source-next-file ()
  "Jump to the next file in the `source-dired-buffer'. The
variable is set when jumping to the file using
`dired-source-find-file'"
  (interactive)
  (with-current-buffer dired-source-buffer
    (dired-next-line 1)
    (dired-source-find-file)))

(defun dired-source-prev-file ()
  "Jump to the next file in the `source-dired-buffer'. The
variable is set when jumping to the file using
`dired-source-find-file'"
  (with-current-buffer dired-source-buffer
  (interactive)
    (dired-next-line -1)
    (dired-source-find-file)))

(define-key image-mode-map (kbd "C-M-n") 'dired-source-next-file)
(define-key image-mode-map (kbd "C-M-p") 'dired-source-prev-file)
(define-key dired-mode-map (kbd "RET") 'dired-source-find-file)

(provide 'fd-dired)
