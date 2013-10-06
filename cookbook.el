;; This is a cookbook I will fill with random elisp I write and might
;; need in the future but dont want loaded with emacs.

;; Helpers for sorting grep results
(defun file-size (filename)
  "Size of the file."
  (nth 7 (file-attributes filename)))

(defun grep-current-line-filesize ()
  ""
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((p1 (point)))
      (search-forward ":") (backward-char)
      (let ((fsize (file-size (buffer-substring p1 (point)))))
	(beginning-of-line)
	(insert (format "filesize:%d| " fsize))))))

(defun remove-grep-line-filesize ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp "^filesize:[0-9]+| " "")))

(defun current-line ()
  (count-lines (point-min) (point)))


(defun point-at-coordinates (col line)
  (save-excursion
    (goto-line line)
    (beginning-of-line)
    (goto-char (+ (point) col))
    (point)))

(defun lesson-name (code)
  "Find the lesson name based on code from a text file extracted
with 'pdftotext -layout'."
  (save-excursion
    (save-restriction
      (beginning-of-buffer)
      (search-forward code)
      (if (looking-at "[\s- ]+[0-9]")
	  (let ((start-col (current-column))
		(end-col (progn (search-forward-regexp "[\s-]*") (current-column)))
		(previous-line (1- (current-line)))
		(next-line (1+ (current-line))))
	    (concat
	     (buffer-substring
	      (point-at-coordinates start-col previous-line)
	      (point-at-coordinates end-col previous-line))
	     (buffer-substring
	      (point-at-coordinates start-col next-line)
	      (point-at-coordinates end-col next-line))))

	;; from here to the next space
	(let ((start (point)))
	  (search-forward-regexp "[\s-][0-9]")
	  (buffer-substring start (- (point) 2)))))))


(defun lesson-codes (code-regex &optional begin-point)
  "Use regexp 22[^0-9 ]+[0-9]+ "
  (save-excursion
    (beginning-of-buffer)
    (goto-char (or begin-point (point-min)))
    (let ((end (search-forward-regexp code-regex nil t))
	  (start (search-backward-regexp code-regex nil t)))
      (when end
	;; Jump to the end of this one
	(search-forward-regexp code-regex nil t)
	;; The answer is this one plus the rest
	(cons (buffer-substring start end)
	      (lesson-codes code-regex (point)))))))
