;; Some fluidb related code.

(defun fluidb-open-branches (branch-num)
  "Open the file of the next branch on the right and this branch
on the left panel, on the next branch search for the [NEW STUFF]
tag and align it with the position on the left."
  (let ((bmin branch-num) (bmax (+ branch-num 1)))
    (delete-other-windows)
    (split-window-right)
    (windmove-right)
    (find-file (format "/tmp/branches/branch%d.txt" bmax))
    (goto-char (point-min)) (search-forward "[NEW STUFF]")
    (let ((l (current-line)))
      (windmove-left)
      (find-file (format "/tmp/branches/branch%d.txt" bmin))
      (goto-line l))))

(defun fluidb-go-to-branch (fn)
  (let ((l (current-line)))
    (find-file
     (format "branch%d.txt"
             (funcall
              fn
              (string-to-number
               (concat
                (seq-filter
                 (lambda (x) (<= ?0 x ?9))
                 (buffer-name)))))))
    (goto-line l)))

(defun fluidb-next-branch () (interactive) (fluidb-go-to-branch #'1+))
(defun fluidb-prev-branch () (interactive) (fluidb-go-to-branch (lambda (x) (- x 1))))

(defun col-substring (col-beg col-end)
  (save-restriction
    (save-excursion
      (beginning-of-line)
      (narrow-to-region (point) (save-excursion (end-of-line) (point)))
      (forward-char col-beg)
      (let ((pt (point)))
        (forward-char (- col-end col-beg))
        (buffer-substring pt (point))))))

(defun column-at (pt)
  (save-excursion
    (save-excursion (goto-char pt) (current-column))))

(defun with-acive-region (x)
  (if (region-active-p) x (error "Select a region")))

(defun assert-single-line-region-active ()
  (unless
      (and (region-active-p)
           (= (current-line (region-end)) (current-line (region-beginning))))
    (error "Region is not active or spans many lines.")))

(defun forward-line-pred-change (from-pt to-pt &optional direction)
  "Go forward lines until the region between the columns changes"
  (interactive "r")
  (assert-single-line-region-active)
  (let ((cb (column-at from-pt))
        (ce (column-at to-pt)))
    (flet ((get-st nil (col-substring cb ce)))
      (let ((old-st (get-st)))
        (while (equal old-st (setq old-st (get-st)))
          (forward-line direction))))))

(defun backward-line-pred-change (from-pt to-pt)
  "Go backward lines until the region between the columns changes"
  (interactive "r")
  (forward-line-pred-change from-pt to-pt -1))

(provide 'fd-fluidb)
