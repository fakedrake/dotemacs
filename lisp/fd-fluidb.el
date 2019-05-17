;; Some fluidb related code.

(add-hook 'text-mode-hook 'activate-fluidb-branch-mode)
(defun activate-fluidb-branch-mode ()
  (let ((basename (file-name-nondirectory (buffer-file-name))))
    (when (and (s-suffix-p ".txt" basename)
               (s-prefix-p "branch" basename))
      (fluidb-branch-mode 1))))

(defvar fluidb-branch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x ]") 'fluidb-next-branch)
    (define-key map (kbd "C-x [") 'fluidb-prev-branch)
    map)
  "Keymap for using `interactive-haskell-mode'.")

(make-face 'gc )

(define-minor-mode fluidb-branch-mode
  "Some stuff for dealing with branches"
  :lighter " fluidb-branch"
  :keymap fluidb-branch-mode-map
  (setq-local fluidb-plan-dir
              (when (buffer-file-name (current-buffer))
                (file-name-directory (buffer-file-name (current-buffer)))))
  (read-only-mode))

(defun fluidb-jump-to-branch (branch &optional plan)
  "Jump to branch BRANCH of PLAN. If no PLAN is provided then
assume the last plan."
  (interactive "nBranch to jump to from final plan: ")
  (find-file (format "%s/branch%04d.txt" (fluidb-infer-plan-dir plan) branch)))

(defun fluidb-kill-branch-buffers ()
  "Kill all buffers that refer to branches"
  (interactive)
  (dolist (b (buffer-list))
    (when (if-let ((bn- (buffer-file-name b)))
              (let ((bn (file-name-nondirectory bn-)))
                (and bn (s-suffix? ".txt" bn) (s-prefix? "branch" bn))))
      (kill-buffer b))))

(defun fluidb-available-items (dir prefix)
  "Find all available numbers of files in a directory that have
prefix."
  (mapcar
   (lambda (x)
     (string-to-number
      (substring (file-name-base x) (length prefix))))
   (seq-filter (lambda (x) (s-prefix-p prefix x)) (directory-files dir))))

(defun fluidb-next-new-stuff ()
  "Look for the next branch with new stuff tag at the current line."
  (interactive)
  (flet ((do-next-branch
          nil
          (fluidb-next-branch)
          (goto-line init-line)
          (move-beginning-of-line nil)))
    (let ((init-line (current-line))
          (init-buffer (current-buffer)))
      (do-next-branch)
      (while (not (looking-at "\\[NEW STUFF\\]"))
        (when (>= (count-lines (point-min) (point-max)) init-line)
          (let ((buf (current-buffer)))
            (bury-buffer buf)
            (do-next-branch)))))))

(defun fluidb-available-plans ()
  (fluidb-available-items "/tmp" "branches"))
(defun fluidb-available-branches (plan)
  (fluidb-available-items (format "/tmp/branches%03d" plan) "branch"))

(defun fluidb-go-to-branch (fn)
  (let ((l (current-line)))
    (find-file
     (format "branch%04d.txt"
             (funcall
              fn
              (string-to-number
               (concat
                (seq-filter
                 (lambda (x) (<= ?0 x ?9))
                 (file-name-nondirectory (buffer-file-name))))))))
    (goto-line l)))

(defun fluidb-infer-plan-dir (&optional plan-number)
  (cond
   (plan-number (format "/tmp/branches%03d" plan-number))
   ((and (boundp 'fluidb-plan-dir) fluidb-plan-dir) fluidb-plan-dir)
   (t (format "/tmp/branches%03d" (car (last (fluidb-available-plans)))))))

(defun fluidb-jump-to-graph-description (&optional plan-number)
  "Jump to a reasonable /tmp/branchesXXX/graph.txt"
  (interactive)
  (let ((dir (fluidb-infer-plan-dir)))
    (find-file (format "%s/graph.txt" dir))))

(defun fluidb-next-branch ()
  "Jump to next branch. It is assumed that the current visited
file has the name 'branchNUM0.txt'. Next branch is branchNUM1.txt
where NUM1 = NUM0 + 1"
  (interactive) (fluidb-go-to-branch #'1+))

(defun fluidb-prev-branch ()
  "Jump to previous branch. It is assumed that the current
visited file has the name 'branchNUM0.txt'. Previous branch is
branchNUM1.txt where NUM1 = NUM0 - 1"
  (interactive) (fluidb-go-to-branch (lambda (x) (- x 1))))

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
