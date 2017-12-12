;; Package
;; Programming realted miscelaneous
;;; Code:

(which-function-mode t)

(add-hook 'before-save-hook 'maybe-delete-whitespace)

(defvar keep-whitespace nil
  "Don't delete trailing whitespaces in a file.")
(defun maybe-delete-whitespace ()
  "if `keep-whitespace' is non-nil delete trailing whitespaces on
each line."
  (interactive)
  (if keep-whitespace
      (message "Unset `keep-whitespace' to remove trailing whitespace on save")
    (delete-trailing-whitespace)))

(setq require-final-newline 'query)
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1))) ;; fix tabcompletion

(electric-pair-mode)
;; Indent buffer
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))
(global-set-key "\C-x\\" 'indent-buffer)

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally)

(defalias '>> 'rsh)
(defalias '<< 'lsh)

(defun github-clone (repo)
  (interactive "MProvide the repository line <username>/<repo>: ")
  (async-shell-command (format "git clone git@github.com:%s" repo)))

(global-set-key (kbd "C-j") 'default-indent-new-line)
(global-set-key (kbd "C-M-l") 'add-dir-local-variable)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-x j p") 'jump-to-project)

(setq git--commit-buttonize-filenames-regex "^\t[^:]+: +\\(.*\\)")

(defmacro btn-cmd (&rest body)
  "Builds a function that runs the specified body and remembers
the point position in the commit buffer between calls of each
function created this way."
  `(lambda ()
     (interactive)
     (let ((commit-buffer (current-buffer)))
       (save-excursion
         ;; Make sure there is a mark
         (unless (boundp 'button-mark)
           (setq-local button-mark (make-marker))
           (set-marker button-mark (point-min)))

         (goto-char (marker-position button-mark))
         (let ((eval-val (progn ,@body)))
           (with-current-buffer-safe commit-buffer
                                     (set-marker button-mark (point)))
           eval-val)))))

(defun activate-previous-button ()
  (let ((btn (previous-button (point))))
    (if btn
        (progn
          (goto-char (button-end btn))
          (button-activate btn))
      (message "Before the first button"))))

(defun activate-next-button ()
  (let ((btn (next-button (point))))
    (if btn
        (progn
          (goto-char (button-end btn))
          (button-activate btn))
      (message "After last button"))))

(defun fd-compilation-git-commit-bindings ()
  (local-set-key (kbd "C-x `") (btn-cmd (activate-next-button)))
  (local-set-key (kbd "M-g n") (btn-cmd (activate-next-button)))
  (local-set-key (kbd "M-g p") (btn-cmd (activate-previous-button))))

(add-hook 'git-comment-hook 'fd-compilation-git-commit-bindings)
(add-hook 'sql-mode-hook (lambda () (setq-local untabify-on-save t)))

(setq git--commit-filename-line-regexp "^[ \t]\\([^:\n]+\\):[ \t]+\\(.+?\\) ?\\((.* content)\\|\\)$")

(defun git-next-diff ()
  (interactive)
  (git-filename-modify-buttons)
  (let ((cur
         (if git--last-button-pressed
             (button-end git--last-button-pressed) (point-min))))
    (if cur (button-activate (next-button cur))
      (error "No more files"))))

(defun git-filename-modify-buttons ()
  (when (not (local-variable-p 'git--last-button-pressed))
    (setq-local git--last-button-pressed nil)
    (dolist (ovl (git--commit-file-overlays))
      (lexical-let*
          ((btn (button-at (overlay-start ovl)))
           (act (button-get btn 'action)))
        (button-put btn 'action
                    (lambda (btn)
                      (setq-local git--last-button-pressed btn)
                      (funcall act btn)
                      (when (not (string= (buffer-name) "*git-commit*"))
                        (previous-window))))))))

(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(global-whitespace-mode t)

;; (add-hook 'after-init-hook #'global-flycheck-mode)
(provide 'fd-misc-programming)
;;; fd-misc-programming ends here
