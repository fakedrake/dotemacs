;;; fd-compilation.el
;; Stuff related to compilation.
(require 's)
(require 'notifications)

(defun compilation-end-defun (compilation-buffer result)
  (with-current-buffer compilation-buffer
    (if (string= (buffer-name) "*compilation*")
        (notifications-notify
         :title (format "Compilation: %s" result)
         :body (format "Cmd: %s" compile-command))
      (notifications-notify
       :title (format "Finished: %s" (buffer-name))))))

(setq compilation-finish-function 'compilation-end-defun)

(defun mapcdr (fn seq)
  (and seq (cons (funcall fn seq) (mapcdr fn (cdr seq)))))

(defun eval-or-nil (fn val)
  (when (funcall fn val) val))

(defun fd-file-in-directory-p (file dir)
  (let* ((true-dir (file-truename dir))
         (default-directory (concat true-dir "/"))
         (true-file (file-truename file)))
    (and (file-exists-p true-file)
         (file-directory-p true-dir)
         (string-prefix-p true-dir true-file))))

(defun fd-project-root (dir-or-file &optional marker)
  "Get the closest ancestor directory containing
  marker-file. marker-file defaults to \".git\""
  (let ((dir (file-truename
              (if (file-directory-p dir-or-file)
                  dir-or-file
                (file-directory-name dir-or-file))))
        (is-root (or (when (functionp marker) marker)
                     (apply-partially 'fd-file-in-directory-p (or marker ".git")))))
    (car
     (remove-if 'null
                (mapcdr
                 (lambda (dl)
                   ;; Keep only the dirs that contain a .dir-locals.el
                   (let ((ds (concat "/" (s-join "/" (reverse dl)))))
                     (when (funcall is-root ds)
                       ds))) (nreverse (s-split "/" dir t)))))))

(defun fd-compilation-root (filename)
  (file-name-as-directory
   (or (when (boundp 'compile-root) compile-root)
       (fd-project-root filename ".dir-locals.el")
       (fd-project-root filename))))

(defun fd-recompile ()
  (interactive)
  (save-excursion
    (hack-local-variables)
    (let ((default-directory
            (fd-compilation-root default-directory)))
      (message "Compiling with %s in %s" compile-command default-directory)
      (compilation-start compile-command t))))

(defun fd-last-error ()
  "Jump to last error."
  (interactive)
  (while (not
          (eq (condition-case err
                  (next-error)
                (error 'cs-last-error-here))
              'cs-last-error-here))))

(defalias 'read-directory-name 'ido-read-directory-name)

(defun fd-compile (command directory)
  (interactive (list
                (read-shell-command "Compile command: "
                                    compile-command)
                (read-directory-name "Root directory: "
                                     (or compilation-directory default-directory))))
  (save-excursion
    (let ((default-directory
            (if (s-ends-with-p "/" directory) directory (concat directory "/"))))
      (find-file ".dir-locals.el")
      (save-buffer)
      (add-dir-local-variable nil 'compile-command command)
      (add-dir-local-variable nil 'compile-root default-directory)
      (save-buffer)
      (bury-buffer)
      (fd-recompile))))

(global-set-key (kbd "C-c r") 'fd-recompile)
(global-set-key (kbd "C-c c c") 'fd-compile)
(global-set-key (kbd "M-g l") 'fd-last-error)
(global-set-key (kbd "M-g t") 'error-end-of-trace)

(defcustom error-trace-max-distance 2
  "Maximum distance between errors of a trace.")

(defun next-error-point (&optional pt reverse)
  "The point of the next error from point. Nil if no error after
this."
  (save-excursion
    (condition-case e
        (progn
          (compilation-next-error (if reverse -1 1) nil (or pt (point)))
          (point))
      ('error nil))))

(defun error-end-of-trace (&optional reverse)
  "Jump to the last error of the next trace. If reverse jump to
the top."
  (interactive)
  (pop-to-buffer (next-error-find-buffer))
  ;; Enter the next trace. Should raise error if we are at the end
  (compilation-next-error 1 nil (or compilation-current-error (point-min)))

  ;; Move to it's end
  (let ((le (internal-end-of-trace (point) reverse)))
    (goto-char le)
    (recenter)
    (compile-goto-error)))

(defun internal-end-of-trace (pt reverse)
  "Find the last error o a trace that we are in."
  (let ((nep (next-error-point pt reverse)))
    ;; There is an error and it is close.
    (if (and nep (<= (count-lines pt nep)
                     error-trace-max-distance))
        (internal-end-of-trace nep reverse)
      pt)))


;; A nice stack for pdb would be:
;;
;; - create buffer with fd-compilation
;; - set inferior-python-mode
;; - use that buffer to initialize gud-pdb
;;
;; To do that I will replace make-comint while running pdb that should
;; create a buffer named

;; (defadvice make-comint (around ad-create-a-gud-buffer first activate)
;;   "if `create-gud-buffer-fn' is non-nil. Use that insetaed of the
;;   standard way. Do not use this outside of the provided macros."
;;   (let ((cbuffer (or gud-buffer
;;                      (funcall 'gud-buffer))))
;;     (if cbuffer
;;         (with-current-buffer cbuffer (rename-buffer (ad-get-arg 1)))
;;       ad-do-it)))

(defvar gud-buffer nil
  "Either a function that evaluates to a buffer or a buffer we
  can switch to. This will override the buffer creation mechanism
  of gud.")

(defmacro gud-recomple (gud-command &rest args)
  (let ((gud-buffer (apply 'fd-recompile args)))
    (funcall gud-command "compilation")))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(provide 'fd-compilation)
