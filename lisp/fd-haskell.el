(require 'haskell-interactive-mode)
(require 'haskell-process)
;; Make sure our mode overrides interactive-haskell-mode
(remove-hook 'haskell-mode-hook 'drninjabatmans-haskell-mode)
(add-hook 'haskell-mode-hook 'drninjabatmans-haskell-mode)
(remove-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-process-type 'auto)
(setq haskell-process-path-ghci "cabal")
(setq haskell-process-args-ghci '("repl"))

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

(defvar hs-compilation-error-regex-alist
  ;; REGEX FILE-GROUP LINE-GROUP COLUMN-GROUP ERROR-TYPE LINK-GROUP
  '((haskell-error
     "^\\(\\(.*\\.l?hs\\):\\([0-9]*\\):\\([0-9]*\\)\\): error:$"
     2 3 4 2 1)))


;; Add hs-compilation-error-regex-alist to the appropriate lists. If
;; the var was updated don't keep adding, edit the result.
(require 'compile)
(dolist (el hs-compilation-error-regex-alist)
  (unless (memq (car el) compilation-error-regexp-alist)
    (add-to-list 'compilation-error-regexp-alist (car el)))

  (let ((ael (assq (car el) compilation-error-regexp-alist-alist)))
    (if ael (setf (cdr ael) (cdr el))
      (add-to-list 'compilation-error-regexp-alist-alist el))))
(require 'haskell-mode)
(defvar drninjabatmans-haskell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'haskell-toggle-src-test)
    map)
  "Keymap for using `interactive-haskell-mode'.")
;;;###autoload
(define-minor-mode drninjabatmans-haskell-mode
  "Some extras for haskell-mode."
  :lighter " DNB-Haskell"
  :keymap drninjabatmans-haskell-mode-map
  (require 'shm)
  (setq comment-auto-fill-only-comments nil
        haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans,-Wall"))
  (define-key interactive-haskell-mode-map (kbd "C-c C-t") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand-from-trigger-key)
  (move-keymap-to-top 'yas-minor-mode)
  ;; (structured-haskell-mode t)
  (turn-on-haskell-indentation)
  (auto-complete-mode -1)
  (ghc-init)
  (if-let ((adv (assq 'ghc-check-syntax-on-save
                      (ad-get-advice-info-field #'save-buffer 'after))))
      (ad-advice-set-enabled adv nil))
  (set-face-attribute 'shm-current-face nil :background nil)
  (message "Using drninjabatman's haskell mode!"))

(defun haskell-jump-src-to-test ()
  (interactive)
  (let ((cabal-file (haskell-cabal-find-file)))
    (if (not cabal-file) (error "Not in haskell project.")
      (let ((test-fname (concat (file-name-directory cabal-file)
                                "tests/"
                                (file-name-nondirectory (buffer-file-name)))))
        (find-file test-fname)))))

(defun move-keymap-to-top (mode)
  (let ((map (assq mode minor-mode-map-alist)))
    (assq-delete-all mode minor-mode-map-alist)
    (add-to-list 'minor-mode-map-alist map)))

(defun haskell-test-to-src ()
  (let ((cabal-file (haskell-cabal-find-file))
        (testname (file-name-base (buffer-file-name))))
    (if (or (not cabal-file)) (error "Not in haskell project.")
      (with-current-buffer (find-file-noselect cabal-file)
        (save-excursion
          (goto-char (point-min))            ;XXX: we asume the first
                                        ;hs-source-dirs encountered is
                                        ;the right one
          (let* ((src-dirs (haskell-cabal-subsection-entry-list
                          (haskell-cabal-section) "hs-source-dirs"))
               (modules (haskell-cabal-subsection-entry-list
                         (haskell-cabal-section) "exposed-modules"))
               (mod-paths (delete-if-not
                           (lambda (x) (string= testname (file-name-base x)))
                           (mapcar 'haskell-cabal-module-to-filename modules)))
               (full-paths (apply 'append
                                  (mapcar
                                   (lambda (dir)
                                     (mapcar (lambda (mp)
                                               (concat default-directory "/" dir "/" mp))
                                             mod-paths))
                                   src-dirs))))
          (delete-if-not 'file-exists-p full-paths)))))))

(defmacro within-cabal-file (&rest body)
  `(let ((cabal-file (haskell-cabal-find-file))
         (testname (file-name-base (buffer-file-name))))
     (if (or (not cabal-file)) (error "Not in haskell project.")
       (with-current-buffer (find-file-noselect cabal-file)
         ,@body))))

(defun haskell-jump-test-to-src ()
  (interactive)
  (let ((srcs (haskell-test-to-src)))
    (if (not srcs) (error "Couldn't find sources from test file (maybe not in .cabal?)." )
      (find-file (car srcs)))))

(defun haskell-toggle-src-test ()
  (interactive)
  (if (not (haskell-cabal-find-file)) (error "Not in haskell project")
    (if (member "tests" (split-string (buffer-file-name) "/"))
        (haskell-jump-test-to-src) (haskell-jump-src-to-test))))

(defun haskell-cabal-get-key (key)
  (haskell-cabal-subsection-entry-list (haskell-cabal-section) key))

(defun haskell-infer-module-name ()
  (car
   (let ((fname (buffer-file-name)))
     (within-cabal-file
      (mapcar
       (lambda (src-dir)
         (replace-regexp-in-string
          "\\.hs$" ""
          (replace-regexp-in-string
           "/" "."
           (substring fname (+ 1 (length src-dir))))))
       (delete-if-not
        (lambda (x) (string-prefix-p x fname))
        (mapcar
         (apply-partially 'concat default-directory)
         (haskell-cabal-get-key "hs-source-dirs"))))))))

(defvar align-haskell-arrows-list '(haskell-arrows
     (regexp   . "[^-=!^&*+<>/| \t\n]\\(\\s-*\\)->\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
     (group    . (1 2))
     (justify . t)
     (tab-stop . nil)
     (modes    . (haskell-mode))))

(require 'hs-lint)
(defun my-haskell-mode-hook ()
  (setq hs-lint-replace-with-suggestions t)
  (rainbow-delimiters-mode 1)
  (local-set-key "\C-cl" 'hs-lint))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(provide 'fd-haskell)
