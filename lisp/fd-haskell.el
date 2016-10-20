(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'drninjabatmans-haskell-mode)
(setq haskell-process-type 'auto)
(setq haskell-process-path-ghci "cabal")
(setq haskell-process-args-ghci '("repl"))

(defalias fd-haskell-mode drninjabatmans-haskell-mode)

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
  (setq comment-auto-fill-only-comments nil
        haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans,-Wall"))
  (message "Using drninjabatman's haskell mode!"))

(defun haskell-jump-src-to-test ()
  (interactive)
  (let ((cabal-file (haskell-cabal-find-file)))
    (if (not cabal-file) (error "Not in haskell project.")
    (let ((test-fname (concat (file-name-directory cabal-file)
                              "tests/"
                              (file-name-nondirectory (buffer-file-name)))))
      (find-file test-fname)))))

(defun haskell-test-to-src ()
  (let ((cabal-file (haskell-cabal-find-file))
        (testname (file-name-base (buffer-file-name))))
    (if (or (not cabal-file)) (error "Not in haskell project.")
      (with-current-buffer (find-file-noselect cabal-file)
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
          (delete-if-not 'file-exists-p full-paths))))))

(defun haskell-jump-test-to-src ()
  (interactive)
  (let ((srcs (haskell-test-to-src)))
    (if (not srcs) (error "Couldn't find sources")
      (find-file (car srcs)))))

(defun haskell-toggle-src-test ()
  (interactive)
  (if (not (haskell-cabal-find-file)) (error "Not in haskell project")
      (if (member "tests" (split-string (buffer-file-name) "/"))
          (haskell-jump-test-to-src) (haskell-jump-src-to-test))))

(provide 'fd-haskell)
