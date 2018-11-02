(add-to-list 'load-path (format "%s/.emacs.d/lisp/haskell" (getenv "HOME")))
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'fd-gud-haskell)
(require 'fd-haskell-holes)
(require 'fd-haskell-insert-definition)
(require 'fd-haskell-interactive)
(require 'fd-haskell-ligatures)
(require 'fd-haskell-modules)
(require 'fd-haskell-test-files)


;; Make sure our mode overrides interactive-haskell-mode
(add-hook 'haskell-mode-hook 'drninjabatmans-haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-process-type 'auto)
(setq haskell-process-path-ghci "stack")
(setq haskell-process-args-ghci '("repl"))

(add-to-list
 'haskell-compilation-error-regexp-alist
 '("^Stopped in [^ \t\r\n]+, \\(?1:[^ \t\r\n]+?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\)\\(?:-\\(?4:[0-9]+\\)\\)$" 1 2 (3 . 4) 0))

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
    (define-key map (kbd "C-x SPC") 'gud-break)
    (define-key map (kbd "C-c C-l") 'haskell-process-load-file-and-then-imports)
    (define-key map (kbd "C-c C-t") 'haskell-toggle-src-test)
    (define-key map (kbd "C-c C-i") 'haskell-jump-to-or-insert-defininition)
    (define-key map (kbd "C-c i") 'haskell-jump-to-or-insert-defininition)
    map)
  "Keymap for using `interactive-haskell-mode'.")

(defun haskell-interactive-haskell-mode-hook-fd ()
  (define-key
    haskell-interactive-mode-map
    (kbd "M-.")
    'haskell-mode-jump-to-def-or-tag))

(add-hook 'haskell-interactive-mode-hook 'haskell-interactive-haskell-mode-hook-fd)
(defun move-keymap-to-top (mode)
  (let ((pair (assq mode minor-mode-map-alist)))
    (if (not pair)
        (error "Map %s not member of minor-mode-map-alist")
      (setq minor-mode-map-alist
            (cons pair (assq-delete-all mode minor-mode-map-alist))))))

;;;###autoload
(define-minor-mode drninjabatmans-haskell-mode
  "Some extras for haskell-mode."
  :lighter " DNB-Haskell"
  :keymap drninjabatmans-haskell-mode-map
  (setq comment-auto-fill-only-comments nil
        haskell-font-lock-symbols t
        haskell-process-args-stack-ghci nil)
  (setq haskell-tags-on-save t
        haskell-stylish-on-save t)
  (when (file-exists-p "/Users/drninjabatman/Library/Haskell/bin/hasktags")
    (setq haskell-hasktags-path "/Users/drninjabatman/Library/Haskell/bin/hasktags"))
  (define-key interactive-haskell-mode-map (kbd "C-c C-l") nil)
  (define-key interactive-haskell-mode-map (kbd "C-c C-t") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand-from-trigger-key)
  (move-keymap-to-top 'yas-minor-mode)
  (flycheck-mode)
  (add-hook 'flycheck-after-syntax-check-hook 'haskell-check-module-name nil t)
  (turn-on-haskell-indentation)
  (auto-complete-mode -1)

                                        ; Proper function detection
  (haskell-decl-scan-mode 1)

                                        ; Linting
  (require 'hs-lint)
  (setq hs-lint-replace-with-suggestions t)
  (rainbow-delimiters-mode 1)
  (local-set-key "\C-cl" 'hs-lint)

                                        ; Check on save
  (if-let* ((adv (assq 'ghc-check-syntax-on-save
                       (ad-get-advice-info-field #'save-buffer 'after))))
      (ad-advice-set-enabled adv nil))
                                        ; (set-face-attribute 'shm-current-face nil :background nil)
  ;; (structured-haskell-mode t)
  ;; (define-key interactive-haskell-mode-map (kbd "C-M-a") 'shm/goto-parent)
  ;; (define-key interactive-haskell-mode-map (kbd "C-M-e") 'shm/goto-parent-end)
  )
(defvar-local haskell-tags-file-dir nil)
(fset 'haskell-cabal--find-tags-dir-old (symbol-function 'haskell-cabal--find-tags-dir))

(defun move-keymap-to-top (mode)
  (let ((map (assq mode minor-mode-map-alist)))
    (assq-delete-all mode minor-mode-map-alist)
    (add-to-list 'minor-mode-map-alist map)))

(defvar align-haskell-arrows-list '(haskell-arrows
                                    (regexp   . "[^-=!^&*+<>/| \t\n]\\(\\s-*\\)->\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
                                    (group    . (1 2))
                                    (justify . t)
                                    (tab-stop . nil)
                                    (modes    . (haskell-mode))))


(add-to-list 'haskell-font-lock-keywords "forall")

;; OVERRIDE
(defun haskell-session-interactive-buffer (s)
  "Get the session interactive buffer."
  (let ((buffer (haskell-session-get s 'interactive-buffer)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (let ((buffer-name (format "*%s*" (haskell-session-name s)))
            (index 0))                  ;CHANGE HERE
        (while (get-buffer buffer-name)
          (setq buffer-name (format "*%s <%d>*" (haskell-session-name s) index))
          (setq index (1+ index)))
        (let ((buffer (get-buffer-create buffer-name)))
          (haskell-session-set-interactive-buffer s buffer)
          (with-current-buffer buffer
            (haskell-interactive-mode)
            (haskell-session-assign s))
          (haskell-interactive-switch)
          buffer)))))

(require 'align)
(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

(require 'haskell-modules)

(defun heads (lst)
  (when lst
    (cons
     (list (car lst))
     (mapcar
      (lambda (x) (cons (car lst) x))
      (heads (cdr lst))))))

(defun rcons (lst c)
  "Put C at the end of LST"
  (if (null lst)
      (list c)
    (cons (car lst) (rcons (cdr lst) c))))

(defun path-list-to-str (path-list)
  "Turn a list of file names into a proper file path."
  (string-join
   (mapcar (lambda (x) (format "/%s" x)) path-list)))

(defun roots-with (dir file)
  "Find the project root from DIR. You will know because itsdsds
  contains FILE."
  (let ((default-directory dir))
    (mapcar
     'path-list-to-str
     (seq-filter
      (lambda (x)
        (file-exists-p (path-list-to-str (rcons x file))))
      (heads (split-string (expand-file-name dir) "/" t))))))

(defun stack-root (dir)
  (let ((roots (roots-with dir "stack.yaml")))
    (when roots (last roots))))

(defun haskell-all-project-modules ()
  (split-string
          (shell-command-to-string
           (format
            "find '%s' \\( -name '.#*' -prune \\) -o -name '*.hs' -exec sed -n 's/^\\(import\\|module\\)\\s*\\(qualified\\s*\\|\\)\\([a-zA-Z.]*\\).*/\\3/p' {} \\+ | sort | uniq"
            (expand-file-name (stack-root default-directory))))))

;;;###autoload
(defun haskell-add-import (module)
  (interactive (list (completing-read
                      "Module: "
                      (haskell-all-project-modules))))
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-max))
      (haskell-navigate-imports)
      (insert (concat "import " module "\n")))))

(defun flycheck-type-of-hole (&optional pos)
  "Get the type of the hole at POS.

POS defaults to `point'."
  (-when-let* ((errors (flycheck-overlay-errors-at (or pos (point)))))
    (with-temp-buffer
      (insert (flycheck-error-message (car errors)))
      (goto-char (point-min))
      (save-match-data
        (when (search-forward-regexp
               "Found hole: _[[:alnum:]_']* :: \\([[:alnum:]_']+\\)"
               nil t)
          (match-string 1))))))

(defun haskell-check-module-name ()
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward flycheck-haskell-module-re nil t)
        (when-let ((decl (match-string-no-properties 1))
                   (decl-pos (match-beginning 1))
                   (inf (haskell-infer-module-name)))
          (unless (string= inf decl)
            (goto-char decl-pos)
            (let ((flycheck-highlighting-mode 'symbols)
                  (line (current-line))
                  (col (current-column)))
              (flycheck-add-overlay
               (flycheck-error-new-at line col 'error
                                      (format "Expected module name %s" inf))))))))))
(defun haskell-doc-mode-print-current-symbol-info ()
  "Print the type of the symbol under the cursor.

This function is run by an idle timer to print the type
 automatically if `haskell-doc-mode' is turned on."
  (and haskell-doc-mode
       (haskell-doc-in-code-p)
       (not haskell-mode-interactive-prompt-state)
       (not (eobp))
       (not executing-kbd-macro)
       ;; Having this mode operate in the minibuffer makes it impossible to
       ;; see what you're doing.
       (not (eq (selected-window) (minibuffer-window)))
       ;; not in string or comment
       ;; take a nap, if run straight from post-command-hook.
       (if (fboundp 'run-with-idle-timer) t
         (sit-for haskell-doc-idle-delay))
       ;; good morning! read the word under the cursor for breakfast
       (string= (haskell-ident-at-point) (car haskell-doc-last-data))
       (haskell-doc-current-info)))

(provide 'fd-haskell)
