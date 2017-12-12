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
;;;###autoload
(define-minor-mode drninjabatmans-haskell-mode
  "Some extras for haskell-mode."
  :lighter " DNB-Haskell"
  :keymap drninjabatmans-haskell-mode-map
  (setq comment-auto-fill-only-comments nil
        haskell-font-lock-symbols t
        haskell-process-args-stack-ghci nil)
  (when (file-exists-p "/Users/drninjabatman/Library/Haskell/bin/hasktags")
    (setq haskell-hasktags-path "/Users/drninjabatman/Library/Haskell/bin/hasktags"))
  (define-key interactive-haskell-mode-map (kbd "C-c C-l") nil)
  (define-key interactive-haskell-mode-map (kbd "C-c C-t") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand-from-trigger-key)
  (move-keymap-to-top 'yas-minor-mode)
  (flycheck-mode)
  (turn-on-haskell-indentation)
  (auto-complete-mode -1)
  (setq-local baginning-of-defun-function 'haskell-beginning-of-defun)
                                        ; (ghc-init)
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
(defun haskell-cabal--find-tags-dir ()
  "Override the tags path"
  (or haskell-tags-file-dir (funcall 'haskell-cabal--find-tags-dir-old)))

(defun haskell-jump-src-to-test ()
  "Being in source jump th the corresponding test"
  (interactive)
  (find-file (haskell-corresponding-test (buffer-file-name))))

(defun haskell-corresponding-test (path)
  "Find a corresponding test file. If the cabal file is
/a/test.cabal and path is /a/b/c/d/e/f.hs we will try in order:
/a/tests/b/c/f.hs /a/tests/b/f.hs, /a/tests/f.h,
/a/tests/b/c/d/f.hs, /a/tests/b/c/f.hs, ..."
  (if-let* ((cabal-file (haskell-cabal-find-file)))
      (let* ((cabal-dir (file-name-directory cabal-file))
             (nondir (file-name-nondirectory path))
             (subpath (save-match-data
                        (split-string
                         (replace-regexp-in-string
                          cabal-dir ""
                          (directory-file-name
                           (file-name-directory path)))
                         "/")))
             (rsubpath (reverse subpath))
             (retfn (lambda (x) (concat cabal-dir
                                        "tests/"
                                        (mapconcat 'identity x "/")
                                        (if x "/" "") nondir)))

                                        ;(defret (funcall retfn (reverse (cdr (reverse rsubpath)))))
             (defret (funcall retfn subpath))
             (ret (funcall retfn subpath)))
        ;; Reduce subpath until it's null then reduce rsubpath.
        (while (and rsubpath (not (file-exists-p ret)))
          (if (not subpath)
              (setq rsubpath (cdr rsubpath)
                    subpath (reverse rsubpath))
            (setq subpath (cdr subpath)))
          (setq ret (funcall retfn subpath)))
        (if rsubpath ret defret))
    (error "Not in a cabal project.")))

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
  `(let ((cabal-file (haskell-cabal-find-file)))
     (if (not cabal-file) (error "Not in haskell project.")
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

(defun pair-list (lst)
  (let ((iter lst) (ret nil))
    (while iter
      (setq ret (cons (cons (car iter) (cadr iter)) ret))
      (setq iter (cddr iter)))
    (reverse ret)))

(defun haskell-collect-imports (&optional buffer)
  (let ((buf (or buffer (current-buffer))) (ret nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^import .*$" nil t)
        (setq ret (cons (buffer-substring (match-beginning 0) (match-end 0)) ret))))
    ret))

(defun haskell-send-imports-internal (process imports done)
  (if (null imports) (and done (funcall done))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list (car imports) process (cdr imports) done)
      :go (lambda (state)
            (message (concat "Importing: " (car state)))
            (haskell-process-send-string (cadr state) (car state)))
      :live (lambda (state response) nil)
      :complete (lambda (state response)
                  (apply 'haskell-send-imports-internal (cdr state)))))))

(defun haskell-process-load-file-and-then-imports ()
  (interactive)
  (call-interactively 'haskell-process-load-file)
  (haskell-send-imports (haskell-process)))

(defun haskell-send-imports (process &optional buffer done)
  (haskell-send-imports-internal process (haskell-collect-imports buffer) done))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun haskell-jump-to-or-insert-defininition ()
  "Look for the last function declaration before the point. If
there is a definion alread jump to that. If not insert one."
  (interactive)
  (if-let* ((decl-pos (save-excursion
                        (end-of-line)
                        (haskell-previous-declaration-search)))
            (fun-name (match-string 1))
            (def-pos (or (haskell-find-definition fun-name decl-pos)
                         (progn
                           (haskell-make-available-line)
                           (haskell-insert-function-stump fun-name)))))
      (goto-char def-pos)))

(defun haskell-make-available-line ()
  "Find a blank line suitable for inserting a function
definition. If there seems to be none before the next block of
code create one."
  (end-of-line)
  (when (re-search-forward "^\\($\\|.\\)" nil t)
    (unless (looking-back "\n")
      (beginning-of-line) (save-excursion (insert "\n")))
    (beginning-of-line)))

(defun haskell-insert-function-stump (fun-name)
  "Insert a function definition stump."
  (insert fun-name) (insert " ")
  (save-excursion (insert " = undefined")))

(defun haskell-previous-declaration-search ()
  "Search backward for a function declaration."
  (save-excursion
    (re-search-backward "^\\([[:alnum:]]*\\) ::")))

(defun haskell-find-definition (func-name &optional decl-point)
  "Search for the definition of FUNC-NAME starting at DECL-POINT
or the beginning of the buffer. Retrn the resulting position. The
return value is the point before the = sign."
  (save-excursion
    (goto-char (or decl-point (point-min)))
    (when (re-search-forward
           (concat "^\\(" func-name "\\)\\( +[[:alnum:]_]+\\)* *=") nil t)
      (if (looking-back " =") (backward-char 2) (backward-char 1))
      (point))))

(defun haskell-beginning-of-defun ()
  (save-match-data (re-seach-backwards "^[[:alnum:]]+")))


(defun haskell-debug-parse-stopped-at (string)
  "Parse the location stopped at from the given string.

For example:

Stopped in Main.main, /home/foo/project/src/x.hs:6:25-36

"
  (let ((index (string-match "Stopped in [^ ]+, \\([^:]+\\):\\(.+\\)\n?"
                             string)))
    (when index
      (list :path (match-string 1 string)
            :span (haskell-debug-parse-span (match-string 2 string))
            :types (cdr (haskell-debug-split-string (substring string index)))))))

(add-to-list 'haskell-font-lock-keywords "forall")

;; Copy the current function in foo and replace the global type
;; variables with concrete types.d
;;
;; (with-current-buffer "EquivCls.hs"
;;   (save-restriction
;;     (narrow-to-defun)
;;     (let ((fun-body (buffer-string)))
;;       (with-current-buffer "foo"
;;         (delete-region (point-min) (point-max))
;;         (insert fun-body)
;;         ;; List of lists of typevars
;;         (let ((typevars
;;                (let* ((arg-rx " *\\([[:alnum:]']+\\) *")
;;                       (funcall-rx (concat ":: forall *\\(\\(" arg-rx "\\)+ \\) *\.")))
;;                  (goto-char (point-min))
;;                  (let (ret)
;;                    ;; Find each function call
;;                    (while (search-forward-regexp funcall-rx nil t)
;;                      (add-to-list
;;                       'ret
;;                       ;;Narrow to call and get arguments
;;                       (save-restriction
;;                         (narrow-to-region (match-beginning 1) (match-end 1))
;;                         (goto-char (point-min))
;;                         (let (ret)
;;                           (save-match-data
;;                             (while (search-forward-regexp arg-rx nil t)
;;                               ;; Get each argument
;;                               (add-to-list 'ret
;;                                            (buffer-substring-no-properties
;;                                             (match-beginning 1) (match-end 1))))
;;                             (reverse ret))))))
;;                    (delete-region (match-beginning 0) (match-end 0))
;;                    (reverse ret))))
;;               ;; Transformation of typevars
;;               (typevar-trans (mapcar (lambda (x) (cons (concat "\\<" x "\\>")
;;                                                   (concat "(TypeVar " x ")")))
;;                                      (car typevars)))
;;               (type-regions (let (ret)
;;                               (save-match-data
;;                                 (while (search-forward-regexp "::\\(\\(.\\|\n\\)*?\\)=" nil t)
;;                                   (add-to-list 'ret (cons (match-beginning 1) (match-end 1))))
;;                                 (reverse ret)))))
;;           (dolist (l type-regions)
;;             (save-restriction
;;               (narrow-to-region (car l) (cdr l))
;;               (dolist (trans typevar-trans)
;;                 (replace-regexp (car trans) (cdr trans))))))))))


;; start tensorflow: (let ((session '((name . "tensorflow") (cabal-dir . "/Users/drninjabatman/Scratch/haskell/")))) (setq haskell-sessions (list session)) (switch-to-buffer "*haskell-process-log*") (haskell-process-start (setq haskell-session session))))

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

(defun stack-root (dir-root)
  (let* ((dir (abbreviate-file-name dir-root))
         (user (nth 2 (file-attributes dir))))
    ;; Look for the first dir that contains stack.yaml or does not
    ;; belong to the user
    (while (and dir
                (not (file-exists-p (concat dir "/stack.yaml")))
                (= user (nth 2 (file-attributes dir))))
      (setq dir (let ((next (file-name-directory (directory-file-name dir))))
                  (unless (string= dir next) next))))
    (or dir dir-root)))

(defun tensorflow-haskell-run-session (dir-root)
  "Run a session for the tensorflow project."
  (interactive
   (list
    (directory-file-name
     (read-directory-name
      "Project root: " (stack-root default-directory)))))

  (let* ((session-name (file-name-base dir-root))
         (session (list (cons 'name session-name)
                        (cons 'cabal-dir dir-root))))
    (when (cl-notany
           (lambda (s) (string= (haskell-session-name s) session-name))
           haskell-sessions)
      (add-to-list 'haskell-sessions session)
      (setq haskell-process-check-cabal-config-on-load nil
            haskell-process-log t)
      (haskell-process-start (setq haskell-session session))
      (switch-to-buffer  "*haskell-process-log*"))))


;; GUD
;;
;; M-x gud-ghci<RET>stack ghci --is-main fluidb:exe:sql2cpp

(defun gud-display-frame ()
  "Find and obey the last filename-and-line marker from the debugger.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (flet ((col-pos (col) (save-excursion (beginning-of-line) (+ col (point)))))
    (when gud-last-frame
      (gud-set-buffer)
      ;; gud-last-frame => (file . line)
      (cond
       ((not (listp (cdr gud-last-frame)))
        (gud-display-line (car gud-last-frame) (cdr gud-last-frame)))
       ;; gud-last-frame => (file line begin-column end-column)
       ((and
         (= 4 (length gud-last-frame))
         (every #'numberp (cdr gud-last-frame)))
        (let* ((file (car gud-last-frame))
               (file-buf (find-file-noselect file t))
               (line (cadr gud-last-frame))
               (expr-begin-col (caddr gud-last-frame))
               (expr-end-col (cadddr gud-last-frame)))
          (gud-display-line file line)
          (with-current-buffer file-buf
            (let ((expr-begin (col-pos expr-begin-col))
                  (expr-end  (col-pos expr-end-col))
                  (pulse-delay .30))
              (message (concat "Expr " (buffer-substring expr-begin expr-end)))
              (pulse-momentary-highlight-region expr-begin expr-end)))))
       ;; TODO: gud-last-frame =>
       ;; (file (begin-line . begin-column) (end-line . end-column))
       ;; Anything else
       (t (error "Unknown gud-last-frame format.")))
      (setq gud-last-last-frame gud-last-frame
	    gud-last-frame nil))))

(defun gud-ghci-marker-filter (string)
  (setq gud-marker-acc (if gud-marker-acc (concat gud-marker-acc string) string))

  (let (start)
    ;; Process all complete markers in this chunk.
    (while (string-match
	    "\\(Logged breakpoint at\\|Stopped in [^ \t\r\n]+,\\) \\(?1:[^ \t\r\n]+?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\)\\(?:-\\(?4:[0-9]+\\)\\|\\)"
	    gud-marker-acc start)
      (setq gud-last-frame
	    (list (match-string 1 gud-marker-acc)
		  (string-to-number (match-string 2 gud-marker-acc))
                  (string-to-number (match-string 3 gud-marker-acc))
                  (string-to-number (match-string 4 gud-marker-acc)))
	    start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" gud-marker-acc start)
      (setq start (match-end 0)))

    ;; If the incomplete line APPEARS to begin with another marker, keep it
    ;; in the accumulator.  Otherwise, clear the accumulator to avoid an
    ;; unnecessary concat during the next call.
    (setq gud-marker-acc (substring gud-marker-acc (match-beginning 0))))
  string)

(defun gud-ghci (command-line)
  (interactive (list (gud-query-cmdline 'gud-ghci)))
  (when (and gud-comint-buffer
	     (buffer-name gud-comint-buffer)
	     (get-buffer-process gud-comint-buffer)
	     (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'ghci)))
    (gdb-restore-windows)
    (error
     "Multiple debugging requires restarting in text command mode"))

  (gud-common-init command-line nil 'gud-ghci-marker-filter)
  (setq-local gud-minor-mode 'ghci)
  (setq paragraph-start comint-prompt-regexp)
  (comint-send-string (get-buffer-process (current-buffer))
                      ":set prompt \"> \"\n:print '\\n'\n")
  (setq comint-prompt-regexp "^> ")

  (gud-def gud-break  ":break %m %l %y" "\C-b" "Set breakpoint at current line.")
  (gud-def gud-stepi  ":step"           "\C-s" "Step one source line with display.")
  (gud-def gud-step   ":stepmodule"     "\C-n" "Step in the module.")
  (gud-def gud-next   ":steplocal"      "n" "Step in the local scope.")
  (gud-def gud-cont   ":continue"       "\C-r" "Continue with display.")
  (gud-def gud-up     ":back"           "<" "Up one stack frame.")
  (gud-def gud-down   ":forward"        ">" "Down one stack frame.")
  (gud-def gud-run    ":trace %e"       "t" "Trace expression.")
  (gud-def gud-print  ":print %e"       "\C-p" "Evaluate Guile expression at point.")
  (run-hooks 'gud-ghci-mode-hook))

(defvar gud-ghci-command-name "stack repl")


(defun gud-format-command (str arg)
  (let ((insource (not (eq (current-buffer) gud-comint-buffer)))
	(frame (or gud-last-frame gud-last-last-frame))
	(buffer-file-name-localized
         (and (buffer-file-name)
              (file-local-name (buffer-file-name))))
	result)
    (while (and str
		(let ((case-fold-search nil))
		  (string-match "\\([^%]*\\)%\\([adefFlpcmy]\\)" str)))
      (let ((key (string-to-char (match-string 2 str)))
	    subst)
	(cond
	 ((eq key ?f)
	  (setq subst (file-name-nondirectory (if insource
						  buffer-file-name-localized
						(car frame)))))
	 ((eq key ?F)
	  (setq subst (file-name-base (if insource
                                          buffer-file-name-localized
                                        (car frame)))))
	 ((eq key ?d)
	  (setq subst (file-name-directory (if insource
					       buffer-file-name-localized
					     (car frame)))))
	 ((eq key ?l)
	  (setq subst (int-to-string
		       (if insource
			   (save-restriction
			     (widen)
			     (+ (count-lines (point-min) (point))
				(if (bolp) 1 0)))
			 (cdr frame)))))
	 ((eq key ?e)
	  (setq subst (gud-find-expr)))
	 ((eq key ?a)
	  (setq subst (gud-read-address)))
	 ((eq key ?c)
	  (setq subst
                (gud-find-class
                 (if insource
                     (buffer-file-name)
                   (car frame))
                 (if insource
                     (save-restriction
                       (widen)
                       (+ (count-lines (point-min) (point))
                          (if (bolp) 1 0)))
                   (cdr frame)))))
	 ((eq key ?p)
	  (setq subst (if arg (int-to-string arg))))

         ;; My additions here
         ((eq key ?m)
          (setq subst
                (gud-find-module
                 (if insource
                     (buffer-file-name)
                   (car frame))
                 (if insource
                     (save-restriction
                       (widen)
                       (+ (count-lines (point-min) (point))
                          (if (bolp) 1 0)))
                   (cdr frame)))))

         ((eq key ?y)
          (setq subst
                (int-to-string
	         (if insource
	             (save-restriction (widen) (current-column))
	           (cdr frame))))))

	(setq result (concat result (match-string 1 str) subst)))
      (setq str (substring str (match-end 2))))
    ;; There might be text left in STR when the loop ends.
    (concat result str)))

(defun gud-find-module (f _line)
  (save-excursion
    (save-restriction
      (save-match-data
        (with-current-buffer (get-file-buffer f)
          (goto-char (point-min))
          (if (re-search-forward "^module[[:space:]]+\\([^[:space:](]+\\)" nil t nil)
              (match-string-no-properties 1)
            ""))))))

(provide 'fd-haskell)
