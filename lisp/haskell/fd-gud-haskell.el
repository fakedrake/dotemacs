;; GUD
;;
;; M-x gud-ghci<RET>stack ghci
(defmacro with-gud-comint-layer-light (&rest body)
  "Run a gud command using haskell-interactive rather than
comint."
  `(let ((comint-prompt-regexp haskell-prompt-regexp)
         (gud-comint-buffer (haskell-interactive-buffer)))
     (flet ((comint-dynamic-complete-filename () (error "Unsupported completion"))
            (comint-input-sender () (error "Unsupported, bad gud mode"))
            (comint-interrupt-subjob () (error "Should support"))
            (comint-line-beginning-position () (error "Unsupported completion"))
            (comint-output-filter (proc str) (haskell-process-filter proc str))
            (comint-stop-subjob () (error "Unsupported, bad target name"))
            (comint-update-fence ())    ; Internal to comint
            (comint-send-string (proc str) (error "Implement"))
            (make-comint (a b c d e) (haskell-session))
            (gdb-restore-windows () (error "Implement gdb-restore-windows")))
       ,@body)))

(defmacro haskell-gud-def (func cmd key &optional doc)
  "Define a gud command like gud-def but the command will use
haskell-interactive rather than comint."
  `(gud-def
    (lambda (&rest args) (with-gud-comint-layer-light (apply ,func args)))
    ,cmd ,key ,doc))

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

(defun gud-ghci ()
  (interactive)
  (with-gud-comint-layer-light
   (when (and gud-comint-buffer
	      (buffer-name gud-comint-buffer)
	      (get-buffer-process gud-comint-buffer)
	      (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'ghci)))
     (gdb-restore-windows)
     (error
      "Multiple debugging requires restarting in text command mode"))

   ;; (gud-common-init command-line nil 'gud-ghci-marker-filter)
   (setq-local gud-minor-mode 'ghci)
   (setq paragraph-start comint-prompt-regexp)
   (comint-send-string (get-buffer-process (current-buffer))
                       ":set prompt \"> \"\n:print '\\n'\n")

   (haskell-gud-def gud-break  ":break %m %l %y" "\C-b" "Set breakpoint at current line.")
   ;; TODO: put _result=... line to minibuffer.
   (haskell-gud-def gud-stepi  ":step"           "\C-s" "Step one source line with display.")
   (haskell-gud-def gud-step   ":stepmodule"     "\C-n" "Step in the module.")
   (haskell-gud-def gud-next   ":steplocal"      "n" "Step in the local scope.")
   (haskell-gud-def gud-cont   ":continue"       "\C-r" "Continue with display.")
   (haskell-gud-def gud-up     ":back"           "<" "Up one stack frame.")
   (haskell-gud-def gud-down   ":forward"        ">" "Down one stack frame.")
   (haskell-gud-def gud-run    ":trace %e"       "t" "Trace expression.")
   (haskell-gud-def gud-print  ":print %e"       "\C-p" "Evaluate Guile expression at point.")
   (run-hooks 'gud-ghci-mode-hook)))

(defvar gud-ghci-command-name "stack repl")
(require 'gud)
(defun gud-format-command (str arg)
  (let ((insource (not (eq (current-buffer) gud-comint-buffer)))
	(frame (or gud-last-frame gud-last-last-frame))
	(buffer-file-name-localized
         (and (buffer-file-name)
              (or (file-remote-p (buffer-file-name) 'localname)
                  (buffer-file-name))))
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

(provide 'fd-gud-haskell)
