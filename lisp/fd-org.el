;; ORG mode
;; (load-library "org-compat") 		;XXX this is bad but i see no better way.

;; (load-library "org-list")
;; (require 'org-element)
(load-library "org")			; Requiring doesnt get the
					; defvar for some reason
                                        ;
(require 'org)
(setq google-translate-translation-directions-alist '(("el" . "en") ("en" . "el")))

;; Bindings
(defun fd-org-mode-hook ()
  (flyspell-mode t)
  ;; Electric pair to handle emphasis
  (set-syntax-table
   (let ((table (make-syntax-table)))
     (modify-syntax-entry ?= "\"" table)
                                        ; (modify-syntax-entry ?* "\"" table) ;; Headings use this too...
     (modify-syntax-entry ?+ "\"" table)
     (modify-syntax-entry ?/ "\"" table)
     table))
  (define-key org-mode-map (kbd "C-c r") 'google-translate-at-point-reverse)
  (define-key org-mode-map "\M-j" 'org-meta-return)
  (define-key org-mode-map "\M-n" 'org-forward-element)
  (define-key org-mode-map "\M-p" 'org-backward-element)
  (define-key org-mode-map (kbd "<C-tab>") 'yas-expand)
  (setq org-return-follows-link t)

  (setq auto-fill-mode 1))

(add-hook 'org-mode-hook 'fd-org-mode-hook)


;; Aspell
(setq ispell-program-name "aspell")


(require 'ox-latex)
(require 'ox-beamer)

(defun fd/org-latex-hook ()
  (setq org-latex-default-figure-position "H")

  ;; XXX: ensure the fonts are all there.
  ;; For arch that would be:
  ;; pacman -S ttf-dejavu ttf-freefont.
  ;;
  ;; Usage: on top of the .org doc put these.
  ;; #+LaTeX_CLASS: fakedrake-org-article
  ;; #+LaTeX_HEADER: <some extra headings>
  (add-to-list 'org-latex-classes
	       '("smarticle"
		 "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{float}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
% \\setmainfont{DejaVu Sans}
\\setmainfont{Arial}
% \\setmonofont[Scale=0.8]{FreeMono}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage[bookmarks, colorlinks, breaklinks]{hyperref}
\\hypersetup{linkcolor=black, citecolor=blue,filecolor=black,urlcolor=MidnightBlue}
\\pagestyle{empty}
\\usepackage{amsmath}
\\usepackage[parfill]{parskip}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  ;; XXX: Ensure pdflatex is available
  ;; for arch that would be:
  ;; pacman -S texlive-most texlive-lang
  ;; XXX: Ensure xetexlatex is available
  (setq org-latex-pdf-process
	'("xelatex -interaction nonstopmode %f"
	  "xelatex -interaction nonstopmode %f")) ;; for multiple passes

  (setq org-file-apps (list
                       '(auto-mode . emacs)
                       (if (eq system-type 'darwin)
                           '("\\.pdf\\'" . "open %s")
                         '("\\.pdf\\'" . "evince %s")))))

(add-hook 'org-mode-hook 'fd/org-latex-hook)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(org-defkey org-mode-map [(shift up)]          nil)
(org-defkey org-mode-map [(shift down)]        nil)
(org-defkey org-mode-map [(shift left)]        nil)
(org-defkey org-mode-map [(shift right)]       nil)
(org-defkey org-mode-map [(meta shift up)]          'org-shiftup)
(org-defkey org-mode-map [(meta shift down)]        'org-shiftdown)
(org-defkey org-mode-map [(meta shift left)]        'org-shiftleft)
(org-defkey org-mode-map [(meta shift right)]       'org-shiftright)


(defun org--notes-file (root-dir notes-filename)
  "Get a notes org file"
  (let* ((rd (directory-file-name root-dir))
         (notes-fullpath (concat rd "/" notes-filename)))
    (when (and (not (string= "/" rd)) (file-exists-p rd))
      (if (file-exists-p notes-fullpath) notes-fullpath
        (org--notes-file (file-name-directory rd) notes-filename)))))

(defun org-open-notes-file ()
  (interactive)
  (let ((notes-file (org--notes-file default-directory "NOTES.org"))
        (buf (current-buffer)))
    (if notes-file
        (progn
          (find-file notes-file)
          ;; XXX: Maybe a notes minor mode?
          (unless (eq buf (current-buffer))
            (setq-local notes-last-buf buf)))
      (error "No NOTES.org found in a parent directory."))))

(global-set-key (kbd "C-c C-n") 'org-open-notes-file)

;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t) (python . t) (latex . t) (haskell . t) (js . t) (dot . t)))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(defun fd--org-doc-begin ()
  (save-excursion
    (goto-char (point-min))
    (while (and (or
                 (looking-at "[ \t]*#\\+")
                 (looking-at "[ \t]*$"))
                (progn (next-line) (< (point) (point-max))))
      (beginning-of-line))
    (point)))

(defun org-export--prepare-file-contents
    (file &optional lines ind minlevel id footnotes with-export-options)
  "Prepare contents of FILE for inclusion and return it as a string.

When optional argument LINES is a string specifying a range of
lines, include only those lines.

Optional argument IND, when non-nil, is an integer specifying the
global indentation of returned contents.  Since its purpose is to
allow an included file to stay in the same environment it was
created (e.g., a list item), it doesn't apply past the first
headline encountered.

Optional argument MINLEVEL, when non-nil, is an integer
specifying the level that any top-level headline in the included
file should have.

Optional argument ID is an integer that will be inserted before
each footnote definition and reference if FILE is an Org file.
This is useful to avoid conflicts when more than one Org file
with footnotes is included in a document.

Optional argument FOOTNOTES is a hash-table to store footnotes in
the included document.

Optional argument WITH-EXPORT-OPTIONS will stop this function
from ignoring export options at the beginning of the file."
  (with-temp-buffer
    (insert-file-contents file)
    (when (not with-export-options)
      (narrow-to-region (fd--org-doc-begin) (point-max)))
    (when lines
      (let* ((lines (split-string lines "-"))
	     (lbeg (string-to-number (car lines)))
	     (lend (string-to-number (cadr lines)))
	     (beg (if (zerop lbeg) (point-min)
		    (goto-char (point-min))
		    (forward-line (1- lbeg))
		    (point)))
	     (end (if (zerop lend) (point-max)
		    (goto-char (point-min))
		    (forward-line (1- lend))
		    (point))))
	(narrow-to-region beg end)))
    ;; Remove blank lines at beginning and end of contents.  The logic
    ;; behind that removal is that blank lines around include keyword
    ;; override blank lines in included file.
    (goto-char (point-min))
    (org-skip-whitespace)
    (beginning-of-line)
    (delete-region (point-min) (point))
    (goto-char (point-max))
    (skip-chars-backward " \r\t\n")
    (forward-line)
    (delete-region (point) (point-max))
    ;; If IND is set, preserve indentation of include keyword until
    ;; the first headline encountered.
    (when (and ind (> ind 0))
      (unless (eq major-mode 'org-mode)
	(let ((org-inhibit-startup t)) (org-mode)))
      (goto-char (point-min))
      (let ((ind-str (make-string ind ?\s)))
	(while (not (or (eobp) (looking-at org-outline-regexp-bol)))
	  ;; Do not move footnote definitions out of column 0.
	  (unless (and (looking-at org-footnote-definition-re)
		       (eq (org-element-type (org-element-at-point))
			   'footnote-definition))
	    (insert ind-str))
	  (forward-line))))
    ;; When MINLEVEL is specified, compute minimal level for headlines
    ;; in the file (CUR-MIN), and remove stars to each headline so
    ;; that headlines with minimal level have a level of MINLEVEL.
    (when minlevel
      (unless (eq major-mode 'org-mode)
	(let ((org-inhibit-startup t)) (org-mode)))
      (org-with-limited-levels
       (let ((levels (org-map-entries
		      (lambda () (org-reduced-level (org-current-level))))))
	 (when levels
	   (let ((offset (- minlevel (apply #'min levels))))
	     (unless (zerop offset)
	       (when org-odd-levels-only (setq offset (* offset 2)))
	       ;; Only change stars, don't bother moving whole
	       ;; sections.
	       (org-map-entries
		(lambda ()
		  (if (< offset 0) (delete-char (abs offset))
		    (insert (make-string offset ?*)))))))))))
    ;; Append ID to all footnote references and definitions, so they
    ;; become file specific and cannot collide with footnotes in other
    ;; included files.  Further, collect relevant footnote definitions
    ;; outside of LINES, in order to reintroduce them later.
    (when id
      (let ((marker-min (point-min-marker))
	    (marker-max (point-max-marker))
	    (get-new-label
	     (lambda (label)
	       ;; Generate new label from LABEL by prefixing it with
	       ;; "-ID-".
	       (format "-%d-%s" id label)))
	    (set-new-label
	     (lambda (f old new)
	       ;; Replace OLD label with NEW in footnote F.
	       (save-excursion
		 (goto-char (+ (org-element-property :begin f) 4))
		 (looking-at (regexp-quote old))
		 (replace-match new))))
	    (seen-alist))
	(goto-char (point-min))
	(while (re-search-forward org-footnote-re nil t)
	  (let ((footnote (save-excursion
			    (backward-char)
			    (org-element-context))))
	    (when (memq (org-element-type footnote)
			'(footnote-definition footnote-reference))
	      (let* ((label (org-element-property :label footnote)))
		;; Update the footnote-reference at point and collect
		;; the new label, which is only used for footnotes
		;; outsides LINES.
		(when label
		  (let ((seen (cdr (assoc label seen-alist))))
		    (if seen (funcall set-new-label footnote label seen)
		      (let ((new (funcall get-new-label label)))
			(push (cons label new) seen-alist)
			(org-with-wide-buffer
			 (let* ((def (org-footnote-get-definition label))
				(beg (nth 1 def)))
			   (when (and def
				      (or (< beg marker-min)
					  (>= beg marker-max)))
			     ;; Store since footnote-definition is
			     ;; outside of LINES.
			     (puthash new
				      (org-element-normalize-string (nth 3 def))
				      footnotes))))
			(funcall set-new-label footnote label new)))))))))
	(set-marker marker-min nil)
	(set-marker marker-max nil)))
    (org-normalize-links file)
    (org-element-normalize-string (buffer-string))))

(defun org-normalize-links (file)
  (goto-char (point-min))
  (setq org-link-search-failed nil)
  (while (not org-link-search-failed)
    (org-next-link)
    (let ((info (org-element-link-parser)))
      (when (and info (string= "file" (plist-get (cadr info) :type)))
        (delete-region (plist-get (cadr info) :begin)
                       (plist-get (cadr info) :end))
        (let* ((default-directory (file-name-directory file))
               (link (expand-file-name (plist-get (cadr info) :path))))
        (insert (org-make-link-string
                 link
                 (plist-get (cadr info) :description))))))))

;; (flycheck-define-checker proselint
;;   "A linter for prose."
;;   :command ("proselint" source-inplace)
;;   :error-patterns
;;   ((warning line-start (file-name) ":" line ":" column ": "
;;         (id (one-or-more (not (any " "))))
;;         (message) line-end))
;;   :modes (text-mode markdown-mode gfm-mode))
(add-hook 'markdown-mode-hook 'fd-markdown-hook)
(defun fd-markdown-hook ()
  (define-key markdown-mode-map (kbd "<S-tab>") nil)
  (define-key markdown-mode-map (kbd "<C-tab>") nil))

;; (add-to-list 'flycheck-checkers 'proselint)

(setq org-agenda-files '("~/.agenda"))
(setq org-agenda-custom-commands
      '(("w" "Agenda and tasks, non-personal"
         ((tags-todo "WORK")))
        ("p" "Agenda and tasks, personal"
         ((tags-todo "-WORK")))))

(setq org-capture-templates
  '(("b"
     "Capture current bibtex entry."
     entry
     '(file "~/.agenda/bibtex.org")
     "* References %?\n\n%a\n\n%:author (%:year): %:title\n   \
         In %:journal, %:pages.")))

(provide 'fd-org)
