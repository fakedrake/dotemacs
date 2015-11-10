s;; ORG mode
;; (load-library "org-compat") 		;XXX this is bad but i see no better way.

;; (load-library "org-list")
;; (require 'org-element)
(load-library "org")			; Requiring doesnt get the
					; defvar for some reason

;; Bindings
(add-hook 'org-mode-hook
	  '(lambda ()
	     (define-key org-mode-map "\M-j" 'org-meta-return)
	     (define-key org-mode-map "\M-n" 'org-forward-element)
	     (define-key org-mode-map "\M-p" 'org-backward-element)
	     (setq org-return-follows-link t)))
(defun fd-org-mode-hook ()
  (define-key org-mode-map "\M-j" 'org-meta-return)
  (define-key org-mode-map "\M-n" 'org-forward-element)
  (define-key org-mode-map "\M-p" 'org-backward-element)
  (define-key org-mode-map (kbd "S-TAB") nil)
  (setq org-return-follows-link t))
  ;(add-to-list 'autopair-extra-pairs '(:everywhere ("=" . "="))))
(add-hook 'org-mode-hook 'fd-org-mode-hook)

;; AutoPairs
;; (add-hook 'org-mode-hook
;;           #'(lambda () (add-to-list 'autopair-extra-pairs '(:everywhere ("=" . "=")))))

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
	       '("fakedrake-org-article"
		 "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{float}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont{DejaVu Sans}
\\setmonofont[Scale=0.8]{FreeMono}
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

  (setq org-file-apps '((auto-mode . emacs)
			("\\.pdf\\'" . "evince %s"))))

(add-hook 'org-mode-hook 'fd/org-latex-hook)

(global-set-key (kbd "C-c l") 'org-store-link)
(org-defkey org-mode-map [(shift up)]          nil)
(org-defkey org-mode-map [(shift down)]        nil)
(org-defkey org-mode-map [(shift left)]        nil)
(org-defkey org-mode-map [(shift right)]       nil)

(provide 'fd-org)
