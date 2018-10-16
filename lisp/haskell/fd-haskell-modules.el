(require 'haskell-modules)
;;;###autoload
(defun haskell-add-import (module)
  (interactive (list (completing-read
                      "Module: "
                      (haskell-session-all-modules
                       (haskell-modules-session)))))
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-max))
      (haskell-navigate-imports)
      (insert (concat "import " module "\n")))))

(defun haskell-check-module-name-and-highlight ()
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

(defun haskell-cabal-get-key (key)
  (haskell-cabal-subsection-entry-list (haskell-cabal-section) key))

(defun haskell-module-name (src-dir)
  (replace-regexp-in-string
   "\\.hs$" ""
   (replace-regexp-in-string
    "/" "."
    (substring fname (+ 1 (length src-dir))))))

(defun haskell-infer-module-name ()
  "Infer the module name. (used in yasnippet)"
  (save-match-data
    (let ((fname (buffer-file-name)))
      (if (haskell-cabal-find-file)
          (car
           (with-cabal-file-buffer
            (mapcar
             haskell-module-name
             (delete-if-not
              (lambda (x) (string-prefix-p x fname))
              (mapcar
               (apply-partially 'concat default-directory)
               (haskell-cabal-get-key "hs-source-dirs"))))))
        (file-name-base fname)))))

(provide 'fd-haskell-modules)
