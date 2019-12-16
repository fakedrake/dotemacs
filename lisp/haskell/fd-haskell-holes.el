(defun flycheck-type-of-hole-raw (&optional pos)
  "Get the type of the hole at POS.

POS defaults to `point'. Use `flycheck-type-of-hole' for some normalizations."
  (-when-let* ((errors (flycheck-overlay-errors-at (or pos (point)))))
    (with-temp-buffer
      (insert (flycheck-error-message (car errors)))
      (goto-char (point-min))
      (save-match-data
        (when (or
               (search-forward-regexp
                "Found hole: _[[:alnum:]_']* :: \\([[:alnum:]_']+\\)"
                nil t)
               (search-forward-regexp
                "• Found type wildcard ‘_’[[:space:]]*standing for ‘\\([^’]*?\\)’"
                nil t))
          (match-string 1))))))

(defun flycheck-type-of-hole (&optional pos)
  (if-let ((hole (flycheck-type-of-hole-raw pos)))
      (cond
       ((string= hole "[Char]") "String")
       (t hole))))

(defun haskell-replace-hole-flycheck (&optional pos)
  "Replace the hole under point "
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (expr (flycheck-type-of-hole)))
    (if (null expr) (error "Failed to get the value of hole.")
      (delete-region (car bounds) (cdr bounds))
      (insert expr))))

(provide 'fd-haskell-holes)
