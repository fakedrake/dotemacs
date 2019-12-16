(defun haskell-error-at-point (pos)
  "The flycheck error at point as taken by the overlay."
  (if-let
      ((err
        (find-if
         'identity
         (mapcar (lambda (ov) (overlay-get ov 'flycheck-error))
                 (overlays-at pos)))))
      (flycheck-error-message err)))

(defmacro haskell-with-current-error-buffer (pos &rest body)
  "Put the error at POS in a tmp buffer and execut BODY"
  `(if-let ((error-at-pt (haskell-error-at-point pos)))
       (with-temp-buffer
         (insert error-at-pt)
         (goto-char (point-min))
         (progn ,@body))
     (error "No flycheck error at point.")))

(defun haskell-type-of-hole (pos)
  "Get the type of the hole at POS using flycheck as a string."
  (if-let*
      ((tyrx "Found type wildcard ‘_\\s_*’[[:space:]]*standing for ‘\\([^’]*\\)’")
       (type
        (haskell-with-current-error-buffer pos
         (save-match-data
           (when
               (search-forward-regexp tyrx nil t)
             (match-string-no-properties 1))))))
      (replace-regexp-in-string "[\n\t ]+" " " type)))

(defun haskell-expand-type-hole ()
  "Repace the hole at point with it's type using flychceck."
  (interactive)
  (if-let ((bounds (bounds-of-thing-at-point 'symbol))
           (ty (haskell-type-of-hole (point))))
      (progn
        (delete-region (car bounds) (cdr bounds))
        (insert ty))))

(provide 'fd-haskell-holes)
