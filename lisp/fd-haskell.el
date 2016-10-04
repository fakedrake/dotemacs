(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'fd-haskell-mode)
(setq haskell-process-type 'auto)
(setq haskell-process-path-ghci "cabal")
(setq haskell-process-args-ghci '("repl"))

(defun fd-haskell-mode ()
  (setq comment-auto-fill-only-comments nil))

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

(provide 'fd-haskell)
