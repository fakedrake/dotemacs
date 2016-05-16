(setq greek-alphabet "αβγδεζηθικλμνξοπρστυφχψωάέήίόύώΆΈΉΊΌΎΏΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ")

(defun greek-char-octals ()
  (mapconcat
   (lambda (c) (format "\\%o" (multibyte-char-to-unibyte c)))
   greek-alphabet
   []))

(setq ispell-program-name "aspell")
;; (setq greek-entry `("el"
;;                     ,(format "[a-zA-Z%s]" greek-alphabet)
;;                     ,(format "[^a-zA-Z%s]" greek-alphabet)
;;                     "['\"]" t ("latin/greek") nil iso-8859-7))
; (add-to-list 'ispell-dictionary-alist greek-entry)
;
(defun maybe-set-greek-prime (start end corrections)
  (let ((word (buffer-substring start end))
        (corr (caaddr corrections))
        (primes '((?ά . ?α) (?έ . ?ε) (?ή . ?η) (?ί . ?ι)
                  (?ό . ο) (?ύ . ?υ) (?ώ . ω))))
    (when (and corr (= (length corr) (length word))
               (= 1 (apply '+ (mapcar*
                               (lambda (c1 c2)
                                 (cond ((= c1 c2) 0)
                                       ((= c1 (cdr (assoc c2 primes))) 1)
                                       (t 2))) word corr))))
      (save-excursion
        (delete-region start end)
        (goto-char start)
        (insert corr)))))
(add-to-list 'flyspell-incorrect-hook 'maybe-set-greek-prime)

(provide 'fd-aspell)
