(require 'flyspell)
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
;; (add-to-list 'ispell-dictionary-alist greek-entry)

(defun take (n ls) (when (> n 0) (cons (car ls) (take (1- n) (cdr ls)))))
(defun replace-region (start end word)
  (save-excursion
    (delete-region start end)
    (goto-char start)
    (insert word)))

(defvar greek-primes-alist
  '((?ά . ?α)
    (?έ . ?ε)
    (?ή . ?η)
    (?ί . ?ι)
    (?ό . ?ο)
    (?ύ . ?υ)
    (?ώ . ?ω)
    (?Ά . ?Α)
    (?Έ . ?Ε)
    (?Ή . ?Η)
    (?Ί . ?Ι)
    (?Ό . ?Ο)
    (?Ύ . ?Υ)
    (?Ώ . ?Ω)))

(defun emphasized-word (word corrections)
  (when corrections
    (let ((corr (car corrections)))
      (or (and (= (length corr) (length word)) (> (length corr) 0)
               (= 1 (apply
                     '+
                     (mapcar*
                      (lambda (c1 c2)
                        (cond ((= c1 c2) 0)
                              ((= c1 (or (cdr (assoc c2 greek-primes-alist)) 0)) 1)
                              (t 2))) word corr))) corr)
          (emphasized-word word (cdr corrections))))))

(defun maybe-set-greek-prime (start end corrections)
  (let* ((word (buffer-substring start end))
         (emphasized (emphasized-word word (take 3 (caddr corrections)))))
    (when emphasized
      (replace-region start end emphasized))))
(add-to-list 'flyspell-incorrect-hook 'maybe-set-greek-prime)

(provide 'fd-aspell)
