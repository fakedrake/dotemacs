
;; Ido mode
(require 'ido)
(require 'ido-speed-hack)
(require 'ido-better-flex)
(require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-save-directory-list-file (my-expand-path ".ido.last"))
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
;; (ido-everywhere t)
;; This is mainly for just swapped letters. It sometimes doesnt catch
;; entire words
(ido-better-flex/enable)
(setq ido-file-extensions-order '(".js" ".c" ".cpp" ".h" ".py" ".org" ".el" ".clj" ".org"))
(setq org-completion-use-ido t)

;; TODO: if you actually are at the buffer at point, rotate.
(defun ido-for-mode (prompt the-mode &optional not-found-fn keyword-fn)
  "Switch to buffer of the-mode using prompt use keyword-fn to
for a defun that will proide alternative names and not-found-fn
will get the user input and the alist of (displayed-name
. buffer) that we were completing on if the search fails so you
can create a buffer and switch to it or match in a stranger way."
  (let* ((keyword-cons (if keyword-fn
			   (lambda (b) (cons (funcall keyword-fn b) b))
			 (lambda (b) (cons b b))))
	 (key-buf-alist (mapcar keyword-cons
				(fd-mode-buffers the-mode)))
	 (input (ido-completing-read prompt key-buf-alist))
	 (target-buffer (cdr (assoc input key-buf-alist))))
    (if target-buffer
	(switch-to-buffer target-buffer)
      (when not-found-fn
	(funcall not-found-fn input key-buf-alist)))))

(defun fd-mode-buffers (the-mode)
  "List of buffers of mode THE-MODE."
  (save-excursion
    (delq
     nil
     (mapcar (lambda (buf)
	       (when (and (buffer-live-p buf) (not (eq (current-buffer) buf)))
		 (with-current-buffer buf
		   (and (eq major-mode the-mode)
			(buffer-name buf)))))
	     (buffer-list)))))

;; D[i,0] = i
;; D[0,j] = j
;; D[i,j] = (s1[i-1] == s2[j-1]) + min(D[i-1,j], D[i,j-1], D[i-1, j-1])
;; D[i,j] = (s1[i-1] == c) + min(D[i-1,j], D[i,j-1], D[i-1, j-1])
(defun levenstein-array (str1 d new-char)
  (let* ((last-row (car d))
         (j (1- (length d)))
         (cost (if (= new-char (aref str1 x)) 1 0)))
      (cons row d)))

(defun levenshtein-distance (str1 str2)
  "Return the edit distance between strings STR1 and STR2."
  (if (not (stringp str1))
      (error "Argument was not a string: %s" str1))
  (if (not (stringp str2))
      (error "Argument was not a string: %s" str2))
  (let* ((make-table ;; Multi-dimensional array object.
          (function
           (lambda (columns rows init)
             (make-vector rows (make-vector columns init)))))
         (tref ;; Table access method.
          (function
           (lambda (table x y)
            (aref (aref table y) x))))
         (tset ;; Table write method.
          (function
           (lambda
             (table x y object)
             (let ((row (copy-sequence (aref table y))))
               (aset row x object)
               (aset table y row)
               object))))
         ;; End table code.
         (length-str1 (length str1))
         (length-str2 (length str2))
         ;; d is a table with lenStr2+1 rows and lenStr2+1 columns
         (d (funcall make-table (1+ length-str1) (1+ length-str2)
                                0))) ;; Initialize to zero.
    ;; i and j are used to iterate over str1 and str2
    (let ((i 0)
          (j 0))
      (while (<= i length-str1) ;; for i from 0 to lenStr1
        (funcall tset d i 0 i) ;; d[i, 0] := i
        (setq i (1+ i))) ;; i++
      (while (<= j length-str2) ;; for j from 0 to lenStr2
        (funcall tset d 0 j j) ;; d[0, j] := j
        (setq j (1+ j)))) ;; j++
    (let ((i 1))
      (while (<= i length-str1) ;; for i from 1 to lenStr1
        (let ((j 1))
          (while (<= j length-str2) ;; for j from 1 to lenStr2
            (let* ((cost
                    ;; if str[i] = str[j] then cost:= 0 else cost := 1
                    (if (equal (aref str1 (1- i)) (aref str2 (1- j)))
                        0
                      1))
                   ;; d[i-1, j] + 1     // deletion
                   (deletion (1+ (funcall tref d (1- i) j)))
                   ;; d[i, j-1] + 1     // insertion
                   (insertion (1+ (funcall tref d i (1- j))))
                   ;; d[i-j,j-1] + cost // substitution
                   (substitution
                    (+ (funcall tref d (1- i) (1- j)) cost)))
              (funcall tset d i j ;; d[i,j] := minimum(
                    (min insertion deletion substitution)))
            (setq j (1+ j)))) ;; j++
        (setq i (1+ i)))) ;; i++
    ;; return d[lenStr1, lenStr2]
    (funcall tref d length-str1 length-str2)))


(setq yas-prompt-functions '(yas-ido-prompt))

(provide 'fd-ido)
