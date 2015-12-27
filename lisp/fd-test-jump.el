(defvar test-basename-format-fn  nil)
(defvar source-basename-format-fn nil)
(defvar test-directory-names '("tests" "test"))


(defun dir.. (p) (file-name-directory (directory-file-name p)))
(defun path-cdr (path)
  "Remove the root dir of a relative path"
  (mapconcat 'identity (cdr (split-string path "/")) "/"))

(defun set-suffix (suffix str)
  (if (string-suffix-p suffix str) str (concat str suffix)))
(defun set-prefix (prefix str)
  (if (string-prefix-p prefix str) str (concat prefix str)))
(defun unset-prefix (prefix str)
  (if (string-prefix-p prefix str) (substring str (length prefix)) str))
(defun unset-suffix (suffix str)
  (if (string-suffix-p suffix str)
      (substring str 0 (- (length str) (length suffix)))
    str))

(defun find-command (dir &rest find-args)
  (cdr
   (reverse
    (split-string
     (or (with-temp-buffer
           (when (= 0 (apply 'call-process
                             "find" nil (current-buffer) nil dir find-args))
             (buffer-string))) "") "\n"))))

(defun find-name (dir name-arg &rest args)
  (apply 'find-command dir "-name" name-arg args))

(defun common-prefix (path1 path2)
  "Find the common path prefix of two paths."
  (if (or (string= path1 "") (string= path1 "/") (string-prefix-p path1 path2))
      path1
    (js2-common-prefix (file-directory-name path1) path2)))

(defun directory-diff (path1 path2)
  "Get the portion of path1 that does not appear in path2"
  (file-relative-name path1 (js2-common-prefix path1 path2)))

(defun candidate-paths (file root-path)
  "A list of matching corresponding files to FILE that have the
prefix ROOT-PATH and their "
  (js2-directory-diff path common-path))

(defun first-match (root postfix list-items-fn)
  "Starting at (concat ROOT POSTFIX) and finishing at ROOT call
list-items-fn until it retruns non-nil"
  (when postfix
    (let* ((real-postfix (if (or (string= postfix ".")
                                 (string= postfix "/")) ""
                           postfix))
           (files (funcall list-items-fn
                           (concat (file-name-as-directory root) real-postfix)))
           (next-postfix (when (not (string= real-postfix ""))
                           (file-directory-name real-postfix))))
      (or files (first-match root next-postfix list-items-fn)))))

(defun existing-path (path)
  (when (file-exists-p path) path))

(defun existing-test-directories (root)
  "List existing test directories based on root"
  (delete-if-not
   'identity
   (mapcar
    (lambda (test-candidate)
      (existing-path (concat (file-name-as-directory root) test-candidate)))
    test-directory-names)))

(defun test-root (path)
  "Get the test directory that is in parent of PATH based on
`test-directory-names'"
  (let ((rel-to-root (file-relative-name (or path default-directory) "/")))
    (car (first-match "/" rel-to-root 'existing-test-directories))))

(defun source-root (path)
  (let ((test-dir (test-root path)))
    (and test-dir (dir.. test-dir))))

(defun test-file-p (path)
  "Check if we are in a test directory"
  (let ((test-dir (test-root path)))
    (and test-dir (string-prefix-p test-dir path))))

(defun prefix-path (path)
  "The path between PATH and the root path."
  (let ((root (if (test-file-p path)
                  (test-root path) (source-root path))))
    (and root (directory-file-name (file-relative-name path root)))))

(defun test-path (path)
  "Given the path of a source file get the path of the
corresponding test file, nil otherwise"
  (let ((root (test-root path))
        (prefix (prefix-path path))
        (test-file-iter
         (lambda (p)
           (list (existing-path (concat (dir.. p)
                                        (funcall test-basename-format-fn p)))))))
    (and root prefix
         (or (car (first-match root prefix test-file-iter))
             (test-path (concat (file-name-as-directory root) (path-cdr prefix)))))))

(defun source-path (path)
  "Given the path of a test file get the path of the
corresponding source file, nil otherwise."
  (let* ((root (source-root path))
         (prefix (prefix-path path))
         (source-file (funcall source-basename-format-fn path))
         (source-file-iter (lambda (p) (find-name p source-file))))
    (and root prefix
         (car (first-match root prefix source-file-iter)))))

(defun toggle-test-source-file (&optional path)
  "If we are in source jump to the test. If we are in test, jump
to source."
  (interactive)
  (let* ((path (or path buffer-file-name))
        (target-path (if (test-file-p path)
                         (source-path path)
                       (test-path path))))
    (if (not target-path) (message "Couldn't toggle source for %s" path)
      (find-file target-path)
      (message "Jumped to %s" target-path))))

(defun py-test-basename-format-fn (dir)
  (set-suffix
   ".py" (set-prefix "test_" (file-name-nondirectory (directory-file-name dir)))))
(defun py-source-basename-format-fn (dir)
  (set-suffix
   ".py" (unset-prefix "test_" (file-name-nondirectory (directory-file-name dir)))))

(setq test-basename-format-fn 'py-test-basename-format-fn)
(setq source-basename-format-fn 'py-source-basename-format-fn)
