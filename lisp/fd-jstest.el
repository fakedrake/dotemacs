(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun js2-test-file-p (filename)
  (and (string= "js" (file-name-extension filename))
       (string= "test" (file-name-extension
                        (file-name-base filename)))))

(defun find-command (dir name-arg)
  (cdr
   (reverse
    (split-string
     (with-temp-buffer
       (call-process "find" nil (current-buffer) nil dir "-name" name-arg)
       (buffer-string)) "\n"))))

(defun js2-test-directory (dir)
  "Search up the directory tree for a directory named test/"
  (let* ((test-dir (concat dir "test")))
    (when dir
      (if (file-directory-p test-dir) test-dir
        (js2-test-directory
         (parent-directory dir))))))

(defun js2-test-file (filename)
  "Get the test file. If this is a test file get this file"
  (if (js2-test-file-p filename) (expand-file-name filename)
    (let ((test-dir (js2-test-directory filename)))
      (when test-dir
        (expand-file-name
         (format "%s/%s.test.js"
                 test-dir
                 (file-name-base filename)))))))

(defun js2-src-file (filename)
  "Get the filename of the test"
  (let* ((extension (file-name-extension filename))
         (basename (file-name-base filename))
         (test-dir (js2-test-directory (file-name-directory filename)))
         ;; src-dir may not be the direct parent of the src file
         (src-dir (if (not (js2-test-file-p filename))
                      (file-name-directory filename)
                    (file-name-directory test-dir)))
         (matching-files (find-command
                          src-dir
                          (format "%s.js" (file-name-base basename)))))
    (when matching-files
      (expand-file-name  (car matching-files)))))

(defun js2-maybe-jump-test ()
  (interactive)
  (let ((test-file (js2-test-file (buffer-file-name))))
    (if test-file
        (find-file test-file)
      (error "Could not locate test file."))))

(defun js2-maybe-jump-src ()
  (interactive)
  (let ((src-file (js2-src-file (buffer-file-name))))
    (if src-file (find-file src-file)
      (error "Could not locate source file."))))

(defun js2-toggle-source-test ()
  (interactive)
  (if (string= "js" (file-name-extension (buffer-file-name)))
    (if (js2-test-file-p (buffer-file-name))
        (js2-maybe-jump-src)
      (js2-maybe-jump-test))
    (error "Not in javascript file.")))


(defun js2-test-this-file ()
  (interactive)
  (compilation-start (format "mocha %s" (js2-test-file (buffer-file-name)))))

(provide 'fd-jstest)
