;;; fd-projects.el --- Summary
;;; Commetary:
;; Open and bury projects intuitively.
;;
;; TODO:
;; - Prioritize open buffers over
;;; Code:
(require 'recentf)

(defvar fd-project--commit-files-command
  "for i in  $(git rev-list HEAD~%d..HEAD); do git show --pretty='format:' --name-only  $i; done | sort | uniq -c | sort -n -r | sed 's/^ *[0-9]* *//'"
  "Command to be formated with the number of commits to be looked
  into.")

(defvar fd-project--commit-depth 10
  "Number of commits to look into when looking for recently
  edited files")

(defvar fd-project--max-files 5
  "Number of files to open for a project.")

(defun fd-project-open-roots ()
  "Open projects."
  (delete-dups
   (delete-if #'null
              (mapcar
               (lambda (b) (with-current-buffer b (current-fd-project-root)))
               (buffer-list)))))

(defun fd-project-roots (files &optional shift-current)
  "Project roots of FILES if they are in a git repository. If
FILES is not provided use recent files."
  (let ((roots
         (delete-if
          (lambda (root) (s-matches? "^\\([^/]\\|/[^\\]+:\\)" root))
          (delete-dups
           (mapcar
            (lambda (d)
              (s-trim
               (shell-command-to-string
                (format "git -C %s rev-parse --show-toplevel" d))))
            (delete-dups
             (mapcar
              'file-directory-name
              (delete-if-not 'file-exists-p files))))))))
    (if (and shift-current (string= (car roots) (current-fd-project-root)))
        (append (cdr roots) (list (car roots)))
      roots)))


(defun fd-project-buffer-file-name (buffer)
  "Buffer file name or default-directory"
  (or (buffer-file-name buffer)
      (with-current-buffer buffer
        default-directory)))

(defun fd-project-buffer-in-project (project buffer)
  (fd-project-file-in-project project
                           (fd-project-buffer-file-name buffer)))

(defun fd-project-file-in-project (project file)
  "Check if buffer corresponds to the project."
  (= 0
     (call-process "git" nil nil nil "-C"
                   project
                   "ls-files"
                   file
                   "--error-unmatch")))

(defun fd-project-common-files (project)
  "List interesting files in a project using
`fd-project--comit-files-command' and `fd-project--commit-depth'. These
are the filesystem files."
  (mapcar (apply-partially 'concat project "/")
          (split-string
           (shell-command-to-string
            (format "cd %s && %s | head -%d"
                    project
                    (format fd-project--commit-files-command fd-project--commit-depth)
                    fd-project--max-files))
           "\n")))

(defun fd-project-recent-files (project)
  "Recently opened files of project"
  (remove-if-not (apply-partially 'fd-project-file-in-project project)
                 (fd-project-all-recent-files)))

(defun fd-project-files (project)
  "A prioritized list of some of the files in project."
  (delete-dups (append
                (fd-project-recent-files project)
                (fd-project-common-files project))))

(defun fd-project-proper-filename (fname)
  "Check for string names not being tramp since it's not certain
that they will be findable."
  (not (delete-if 'null (mapcar
                         (lambda (pre) (string-prefix-p pre fname))
                         '("/sudo:" "/ssh:")))))

(defun fd-project-read-root (pred filename-list &optional shift-current)
  "Read root of project for a list of files. If not current "
  (let ((roots (fd-project-roots filename-list shift-current)))
    (ido-completing-read pred roots nil t)))

(defun fd-project-buffers (project)
  "A list of open buffers in the project. Ordered as they were in
buffer-list."
  (delete-if-not
   (apply-partially 'fd-project-buffer-in-project project)
   (buffer-list)))

(defun fd-project-all-recent-files (&optional files)
  (delete-if-not 'fd-project-proper-filename (or files recentf-list)))

(defun fd-project-open (project)
  "Open project. Preserve the sequence of buffers in the buffer
list and prioritize recent buffers over unopened ones. Unopened
buffers are ones that have recent commits."
  (interactive
   (list (fd-project-read-root "Open project: " (fd-project-all-recent-files) t)))
  (let ((bufs (fd-project-buffers project)))
    ;; First get all the files we dont have, they will mess up the
    ;; buffer-list ordering
    (mapc 'find-file (reverse (fd-project-files project)))
    ;; Restore ordering for our old buffers and put them at the top.
    (mapc 'switch-to-buffer (reverse bufs))))

(defun current-fd-project-root ()
  (if (boundp 'current-project)
      current-project
    (let ((path (shell-command-to-string
                 (format "git -C %s rev-parse --show-toplevel" default-directory))))
      (when (s-starts-with? "/" path)
        (setq-local current-project (s-trim path))))))

(defun bury-project (root)
  "Bury all the buffers that have default directory in a
project."
  (interactive (list (fd-project-read-root
                      "Bury project: "
                      (fd-project-all-recent-files
                       (mapcar (lambda (b)
                                 (with-current-buffer b default-directory))
                               (buffer-list))))))
  (dolist (b (buffer-list))
    (when (file-in-directory-p (fd-project-buffer-file-name b) root)
      (with-current-buffer b (bury-buffer))))
  (message (format "Buried buffers '%s'" root)))

(defun kill-project (root)
  "Kill all the buffers that have default directory in a
project."
  (interactive (list (fd-project-read-root
                      "Kill project: "
                      (mapcar (lambda (b)
                                (with-current-buffer b default-directory))
                              (buffer-list)))))
  (dolist (b (buffer-list))
    (when (file-in-directory-p (fd-project-buffer-file-name b) root)
      (kill-buffer b)))
  (message (format "Killed buffers '%s'" root)))

(defun switch-project (root)
  "Kill all the buffers that have default directory in a
project."
  (interactive (list (fd-project-read-root
                      "Switch to project: "
                      (fd-project-all-recent-files
                       (mapcar (lambda (b)
                                 (with-current-buffer b default-directory))
                               (buffer-list))) t)))
  (dolist (b (reverse (buffer-list)))
    (when (file-in-directory-p (fd-project-buffer-file-name b) root)
      (switch-to-buffer b)))
  (message (format "Switched to buffers '%s'" root)))

(global-set-key (kbd "C-c p f") 'fd-project-open)
(global-set-key (kbd "C-c p o") 'fd-project-open)
(global-set-key (kbd "C-c p b") 'bury-project)
(global-set-key (kbd "C-c p k") 'kill-project)
(global-set-key (kbd "C-c p s") 'switch-project)

(provide 'fd-projects)
;;; fd-projects.el ends here
