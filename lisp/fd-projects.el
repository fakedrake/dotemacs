;;; fd-projects.el --- Summary
;;; Commetary:
;; Open and bury projects intuitively.
;;
;; TODO:
;; - Prioritize open buffers over
;;; Code:
(require 'recentf)

(defvar project--commit-files-command
  "for i in  $(git rev-list HEAD~%d..HEAD); do git show --pretty='format:' --name-only  $i; done | sort | uniq -c | sort -n -r | sed 's/^ *[0-9]* *//'"
  "Command to be formated with the number of commits to be looked
  into.")

(defvar project--commit-depth 10
  "Number of commits to look into when looking for recently
  edited files")

(defvar project--max-files 5
  "Number of files to open for a project.")

(defun project-open-roots ()
  "Open projects."
  (delete-dups
   (delete-if #'null
              (mapcar
               (lambda (b) (with-current-buffer b (current-project-root)))
               (buffer-list)))))

(defun project-roots (files &optional shift-current)
  "Project roots of FILES if they are in a git repository. If
FILES is not provided use recent files."
  (let ((roots
         (delete-if-not
          (apply-partially 'string-prefix-p "/")
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
    (if (and shift-current (string= (car roots) (current-project-root)))
        (append (cdr roots) (list (car roots)))
      roots)))


(defun project-buffer-file-name (buffer)
  "Buffer file name or default-directory"
  (or (buffer-file-name buffer)
      (with-current-buffer buffer
        default-directory)))

(defun project-buffer-in-project (project buffer)
  (project-file-in-project project
                           (project-buffer-file-name buffer)))

(defun project-file-in-project (project file)
  "Check if buffer corresponds to the project."
  (= 0
     (call-process "git" nil nil nil "-C"
                   project
                   "ls-files"
                   file
                   "--error-unmatch")))

(defun project-common-files (project)
  "List interesting files in a project using
`project--comit-files-command' and `project--commit-depth'. These
are the filesystem files."
  (mapcar (apply-partially 'concat project "/")
          (split-string
           (shell-command-to-string
            (format "cd %s && %s | head -%d"
                    project
                    (format project--commit-files-command project--commit-depth)
                    project--max-files))
           "\n")))

(defun project-recent-files (project)
  "Recently opened files of project"
  (remove-if-not (apply-partially 'project-file-in-project project)
                 recentf-list))

(defun project-files (project)
  "A prioritized list of some of the files in project."
  (delete-dups (append
                (project-recent-files project)
                (project-common-files project))))

(defun project-read-root (pred filename-list &optional shift-current)
  "Read root of project for a list of files. If not current "
  (let ((roots (project-roots filename-list shift-current)))
    (ido-completing-read pred roots nil t)))

(defun project-buffers (project)
  "A list of open buffers in the project. Ordered as they were in
buffer-list."
  (delete-if-not
   (apply-partially 'project-buffer-in-project project)
   (buffer-list)))

(defun project-open (project)
  "Open project. Preserve the sequence of buffers in the buffer
list and prioritize recent buffers over unopened ones. Unopened
buffers are ones that have recent commits."
  (interactive
   (list (project-read-root "Open project: " recentf-list t)))
  (let ((bufs (project-buffers project)))
    ;; First get all the files we dont have, they will mess up the
    ;; buffer-list ordering
    (mapc 'find-file (reverse (project-files project)))
    ;; Restore ordering for our old buffers and put them at the top.
    (mapc 'switch-to-buffer (reverse bufs))))

(defun current-project-root ()
  (or current-project
      (let ((path (git--exec-string-no-error "rev-parse" "--show-toplevel")))
        (when (s-starts-with? "/" path)
          (setq-local current-project (s-trim path))))))

(defun bury-project (root)
  "Bury all the buffers that have default directory in a
project."
  (interactive (list (project-read-root
                      "Bury project: "
                      (mapcar (lambda (b)
                                (with-current-buffer b default-directory))
                              (buffer-list)))))
  (dolist (b (buffer-list))
    (when (file-in-directory-p (project-buffer-file-name b) root)
      (with-current-buffer b (bury-buffer))))
  (message (format "Buried buffers '%s'" root)))

(defun kill-project (root)
  "Kill all the buffers that have default directory in a
project."
  (interactive (list (project-read-root
                      "Kill project: "
                      (mapcar (lambda (b)
                                (with-current-buffer b default-directory))
                              (buffer-list)))))
  (dolist (b (buffer-list))
    (when (file-in-directory-p (project-buffer-file-name b) root)
      (kill-buffer b)))
  (message (format "Killed buffers '%s'" root)))

(defun switch-project (root)
  "Kill all the buffers that have default directory in a
project."
  (interactive (list (project-read-root
                      "Switch to project: "
                      (mapcar (lambda (b)
                                (with-current-buffer b default-directory))
                              (buffer-list)) t)))
  (dolist (b (reverse (buffer-list)))
    (when (file-in-directory-p (project-buffer-file-name b) root)
      (switch-to-buffer b)))
  (message (format "Switched to buffers '%s'" root)))

(global-set-key (kbd "C-c p f") 'project-open)
(global-set-key (kbd "C-c p o") 'project-open)
(global-set-key (kbd "C-c p b") 'bury-project)
(global-set-key (kbd "C-c p k") 'kill-project)
(global-set-key (kbd "C-c p s") 'switch-project)

(provide 'fd-projects)
;;; fd-projects.el ends here
