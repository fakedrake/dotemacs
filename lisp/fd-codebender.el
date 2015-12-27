(defun codebender-log-action-codes ()
  (interactive)
  (find-file "/Users/drninjabatman/Projects/Codebendercc/UserResearch/README.md"))

(defun codebender-gregorian-day-diff (days &optional date)
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian
       (or date (calendar-current-date)))
      days)))

;; (defvar codebender-end-date '(05 15 2014))
(defvar codebender-end-date nil)
(unless (boundp 'codebender-metrics-url)
  (defvar codebender-metrics-url "https://codebender.cc/"))
(defun codebender-url-browse-user (&optional user days)
  (interactive (list (string-to-number
                      (read-string (format
                                    "Insert user name to browse in metrics(%d): "
                                    (number-at-point))))))
  (let* ((d (codebender-gregorian-day-diff -8 codebender-end-date))
         (now (codebender-gregorian-day-diff 0 codebender-end-date))
         (user (if (= user 0) (number-at-point) user))
         (dates (format "&startingDate=%d-%02d-%02d&endingDate=%d-%02d-%02d"
                        (calendar-extract-year d)
                        (calendar-extract-month d)
                        (calendar-extract-day d)
                        (calendar-extract-year now)
                        (calendar-extract-month now)
                        (calendar-extract-day now)))
         (if user
             (browse-url
              (format
               "%s?user=%d&related=0"
               codebender-metrics-url
               user))
           (error "Enter a valid user id.")))))



(defgroup cblogs nil
  "cblogs group.")

(defgroup cblogs-lock-faces nil
  "Faces for cblogs."
  :group 'cblogs
  :group 'faces)

(defmacro cblogs-defface (sym &optional bg fg w)
  (let ((attrs (append
                (when w (list :weight w))
                (when fg (list :foreground fg))
                (when bg (list :background bg))))
        (face-sym (make-symbol (format "%s-face" (symbol-name sym)))))
    `(progn
       (defvar ,sym (quote ,face-sym))
       (defface
           ,face-sym '((t ,attrs))
           "Face for cblogs mode"
           :group 'cblogs-lock-faces))))

(cblogs-defface cblogs-return-fail "red2" "white")
(cblogs-defface cblogs-flash-button "LightBlue" "black")
(cblogs-defface cblogs-programmer-flash-button "darkcyan")
(cblogs-defface cblogs-flash-project "LightGreen" "black")
(cblogs-defface cblogs-bps-threshold "yellow1" "black")
(cblogs-defface cblogs-port nil "darkred" bold)
(cblogs-defface cblogs-return-success "green4")
(cblogs-defface cblogs-return-reader-timeout "lightpink" "black")
(cblogs-defface cblogs-ports-update "yellow3" "black")

(defvar cblogs-font-lock-keywords
  (list
   (cons "FLASH_BUTTON" '(0 cblogs-flash-button))
   (cons "FLASH_W_PROGRAMMER_BUTTON" '(0 cblogs-prorgrammer-flash-button))
   (cons "FLASH_PROJECT" '(0 cblogs-flash-project))
   (cons "PLUGIN_SERIAL_LIB_PORTS_JSON" '(0 cblogs-ports-update))
   (cons "APP_BPS_THRESHOLD" '(0 cblogs-bps-threshold))
   (cons "\"port\":\".*?\"" '(0 cblogs-port))
   (cons "\"return_value\":20000" '(0 cblogs-return-reader-timeout))
   (cons "\"return_value\":[^0][^,]*" '(0 cblogs-return-fail))
   (cons "\"return_value\":0" '(0 cblogs-return-success))
   )
  "Faces for font lock.")

(defvar cblogs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'cblogs-next-same-event)
    (define-key map (kbd "M-p") 'cblogs-previous-same-event)
    (define-key map (kbd "C-x t d") 'cblogs-timediff)
    map))

(define-minor-mode cblogs-mode
  "Highlinght logs"
  :global nil :lighter " CB-Logs" :init-value nil
  (if cblogs-mode (enable-cblogs) (disable-cblogs)))

(defun enable-cblogs ()
  (message "Enabling codebender log mode...")
  (font-lock-add-keywords nil cblogs-font-lock-keywords 'prepend)
  (font-lock-flush)
  (toggle-truncate-lines 1)
  (message "Enabled codebender log mode"))

(defun disable-cblogs ()
  (message "Disabling codebender log mode...")
  (font-lock-remove-keywords nil cblogs-font-lock-keywords)
  (font-lock-flush)
  (toggle-truncate-lines -1)
  (message "Disabled codebender log mode..."))

(defvar cblogs-separator "\t"
  "Separators for fields")

(require 'parse-time)
(defun cblogs-time (&optional pos)
  (save-excursion
    (goto-char (or pos (point)))
    (apply 'encode-time
           (parse-time-string
            (car
             (split-string
              (buffer-substring (save-excursion (beginning-of-line) (point))
                                (save-excursion (end-of-line) (point)))
              cblogs-separator))))))

;; XXX: If we are on the last line (an empty line) we can't find a
;; correct time. Go up a line in that case.
(defun cblogs-timediff (&optional start end)
  (interactive)
  (let* ((s (or start (if (use-region-p) (region-beginning) (point-min))))
         (e (or end (if (use-region-p) (region-end) (point-max))))
         (td (time-subtract (cblogs-time e) (cblogs-time s))))
    (if (called-interactively-p)
        (message "Time the region sees: %d seconds" (time-to-seconds td))
      td)))

(defun cblogs-this-line (&optional pos)
  (save-excursion
    (goto-char (or pos (point)))
    (buffer-substring (save-excursion (beginning-of-line) (point))
                      (save-excursion (end-of-line) (point)))))

(defun cblogs-show-minibuffer (present regex &optional group)
  "Show the match of the current line in modeline"
  (let ((domain (cblogs-this-line (point))))
    (string-match regex domain)
    (message "%s%s" present (match-string group domain))))


(defun cblogs-browse-referer-url ()
  (interactive)
  (let* ((line (cblogs-this-line))
         (url-rx "\"referer_url\":\"\\(.*?\\)\"")
         (url (progn (string-match url-rx line)
                     (replace-regexp-in-string "\\\\/" "/" (match-string 1 line)))))
    (browse-url url)))

(defun cblogs-current-event-description ()
  (interactive)
  (let* ((line (cblogs-this-line))
         (name-rx "^[^\t]*\t[^\t]*\t\\([A-Z_]*\\)"))
    (string-match name-rx line)
    (match-string 1 line)))

(defun cblogs-next-same-event ()
  (interactive)
  (end-of-line)
  (search-forward (cblogs-current-event-description))
  (beginning-of-line))

(defun cblogs-previous-same-event ()
  (interactive)
  (beginning-of-line)
  (search-backward (cblogs-current-event-description))
  (beginning-of-line))

;; Idle timer
(defvar cblogs-idle-timer nil)

;; callback function
(defun cblogs-idle-timer-callback ()
  (message "I have been called (%s)" (current-time-string)))

;; start functions
(defun cblogs-idle-timer-run-once ()
  (interactive)
  (when (timerp cblogs-idle-timer)
    (cancel-timer cblogs-idle-timer))
  (setq idle-timer-cookbook-timer
        (run-with-idle-timer 1 nil #'cblogs-idle-timer-callback)))

(defun cblogs-idle-timer-start ()
  (interactive)
  (when (timerp cblogs-idle-timer)
    (cancel-timer cblogs-idle-timer))
  (setq cblogs-idle-timer-timer
        (run-with-timer 1 1 #'cblogs-idle-timer-callback)))

;; stop function
(defun cblogs-idle-timer-stop ()
  (interactive)
  (when (timerp cblogs-idle-timer)
    (cancel-timer cblogs-idle-timer))
  (setq cblogs-idle-timer nil))

(provide 'fd-codebender)
