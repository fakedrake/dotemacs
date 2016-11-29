(require 'emms-setup)
(require 'emms-info)
(require 'emms-stream-info)
(require 'emms-source-file)
(require 'emms-mark)
(require 'emms-history)
(require 'emms-tag-editor)
(setq emms-source-file-default-directory "~/Music/")
(emms-all)
(emms-default-players)

(setq emms-history-file "~/.emacs.d/emms-history")
(emms-history-load)
(setq emms-player-list '(emms-player-mpg321
                         emms-player-ogg123
                         emms-player-mplayer))

(setq emms-stream-info-backend 'mplayer)
(setq emms-polling-stream-info nil)
(setq emms-stream-info-polling-interval 2)
(defun emms-maybe-poll-track-info (track polling-thread)
  "Will check the invariants of polling:

- polled track is playing
- polled track is polled by only one thread

So this function should be safe to call as much as you want and it won't spam"
  (let* ((continuing-poll (and
                           (eq (cdr emms-polling-stream-info) polling-thread)
                           (eq (car emms-polling-stream-info) track)))
         (no-other-polls (null emms-polling-stream-info))
         (polled-by-someone-else (not (or no-other-polls continuing-poll)))
         (currently-playing (and emms-player-playing-p
                                 (eq (emms-playlist-current-selected-track) track)))
         (polling-current-track (eq (car emms-polling-stream-info)
                                    (emms-playlist-current-selected-track))))
    (when (not polling-current-track) (setq emms-polling-stream-info nil))
    (when (and currently-playing (not polled-by-someone-else))
      (setq emms-polling-stream-info (cons track polling-thread))
      (run-with-timer
       emms-stream-info-polling-interval nil
       (apply-partially 'later-do 'emms-stream-info-initialize track)))))

(defun emms-stream-info-initialize (track &optional polling-thread)
  "Add track information to TRACK when playing http stream.
This is a useful element for `emms-info-functions'."
  (let* ((name (emms-track-name track))
         (url (or (emms-track-get track 'fd-stream-url)
                  (and (string-prefix-p "http" name) name)))
         (track-is-stream url))
    (when track-is-stream
      (emms-track-set track 'fd-stream-url url)
      (emms-stream-info-call-backend-elisp
       url (lexical-let ((track track)
                         (url url)
                         (polling-thread (or polling-thread (random))))
             (lambda (name genre bitrate nowplaying)
               (emms-maybe-poll-track-info track polling-thread)
               (emms-track-set track 'name url)
               (emms-track-set track 'info-title nowplaying)
               (emms-mode-line-alter)))))
    t))

(defun emms-stream-info-mine (name-rx genre-rx bitrate-rx nowplaying-rx)
  "Mine info from a buffer where the stdout of a command was dumped."
  (let ((name "N/A") (genre "N/A") (bitrate "N/A") (nowplaying "N/A"))
    (emms-stream-info-defreg name name-rx)
    (emms-stream-info-defreg genre genre-rx)
    (emms-stream-info-defreg bitrate bitrate-rx)
    (emms-stream-info-defreg nowplaying nowplaying-rx)
    (list name genre bitrate nowplaying)))

(defun emms-backend-sentinel (cb proc status)
  "The sentinel for the command"
  (cond
   ((string-prefix-p "finished" status) (funcall cb))
   ((not (string-prefix-p "open" status))
    (error "Error getting stream info: " status))))

(defun emms-apply-then-kill-buf (cb buf)
  (setq emms-polling-stream-info nil)
  (with-current-buffer buf (funcall cb))
  (kill-buffer buf))

(defun emms-run-backend (cb prog &rest args)
  "Backend command for running mplayer on URL."
  (let ((buf (concat " *stream-info-" (number-to-string (random)) "*")))
    (set-process-sentinel
     (apply 'start-process "stream-info" buf prog args)
     (apply-partially 'emms-backend-sentinel
                      (apply-partially 'emms-apply-then-kill-buf
                                       cb buf)))))

(defun emms-stream-info-vlc-backend (url done)
  "Backend command for running VLC on URL. Done gets as arguments
the stream information."
  (let ((cb (apply-partially
             'eval `(apply ,done (emms-stream-info-mine
                                  "'Title' = '\\(.*\\)'"
                                  "Genre: \\(.*\\)"
                                  "bitrate:\\([0-9].+\\)"
                                  "'Now Playing' = '\\(.+?\\)'")))))
    (emms-run-backend cb "vlc" "-vvv" "--intf" "dummy" "--stop-time" "1"
                      "--noaudio" url "vlc:quit")))

(defun emms-stream-info-mplayer-backend (url done)
  "Backend command for running MPlayer on URL."
  (let ((cb (apply-partially
             'eval `(apply ,done (emms-stream-info-mine
                                  "^Name[ ]+:[ ]+\\(.*\\)$"
                                  "^Genre[ ]+:[ ]+\\(.*\\)$"
                                  "^Bitrate[ ]+:[ ]+\\(.*\\)$"
                                  "ICY Info: StreamTitle='\\(.+?\\)'")))))
    (emms-run-backend cb "mplayer" "-nocache" "-endpos" "0" "-vo" "null"
                      "-ao" "null" url)))

(defun emms-stream-info-call-backend-elisp (url cb)
  "Call backend and return a list of stream information for URL."
  (cond
   ((eq emms-stream-info-backend 'vlc) (emms-stream-info-vlc-backend url cb))
   ((eq emms-stream-info-backend 'mplayer) (emms-stream-info-mplayer-backend url cb))
   (t (error "Unknown backend"))))

(defun mplayer-stream-start-listening ()
  (emms-stream-info-initialize (emms-playlist-current-selected-track)))

(add-hook 'emms-player-started-hook 'mplayer-stream-start-listening)

(add-hook 'emms-info-functions 'emms-stream-info-initialize)
(setq emms-info-asynchronously t)

(provide 'fd-emms)
