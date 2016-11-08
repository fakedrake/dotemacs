(require 'emms-setup)
(require 'emms-info)
(require 'emms-stream-info)
(require 'emms-source-file)
(require 'emms-mark)
(require 'emms-history)
(require 'emms-tag-editor)
(emms-all)
(emms-default-players)

(setq emms-history-file "~/.emacs.d/emms-history")
(emms-history-load)
(setq emms-player-list '(emms-player-mpg321
                         emms-player-ogg123
                         emms-player-mplayer))

(setq emms-stream-info-backend 'mplayer)

(setq emms-source-file-default-directory "~/Music/")

(defun emms-stream-info-initialize (track)
  "Add track information to TRACK when playing http stream.
This is a useful element for `emms-info-functions'."
  (let* ((name (emms-track-name track))
         (url (or (emms-track-get track 'stream-url)
                  (and (string-prefix-p "http" name) name))))
    (when url
      (emms-track-set track 'stream-url url)
      (emms-stream-info-call-backend-elisp
       url (lambda (name genre bitrate nowplaying)
             (run-with-timer
              10 nil
              (apply-partially 'later-do 'emms-stream-info-initialize track))
             (when new-name
               (emms-track-set track 'name new-name)
               (emms-mode-line-alter))))
      t)))


(defun emms-stream-info-mine (name-rx genre-rx bitrate-rx nowplaying-rx)
  (let ((name "N/A")
	(genre "N/A")
	(bitrate "N/A")
	(nowplaying "N/A"))
    (emms-stream-info-defreg name name-rx)
    (emms-stream-info-defreg genre genre-rx)
    (emms-stream-info-defreg bitrate bitrate-rx)
    (emms-stream-info-defreg nowplaying nowplaying-rx)
    (list name genre bitrate nowplaying)))

(defun emms-backend-sentinel (cb proc status)
  (cond
   ((string-prefix-p "finished" status) (funcall cb))
   ((not (string-prefix-p "open" status))
    (error "Error getting stream info: " status))))

(defun emms-run-backend (cb prog &rest args)
  "Backend command for running mplayer on URL."
  (set-process-sentinel
   (apply 'start-process "stream-info" " *stream-info*" prog args)
   (apply-partially 'emms-backend-sentinel
                    (lambda () (with-current-buffer (get-buffer " *stream-info*")
                            (funcall cb))))))

(defun emms-stream-info-vlc-backend (url done)
  "Backend command for running VLC on URL. Done gets as arguments
the stream information."
  (let ((cb (apply-partially
             'eval `(apply ,done (emms-stream-info-mine
                                  "^Name[ ]+:[ ]+\\(.*\\)$"
                                  "^Genre[ ]+:[ ]+\\(.*\\)$"
                                  "^Bitrate[ ]+:[ ]+\\(.*\\)$"
                                  "ICY Info: StreamTitle='\\(.+?\\)'")))))
    (emms-run-backend cb "vlc" "-vvv" "--intf" "dummy" "--stop-time" "1"
                      "--noaudio" url "vlc:quit")))

(defun emms-stream-info-mplayer-backend (url done)
  "Backend command for running VLC on URL."
  (let ((cb (apply-partially
             'eval `(apply ,done (emms-stream-info-mine
                                  "'Title' = '\\(.*\\)'"
                                  "Genre: \\(.*\\)"
                                  "bitrate:\\([0-9].+\\)"
                                  "'Now Playing' = '\\(.+?\\)'")))))
    (emms-run-backend cb "mplayer" "-nocache" "-endpos" "0" "-vo" "null"
                      "-ao" "null" url)))

(defun emms-stream-info-call-backend-elisp (url cb)
  "Call backend and return a list of stream information for URL."
  (cond
   ((eq emms-stream-info-backend 'mplayer)
    (emms-stream-info-mplayer-backend url)
    ((eq emms-stream-info-backend 'vlc) (emms-stream-info-vlc-backend url))
    ((eq emms-stream-info-backend 'mplayer) (emms-stream-info-mplayer-backend url))
    (t (error "Unknown backend")))))

(setq emms-info-functions '(emms-stream-info-initialize))

(provide 'fd-emms)
