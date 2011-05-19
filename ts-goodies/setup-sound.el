;; ==============================
;; Emms / mpd
;; ==============================

;; emms basic config

(require 'emms-setup)

;; emms-setup provides four functions for automatic setup of emms.

;(emms-minimalistic)
;(emms-standard) ; use the standard default config
(emms-all) ; use all stable features
;(emms-devel)

;; setup default players
(emms-default-players)

;; choose function used to find files; the info manual says simply
;; that the -find function is faster and should be used if gnu find is
;; present.
(setq emms-source-file-directory-tree-function
      'emms-source-file-directory-tree-find)

;; where are my music files
(setq emms-source-file-default-directory
      "/extra/multimedia/musica/")

; from http://dryice.name/blog/emacs/playing-media-files-within-emacs/
(require 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-music-directory "/extra/multimedia/musica/")

(defun ts-file-is-in-mpd-dir (file)
  (if (string-match (format "^%s" emms-player-mpd-music-directory)
                    file)
      t
    nil))

(defun ts-emms-play-file-no-mpd (file)
  (setq emms-player-list (delete 'emms-player-mpd emms-player-list))
  (message "%s" emms-player-list)
  (emms-stop)
  (emms-play-file file)
  (add-to-list 'emms-player-list 'emms-player-mpd))

(defun ts-dired-play-file-maybe-mpd (&optional filename)
  (interactive)
  (let ((file (or filename
                  (dired-get-file-for-visit))))
    (if (ts-file-is-in-mpd-dir file)
        (emms-play-file file)
      (ts-emms-play-file-no-mpd file))))

(defun dired-advertised-find-file (&rest options)
  "In dired, visit the file or directory named on this line,
except if it is a known multimedia file, in which case play with
emms."
  (interactive)
  (let ((find-file-run-dired t)
        (file (dired-get-file-for-visit)))
    (if (string-match ".*\\(mp3\\|ogg\\|flac\\)$"
                      file)
        (ts-dired-play-file-maybe-mpd file)
      (find-file file))))

;; library to download and save lyrics.  see also lyric-mode, which
;; aids to write lrc files, lyrics with timings.
;;
;; http://www.google.com.br/custom?hl=pt&cof=&domains=letras.terra.com.br&
;; q=chico+buarque+jumento&btnG=Pesquisar&sitesearch=letras.terra.com.br
(load-library "emms-get-lyrics")

(provide 'setup-sound)
