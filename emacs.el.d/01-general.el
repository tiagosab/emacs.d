;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

; Max size of buffer log
(setq large-file-warning-threshold 20000000
      message-log-max 1000)

;; Activate commands
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)


