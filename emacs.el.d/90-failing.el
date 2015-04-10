; From 90 on, space reserved to snippets that are failing

;; Tried viewer.el from emacswiki, but did not really work
;; (require 'viewer)
;; (viewer-stay-in-setup)
;; (setq viewer-modeline-color-unwritable "tomato"
;;       viewer-modeline-color-view "dark blue")
;; (viewer-change-modeline-color-setup)

;; ; If you want to open any file by `view-mode', add the following:
;; (viewer-aggressive-setup t)

(autoload 'irc-maybe "ts-setup-erc"
  "open erc"
  'interactive)
