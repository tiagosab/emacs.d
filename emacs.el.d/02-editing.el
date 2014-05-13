; Yank at point instead of at click
(setq mouse-yank-at-point t)

;; don't automatically add new lines when scrolling down at
;; the bottom of a buffer
(setq next-line-add-newlines nil)

;; set utf8 input
(set-keyboard-coding-system 'mule-utf-8)

;; Enable syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Enable paren-mode
(show-paren-mode t)

;; indent with spaces only
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
