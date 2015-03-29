;; -*- emacs-lisp -*-

;; ===========================
;; Visual interface settings
;; ===========================

; I like traditional whole-char cursor
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

; I would turn hl-line on if it could use different colors
; on X and on the console.
;(setq hl-line-sticky-flag t)
;(global-hl-line-mode nil)
;(set-face-background 'hl-line "RoyalBlue4")
;(set-face-background 'hl-line "gray")
;(set-face-background 'hl-line "gray12")
;(set-face-foreground 'hl-line nil)

;; load zenburn color theme
(add-to-list 'custom-theme-load-path (concat ts-library-dir "/zenburn"))
(load-theme 'zenburn t)

;; display the current time
(display-time)

;; Show column number at bottom of screen
(column-number-mode 1)

;; no toolbar, no scrollbar, no fringes
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

; make all frames fullscreen
(modify-all-frames-parameters '((fullscreen . maximized)))

;; Set font size
(set-face-attribute 'default nil :height 110)

;; Visual feedback of mark
(setq-default transient-mark-mode t)

;; number of lines of margin at the top and bottom of a window.
;; recenter the window whenever point gets within this many lines of
;; the top or bottom of the window.
(setq scroll-margin 0)

;; Pgup/dn will return exactly to the starting point.
(setq scroll-preserve-screen-position 1)

;; show parent dirs name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-min-dir-content 2)
; other interesting feature, but conflicts with min-dir-content
; (setq uniquify-strip-common-suffix t)

;; scroll just one line when hitting the bottom of the window
(setq scroll-step 1)
(setq scroll-conservatively 8)

(defun ts-make-frame-fullscreen (frame)
  (message "Making frame fullscreen.")
  (set-frame-parameter frame 'fullscreen 'fullboth))

(defun ts-choose-theme ()
  "From http://superuser.com/questions/579349/how-to-make-emacsclient-uses-differnt-themes-in-terminal-and-x-window"
  (message "oi")
  (if (window-system)
      (progn
        (disable-theme 'manoj-dark) ; in case it was active
        (enable-theme 'zenburn))
    (progn
      (disable-theme 'zenburn) ; in case it was active
      (enable-theme 'manoj-dark))))

;; This is not working. Flickering is too much, and it happened that
;; emacs did not recover of a try
;(add-hook 'focus-in-hook
;'ts-choose-theme)

(add-hook 'after-make-frame-functions
          'ts-make-frame-fullscreen)
