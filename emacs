;; My self
(setq user-full-name "Tiago Saboga")
(setq user-mail-address "tiagosaboga@gmail.com")

;; Enable syntax highlighting

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; set utf8 input
(set-keyboard-coding-system 'mule-utf-8)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(home-end-enable t)
 '(ibuffer-enable t)
 '(jde-jdk-registry (quote (("1.5" . "/usr/lib/jvm/java-1.5.0-sun-1.5.0.11"))))
 '(large-file-warning-threshold 20000000)
 '(load-home-init-file t t)
 '(scroll-margin 5)
 '(tab-width 4)
 '(wdired-enable t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; ============================
;; Key mappings
;; ============================

;; use F1 key to go to a man page
(global-set-key [f1] 'man)
;; use F3 key to kill current buffer
(global-set-key [f3] 'kill-this-buffer)
;; use F5 to get help (apropos)
(global-set-key [f5] 'apropos)
;; use F9 to open files in hex mode
(global-set-key [f9] 'hexl-find-file)

;; goto line function C-c C-g
(global-set-key [ (control c) (control g) ] 'goto-line)

(global-set-key [ (meta /) ] 'hippie-expand)

;; easy commenting out of lines
(autoload 'comment-out-region "comment" nil t)
(global-set-key "\C-cq" 'comment-out-region)

;; display the current time
(display-time)

;; Show column number at bottom of screen
(column-number-mode 1)

;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight matches from searches
(setq isearch-highlight t)
(setq search-highlight t)
(setq-default transient-mark-mode t)

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; ===========================
;; Behaviour
;; ===========================

;; Pgup/dn will return exactly to the starting point.
(setq scroll-preserve-screen-position 1)

;; don't automatically add new lines when scrolling down at
;; the bottom of a buffer
(setq next-line-add-newlines nil)

;; scroll just one line when hitting the bottom of the window
(setq scroll-step 1)
(setq scroll-conservatively 8)

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; no menu
(menu-bar-mode -1)

;; turn off the toolbar
(tool-bar-mode -1)

;; turn on word wrapping in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; indent with spaces only
(setq-default indent-tabs-mode nil)

;; load debian copyright mode
(load "/home/tiago/.emacs.d/debian-mr-copyright-mode.el")
;; (load "python")

;; add support for recent files list
;; recentf stuff
;;(require 'recentf)
;;(recentf-mode 1)
;;(setq recentf-max-menu-items 25)
;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)