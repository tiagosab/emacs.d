;; -*- emacs-lisp -*-
;; My self
(setq user-full-name "Tiago Saboga")
(setq user-mail-address "tiagosaboga@gmail.com")

;; ===========================
;; General interface settings
;; ===========================

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; display the current time
(display-time)

;; Show column number at bottom of screen
(column-number-mode 1)

;; no menu, no toolbar, no scrollbar, no fringes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

;; black background - moved to Xresources/Xdefaults

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; add support for recent files list
;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; Emacs Load Path
;;(setq load-path (cons "~/lib/emacs" load-path))
(add-to-list 'load-path "~/lib/emacs")
(add-to-list 'load-path "~/lib/emacs/http")
(add-to-list 'load-path "~/lib/emacs/ljupdate-read-only/")

; load my general-purpose library
(load-library "tiago")
(global-set-key (kbd "C-x g") 'ts-gnus)

; make scripts executable when saving
(add-hook 'after-save-hook
          '(lambda ()
             (progn
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
;                    (shell-command (concat "chmod u+x " buffer-file-name))
                    (let ((mode (file-modes buffer-file-name)))
                      (chmod buffer-file-name
                             (logior mode 64))) ; add executable bit
                    (message (concat "Saved as script: " buffer-file-name))
                    ))))

(setq find-function-C-source-directory
      "/usr/local/src/debian-packages/emacs-snapshot-20090725/src")

;; ==============================
;; Basic editor settings
;; ==============================

(setq large-file-warning-threshold 20000000
      tab-width 4)

;; Visual feedback of mark
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq-default transient-mark-mode t)

;; never never open a separate file dialog
(setq use-file-dialog nil)

;; number of lines of margin at the top and bottom of a window.
;; recenter the window whenever point gets within this many lines of
;; the top or bottom of the window.
(setq scroll-margin 0)

;; Enable syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Enable paren-mode
(show-paren-mode t)

;; set utf8 input
(set-keyboard-coding-system 'mule-utf-8)

;; Pgup/dn will return exactly to the starting point.
(setq scroll-preserve-screen-position 1)

;; don't automatically add new lines when scrolling down at
;; the bottom of a buffer
(setq next-line-add-newlines nil)

;; indent with spaces only
(setq-default indent-tabs-mode nil)

;; scroll just one line when hitting the bottom of the window
(setq scroll-step 1)
(setq scroll-conservatively 8)

;; ==============================
;; Minor mode config
;; ==============================

;; highlight matches from searches
(setq isearch-highlight t)
(setq search-highlight t)

; allow scrolling commands during incremental search
(setq isearch-allow-scroll t)

;; ==============================
;; Major modes config
;; ==============================

;; do not use separate frame for woman
(setq woman-use-own-frame nil)

;(setq dired-listing-switches "-al --time-style=\"+%b %d %y\"")

;; replace auto-fill by longlines-mode in logjam
(add-hook 'lj-compose-common-hook 'turn-off-auto-fill nil t)
(add-hook 'lj-compose-common-hook 'longlines-mode nil t)

;;; My modes
(load-library "alunos")
(setq alunos-dir "~/home/aulasfran/alunos/")

(load-library "tresor")
(setq trs-switch-to-buffer 'display-buffer)

(load-library "robert")
(setq rob-switch-to-buffer 'display-buffer)

(add-to-list 'load-path "~/src/third-party/emacs-ditz/")
(require 'ditz)

(require 'ljupdate)
(require 'tc)

;;; load debian copyright mode
;; (load "/home/tiago/.emacs.d/debian-mr-copyright-mode.el")
;; (load "debian-mr-copyright-mode")
(autoload 'debian-mr-copyright-mode "debian-mr-copyright-mode"
  "Major mode for editing machine-readable copyright files (i.e. debian/copyright)."
  t)

;; ===========================
;; Dired
;; ===========================

;(require 'gnus-dired) ;, isn't needed due to autoload cookies
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

; Define key (V) in dired to view file in other window.
;
; I just redefine the view-file function to point to
; view-file-other-window, and back again. Is this any better (or worse
; then redefining dired-view-file anew? I've done that in tiago.el,
; but the current approach involves less repetition, and at least has
; served to learn how to use fset/symbol-function.
(define-key dired-mode-map "V"
  (lambda ()
    (interactive)
    (let (old)
      (fset 'old (symbol-function 'view-file))
      (fset 'view-file (symbol-function 'view-file-other-window))
      (dired-view-file)
      (fset 'view-file (symbol-function 'old)))))

(setq wdired-enable t)

;; ===========================
;; Text mode
;; ===========================

;; turn on word wrapping in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; turn on longlines-mode in text mode
;; (add-hook 'text-mode-hook 'longlines-mode)

;; ==============================
;; Ibuffer
;; ==============================

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-show-empty-filter-groups nil)

;; reverse the default sorting order
;(setq ibuffer-default-sorting-reversep nil)

(setq ibuffer-saved-filter-groups
  (quote
   (("default"
     ("dired" (mode . dired-mode))
     ("gnus" (or
              (mode . message-mode)
              (mode . bbdb-mode)
              (mode . mail-mode)
              (mode . gnus-group-mode)
              (mode . gnus-summary-mode)
              (mode . gnus-article-mode)
              (name . "^\\.bbdb$")
              (name . "^\\.newsrc-dribble")
              (name . "sent")
              ))
     ("python" (mode . python-mode))
     ("shell" (mode . shell-script-mode))
     ("lisp" (or
              (mode . emacs-lisp-mode)
              (mode . lisp-mode)
              (name . "^\\*scratch\\*$")
              (name . "^\\*Messages\\*$")
              (name . ".emacs")
              (name . "^.*\\.el.*$")
              ))
     ("man"  (or
              (mode . Man-mode)
              (name . "^\\*info\\*$")
              (mode . woman-mode)
              ))
     ("tex" (or
             (name . "\.aux$")
             (name . "\.tex$")
             (name . "\.texi$")
             (name . "\.log$")
             (name . "\.output$")
             (mode . tex-mode)
             (mode . latex-mode)
             ))
     ("internal" (name . "^\\*.*\\*$"))
     ))))


;; Hide gnus group
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (dolist (grp '("gnus" "lisp" "man" "internal" "dired"))
              (setq ibuffer-hidden-filter-groups
                    (cons grp ibuffer-hidden-filter-groups)))))

;; load default groups (set above).
;; must run before previous hook, so it must be added later.
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; run this when fiddling with ibuffer hooks
;;(setq ibuffer-mode-hook nil)

;; ==============================
;; TeX / LaTeX / AucTeX
;; ==============================

; (TeX-global-PDF-mode t)

;; ==============================
;; Mail
;; ==============================

; The default send-mail-function , sendmail-send-it, uses local
; sendmail (which would be fine, since I have esmtp configured
; locally), but it provides no way to add custom options to sendmail
; (which I need, since my conf file is not in the normal place. So, I
; have to use the internal smtp handler.

(setq send-mail-function 'smtpmail-send-it)

(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'smtpmail-send-it)

(setq message-directory "~/Gnumail/")

; configure smtp to use gmail.
(load-file "/home/tiago/etc/sensible-data/emacs-gmail.el")

; nmh
(setq mh-recursive-folders-flag t)

;; ==============================
;; BBdb
;; ==============================

(require 'bbdb)
(bbdb-initialize 'gnus 'message) ; 'w3)
(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-legal-zip-codes '("^$"
                             "^[ 	\n]*[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[ 	\n]*$"
                             "^[ 	\n]*\\([0-9][0-9][0-9][0-9][0-9][0-9]?\\)[ 	\n]*-?[ 	\n]*\\([0-9][0-9][0-9][0-9]?\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Za-z0-9]+\\)[ 	\n]+\\([A-Za-z0-9]+\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Z]+\\)[ 	\n]*-?[ 	\n]*\\([0-9]+ ?[A-Z]*\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Z]+\\)[ 	\n]*-?[ 	\n]*\\([0-9]+\\)[ 	\n]+\\([0-9]+\\)[ 	\n]*$"))
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (bbdb-insinuate-message)

;; ==============================
;; Jabber
;; ==============================

(setq jabber-account-list '(
                            ("tiagosaboga@gmail.com"
                             (:network-server . "talk.google.com")
                             (:port . 443)
                             (:connection-type . ssl))))

;; ===========================
;; Load stuff
;; ===========================

(put 'erase-buffer 'disabled nil)

;; ============================
;; More Key mappings (others keys are mapped above).
;; ============================

(setq ctl-ç-map (make-sparse-keymap))
(defalias 'ctl-ç-prefix ctl-ç-map)
(global-set-key (kbd "C-ç") 'ctl-ç-prefix)
(define-key ctl-ç-map "(" 'ts-corr-paren)
(define-key ctl-ç-map "[" 'ts-corr-brack)
(define-key ctl-ç-map "{" 'ts-corr-curl)

; quick help on help
(define-key help-map "h"
  (lambda ()
    (interactive)
    (message
     (concat "a commands; b bindings; c key command; "
             "f function; F command manual; i info;\n"
             "k key command docs; K key command manual; "
             "l lossage; m mode docs; p packages; r emacs manual"))))

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

;; I kept changing C-x C-d and C-x d (list-directory). Now they are
;; the same: why use list-directory if you have dired?.
(global-set-key [ (control x) (control d) ] 'dired)

;; easy commenting out of lines
(autoload 'comment-out-region "comment" nil t)
(global-set-key "\C-cq" 'comment-out-region)

(global-set-key "\C-xf" 'find-function)

(global-set-key (kbd "A-x A-r") 'ts-find-alternative-file-with-sudo)
