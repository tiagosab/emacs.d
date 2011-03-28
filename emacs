;; -*- emacs-lisp -*-
;; My self
(setq user-full-name "Tiago Saboga")
(setq user-mail-address "tiagosaboga@gmail.com")

;; ===========================
;; General interface settings
;; ===========================

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(setq hl-line-sticky-flag t)
(global-hl-line-mode nil)
;(set-face-background 'hl-line "RoyalBlue4")
;(set-face-background 'hl-line "gray")
(set-face-background 'hl-line "gray12")
(set-face-foreground 'hl-line nil)

(defun ts-next-hl-line-face-background ()
  (interactive)
  (let ((current (face-background 'hl-line))
        (colors '("RoyalBlue4" "gray" "gray12" "black"))
        (new nil))
    (setq new
          (let ((first (car colors)))
            (while colors
              (let ((color (car colors)))
                (setq colors (cdr colors))
                (when (string-match current color)
                  (setq new (nth 0 colors))
                  (setq colors ())
                  )))
            (if (not new)
                (setq new first))
            new))
    (message (concat "New hl-line background: " new))
    (set-face-background 'hl-line new)))
  
(global-set-key (kbd "C-c C-ç") 'ts-next-hl-line-face-background)

;; display the current time
(display-time)

;; Show column number at bottom of screen
(column-number-mode 1)

;; no menu, no toolbar, no scrollbar, no fringes
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)

;; fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
; this is useful when not running a tiling window manager
; (add-hook 'after-make-frame-functions 'toggle-fullscreen)

(setq mouse-yank-at-point t)

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
(add-to-list 'load-path "~/etc/emacs.d")

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
                    (message (concat "Saved as script: " buffer-file-name))))))

; where should emacs find its C source file
(setq find-function-C-source-directory
      "/usr/local/src/debian-packages/emacs-snapshot-20090725/src")

; work around bug: when activating menu-bar in a tiling wm, minibuffer
; is no longer visible; does not work.
(add-hook 'menu-bar-mode-on-hook
          (lambda ()
            (set-frame-height nil 35)))

; abbrevs for find-file
;
; I would like to setup something like that, but it is not working for
; now.  I suppose the bindings are the problem, but both ways I have
; found insist not to work: SPC is always bound to
; self-insert-command.
(defun geosoft-parse-minibuffer ()
  ;; Extension to the complete word facility of the minibuffer
  (interactive)
  (backward-char 4)
  (setq found t)
  (cond
     ; local directories
     ((looking-at "pyti") (setq directory "/home/tiago/src/pyti/"))
     ((looking-at "home") (setq directory "/home/tiago/home/"))
     ((looking-at ".src") (setq directory "/home/tiago/src/"))
     (t (setq found nil)))
  (cond (found (beginning-of-line)
                (kill-line)
                (insert directory))
         (t     (forward-char 4)
                (minibuffer-complete))))
;; (define-key minibuffer-local-completion-map
;;   " " 'geosoft-parse-minibuffer)
;; (add-hook 'minibuffer-setup-hook
;;           '(lambda ()
;;              (define-key minibuffer-local-completion-map
;;                " " 'geosoft-parse-minibuffer)))

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

;; show parent dirs name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-min-dir-content 2)
; other interesting feature, but conflicts with min-dir-content
; (setq uniquify-strip-common-suffix t)


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

;; set plantuml location
(setq plantuml-jar-path "/home/tiago/lib/java/plantuml.jar")

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

; if we have autoinsert, load my config
(and (require 'autoinsert nil t)
     (load-library "ts-autoinsert"))

(add-to-list 'load-path "~/src/third-party/emacs-ditz/")
(require 'ditz)

(require 'ljupdate)
(require 'tc)
(require 'stumpwm-mode)

(add-to-list 'load-path "~/lib/emacs/eldonkey/")
(load-file "/home/tiago/etc/sensible-data/mldonkey.el")
(require 'eldonkey)

; (load-library "mldonkey-config")

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
; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

; If we are to change dired-mode-map, we must:
(require 'dired)

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

; open file in external viewer (according to system configuration; see
; man(1) run-mailcap
(define-key dired-mode-map (kbd "s-s") 'ts-dired-external-see)

(setq dired-guess-shell-alist-user
      '(("^.*\.odt$" "odt2txt")))

(setq wdired-enable t)

(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

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

;; reverse the default sorting order
;(setq ibuffer-default-sorting-reversep nil)

(setq ibuffer-saved-filter-groups
  (quote
   (("default"
     ("docdac" (filename . "/home/tiago/src/paudearara"))
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
            ;; (dolist (grp '("gnus" "lisp" "man" "internal" "dired"))
            ;;   (setq ibuffer-hidden-filter-groups
            ;;         (cons grp ibuffer-hidden-filter-groups)))
            (setq ibuffer-show-empty-filter-groups nil)
              ))

;; load default groups (set above).
;; must run before previous hook, so it must be added later.
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; run this when fiddling with ibuffer hooks
;;(setq ibuffer-mode-hook nil)

;; ==============================
;;; Org-mode
;; ==============================

(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(eval-after-load "org-latex"
  '(setq org-export-latex-classes
         (append '(
                   ("matharticle"
                    "\\documentclass[11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{amsmath}"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
          org-export-latex-classes)))

;; ==============================
;;; TeX / LaTeX / AucTeX
;; ==============================

(eval-after-load 'latex
  '(let
       ((hooks '(outline-minor-mode
                 ts-auctex-hide-document-class-begin
                 TeX-fold-mode
                 TeX-fold-buffer)))
     (setq hooks (nreverse hooks))
     (while hooks
       (add-hook 'LaTeX-mode-hook
                 (car hooks))
       (setq hooks (cdr hooks)))))

;;; Mail

; The default send-mail-function , sendmail-send-it, uses local
; sendmail (which would be fine, since I have esmtp configured
; locally), but it provides no way to add custom options to sendmail
; (which I need, since my conf file is not in the normal place. So, I
; have to use the internal smtp handler.

;; (setq send-mail-function 'smtpmail-send-it)

;; (setq mail-user-agent 'gnus-user-agent)
;; (setq message-send-mail-function 'smtpmail-send-it)
;;
;; (setq message-directory "~/Gnumail/")

; This is the prefix for temporary files used by gnus when downloading
; files (in my case (the mail case), in fact, it just copies the
; contents from files in another dir
;; (setq mail-source-incoming-file-prefix "Incoming")
; This chooses when to delete those temporary files; t is immediately,
; nil is never, or a number of days. Default is nil in CVS, t in
; released versions.
;; (setq mail-source-delete-incoming nil)

; configure smtp to use gmail.
; (load-file "/home/tiago/etc/sensible-data/emacs-gmail.el")

; nmh
; (setq mh-recursive-folders-flag t)

; directory from which all other Gnus file variables are derived.
; must be set in .emacs instead of gnus.el
; (setq gnus-directory "~/Gnumail/")

; if gnus is not running, message-mode will store drafts in this
; directory (under message-directory). They are now pointing to the
; same place, what happens when one just sets gnus-directory and
; message-directory to the same dir. Is this safe? "drafts" is the
; default value.
;; (setq message-auto-save-directory "drafts")

;; ==============================
;; BBdb
;; ==============================

(require 'bbdb)
; (bbdb-initialize 'gnus 'message) ; 'w3)
(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-legal-zip-codes '("^$"
                             "^[ 	\n]*[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[ 	\n]*$"
                             "^[ 	\n]*\\([0-9][0-9][0-9][0-9][0-9][0-9]?\\)[ 	\n]*-?[ 	\n]*\\([0-9][0-9][0-9][0-9]?\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Za-z0-9]+\\)[ 	\n]+\\([A-Za-z0-9]+\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Z]+\\)[ 	\n]*-?[ 	\n]*\\([0-9]+ ?[A-Z]*\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Z]+\\)[ 	\n]*-?[ 	\n]*\\([0-9]+\\)[ 	\n]+\\([0-9]+\\)[ 	\n]*$"))
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (bbdb-insinuate-message)

; moy-bbdb is a library to add addresses to which I sent mail to bbdb.
(autoload 'bbdb/send-hook "moy-bbdb"
  "Function to be added to `message-send-hook' to notice records
when sending messages" t)
(add-hook 'message-send-hook 'bbdb/send-hook) ; If you use Gnus
;(add-hook 'mail-send-hook 'bbdb/send-hook) ; For other mailers
;                                           ; (VM, Rmail)


;; ==============================
;; W3m
;; ==============================

(setq w3m-use-cookies t)
(setq w3m-follow-redirection 20)

;; ==============================
;; Jabber
;; ==============================

(eval-after-load "jabber"
  '(progn
     (setq jabber-account-list '(
                                 ("tiagosaboga@gmail.com"
                                  (:network-server . "talk.google.com")
                                  (:port . 443)
                                  (:connection-type . ssl))))
     (setq jabber-show-offline-contacts nil)
     (set-face-attribute 'jabber-title-large nil
                         :width 'expanded
                         :height 1.5)
     (set-face-attribute 'jabber-title-medium nil
                         :height 1.5)))

;; ==============================
;; Emms / mpd
;; ==============================

; emms basic config

; emms-setup provides four functions for automatic setup of emms.
(require 'emms-setup)
;(emms-minimalistic)
; (emms-standard) ; use the standard default config
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

(setq super-dict-map (make-sparse-keymap))
(defalias 'super-d-prefix super-dict-map)
(global-set-key (kbd "s-d") 'super-d-prefix)
(define-key super-dict-map "d" 'dict)
(define-key super-dict-map "t" 'tresor)
(define-key super-dict-map "r" 'robert)


(global-set-key (kbd "s-t") 'tresor)
(global-set-key (kbd "s-w") 'w3m)
(global-set-key (kbd "C--") 'undo)

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
(global-set-key "\C-x\M-f" 'find-library)

(global-set-key (kbd "A-x A-r") 'ts-find-alternative-file-with-sudo)

;; use view-mode instead of just toggling read-only flag
(global-set-key (kbd "C-x C-q") 'view-mode)

(put 'narrow-to-page 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)

;; ===========================
;; Wanderlust
;; ===========================

;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "tiagosaboga@gmail.com") 
(setq elmo-imap4-default-authenticate-type 'clear) 
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t) 

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "tiagosaboga@gmail.com")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t)

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))
