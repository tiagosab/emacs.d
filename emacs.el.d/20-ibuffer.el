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

