(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'mu4e)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(global-unset-key (kbd "C-z"))

(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.03)

(key-chord-define-global "jn" (lambda () (interactive) (switch-to-buffer nil)))
(key-chord-define-global "sc" (lambda () (interactive)(switch-to-buffer "*scratch*")))
(key-chord-define-global "bn" 'bury-buffer)

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

(define-key mu4e-headers-mode-map "|" 'mu4e-view-pipe)

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
(global-set-key [f1] 'view-mode)
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
