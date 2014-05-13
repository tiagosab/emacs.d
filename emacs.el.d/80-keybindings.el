(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'ts-gnus)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.03)

(key-chord-define-global "jn" (lambda () (interactive) (switch-to-buffer nil)))
(key-chord-define-global "sc" (lambda () (interactive)(switch-to-buffer "*scratch*")))
(key-chord-define-global "bn" 'bury-buffer)
