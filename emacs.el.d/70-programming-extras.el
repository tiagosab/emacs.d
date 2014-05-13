; Color matching parens
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)

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
