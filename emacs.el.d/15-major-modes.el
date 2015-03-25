;; do not use separate frame for woman
(setq woman-use-own-frame nil)

; if we have autoinsert, load my config
(and (require 'autoinsert nil t)
     (load-library "ts-autoinsert"))

(add-to-list 'load-path "~/src/third-party/emacs-ditz/")
(require 'ditz)

(autoload 'debian-mr-copyright-mode "debian-mr-copyright-mode"
  "Major mode for editing machine-readable copyright files (i.e. debian/copyright)."
  t)

;; turn on word wrapping in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
