;; ===========================
;; Dired
;; ===========================

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
; man(1) run-mailcap)
(define-key dired-mode-map (kbd "s-s") 'ts-dired-external-see)

(setq dired-guess-shell-alist-user
      '(("^.*\.odt$" "odt2txt")))

(setq wdired-enable t)

(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
