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
