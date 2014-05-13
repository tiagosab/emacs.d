;; ==============================
;;; Org-mode
;; ==============================

; Load org-mode from git
(add-to-list 'load-path
             (concat ts-library-dir "/org-mode/lisp"))

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
