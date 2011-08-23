;;
;; Quick access to some files
;;

;; This first section should stay first in this file. After that,
;; I prefer putting first the most recent ones.
(defun ts-emacs ()
  (interactive)
  (find-file "/home/tiago/.emacs"))

(defun ts-ts ()
  (interactive)
  (find-file "/home/tiago/lib/emacs/ts-goodies/tiago.el"))

(defun ts-emanotes ()
  "Load my emacs notes file.

Implemented on 2011-04-18."
  (interactive)
  (find-file "/home/tiago/lib/emacs/emanotes.org"))
;; END first section

(defun ts-concreto ()
  (interactive)
  (let ((concreto-buffer (dired "/home/tiago/h/c")))
    (with-current-buffer concreto-buffer
      (mapc 'dired-maybe-insert-subdir
            '("concreto" "noticias" "meta"))
      (goto-char (point-min)))))

(require 'ipython)
(defun ts-open-dacdoc ()
  (interactive)
  (find-file "/home/tiago/src/paudearara/repo/docdac/dacdoc.py")
  (find-file "/home/tiago/src/paudearara/repo/sabase/sabase.py")
  (w3m "file:///usr/share/doc/python2.6-doc/html/index.html")
  (py-shell)
  (dired "/home/tiago/src/paudearara/repo")
  (with-current-buffer "repo\\paudearara\\src"
    (dired-maybe-insert-subdir "qt_design")
    (dired-maybe-insert-subdir "sabase")
    (dired-maybe-insert-subdir "docdac/ui")
    (dired-maybe-insert-subdir "docdac")
    (dired-maybe-insert-subdir "docs"))
  (find-file "/home/tiago/src/paudearara/repo/docs/devel.org"))

(defun ts-sabase-test ()
  (interactive)
  (shell-command "/home/tiago/src/paudearara/sabase.py -t &"))

(defun ts-sabase-mkt ()
  (interactive)
  (shell-command "/home/tiago/src/paudearara/sabase.py -mkt &"))

(defun ts-dacdoc-t ()
  (interactive)
  (shell-command "call-dacdoc.py -t &"))

(defun ts-alunos ()
  (interactive)
  (find-file "/home/tiago/src/elisp/repo/alunos.el"))

(defun ts-latex ()
  (interactive)
  (find-file "/home/tiago/var/rautu/latex.tex"))

(defun ts-gramatica ()
  (interactive)
  (dired "/home/tiago/home/portugues"))

(defun ts-mldonkey ()
  (interactive)
  (dired "/srv/mldonkey/incoming/files"))

(defun ts-concursos ()
  (interactive)
  (dired "/home/tiago/comuna/concursos/estudos"))

(require 'a2r)
(defun ts-machado ()
  (interactive)
  (find-file
   "/home/tiago/comuna/concursos/estudos/português/machado-esau-jaco.txt")
  (delete-other-windows)
  (end-of-buffer)
  (re-search-backward "^\\(\[0-9\]\\{1,2\\}\\)$")
  (next-line)
  (let ((number (match-string 1)))
    (find-file-other-window
     "/home/tiago/comuna/concursos/estudos/português/textos/esau-e-jaco.txt")
    (view-mode t)
    (ts-auto-view-mode)
    (beginning-of-buffer)
    (when number
      (re-search-forward
       (format "CAPÍTULO\[ \]*%s" (arabic-to-roman (string-to-number number))))
      (recenter 1))))

(provide 'setup-myfiles)
