;;; downloaded from 
;;; https://kanis.fr/svn/trunk/wk/lisp/emacs.d/ivan-autoinsert.el

;;; ivan-autoinsert.el --- add text automatically in new buffer

(require 'ivan-fun)
(require 'ivan-var)

(eval-when-compile
  (require 'autoinsert))

(defvar ivan-autoinsert-top-comment nil
  "Block of text to insert at top, nil to insert nothing")

(defvar ivan-autoinsert-bottom-comment
  (concat
   " Copyright (C) " (substring (current-time-string) -4) " Ivan Kanis
 Author: Ivan Kanis
 \044Id$

 This program is free software ; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation ; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY ; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program ; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA")
  "Block of text to insert at bottom, nil to insert nothing")

(defun ivan-autoinsert-python ()
  "Python template"
  (insert "#!/usr/bin/env python
# -*- coding: utf-8 -*-\n")
  (ivan-autoinsert-insert "#" ivan-autoinsert-top-comment)
  (insert "\n\nif __name__ == \"__main__\":\n    ")
  (save-excursion
    (insert "\n\n")
    (ivan-autoinsert-local-variables "#" "python")
    (ivan-autoinsert-insert "#" ivan-autoinsert-bottom-comment)
    (insert "\n")))

(defun ivan-autoinsert-perl ()
  "Perl template"
  (insert "#!/usr/bin/env perl\n")
  (ivan-autoinsert-insert "#" ivan-autoinsert-top-comment)
  (insert "\n
use strict;
use warnings;\n\n")
  (save-excursion
    (insert "\n\n")
    (ivan-autoinsert-local-variables "#" "perl")
    (ivan-autoinsert-insert "#" ivan-autoinsert-bottom-comment)
    (insert "\n")))

(defun ivan-autoinsert-shell ()
  "Shell script template, auto detect Korn Shell"
  (let ((shell (if (file-exists-p "/bin/ksh") "ksh" "sh")))
    (insert "#!/bin/" shell "\n")
    (and ivan-autoinsert-top-comment
         (progn
           (ivan-autoinsert-insert "#" ivan-autoinsert-top-comment)
           (insert "\n")))
    (insert "\n")
    (save-excursion
      (insert "\n\n")
      (ivan-autoinsert-local-variables "#" shell)
      (and ivan-autoinsert-bottom-comment
           (progn
             (insert "\n\n")
             (ivan-autoinsert-insert "#" ivan-autoinsert-bottom-comment)))
      (insert "\n"))))

(defun ivan-autoinsert-makefile ()
  (ivan-autoinsert-insert "#" ivan-autoinsert-top-comment)
  (insert "\n\n")
  (save-excursion
    (insert "\n\n")
    (ivan-autoinsert-local-variables "#" "make -f")
    (ivan-autoinsert-insert "#" ivan-autoinsert-bottom-comment)
    (insert "\n")))

(defun ivan-autoinsert-c ()
  "C template"
  (if ivan-autoinsert-top-comment
      (progn
        (ivan-autoinsert-c-start)
        (ivan-autoinsert-insert " *" ivan-autoinsert-top-comment)
        (ivan-autoinsert-c-end)))
  (insert "\n")
  (save-excursion
    (insert "\n\n")
    (if (string-match "\\.\\(h\\|hpp\\)$" buffer-file-name)
        (let ((name
               (concat "_"
                       (upcase (file-name-nondirectory
                                (file-name-sans-extension buffer-file-name)))
                       "_H")))
          (insert "
#ifndef " name "
#define " name "
#endif /* " name " */\n"))
      (ivan-autoinsert-c-start)
      (ivan-autoinsert-local-variables " *" "cc ")
      (ivan-autoinsert-c-end))
    (if ivan-autoinsert-bottom-comment
        (progn
          (ivan-autoinsert-c-start)
          (ivan-autoinsert-insert " *" ivan-autoinsert-bottom-comment)
          (ivan-autoinsert-c-end)))))

(defun ivan-autoinsert-c-start ()
  (insert "/*\n"))

(defun ivan-autoinsert-c-end ()
  (insert "\n */\n"))

(defun ivan-autoinsert-elisp ()
  "Elisp template"
  (and ivan-autoinsert-top-comment
       (progn
         (ivan-autoinsert-insert ";;" ivan-autoinsert-top-comment)
         (insert "\n")))
  (insert ";;; " (file-name-nondirectory (buffer-file-name))
          " --- Describe me

\;;; Commentary:

\;;; THANKS:

\;;; BUGS:

\;;; INSTALLATION:

\;;; Code:

")
  (save-excursion
    (insert "\n\n")
    (ivan-autoinsert-local-variables ";;" "make -f")
    (and ivan-autoinsert-bottom-comment
         (progn
           (insert "\n\n")
           (ivan-autoinsert-insert ";;" ivan-autoinsert-bottom-comment)))
    (insert "\n")))

(defun ivan-autoinsert-latex ()
  (insert "\\documentclass[12pt]{article}

\\usepackage{pslatex}

\\begin{document}

\\title{put title here}
\\author{Ivan Kanis}
\\maketitle

\\begin{center}
$ \044Id$ $
\\end{center}

\\end{document}
"))

(defun ivan-autoinsert-local-variables (comment command)
  (setq-default
   compile-command (concat command " "
                           (file-name-nondirectory buffer-file-name)))
  (ivan-autoinsert-insert comment (concat
                                   " \114ocal Variables:
 compile-command: \"" compile-command "\"
 End:")))

(defun ivan-autoinsert-replace (regexp newtext string)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If it does not match, nil is returned instead of the new string.
I took it from `dired'."
  (let ((start 0) ret)
    (while (string-match regexp string start)
      (let ((from-end (- (length string) (match-end 0))))
        (setq ret (setq string (replace-match newtext t t string)))
        (setq start (- (length string) from-end))))
    ret))

(defun ivan-autoinsert-insert
  (comment string &optional comment-start comment-end)
  "Insert STRING with each line preceded by a COMMENT
Optional COMMENT-START and COMMENT-END handle C style comment"
  (and string
       (insert (ivan-autoinsert-replace "^" comment string))))

(setq
 auto-insert t
 auto-insert-alist
 '((("letter.*\\.tex\\'" "Style for LaTeX class lettre")
    nil
    '(and (not ivan-var-letter-english)
          (ivan-french-input))
    '(setq v2 (yes-or-no-p "Is there one signature? "))
    "% " (current-time-string)
    "\n\\documentclass[12pt,origdate]{lettre}

\\usepackage{ucs}
\\usepackage[utf8x]{inputenc}\n"
    (and (not ivan-var-letter-english) "\\usepackage[french]{babel}
\\usepackage[OT1]{fontenc}\n")
    "\\usepackage{pslatex}

\\begin{document}
\\institut{"
    (substring (read-file-name
                "Institut: "
                (concat "~/texmf/tex/latex/lettre/"
                        (if ivan-var-letter-english "en" "fr") "-"
                        (and (not v2) "li.ins")) nil t)
               25 -4)
    "}

\\begin{letter}{\n"
    ((read-from-minibuffer "Enter address: "
                           nil nil nil nil nil t) str "\\\\\n")
    "}
\\pagestyle{plain}
\\marge{0mm}\n"
    (let ((subject (read-from-minibuffer "Subject: "
                                         nil nil nil nil nil t)))
      (if (string= subject "")
          nil
        (concat "\\conc{" subject "}\n")))

    "\\opening{" (if ivan-var-letter-english "Dear Sir or Madam," "Madame, Monsieur,") "}\n\n"
    _
    "\n\n\\closing{"
    (if ivan-var-letter-english "Yours sincerely"
      (concat "Veuillez agréer, Madame, Monsieur, l'expression de "
              (if v2 "mes" "nos")
              "\n  salutations distinguées."))
    "}

\\end{letter}
\\end{document}"
    ;; fix obscur bug where file is not saved in unicode
    '(set-buffer-file-coding-system 'utf-8))

   (cperl-mode .  ivan-autoinsert-perl)
   (sh-mode . ivan-autoinsert-shell)
   (c-mode . ivan-autoinsert-c)
   (python-mode . ivan-autoinsert-python)
   ("[Mm]akefile\\'" . ivan-autoinsert-makefile)
   (emacs-lisp-mode . ivan-autoinsert-elisp)
   (latex-mode . ivan-autoinsert-latex))
 auto-insert-query nil)

;; Copyright (C) 2007 Ivan Kanis
;; Author: Ivan Kanis
;; $Id$
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

