(provide 'tiago)

(defun ts-remove-line-breaks (from to)
  (interactive r)
)

(defun ts-alunos (aluno)
  (interactive "sAluno: ")
  (find-file (concat "/home/tiago/home/aulasfran/alunos/" aluno "/Notes.muse"))
  (end-of-buffer)
  (insert "\n")
  (ts-insert-date-string)
)

(defun ts-properties ()
  (interactive)
  (message "%s" (get-char-property-and-overlay (point) 'face)))

;; Insert date string
(defun ts-insert-date-string ()
  "Insert a nicely formated date string."
   (interactive)
   (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

(defun ts-insert-gpl-boilerplate ()
  (interactive)
  (save-excursion
    (let ((pt (point)))
      (insert " Copyright (C) ")
      (insert (format-time-string "%Y"))
      (insert (format " %s <%s>" user-full-name user-mail-address))
      (insert "
 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

")
      (comment-region pt (point)))))


(defun ts-mrcopyright-helper ()
  (interactive)
  (let ((current (current-buffer))
        (buffer (get-buffer-create "* copyright-helper")))
    (while (re-search-forward "^.*NC\n")
      (let ((str (match-string 0)))
        (save-excursion
          (set-buffer buffer)
          (insert str))))))

(defun ts-a ()
  (interactive)
  (re-search-forward "^.*NC\n")
  (message "%s" (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun ts-find-file-from-current-line (filename &optional wildcards)
  "Edit file FILENAME, in another window. Defaults to filename at point.

Like \\[find-file] (which see), but creates a new window or
reuses an existing one.  See the function `display-buffer'.

Interactively, the default if you just type RET is the filename
at point in the current directory, but the visited file name is
available through the minibuffer history: type M-n to pull it
into the minibuffer.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive
   (ts-find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
	(mapcar 'switch-to-buffer (nreverse value))
      (switch-to-buffer value))))

(defun ts-find-file-read-args (prompt mustmatch)
  (list (let ((find-file-default (ts-get-filename-at-point)))
          (minibuffer-with-setup-hook
              (lambda () (setq minibuffer-default find-file-default))
            (read-file-name prompt default-directory nil mustmatch find-file-default)))
        t))

(defun ts-insert-TC ()
  (interactive)
  (end-of-line)
  (backward-char 3)
  (if (not (looking-at " - "))
      (progn 
        (end-of-line)
        (insert " - "))
    (end-of-line))
  (insert "NC")
  (forward-char 1))
  
(defun ts-get-filename ()
  (interactive)
  (message (ts-get-filename-at-point)))

(defun ts-get-filename-at-point ()
  (or 
   (save-excursion
     (re-search-backward "\\s-" nil 1)
     (forward-char 1)
     (let ((beg (point)))
       (re-search-forward "\\(\\s-\\|$\\)" nil nil)
       (let ((end (point)))
         (buffer-substring-no-properties beg end))))
   ""
   ))

(defun ts-cur ()
  (interactive)
  (message (ts-get-filename-at-point)))

(defun ts-point()
  (interactive)
  (message "%s" (point)))

(defun ts-logme (txt)
  (save-excursion
    (set-buffer (get-buffer-create "*tiago-log*"))
    (end-of-buffer)
    (insert (format "%s\n" txt))))
