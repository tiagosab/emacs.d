 ;; Copyright (C) 2009 Tiago Saboga <tiagosaboga@gmail.com>
 ;; This program is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation; either version 2 of the License, or
 ;; (at your option) any later version.

 ;; This program is distributed in the hope that it will be useful,
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;; GNU General Public License for more details.

 ;; You should have received a copy of the GNU General Public License
 ;; along with this program; if not, write to the Free Software
 ;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; alunos

(require 'message)
(require 'tiago)

(defgroup alunos nil "Alunos"
  :group 'applications)

(defcustom alunos-date-face 'lazy-highlight
; font-latex-sectioning-2-face
  "The face to use to highlight dates"
  :type 'face
  :group 'alunos)

(defcustom alunos-header-name-face font-lock-variable-name-face
  "The face to use to highlight headers"
  :type 'face
  :group 'alunos)

(defcustom alunos-header-text-face font-lock-comment-delimiter-face
  "The face to use to highlight headers"
  :type 'face
  :group 'alunos)

(defcustom al-mail-subject "Dernier cours"
  "Subject of messages sent to students."
  :type 'string
  :group 'alunos)

(defcustom alunos-dir "$HOME/alunos"
  "Location of students directories."
  :type 'directory
  :group 'alunos)

(defcustom alunos-subdirs "EX"
  "Space-separated list of subdirs of alunos-dir that contain
alunos"
  :type 'string
  :group 'alunos)

(defvar al-tempbuffer nil
  "Internal variable")

(defvar alunos-mode-map
  (let ((map (make-keymap)))
    (define-key map 
      (kbd "\C-c d") 'ts-insert-date-string)
    (define-key map
      (kbd "\C-c m") 'al-sendmail)
    map))

(defun al-font-lock-make-header-matcher (regexp)
  "Adapted from message-font-lock-make-header-matcher."
  (let ((form
         `(lambda (limit)
            (let ((start (point)))
              (goto-char (point-min))
              (if (re-search-forward "^[ \t\n]*$"
                                     nil t)
                  (setq limit (min limit (match-beginning 0))))
              (goto-char start)
              (and (< start limit)
                   (re-search-forward ,regexp limit t))))))
    (if (featurep 'bytecomp)
        (byte-compile form)
      form)))

(defvar alunos-font-lock-defaults
  (list `(
          ("^[ \t]*\\*\\{3\\}.*\\*\\{3\\}[ \t]*" . alunos-date-face)
          ,(let ((content "[ \t]*\\(.+\\(\n[ \t].*\\)*\\)\n?"))
             `(,(al-font-lock-make-header-matcher
                 (concat "^\\([A-Z][^: \n\t]+:\\)" content))
               (1 alunos-header-name-face)
               (2 alunos-header-text-face nil t)))
          )
        t ; keywords only
        ))

(defun alunos (aluno &optional readonly)
  "Open notes file of student ALUNO. If READONLY is true, don't
insert date and turn on view-mode.

Interactively, read ALUNO in minibuffer, completing for directory
entries in ALUNOS-DIR."
  (interactive (list (al-read-aluno) current-prefix-arg))
  (find-file (concat alunos-dir aluno "/Notes.muse"))
  (end-of-buffer)
  (when (null readonly)
    (if (looking-back "[ \t\n]*" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
    (insert "\n\n***")
    (ts-insert-date-string)
    (insert "***\n\n"))
  (rename-buffer aluno)
  (alunos-mode)
  (when readonly
    (view-mode t)))

(defun al-read-aluno ()
  (completing-read "Aluno: " (al-get-alunos-list)))

(defun al-get-alunos-list ()
  (let* ((ls-out (shell-command-to-string (concat "ls -1 " alunos-dir)))
         (ls (split-string ls-out "\n" t)))
    ls))

(define-derived-mode alunos-mode text-mode "Alunos"
;  "Major mode for taking notes about students."
  (set (make-local-variable 'mail-header-separator)
       "[ \t]*")
  (set (make-local-variable 'font-lock-defaults)
        alunos-font-lock-defaults)
)

(defun al-sendmail (&optional beg end)
  (interactive)
  (let ((get-last-string (al-get-last-string-callback)))
    (add-hook 'message-mode-hook get-last-string)
    (compose-mail (al-students-addresses)
                  al-mail-subject ; subject
                  nil ; other-headers
                  nil ; continue
                  'pop-to-buffer ; switch-function
                  ; the next option would be yank-action, but
                  ; unfortunately it is not fully implemented in
                  ; message-mode
                  ; (list 'insert-buffer (al-get-buffer-with-last-class))
                  )
    (remove-hook 'message-mode-hook get-last-string)))

(defun al-get-last-string-callback ()
  (let (beg
        (end (point-max)))
    (save-excursion
      (end-of-buffer)
      (re-search-backward "^\\*\\{3\\}.*\\*\\{3\\}")
      (setq beg (point))
      (let ((curbuffer (current-buffer))
            (tempbuffer (get-buffer-create " *alunos-message-pasting")))
        (set-buffer tempbuffer)
        (erase-buffer)
        (insert-buffer-substring-no-properties curbuffer beg end)
        (setq al-tempbuffer tempbuffer)
        )
      (lambda ()
        (insert-buffer-substring-no-properties al-tempbuffer); beg (point-max))
        ))))

(defun al-students-addresses ()
  (save-excursion
    (beginning-of-buffer)
    (let (addresses)
      (while (not (looking-at "[ \t]*$"))
        (if (looking-at "[Cc]ourriel:[ \t]*\\(.*@.*\\)")
            (progn
              (setq addresses (cons (match-string 1) addresses)))
           )
        ;(next-line))
        (forward-line))
      (mapconcat 'identity addresses ", "))))
