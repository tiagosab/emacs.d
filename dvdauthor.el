;;; dvdauthor-mode.el --- major mode for editing dvdauthor xml files

;; Copyright (C) 2009  Tiago Saboga

;; Author: Tiago Saboga <tiagosaboga@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(define-skeleton dvda-minimal-skeleton
  "Insert a minimal skeleton for a dvdauthor file."
  nil
  "<dvdauthor>" \n
  "<vmgm>" \n
  "</vmgm>" \n
  "<titleset>" \n
  "<titles>" \n
  "<pgc>" \n
  "<vob file=\"" - "\" />" \n
  "</pgc>" \n
  "</titles>" \n
  "</titleset>" \n
  "</dvdauthor>" \n
  )

(defvar dvda-minimal-skeleton
"<dvdauthor>
 <vmgm>
 </vmgm>
 <titleset
   <titles>
     <pgc>
       <vob file= />
     </pgc>
   </titles>
 </titleset>
</dvdauthor>
"
)

(add-to-list 'auto-mode-alist '("\\.dvdauthor\\'" . dvdauthor-mode))

(defun run-dvdauthor ()
  (interactive)
  (save-buffer)
  (let (xml-file)
    (string-match "[^/]$" buffer-file-name)
    (setq xml-file (match-string 0 buffer-file-name))
    (let ((dvdauthor (generate-new-buffer "dvdauthor")))
      (pop-to-buffer dvdauthor)
      (shell-command (format "dvdauthor -x %s &" xml-file) t))))

(defun dvda-find-mpg-files ()
  (let ((mybuffer (get-buffer-create " *dvdauthor-mode*")))
    (save-excursion
      (set-buffer mybuffer)        
      (erase-buffer)
      (shell-command (concat "find" " ." " -iname" " \\*mpg") t)
      (let (files)
        (beginning-of-buffer)
        (while (re-search-forward ".*mpg" nil t)
          (setq files (cons (match-string 0) files)))
        (kill-buffer mybuffer)
        files))))

(defun dvda-insert-files-as-vob-file-entries ()
  (interactive)
  (let ((files (dvda-find-mpg-files)))
    (dolist (file files)
      (insert (format "<vob file=\"%s\" />\n" file)))))

(defun dvda-insert-minimal-skeleton ()
  (interactive)
  (insert dvda-minimal-skeleton)
  (re-search-backward "\"\"")
  (forward-char 1))

(define-derived-mode dvdauthor-mode xml-mode "dvda"
  "Major mode to edit dvdauthor xml files."
  ())

(provide 'dvdauthor-mode)
;;; teste.el ends here

