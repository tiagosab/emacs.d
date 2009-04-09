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

;;; Robert
(require 'url)
(require 'button)

(if (< emacs-major-version 23)
    (require 'coding-system-from-name))

(defgroup robert nil "Dictionnaire - Le Grand Robert"
  :group 'applications)

(defcustom robert-entry-face 'font-lock-function-name-face
  "The face to use to highlight the current entry"
  :type 'face
  :group 'robert)

(defcustom robert-example-face 'font-lock-comment-face
  "The face to use to highlight examples"
  :type 'face
  :group 'robert)

(defcustom robert-bold-face 'font-lock-variable-name-face
  "The face to use to highlight bold text (mainly section letters and numbers"
  :type 'face
  :group 'robert)

(defcustom robert-italic-face (list :slant 'italic)
  "The face to use to highlight italic text"
  :type 'face
  :group 'robert)

(defcustom robert-author-face (list :weight 'ultra-bold)
  "The face to use to highlight example's authors names"
  :type 'face
  :group 'robert)

(defcustom robert-sup-face ()
  "The face to use to highlight example's authors names"
  :type 'face
  :group 'robert)

(defcustom robert-definition-face 'font-lock-keyword-face
  "The face to use to highlight definitions"
  :type 'face
  :group 'robert)

(defvar robert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map 
      (kbd "RET") 'robert)
    (define-key map 
      (kbd "e") 'trs-toggle-exemples)
    (define-key map
      (kbd "d") 'trs-toggle-definitions)
    (define-key map
      (kbd "s") 'trs-toggle-syntagmes)
    (define-key map
      (kbd "o") 'trs-toggle-others)
    (define-key map
      (kbd "t") 'trs-all-visible)
    (define-key map
      (kbd "m") 'trs-toggle-minimal)
    (define-key map
      (kbd "q") 'bury-buffer)
    (define-key map
      (kbd "h") 'describe-mode)
    (define-key map
      (kbd "?") 'describe-mode)
;    (define-key map
    map))

(let ((hard-newline (propertize "\n" 'hard t)))
  (defvar robert-vtoolbar-categories-faces
    (list (list "vitemselected" 'rob-delete-region)
          (list "<a href" 'rob-make-button nil hard-newline)
          (list "<sup>" robert-sup-face " [" "]")
          ))
  
  (defvar robert-contents-categories-faces
    (list (list "<s>" robert-definition-face "   ")
          (list "<e>" robert-entry-face)
          (list "<x2>" robert-definition-face)
          (list "<x3>" robert-definition-face)
          (list "<x4>" robert-definition-face)
          (list "<b>" robert-bold-face)
          (list "<i>" robert-italic-face)
          (list "<d>" robert-sup-face " [" "]")
          (list "<n>" robert-author-face "   ")
          (list "<c>" nil hard-newline)
          (list "tlf_tabulation" nil hard-newline)
          (list "tlf_parsynt" nil (concat hard-newline "      "))
          (list "tlf_paraputir" nil hard-newline)
          (list "tlf_parothers" nil hard-newline) ; remarques, éthymologie, bibliographie, statistiques
          (list "tlf_contentbox" nil hard-newline)
          (list "tlf_csyntagme")
          )
    "Please document me."
    )
  (defvar robert-footer-categories-faces
    (list (list "footer" nil hard-newline)))
  )

(define-derived-mode robert-mode text-mode "Robert"
  "Major mode for reading entries from the 'Grand Robert' dictionary.

Keys:
e - toggle visibility of examples
d - toggle visibility of definitions
s - toggle visibility of syntagmes
o - toggle visibility of other (bibliographie, etymologie, 
                               remarques, statistiques)
m - toggle visibility of less important sections
t - tout visible

q - quit (bury buffer)
RET - search the trésor for the selected word
h / ? - display this help
"
  (let ((inhibit-read-only t))
    (beginning-of-buffer)
    (rob-make-invisible-parts)
    (make-local-variable 'fill-nobreak-invisible)
    (setq fill-nobreak-invisible t)
    (set (make-local-variable 'fill-nobreak-predicate) (cons 'fill-french-nobreak-p fill-nobreak-predicate))
    (use-hard-newlines t)
    (fill-region (point-min) (point-max))
    (toggle-read-only t)
    )
  (beginning-of-buffer)
)

(defun rob-delete-region (beg end &rest args)
  (delete-region beg end))

(defun rob-make-button (beg end tag-open)
  (string-match "\/definition\/[^']+" tag-open)
  (make-button beg end 'action 'robert-open 'link (match-string 0 tag-open)))

(defun robert-open (button)
  (let ((link (button-get button 'link)))
    (url-retrieve (concat "http://www.cnrtl.fr" link) 'rob-process-page )))

(defun rob-delete-until (regexp)
  (let ((beg (point)))
    (if (search-forward-regexp regexp)
        (progn
          (goto-char (match-beginning 0))
          (delete-region beg (point))
          ))
    ))

(defun rob-make-invisible-parts ()
  (rob-delete-until "<div id=\"vtoolbar.*?>")
  (rob-parse-subtree robert-vtoolbar-categories-faces)
  (rob-delete-until "<div id=\"contentbox.*?>")
  (rob-parse-subtree robert-contents-categories-faces)
  (rob-delete-until "<div id=\"footer\">")
  (rob-parse-subtree robert-footer-categories-faces)
  (delete-region (point) (point-max))
)

(defun rob-parse-subtree (categories-faces)
  (let (subitems ; list of (beginning tag-open (category-face)) lists
        (working nil))
    (while (and
            (or
             (not working)
             (> (length subitems) 0))
            (search-forward-regexp "<\\(/\\)?.*?>"
                                  nil ; not bound
                                  t ; if fail, just return nil (no error)
                                  ))
      (setq working t)
      (let ((data (match-string 0)))
        (delete-region (match-beginning 0) (match-end 0))
        (if (match-string 1)
            (let ((current (car subitems)))
              (let ((beg (car current))
                    (tag-open (car (cdr current)))
                    (cat-face (car (cdr (cdr current)))))
                (when (car cat-face)
                  (if (functionp (nth 1 cat-face))
                      (funcall (nth 1 cat-face) beg (point) tag-open)
                    (let ((overlay (make-overlay beg (point))))
                      (overlay-put overlay 'face (nth 1 cat-face))
                      (overlay-put overlay 'cat (nth 0 cat-face))
                      )))
                      ;(overlay-put overlay 'before-string (nth 2 cat-face)) ; using overlays here prevents normal movement
                      ;(overlay-put overlay 'after-string (nth 3 cat-face)) ;  and does not change filling
                (if (nth 3 cat-face)
                    (insert (nth 3 cat-face)))
                (setq subitems (cdr subitems))
                ))
          (if (not (string-match "/>" data))
              (let ((item)
                    (counter 0))
                (while (not item)
                  (let ((cat-face (nth counter categories-faces)))
                    (if (nth 0 cat-face)
                        (if (string-match (nth 0 cat-face) data)
                            (setq item cat-face)
                          (setq counter (+ 1 counter)))
                      (setq item '(nil nil)))))
                (if (nth 2 item)
                    (insert (nth 2 item)))
                (setq subitems (cons (list (point-marker) data item) subitems))
                )
            (if (string-match "<br/>" data)
                (insert "\n"))
            ))))))

(defun robert-test ()
  (interactive)
  (let ((buffer (get-buffer-create " rob-tarte")))
    (set-buffer buffer))
  (end-of-buffer)
  (if (eq 1 (point))
      (url-retrieve (format "http://www.cnrtl.fr/definition/%s" "tarte") 'rob-preprocess-page)
    (rob-process-page)
    ))

(defsubst rob-default-word-entry ()
  "Make a guess at a default entry.
This guess is based on the text surrounding the cursor."
  (let ((word (or (current-word)
                  "")))
    (if (string-match "[._]+$" word)
        (substring word 0 (match-beginning 0))
      word)))

(defun rob-get-charset-from-url-buffer ( buffer )
  (save-excursion
    (set-buffer buffer)
    (beginning-of-buffer)
    (re-search-forward "^Content-Type: text/html; charset=\\(.*\\)$")
    (match-string 1)))

(defun rob-get-definition ( &rest args )
  (let ((buffer (current-buffer))
        (tr-buffer (get-buffer-create "*Robert*"))
        )
    (set-buffer tr-buffer)
    (erase-buffer)
    (call-process "derobeur.py" nil t nil word)
    (pop-to-buffer tr-buffer)))
;    (robert-mode)))

(defun robert (word)
  "Fetch the page from Robert"
  (interactive (list (let* ((default-entry (rob-default-word-entry))
	     (input (read-string
		     (format "Mot à rechercher%s: "
			     (if (string= default-entry "")
                     ""
			       (format " (défaut %s)" default-entry))))))
	(if (string= input "")
	    (if (string= default-entry "")
		(error "No dict args given") default-entry) input))))
  (rob-get-definition))
  
;  (url-retrieve (format "http://www.cnrtl.fr/definition/%s" word) 'rob-process-page ))

(defun rob-preprocess-page( &rest args )
  (let ((buffer (current-buffer)))
    (set-buffer " rob-tarte")
    (insert-buffer-substring buffer)
    (beginning-of-buffer)
    (rob-process-page))
  )

(defun rob-toggle-exemples ()
  (interactive)
  (rob-toggle-by-type '("tlf_cexemple")))

(defun rob-toggle-definitions ()
  (interactive)
  (rob-toggle-by-type '("tlf_cdefinition")))

(defun rob-toggle-syntagmes ()
  (interactive)
  (rob-toggle-by-type '("tlf_parsynt" "tlf_csyntagme")))

(defun rob-toggle-others ()
  "Toggle visibility of paragraphs marked as others (mainly
remarques, bibliographie, statistiques, étymologie)."
  (interactive)
  (rob-toggle-by-type '("tlf_parothers")))

(defun rob-toggle-minimal ()
  "Toggle visibility of every part but definitions and headers"
  (interactive)
  (rob-toggle-by-type '("tlf_cexemple"
                        "tlf_cauteur"
                        "tlf_parsynt"
                        "tlf_parothers")))

(defun rob-all-visible ()
  "View all parts of definition"
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max))))
    (while overlays
      (overlay-put (car overlays) 'invisible nil)
      (setq overlays (cdr overlays))))
  (save-excursion
    (let ((inhibit-read-only t))
      (fill-region (point-min) (point-max)))))


(defun rob-toggle-by-type ( types &optional invisible )
  "Toggle invisible property of all overlays of type types. types
should be a list of strings, which should each be present in the
first column of some rob-*-categories-faces variable. The second
argument, invisible, causes the function to explicitly set the
state instead of toggling it. If it is positive, text will be
visible; else, text will be invisible."
  (while types
    (let ((overlays (overlays-in (point-min) (point-max))))
      (while overlays
        (let (value
              (overlay (car overlays)))
          (if (equal (overlay-get overlay 'cat)
                     (car types))
              (progn
                (if invisible
                    (if (> 0 invisible)
                        (setq value nil)
                      (setq value t))
                  (setq value (not (overlay-get overlay 'invisible))))
                (overlay-put overlay 'invisible (not (overlay-get overlay 'invisible))))
            )
          (setq overlays (cdr overlays))
          )))
    (setq types (cdr types))
    )
  (let ((inhibit-read-only t))
    (save-excursion
      (fill-region (point-min) (point-max))))
  )

