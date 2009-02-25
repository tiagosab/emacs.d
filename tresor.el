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

;;; Trésor
(require 'url)
(require 'button)
(require 'mime)

(defgroup tresor nil "Dictionnaire Trésor de la Langue Française"
  :group 'dict)

(defcustom tresor-entry-face 'font-lock-function-name-face
  "The face to use to highlight the current entry"
  :type 'face
  :group 'tresor)

(defcustom tresor-example-face 'font-lock-comment-face
  "The face to use to highlight examples"
  :type 'face
  :group 'tresor)

(defcustom tresor-bold-face 'font-lock-variable-name-face
  "The face to use to highlight bold text (mainly section letters and numbers"
  :type 'face
  :group 'tresor)

(defcustom tresor-italic-face (list :slant 'italic)
  "The face to use to highlight italic text"
  :type 'face
  :group 'tresor)

(defcustom tresor-author-face (list :weight 'ultra-bold)
  "The face to use to highlight example's authors names"
  :type 'face
  :group 'tresor)

(defcustom tresor-sup-face ()
  "The face to use to highlight example's authors names"
  :type 'face
  :group 'tresor)

(defcustom tresor-definition-face 'font-lock-keyword-face
  "The face to use to highlight definitions"
  :type 'face
  :group 'tresor)

(defvar tresor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map 
      (kbd "RET") 'tresor)
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
  (defvar tresor-vtoolbar-categories-faces
    (list (list "vitemselected" 'trs-delete-region)
          (list "<a href" 'trs-make-button nil hard-newline)
          (list "<sup>" tresor-sup-face " [" "]")
          ))
  
  (defvar tresor-contents-categories-faces
    (list (list "tlf_cexemple" tresor-example-face "   ")
          (list "tlf_cmot" tresor-entry-face)
          (list "tlf_cdefinition" tresor-definition-face)
          (list "<b>" tresor-bold-face)
          (list "<i>" tresor-italic-face)
          (list "<sup>" tresor-sup-face " [" "]")
          (list "tlf_cauteur" tresor-author-face "   ")
          (list "tlf_parah" nil hard-newline)
          (list "tlf_tabulation" nil hard-newline)
          (list "tlf_parsynt" nil (concat hard-newline "      "))
          (list "tlf_paraputir" nil hard-newline)
          (list "tlf_parothers" nil hard-newline) ; remarques, éthymologie, bibliographie, statistiques
          (list "tlf_contentbox" nil hard-newline)
          (list "tlf_csyntagme")
          )
    "Missing:
- tlf_cdomaine
- tlf_cemploi
- tlf_ccrochet - texte entre [crochets]
- tlf_smallcaps - noms d'auteur, personnages de théâtre
- tlf_cvedette - mot?
- tlf_ccode - catégorie grammaticale
- tlf_ctitre - du livre de l'exemple
- tlf_cdate - date du livre de l'exemple"
    )
  )

(define-derived-mode tresor-mode text-mode "Trésor"
  "Major mode for reading entries from the 'Trésor de la Langue
Française' dictionary.

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
    (trs-make-invisible-parts)
    (make-local-variable 'fill-nobreak-invisible)
    (setq fill-nobreak-invisible t)
    (set (make-local-variable 'fill-nobreak-predicate) (cons 'fill-french-nobreak-p fill-nobreak-predicate))
    (use-hard-newlines t)
    (fill-region (point-min) (point-max))
    (toggle-read-only t)
    )
  (beginning-of-buffer)
)

(defun trs-delete-region (beg end &rest args)
  (delete-region beg end))

(defun trs-make-button (beg end tag-open)
  (string-match "\/definition\/[^']+" tag-open)
  (make-button beg end 'action 'tresor-open 'link (match-string 0 tag-open)))

(defun tresor-open (button)
  (let ((link (button-get button 'link)))
    (url-retrieve (concat "http://www.cnrtl.fr" link) 'trs-process-page )))

(defun trs-delete-until (regexp)
  (let ((beg (point)))
    (if (search-forward-regexp regexp)
        (progn
          (goto-char (match-beginning 0))
          (delete-region beg (point))
          ))
    ))

(defun trs-make-invisible-parts ()
  (trs-delete-until "<div id=\"vtoolbar.*?>")
  (trs-parse-subtree tresor-vtoolbar-categories-faces)
  (trs-delete-until "<div id=\"contentbox.*?>")
  (trs-parse-subtree tresor-contents-categories-faces)
  (trs-delete-until "<div id=\"footer\">")
  (trs-parse-subtree nil)
  (delete-region (point) (point-max))
)

(defun trs-parse-subtree (categories-faces)
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

(defun tresor-test ()
  (interactive)
  (let ((buffer (get-buffer-create " trs-tarte")))
    (set-buffer buffer))
  (end-of-buffer)
  (if (eq 1 (point))
      (url-retrieve (format "http://www.cnrtl.fr/definition/%s" "tarte") 'trs-preprocess-page)
    (trs-process-page)
    ))

(defsubst trs-default-word-entry ()
  "Make a guess at a default entry.
This guess is based on the text surrounding the cursor."
  (let ((word (or (current-word)
                  "")))
    (if (string-match "[._]+$" word)
        (substring word 0 (match-beginning 0))
      word)))

(defun trs-process-page ( &rest args )
  (let ((buffer (current-buffer))
        (tr-buffer (get-buffer-create "*Trésor*"))
        )
    (set-buffer tr-buffer)
    (let ((inhibit-read-only t))
      (fundamental-mode)
      (erase-buffer)
      (let ((mimemsg (mime-parse-buffer buffer)))
        (mime-insert-entity-body mimemsg)
        (let (charset
              (params (mime-entity-parameters mimemsg)))
          (while (and (not charset)
                      params)
            (let ((param (car params)))
              (if (equal (car param)
                         "charset")
                  (setq charset (cdr param))
                (setq params (cdr params)))))
          (decode-coding-region (point-min) 
                                (point-max) 
                                (coding-system-from-name charset))
          )))
    (pop-to-buffer tr-buffer)
    (tresor-mode)))

(defun tresor (word)
  "Fetch the page from Trésor"
  (interactive (list (let* ((default-entry (trs-default-word-entry))
	     (input (read-string
		     (format "Mot à rechercher%s: "
			     (if (string= default-entry "")
				 ""
			       (format " (défaut %s)" default-entry))))))
	(if (string= input "")
	    (if (string= default-entry "")
		(error "No dict args given") default-entry) input))))
  (url-retrieve (format "http://www.cnrtl.fr/definition/%s" word) 'trs-process-page ))

(defun trs-preprocess-page( &rest args )
  (let ((buffer (current-buffer)))
    (set-buffer " trs-tarte")
    (insert-buffer-substring buffer)
    (beginning-of-buffer)
    (trs-process-page))
  )

(defun trs-toggle-exemples ()
  (interactive)
  (trs-toggle-by-type '("tlf_cexemple")))

(defun trs-toggle-definitions ()
  (interactive)
  (trs-toggle-by-type '("tlf_cdefinition")))

(defun trs-toggle-syntagmes ()
  (interactive)
  (trs-toggle-by-type '("tlf_parsynt" "tlf_csyntagme")))

(defun trs-toggle-others ()
  "Toggle visibility of paragraphs marked as others (mainly
remarques, bibliographie, statistiques, étymologie)."
  (interactive)
  (trs-toggle-by-type '("tlf_parothers")))

(defun trs-toggle-minimal ()
  "Toggle visibility of every buffer but definitions and headers"
  (interactive)
  (trs-toggle-by-type '("tlf_cexemple"
                        "tlf_cauteur"
                        "tlf_parsynt"
                        "tlf_parothers"
                        "tlf_csyntagme")))

(defun trs-all-visible ()
  "View all parts of definition"
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max))))
    (while overlays
      (overlay-put (car overlays) 'invisible nil)
      (setq overlays (cdr overlays))))
  (save-excursion
    (let ((inhibit-read-only t))
      (fill-region (point-min) (point-max)))))


(defun trs-toggle-by-type ( types &optional invisible )
  "Toggle invisible property of all overlays of type types. types
should be a list of strings, which should each be present in the
first column of some trs-*-categories-faces variable. The second
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

