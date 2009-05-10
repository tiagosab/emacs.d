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

;; Needs derobeur.py version 0.2

;;; Robert
(require 'button)

(defgroup robert nil "Dictionnaire - Le Grand Robert"
  :group 'applications)

(defcustom derobeur-command "derobeur.py"
  "The command to use to run derobeur."
  :type '(string)
  :group 'robert)

(defcustom rob-switch-to-buffer 'pop-to-buffer
  "The function to use to switch to tresor buffer"
  :type 'function
  :group 'robert)

(defcustom robert-entry-face 'font-lock-function-name-face
  "The face to use to highlight the current entry <s>"
  :type 'face
  :group 'robert)

(defcustom robert-meaning-face 'font-lock-constant-face
  "The face to use to highlight the current meaning <e>"
  :type 'face
  :group 'robert)

(defcustom robert-definition-face 'font-lock-keyword-face
  "The face to use to highlight definitions <d>"
  :type 'face
  :group 'robert)

(defcustom robert-citation-face 'font-lock-builtin-face
  "The face to use to highlight <c>"
  :type 'face
  :group 'robert)

(defcustom robert-author-face 'font-latex-sectioning-2-face
  "The face to use to highlight <a>"
  :type 'face
  :group 'robert)

(defcustom robert-x2-face 'font-lock-comment-face
  "The face to use to highlight <x2>"
  :type 'face
  :group 'robert)

(defcustom robert-x3-face 'font-lock-doc-face
  "The face to use to highlight <x3>"
  :type 'face
  :group 'robert)

(defcustom robert-x4-face 'font-lock-function-name-face
  "The face to use to highlight <x4>"
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

(defvar robert-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map 
      (kbd "RET") 'robert)
    (define-key map 
      (kbd "e") 'rob-toggle-exemples)
    (define-key map
      (kbd "d") 'rob-toggle-definitions)
    (define-key map
      (kbd "s") 'rob-toggle-syntagmes)
    (define-key map
      (kbd "o") 'rob-toggle-others)
    (define-key map
      (kbd "t") 'rob-all-visible)
    (define-key map
      (kbd "m") 'rob-toggle-minimal)
    (define-key map
      (kbd "q") 'bury-buffer)
    (define-key map
      (kbd "h") 'describe-mode)
    (define-key map
      (kbd "?") 'describe-mode)
;    (define-key map
    map))

(let ((hard-newline (propertize "\n" 'hard t)))
  (setq robert-contents-categories-faces
;  (defvar robert-contents-categories-faces
    (list (list "<s>" robert-entry-face)
          (list "<e>" robert-meaning-face); hard-newline)
          (list "<d>"); robert-definition-face)
          (list "<c>" 'rob-parse-citation); "      "); (concat hard-newline "      "))
          (list "<a>" 'rob-parse-citation-author); "        "); robert-author-face "        ") 
                                        ;(concat hard-newline "        ") hard-newline)
          (list "<x2>" robert-x2-face)
          (list "<x3>" robert-x3-face)
          (list "<x4>" robert-x4-face)
          (list "<b>" robert-bold-face)
          (list "<i>" robert-italic-face)
          (list "<n>" nil "   ")
          )
;    "Please document me."
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
    (remove-hook 'text-mode-hook 'turn-on-auto-fill t)
    (beginning-of-buffer)
    (rob-parse-subtree robert-contents-categories-faces)
;    (make-local-variable 'fill-nobreak-invisible)
;    (setq fill-nobreak-invisible t)
    (set (make-local-variable 'fill-nobreak-predicate) 
         (cons 'fill-french-nobreak-p fill-nobreak-predicate))
    (set (make-local-variable 'paragraph-start)
    ;;     "Never-matching regexp.")
         " ") ; I do not understand why I need this space here.
;;    (set (make-local-variable 'paragraph-separate)
;;         "--- This regexp must never match ---")
;    (use-hard-newlines t)
    (set (make-local-variable 'adaptive-fill-mode) t)
    ;(fill-region (point-min) (point-max))
    (fill-individual-paragraphs (point-min) (point-max))
    (toggle-read-only t)
    )
  (beginning-of-buffer)
)

(defun rob-parse-citation(beg end cat)
  (save-excursion
    (let ((end (or (let ((end-marker (make-marker)))
                     (re-search-forward "<a>" nil t)
                     (set-marker end-marker (match-beginning 0)))
                   end)))
      (goto-char beg)
      (if (looking-back "[\n \t]*")
          (delete-region (match-beginning 0) (match-end 0)))
      (insert "\n      ")
      (while (re-search-forward "\n[ \t]*" end t)
        (replace-match "\n      "))
      (let ((overlay (make-overlay beg end)))
        (overlay-put overlay 'face robert-citation-face)
        (overlay-put overlay 'cat cat)
        ))))

(defun rob-parse-citation-author (beg end cat)
  (save-excursion
    (goto-char beg)
    (if (looking-back "[\n \t]*")
        (delete-region (match-beginning 0) (match-end 0)))
    (insert "        ")
    (let* ((end (re-search-forward "\n"))
           (overlay (make-overlay beg end)))
      (overlay-put overlay 'face robert-author-face)
      (overlay-put overlay 'cat cat)
      )))


(defun rob-delete-region (beg end &rest args)
  (delete-region beg end))

(defun rob-delete-until (regexp)
  (let ((beg (point)))
    (if (search-forward-regexp regexp)
        (progn
          (goto-char (match-beginning 0))
          (delete-region beg (point))
          ))
    ))

(defun rob-only-parse ()
  (interactive)
  (beginning-of-buffer)
  (rob-parse-subtree robert-contents-categories-faces))

(defun rob-parse-subtree (categories-faces)
  (let (subitems) ; list of (beginning tag-open (category-face)) lists
    (while
        (search-forward-regexp "<\\(/\\)?.*?>"
                               nil ; not bound
                               t ; if fail, move to limit (no error)
                               )
      (let ((data (match-string 0)))
        (delete-region (match-beginning 0) (match-end 0))
        (let ((current (car subitems)))
          (let ((beg (car current))
                (tag-open (or (car (cdr current))
                              ""))
                (cat-face (car (cdr (cdr current)))))
            (when (car cat-face)
              (if (functionp (nth 1 cat-face))
                  (funcall (nth 1 cat-face) beg (point-marker) tag-open)
                (let ((overlay (make-overlay beg (point))))
                  (overlay-put overlay 'face (nth 1 cat-face))
                  (overlay-put overlay 'cat (nth 0 cat-face))
                  ))
              (when (nth 3 cat-face)
                (insert (nth 3 cat-face))))
            (setq subitems (cdr subitems))
            (if (not (match-string 1))
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
              (message (match-string 1))
              )
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
    (let ((inhibit-read-only t))
      (auto-fill-mode nil)
      (erase-buffer)
      (kill-all-local-variables)
      (call-process derobeur-command nil t nil word)
      (funcall rob-switch-to-buffer tr-buffer))
;;))
    (robert-mode)))

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
  (rob-toggle-by-type '("<e>" "<a>")))

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
                     (car types)) ; We're looking only at the first
                                  ; element of the list.
                                  ; TODO: correct this, here and on
                                  ; tresor.el
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

