;;; debian-mr-copyright-mode.el --- major mode for machine-readable debian/copyright files

;; Copyright (C) 2001, 2003 Free Software Foundation, Inc.
;; Copyright (C) 2003, 2004, 2005 Peter S Galbraith <psg@debian.org>
;; Copyright (C) 2008 Vincent Fourmond

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; debian-mr-copyright-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;; This mode is very loosely based on debian-control-mode.el
;;
;; ChangeLog:
;; (Vincent Fourmond 22/3/2008)
;;   * added a debian-mr-copyright-scan-files
;;     interactive function that displays the files covered by the
;;     copyright file (and in particular files *not* covered !!!)
;;   * special highlighting for X- fields

;;; Code:

(require 'font-lock)

(defgroup debian-mr-copyright nil "Machine-readable debian copyright files"
  :group 'tools)

(defcustom debian-mr-copyright-field-face 'font-lock-keyword-face
  "The face to use to highligh fields names"
  :type 'face
  :group 'debian-mr-copyright)

(defcustom debian-mr-copyright-value-face 'font-lock-variable-name-face
  "The face to use to highlight fields values."
  :type 'face
  :group 'debian-mr-copyright)

(defcustom debian-mr-copyright-files-face 'font-lock-type-face
  "The face to use to highlight file globs."
  :type 'face
  :group 'debian-mr-copyright)

(defcustom debian-mr-copyright-invalid-face 'font-lock-warning-face
  "The face to use to highlight invalid licenses."
  :type 'face
  :group 'debian-mr-copyright)

(defcustom debian-mr-copyright-string-face 'font-lock-string-face
  "The face to use for strings (such as full text licenses)"
  :type 'face
  :group 'debian-mr-copyright)


(defcustom debian-mr-copyright-uncovered-face 'font-lock-warning-face
  "The face to use for uncovered files"
  :type 'face
  :group 'debian-mr-copyright)

(defcustom debian-mr-copyright-custom-face 'font-lock-function-name-face
  "The face to use for custom fields"
  :type 'face
  :group 'debian-mr-copyright)




(defvar debian-mr-copyright-syntax-table nil
  "Syntax table used in debian-mr-copyright-mode buffers.")

(if debian-mr-copyright-syntax-table
    ()
  (setq debian-mr-copyright-syntax-table (make-syntax-table))
  ;; Support # style comments
  (modify-syntax-entry ?#  "<"  debian-mr-copyright-syntax-table)
  (modify-syntax-entry ?\n "> " debian-mr-copyright-syntax-table))

(defvar debian-mr-copyright-field-regexp "^\\(\\(\\sw\\|-\\)+:\\)")

;; (defvar debian-mr-copyright-value-regexp 
;;   "[-a-zA-Z0-9+.*,[:blank:]]+")

;; Valid licenses, extracted from the web page
;; http://wiki.debian.org/Proposals/CopyrightFormat
;; 15/03/08
(defvar debian-mr-copyright-valid-licenses
  '("GPL-any" "GPL-1" "GPL-1+" "GPL-2" "GPL-2+" "GPL-3" "GPL-3+"
    "LGPL-any" "LGPL-2" "LGPL-2+" "LGPL-2.1" "LGPL-2.1+" "LGPL-3" "LGPL-3+"
    "PSF" "PSF-2" "GFDL-any" "GFDL-1.2" "GFDL-1.2+" "GAP" "BSD-2" "BSD-3" 
    "BSD-4" "Apache-1.0" "Apache-1.1" "Apache-2.0" "MPL-1.1" "Artistic"
    "Artistic-2.0" "LPPL-1.3a" "ZPL" "ZPL-2.1" "EPL-1.1" "EFL-2" 
    "CC-BY-3" "ZLIB" "other")
  )

(defvar debian-mr-copyright-value-regexp  ".+")

(defvar debian-mr-copyright-imenu-expression 
  (concat "^\\(Files\\):\\(" 
	  debian-mr-copyright-value-regexp
	  "\\)$")
  "The regular expression for imenu elements" )

(defvar debian-mr-copyright-fields
  '("Copyright")
  "Valid fields names (but Files and License).")

(defvar debian-mr-copyright-fields-regexp
  (concat
   "^"
   (let ((max-specpdl-size 1000))
     (regexp-opt debian-mr-copyright-fields t))
   ":\\(" 
   debian-mr-copyright-value-regexp
   "\\)$" )
  "font-lock regexp matching known fields.")

(defvar debian-mr-copyright-license-regexp
  (concat
   "^\\(License\\):\\(?:[[:blank:]]*" 
   (let ((max-specpdl-size 1000))
     (regexp-opt debian-mr-copyright-valid-licenses t))
   "[[:blank:]]*\\|\\(.+\\)\\)$" )
  "font-lock regexp matching known or unkown licenses.")

;\\(\\(?:^[[:blank:]].*$\\)*\\)

(defvar debian-mr-copyright-files-regexp
  (concat 
   "^\\(Files\\):\\(" 
   debian-mr-copyright-value-regexp
   "\\)$" )
  "font-lock regexp matchin Files: fields.")




(defvar debian-mr-copyright-font-lock-keywords
  `(
    ("^[[:blank:]].*$" 0 debian-mr-copyright-string-face)
    ("^\\(X-[^[:blank:]]+\\):" 1 debian-mr-copyright-custom-face)
    (,debian-mr-copyright-fields-regexp
     (1 debian-mr-copyright-field-face)
     (2 debian-mr-copyright-string-face))
    (,debian-mr-copyright-files-regexp
     (1 debian-mr-copyright-field-face)
     (2 debian-mr-copyright-files-face))
    (,debian-mr-copyright-license-regexp
     (1 debian-mr-copyright-field-face)
     (2 debian-mr-copyright-value-face nil t) ;Not to fail if missing
     (3 debian-mr-copyright-invalid-face nil t)
     )
    )
  )


;;     (,debian-mr-copyright-license-regexp
;;      (1 debian-mr-copyright-field-face)
;;      (2 debian-mr-copyright-files-face))


(defvar debian-mr-copyright-mode-menu nil)

;;;###autoload

(define-derived-mode debian-mr-copyright-mode fundamental-mode "Debian Control"
  "A major mode for editing machine-readable Debian copyright files (i.e. debian/copyright)."
  (if (< emacs-major-version 21)
      (message "debian-mr-copyright-mode only supports emacsen version >= 21; disabling features")
    (progn
      (set-syntax-table debian-mr-copyright-syntax-table)
      ;; Comments
      (make-local-variable 'comment-start-skip)  ;Need this for font-lock...
      (setq comment-start-skip "\\(^\\|\\s-\\);?#+ *") ;;From perl-mode
      (make-local-variable 'comment-start)
      (make-local-variable 'comment-end)
      (setq comment-start "#"
            comment-end "")

      ;; Stuff for imenu
      (set (make-local-variable 'imenu-generic-expression)
	   (list (list nil debian-mr-copyright-imenu-expression 2)))
	   
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults 
            '(debian-mr-copyright-font-lock-keywords
              nil           ;;; Keywords only? No, let it do syntax via table.
              nil           ;;; case-fold?
              nil           ;;; Local syntax table.
              nil           ;;; Use `backward-paragraph' ? No
              ))
      )
    )
  )


;; (defun debian-mr-copyright-scan-files ()
;;   (interactive)
;;   "Scans all Files: fields starting from this file up to the end of the file
;; and displays filenames that match the current rule"
;;   (make-local-variable 'dmrc-process)
;;   (make-local-variable 'dmrc-process-buffer)
;;   (if (not (processp dmrc-process))
;;       (progn
;; 	(setq dmr-process 
;; 	      (start-process "bidule" "biniou" "sh" "-c"  "cd ..; find"))
;; 	(setq dmr-process-buffer (process-buffer dmr-process))
;; 	)
;;     )
  
;;   )


;; The following function parses a debian-mr-copyright file and displays
;; the results.
;;
;; It currently cannot deal with multiple lines in the Files: field.
(defun debian-mr-copyright-scan-buffer ()
  (interactive)
  "Scans the buffer for Files: and License: statements, and returns
a list of lists:

  (FILE-GLOB FILE-LICENSE POINT)
  FILE-GLOB is the list of globs
  FILE-LICENSE is the short text of the license
  POINT the position of the last bit of the Files: line
"
  (let ((globs-list '()) (current-license) (current-files) (current-point))
    (save-excursion 
      (goto-char (point-min))
      (while (re-search-forward 
              "^\\(?:\\(Files\\)\\|\\(License\\)\\):[[:blank:]]*\\(.*\\)$" 
              (point-max) t)
        (if (match-string 1)
            (progn
              (setq current-files (match-string 3))
              (setq current-point (point))
              )
          (if current-files
              (progn
                (setq current-license (match-string 3))
                (setq current-files (split-string current-files 
                                                  "[[:blank:]]*,[[:blank:]]*"))
                (while current-files
                  (setq globs-list 
                        (append globs-list (list (list (car current-files) current-license current-point))))
                  (setq current-files (cdr current-files))
                  )
                )
            )
          )
        )
    ; (message "List %s" globs-list)
    globs-list
    )
  ))


(define-derived-mode debian-mr-coverage-mode fundamental-mode 
  "Debian Copyright Coverage"
  "The major mode for displaying debian.copyright file coverage"
  (make-local-variable 'debian-coverage-keywords)
  (setq debian-coverage-keywords
	`((,(concat "^[[:blank:]]*\\(?:" 
		    (let ((max-specpdl-size 1000))
		      (regexp-opt debian-mr-copyright-valid-licenses t))
		    "\\|\\(uncovered\\)\\|\\([^[:blank:]]+\\)\\)[[:blank:]]")
	   (1 debian-mr-copyright-value-face nil t)
	   (2 debian-mr-copyright-uncovered-face nil t)
	   (3 debian-mr-copyright-invalid-face nil t)
	   )))
  (setq font-lock-defaults '(debian-coverage-keywords 
			       t nil nil nil))
  )



(defun debian-mr-copyright-scan-files ()
  (interactive)
  "Use find to get the files covered by the copyright files, and those
*not* covered"
  (let ((glob-list (debian-mr-copyright-scan-buffer))
	(files (make-hash-table :test 'equal)) (current-glob) (current-info)
	(current-files) (current-file) (sorted-files) 
	(max-license-length 0) (line-format) )
    (while glob-list
      (setq current-glob (car (car glob-list)))
      (setq current-info (cdr (car glob-list)))
      ;; We add ./ in front of the pattern if it contains
      ;; a slash and does not start by ./
      (if (and (string-match "/" current-glob) 
	       (not (string-match "^\\./" current-glob)))
	  (setq current-glob (format "./%s" current-glob)))
      (setq current-files 
	    (shell-command-to-string 
	     (format "cd ..; find . -wholename \"%s\" -type f" current-glob)))
      (if (= (length current-files) 0)
	  (setq current-files 
	    (shell-command-to-string 
	     (format "cd ..; find . -name \"%s\" -type f" current-glob))))
      (if (= (length current-files) 0)
	  (warn "Glob %s found no match" current-glob)
	(setq current-files (split-string  current-files "\n"))
	(while current-files
	  (setq current-file (car current-files))
	  (if (> (length current-file) 0)
	      (puthash (car current-files) current-info files))
	  (setq current-files (cdr current-files)))
	)
      (setq glob-list (cdr glob-list))
      )
    ;; Now, we look for all the files, and signal the ones which are not
    ;; covered by the copyright stuff:
    (setq current-files  (split-string (shell-command-to-string 
					"cd ..; find . -type f") "\n"))
    (while current-files
      (setq current-file (car current-files))
      (if (> (length current-file) 0)
	  (if (gethash current-file files)
	      ()
	    (puthash current-file '("uncovered" 0) files)
	    ))
      (setq current-files (cdr current-files)))
    ;; Now, we need to order the files:
    (message "Hash: %s" files)
    (maphash (lambda (key val)
	       (if (> (length (car val)) max-license-length)
		   (setq max-license-length (length (car val))))
	       (setq sorted-files (cons key sorted-files)))
	     files)
    (setq sorted-files (sort sorted-files 'string< ))
    (setq line-format (format "%%%ds  %%s\n" max-license-length))
    ;; We now prepare the *File Coverage* buffer
    (switch-to-buffer "*File Coverage*")
    ;; Neat syntax coloring...
    (make-local-variable 'font-lock-defaults)
    
    (erase-buffer)
    (while sorted-files
      (setq current-file (car sorted-files))
      (insert (format line-format (car (gethash current-file files)) current-file))
      (setq sorted-files (cdr sorted-files))
      )
    (debian-mr-coverage-mode)
    )
  )

  


(add-to-list 'auto-mode-alist '("/debian/copyright\\'" . 
				debian-mr-copyright-mode))

;;;###autoload(add-to-list 'auto-mode-alist '("/debian/copyright\\'" . debian-mr-copyright-mode))

(provide 'debian-mr-copyright-mode)

;;; debian-mr-copyright-mode.el ends here
