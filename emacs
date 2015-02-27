;; -*- emacs-lisp -*-
;; My self
(setq user-full-name "Tiago Saboga")
(setq user-mail-address "tiagosaboga@gmail.com")
(setq ts-library-dir (expand-file-name "~/lib/emacs"))
(setq ts-emacs.d-dir (expand-file-name (concat ts-library-dir "/emacs.el.d")))


;;; Emacs Load Path

;; (if (not (string-match "Debian" (emacs-version)))
;;     (progn
;;       (add-to-list 'load-path "/usr/share/emacs/site-lisp")
;;       (load-library "debian-startup")
;;       (debian-startup 'emacs24)))

(if (< emacs-major-version 24)
    (add-to-list 'load-path
                 (concat ts-library-dir "/23to24")))

(add-to-list 'load-path ts-library-dir)

;; Add all subdirs of my library dir to load-path
(let ((contents (directory-files ts-library-dir))
      (default-directory ts-library-dir))
  (dolist (file contents)
    (when (and (string-match "\\`[[:alnum:]]" file)
               (not (string-match "\\.elc?\\'" file))
               (not (string-match "23to24" file))
               (file-directory-p file))
      (let ((expanded (expand-file-name file)))
        (message "Appending to load-path")
        (message expanded)
        (add-to-list 'load-path expanded)))))


;;; Load my config snippets
(let ((contents (directory-files ts-emacs.d-dir))
      (default-directory ts-emacs.d-dir))
  (dolist (file contents)
    (when (and (string-match "[.-a-z0-9]*el$" file)
               (file-exists-p file)
               (not (file-directory-p file)))
      (let ((expanded (expand-file-name file)))
        (load-file file)))))

;; The following would be great, but append dirs to load-path
;; instead of add at the beginning.
; (let ((default-directory "~/lib/emacs"))
;   (normal-top-level-add-to-load-path '("."))
;   (normal-top-level-add-subdirs-to-load-path))

; load my general-purpose library
(load-library "tiago")

