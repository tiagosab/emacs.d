;; auto-insert stuff
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-query nil) ;; don't ask, just do it.
(setq auto-insert-directory (concat (getenv "HOME") "/etc/templates/"))
(setq auto-insert-alist
      '(
        ("\\.py$" . ["python.tpl" auto-update-vars-go-to-point])
        ))
; insert template if possible, but mark as unmodified
(setq auto-insert 'other)

;; function replaces the string '@@@' by the current file
;; name. You could use a similar approach to insert name and date into
;; your file.

(defun auto-update-vars-go-to-point ()
  (save-excursion
    ;; Replace @SHORTNAME@ with file name sans suffix
    (while (search-forward "@SHORTNAME@" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (concat
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))
                        ".h") t))))
  (save-excursion
    ;; Replace @FULLNAME@ with file name
    (while (search-forward "@FULLNAME@" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-nondirectory buffer-file-name)))))
  (save-excursion
    ;; replace @YEAR@ with today's year
    (while (search-forward "@YEAR@" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (format-time-string "%Y" (current-time)))
        )))
  (when (search-forward "@POINT@" nil t)
    (replace-match ""))
    )