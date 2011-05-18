(require 'cl-macs)

(setq org-publish-project-alist
      '(
        ("org-hack-blog"
         ;; My hack blog
         :base-directory "~/h/hack/"
         :base-extension "org"
         
         ;; The jekyll project
         :publishing-directory "~/h/blog"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t  ;; Only export html body
         )
        (
(defun ts-move-up-whole-row ()
  (interactive)
  (save-excursion
    (let ((in-the-table t))
      (while in-the-table
        (ts-remove-this-cell-move-below)
        (next-line)))))

(defun ts-remove-this-cell-move-below ()
  (interactive)
  (ts-kill-current-cell t)
  (save-excursion
    (next-line)
    (ts-kill-current-cell))
  (yank))

(defun ts-kill-current-cell (&optional do-not-insert-spaces)
  (interactive)
  (re-search-backward "|")
  (forward-char)
  (let ((beg (point-marker)))
    (re-search-forward "|")
    (backward-char)
    (kill-region beg (point)))
  (unless do-not-insert-spaces
    (let ((len (length (current-kill 1 t))))
      (dotimes (i len) (insert " ")))))


