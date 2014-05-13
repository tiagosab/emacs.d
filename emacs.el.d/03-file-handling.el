;; add support for recent files list
;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)


(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ;don't clutter my fs and put backups into tmp
      backup-directory-alist `((".*" . ,temporary-file-directory))
      ;; never never open a separate file dialog
      use-file-dialog nil
      )
