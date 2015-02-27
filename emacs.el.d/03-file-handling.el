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

(defun ts-browse-url-browser-function (url &optional ignored)
  "Pass the specified URL to the \"xdg-open\" command.
xdg-open is a desktop utility that calls your preferred web browser.
The optional argument IGNORED is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "google-chrome" nil 0 nil url))

(setq browse-url-browser-function
      'ts-browse-url-browser-function)
