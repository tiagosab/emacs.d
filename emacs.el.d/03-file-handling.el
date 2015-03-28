;; add support for recent files list
;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)


(setq
 ;; save all autosave dirs in /tmp/, not only remote ones
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; don't clutter my fs and put backups into tmp
 backup-directory-alist `((".*" . ,temporary-file-directory))
 ;; never never open a separate file dialog
 use-file-dialog nil
 )

(defun ts-browse-url-browser-function (&optional url browser)
  "Pass the specified URL to the chosen browser.
Known browsers are eww (1) and google-chrome (2)"
  (let ((url (if (eq url nil)
                (car (browse-url-interactive-arg "U: "))
              url))
        (browser
         (if (eq browser nil)
             (read-from-minibuffer "Choose 1 for eww, 2 for chrome: ")
           (prefix-numeric-value current-prefix-arg))))
    (message (format "opening with '%s'" browser))
    (cond ((equal browser "1")
           (eww url))
          ((equal browser "2")
           (message "launching chrome")
           (call-process "google-chrome" nil 0 nil url)))))

(setq browse-url-browser-function
      '((".*emacswiki.*" . eww-browse-url)
        (".*wikipedia.*" . eww-browse-url)
        ("." . ts-browse-url-browser-function)))
