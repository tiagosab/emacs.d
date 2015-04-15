(if (boundp 'ts-emacs-is-portable)
    (progn
      (require 'server)
      (unless (server-running-p (server-start)))))
      ;; (set-variable 'magit-git-executable
      ;;               (concat usb-drive-letter
      ;;                       "PortableApps/GitPortable/App/Git/bin/git.exe"))))
