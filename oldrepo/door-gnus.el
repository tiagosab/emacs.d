; from http://www.emacswiki.org/emacs/SwitchToGnus

(defun door-gnus ()
  "Switch between gnus and non-gnus buffers, preserving window configurations."
  (interactive)
  (let ((bufname (buffer-name)))
    (if (or
         (string-equal "*Group*" bufname)
         (string-equal "*BBDB*" bufname)
         (string-match "\*Summary" bufname)
         (string-match "\*mail" bufname)
         (string-match "\*wide" bufname)
         (string-match "\*reply" bufname)
         (string-match "\*Article" bufname))
        (progn
          (door-bury-gnus))
      (if (get-buffer "*Group*")
          (door-unbury-gnus)
        (progn
          (setq gnus-unbury-window-configuration
                (current-window-configuration))
          (delete-other-windows)
          (gnus))))))

(defun door-unbury-gnus ()
  (interactive)
  (setq gnus-unbury-window-configuration nil)
  (let ((buf nil)
        (bufname nil))
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (when (and (boundp 'gnus-bury-window-configuration)
                 gnus-bury-window-configuration)
        (unless gnus-unbury-window-configuration
          (setq gnus-unbury-window-configuration
                (current-window-configuration)))
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
          (bury-buffer buf))
        (set-window-configuration gnus-bury-window-configuration)))))

(defun door-bury-gnus ()
  (interactive)
  (setq gnus-bury-window-configuration nil)
  (let ((buf nil)
        (bufname nil))
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (when (or
             (string-equal "*Group*" bufname)
             (string-equal "*BBDB*" bufname)
             (string-match "\*Summary" bufname)
             (string-match "\*mail" bufname)
             (string-match "\*reply" bufname)
             (string-match "\*wide" bufname)
             (string-match "\*Article" bufname))
        (unless gnus-bury-window-configuration
          (setq gnus-bury-window-configuration (current-window-configuration)))
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
          (bury-buffer buf))
        (set-window-configuration gnus-unbury-window-configuration)))))
