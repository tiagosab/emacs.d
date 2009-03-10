(defvar important-buffers '() )
;; (setq ts-important-buffers '() )
;; (setq debug-on-quit nil)

(defun mark-current-buffer-as-important ()
  "Toggle important mark on current buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (memq buffer ts-important-buffers)
        (setq ts-important-buffers (delq buffer ts-important-buffers))
      (add-to-list 'ts-important-buffers buffer))))

(defun goto-next-important-buffer ()
  "Go to next buffer marked as important.
TODO: cycle through all buffers, not only the first ones."
  (interactive)
  (let (important-buffers)
    (while important-buffers
      (let (nextbuffer)
        (setq nextbuffer (pop important-buffers))
        (add-to-list 'important-buffers nextbuffer)
        (if (not (get-buffer-window nextbuffer))
            (progn
              (pop-to-buffer nextbuffer)
              (if important-buffers
                  (progn
                    (setq imp-buffers 
                          (nconc imp-buffers important-buffers))
                    ; stop while loop if then-clause is executed
                    (setq important-buffers '())))))))
    (setq important-buffers imp-buffers)))
