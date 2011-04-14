(defvar important-buffers '() )

;(defalias 'imp-show-buffer 'pop-to-buffer) ; other window, select it
;(defalias 'imp-show-buffer 'display-buffer) ; other window, don't select
(defalias 'imp-show-buffer 'switch-to-buffer) ; current window

(defun mark-current-buffer-as-important ()
  "Toggle important mark on current buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (if (memq buffer important-buffers)
        (setq important-buffers (delq buffer important-buffers))
      (add-to-list 'important-buffers buffer))))

(defun goto-next-important-buffer ()
  "Go to next buffer marked as important.
TODO: cycle through all buffers, not only the first ones."
  (interactive)
  (let (imp-buffers)
    (while important-buffers
      (let (nextbuffer)
        (setq nextbuffer (pop important-buffers))
        (add-to-list 'imp-buffers nextbuffer)
        (if (not (get-buffer-window nextbuffer))
            (progn
              (imp-show-buffer nextbuffer)
              (if important-buffers
                  (progn
                    (setq imp-buffers 
                          (nconc imp-buffers important-buffers))
                    ; stop while loop if then-clause is executed
                    (setq important-buffers '())))))))
    (setq important-buffers imp-buffers)))
