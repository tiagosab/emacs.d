;; this file contains functions that should mostly be organized
;; somewhere else.

(defun ts-ip ()
  "Discover current ip.

Implemented before 2011-04-18."
  (interactive)
  (browse-url-emacs "http://www.whatismyip.com/automation/n09230945.asp"))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun ts-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

;;
;; These should be implemented. But I should immediately document
;; them, as I do not know what I wanted the functions to do.
;;

(defun ts-prepare-file-to-exec ()
  (interactive)
  (message "Not Implemented"))

(defun ts-remove-line-breaks (from to)
  (interactive r)
  (message "Not Implemented"))

(defun ts-properties ()
  (interactive)
  (message "%s" (get-char-property-and-overlay (point) 'face)))

(defun ts-mrcopyright-helper ()
  (interactive)
  (let ((current (current-buffer))
        (buffer (get-buffer-create "* copyright-helper")))
    (while (re-search-forward "^.*NC\n")
      (let ((str (match-string 0)))
        (save-excursion
          (set-buffer buffer)
          (insert str))))))

(defun ts-find-file-from-current-line (filename &optional wildcards)
  "Edit file FILENAME, in another window. Defaults to filename at point.

Like \\[find-file] (which see), but creates a new window or
reuses an existing one.  See the function `display-buffer'.

Interactively, the default if you just type RET is the filename
at point in the current directory, but the visited file name is
available through the minibuffer history: type M-n to pull it
into the minibuffer.

Interactively, or if WILDCARDS is non-nil in a call from Lisp,
expand wildcards (if any) and visit multiple files."
  (interactive
   (ts-find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
        (mapcar 'switch-to-buffer (nreverse value))
      (switch-to-buffer value))))

(defun ts-find-file-read-args (prompt mustmatch)
  (list (let ((find-file-default (ts-get-filename-at-point)))
          (minibuffer-with-setup-hook
              (lambda () (setq minibuffer-default find-file-default))
            (read-file-name prompt default-directory nil mustmatch find-file-default)))
        t))

(defun ts-insert-TC ()
  (interactive)
  (end-of-line)
  (backward-char 3)
  (if (not (looking-at " - "))
      (progn
        (end-of-line)
        (insert " - "))
    (end-of-line))
  (insert "NC")
  (forward-char 1))

(defun ts-get-filename ()
  (interactive)
  (message (ts-get-filename-at-point)))

(defun ts-get-filename-at-point ()
  (or
   (save-excursion
     (re-search-backward "\\s-" nil 1)
     (forward-char 1)
     (let ((beg (point)))
       (re-search-forward "\\(\\s-\\|$\\)" nil nil)
       (let ((end (point)))
         (buffer-substring-no-properties beg end))))
   ""))

(defun ts-cur ()
  (interactive)
  (message (ts-get-filename-at-point)))

(defun ts-point()
  (interactive)
  (message "%s" (point)))

(defun ts-logme (txt)
  (save-excursion
    (set-buffer (get-buffer-create "*tiago-log*"))
    (end-of-buffer)
    (insert (format "%s\n" txt))))

(defun ts-dec-from-octal (n)
  "Convert an integer in octal to its decimal representation. No
  checking is done on the validity of the octal number.
  Written originally to convert file modes (permission bits)."
  (let ((i 0)
        (o 0))
    (while (> n 0)
      (setq o (+ o (* (% n 10) (expt 8 i))))
      (setq n (/ n 10))
      (setq i (1+ i)))
    o))

(defun ts-new-frame-maybe ()
  (if (ts-i-am-in-X-p)
      (ts-new-X-frame-maybe)
    (new-frame)))

(defun ts-i-am-in-X-p ()
  (return t))

(defun ts-new-X-frame-maybe ()
  (if ts-current-display-has-frame
      (raise-frame)
    (new-frame)))

(defun ts-current-display-has-frame-p ()
  (return value))

(defun ts-set-buffer-local-key (key func)
  (interactive "KSet key on this buffer: \naCommand: ")
  (let ((name (format "%s-magic" (buffer-name))))
    (eval
     `(define-minor-mode ,(intern name)
        "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
       `(define-key ,map ,key func)))
    (funcall (intern name) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ts-auctex-hide-document-class-begin ()
  (interactive)
  (outline-minor-mode 1)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "documentclass" nil t)
      (hide-entry))
    (when (re-search-forward "begin{document" nil t)
      (hide-entry))))

(defun ts-auctex-narrow-to-body ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "begin{document}.*\n" nil t)
      (let ((beg (point)))
        (when (re-search-forward "end{document}" nil t)
          (beginning-of-line)
          (let ((end (point)))
            (narrow-to-region beg end)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnus / Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Go to gnus, doing the right thing: opens Summary buffer if it
;; exists, else Group buffer if it exists, else start up gnus.
(defun ts-gnus ()
  (interactive)
  (let ((done nil)
        (buflist (buffer-list)))
    (while buflist
      (let (summary group
                    (buf (car buflist)))
        (if (string-match "\*Summary.*" (buffer-name buf))
            (setq summary buf)
          (if (string-match "\*Group.*" (buffer-name buf))
              (setq group buf)))
        (let ((buffer-to-go (or summary group)))
          (if buffer-to-go
              (progn (switch-to-buffer buffer-to-go)
                     (setq buflist nil)
                     (setq done t))
            (setq buflist (cdr buflist))))))
    (if (not done)
          (gnus))))

;; Method to attach current file to a mail composition.
(defun ts-message-attach ()
  "Attach current buffer's file to a gnus message composition.
TS: Originally copied from gnus-dired-attach"
  (interactive)
  ;; buffer is modified?
  (if (buffer-modified-p)
      (and
       (y-or-n-p "Buffer modified. Do you want to save it?")
       (save-buffer)))
  (let ((file (buffer-file-name)))
    (ts-message-attach-1 file)))

(defun ts-message-attach-1 (file)
  (let ((destination nil)
        (bufs nil))
    (setq bufs (gnus-dired-mail-buffers))
    ;; set up destination mail composition buffer
    ;; TS: This code is verbatim from original function, and
    ;; TS: I think it should be reviewed (no need to ask twice).
    (if (and bufs
             (y-or-n-p "Attach files to existing mail composition buffer? "))
        (setq destination
                (if (= (length bufs) 1)
                    (get-buffer (car bufs))
                  (completing-read "Attach to which mail composition buffer: "
                                   (mapcar
                                    (lambda (b)
                                      (cons b (get-buffer b)))
                                    bufs)
                                   nil t)))
      ;; setup a new mail composition buffer
      (let ((mail-user-agent gnus-dired-mail-mode)
            ;; A workaround to prevent Gnus from displaying the Gnus
            ;; logo when invoking this command without loading Gnus.
            ;; Gnus demonstrates it when gnus.elc is being loaded if
            ;; a command of which the name is prefixed with "gnus"
            ;; causes that autoloading.  See the code in question,
            ;; that is the one first found in gnus.el by performing
            ;; `C-s this-command'.
            (this-command (if (eq gnus-dired-mail-mode 'gnus-user-agent)
                              'gnoose-dired-attach
                            this-command)))
        (compose-mail))
      (setq destination (current-buffer)))
      ;; set buffer to destination buffer, and attach files
      (set-buffer destination)
      (goto-char (point-max))           ;attach at end of buffer
      (mml-attach-file file
                       (or (mm-default-file-encoding file)
                           "application/octet-stream") nil)
      (message "Attached file(s) %s" file)))

(defun ts-open-economist-story-in-w3m ()
  (interactive)
  (gnus-summary-select-article-buffer)
  (beginning-of-buffer)
  (re-search-forward "^URL: ")
  (w3m-view-this-url)
  (switch-to-buffer "*w3m*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ts-dired-view-file-other-window ()
  "In Dired, examine a file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (dired file))
      (view-file-other-window file))))

(defun ts-dired-external-see ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-regular-p file)
        (start-process "see" nil "see" file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python add-ons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tspy-add-import ()
  (interactive)
  (let ((module (car (split-string (ts-get-filename-at-point) "\\.")))
        (point (point-marker)))
    (save-excursion
      (beginning-of-buffer)
      (if (re-search-forward "^import" point t)
          (progn
           (end-of-line)
           (insert (format "\nimport %s" module)))
        (message "No 'import' line found.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing goodies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Insert date string
(defun ts-insert-date-string ()
  "Insert a nicely formated date string."
   (interactive)
   (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

(defun ts-convert-x-sampa-french-to-ipa ()
  "Convert french phonetic representation from X-SAMPA to IPA."
  (interactive)
  (let ((conversion-alist '(("ã" . "~a")
                            ("ɛ̃" . "~E")
                            ("õ" . "~o")
                            ("œ̃" . "~9")
                            ("a" . "a")
                            ("ɑ" . "A")
                            ("e" . "e")
                            ("ɛ" . "E")
                            ("ə" . "@")
                            ("i" . "i")
                            ("o" . "o")
                            ("ɔ" . "O")
                            ("ø" . "2")
                            ("œ" . "9")
                            ("u" . "u")
                            ("y" . "y")
                            ("j" . "j")
                            ("ɥ" . "H")
                            ("w" . "w")
                            ("b" . "b")
                            ("d" . "d")
                            ("f" . "f")
                            ("g" . "g")
                            ("k" . "k")
                            ("l" . "l")
                            ("m" . "m")
                            ("n" . "n")
                            ("ɲ" . "J")
                            ("p" . "p")
                            ("R" . "R")
                            ("s" . "s")
                            ("ʃ" . "S")
                            ("t" . "t")
                            ("v" . "v")
                            ("z" . "z")
                            ("ʒ" . "Z")))
        (case-fold-search nil))
    (save-excursion
      (if (looking-back "[]/]")
          (let* ((end-char (match-string 0))
                 (beg-char (if (equal end-char "/")
                               "/"
                             "\\[")))
            (backward-char)
            (when
                (looking-back (format "%s\\([^ \n]+\\)" beg-char))
              (let* ((phonems (match-string 1)))
                (save-match-data
                  (while conversion-alist
                    (when
                        (string-match (cdar conversion-alist) phonems)
                      (setq phonems (replace-match
                                     (caar conversion-alist) t t
                                     phonems)))
                    (setq conversion-alist (cdr conversion-alist))))
                (replace-match phonems t t nil 1))))))))

;; For (, [ and {, write the corresponding ), ] or } at once.
;; see skeleton mode, and emacswiki has a page for that.
(defun ts-corr-paren ()
  "Print () and move the cursor backone step."
  (interactive)
  (insert "()")
  (backward-char))

(defun ts-corr-curl ()
  "Print {} and move the cursor back one step."
  (interactive)
  (insert "{}")
  (backward-char))

(defun ts-corr-brack ()
  "Print [] and move the cursor back one step."
  (interactive)
  (insert "[]")
  (backward-char))

(defun ts-map-get (key map)
  (let ((mymap map)
        value)
    (while mymap
      (let* ((curvalue (pop mymap))
             (curkey (car curvalue)))
        (message "%s: %s" curvalue curkey)
        (if (string-equal curkey key)
            (progn
              (message "They match!")
              (setq mymap nil)
              (setq value (car (cdr curvalue)))))))
    (or value key)))

(defun ts-ins-double (opening)
  (interactive "*c")
  (let ((char (char-to-string opening))
        (pairs '(("[" "]")
                 ("(" ")")
                 ("{" "}"))))
    (insert char (ts-map-get char pairs))
    (backward-char)))

(defun ts-find-alternate-file-with-su ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/su::"
             buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; oficial de chancelaria
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ts-read-classificados ()
  (save-excursion
    (find-file "/home/tiago/comuna/concursos/oficial_chancelaria/classificados-cacd-3-fase.txt")
    (goto-char (point-min))
    (let (classificados-list
          (eof nil))
      (while (not eof)
        (re-search-forward "[^\n]*" nil)
        (message "%s - %s" (add-to-list 'classificados-list (match-string 0)) (point))
        (if (equal (point) (point-max))
            (setq eof t)))
      (return classificados-list))))

(defun ts-get-class ()
  (interactive)
  (ts-read-classificados))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-enter view-mode after save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ts-auto-view-mode ()
  (interactive)
  (add-hook 'after-save-hook
            '(lambda ()
               (view-mode 1))
            t
            t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert standard parts of files
;; these should use the emacs mecanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ts-insert-gpl-boilerplate ()
  (interactive)
  (save-excursion
    (let ((pt (point)))
      (insert " Copyright (C) ")
      (insert (format-time-string "%Y"))
      (insert (format " %s <%s>" user-full-name user-mail-address))
      (insert "
 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

")
      (comment-region pt (point)))))

(defun ts-insert-latex-headers ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert
     "\\documentclass[landscape,oneside,twocolumn,11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{palatino}
%\\usepackage[portuges]{babel}
\\usepackage[frenchb]{babel}
%\\usepackage[margin=2cm,columnsep=1.5cm]{geometry}
\\usepackage[left=2cm,right=8cm,top=3cm,bottom=3cm,marginparwidth=5cm]{geometry}
\\usepackage[pagewise,modulo]{lineno}

\\clubpenalty=9996
\\widowpenalty=9999
\\brokenpenalty=4991
\\predisplaypenalty=10000
\\postdisplaypenalty=1549
\\displaywidowpenalty=1602


%\\title{}
%\\author{}
%\\date{}

\\begin{document}

%\\linenumbers
%\\maketitle
")
    (goto-char (point-max))
    (insert "\n\\end{document}\n"))
  (latex-mode))

;;;;;;;;;;;;;;;;; Hide comments ;;;;;;;;;;
; ignominiously stolen from
; http://www.webservertalk.com/archive310-2005-6-1074609.html

(defun ts-overlay-comments(beg end attrs)
  (save-excursion
    (goto-char beg)
    (let (state comment-start comment-end overlay)
      (while (nth 4 (setq state
                          (parse-partial-sexp (point) end nil nil nil t)))
        (goto-char (nth 8 state))
        (setq comment-start (point))
        (forward-comment 1)
        (setq comment-end (point))
        (while (= (char-before comment-end) ?\n)
          (setq comment-end (1- comment-end)))
        (setq overlay (make-overlay comment-start comment-end))
        (mapc #'(lambda (attr)
                  (overlay-put overlay (car attr) (cdr attr)))
              attrs)))))

(defun ts-hide-comments()
  (interactive)
  (ts-overlay-comments (point-min)
                       (point-max)
                       '((category . comment) (invisible . comment))))

(defun ts-show-comments()
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (if (eq (overlay-get ov 'category) 'comment)
        (delete-overlay ov))))

(defun ts-next-hl-line-face-background ()
  (interactive)
  (let ((current (face-background 'hl-line))
        (colors '("gray" "gray12"))
        (new nil))
    (setq new
          (let ((first (car colors)))
            (while colors
              (let ((color (car colors)))
                (setq colors (cdr colors))
                (when (string-match current color)
                  (setq new (nth 0 colors))
                  (setq colors ())
                  )))
            (if (not new)
                (setq new first))
            new))
    (message (concat "New hl-line background: " new))
    (set-face-background 'hl-line new)))

(global-set-key (kbd "C-c C-ç") 'ts-next-hl-line-face-background)

;; fullscreen
(defun ts-toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

; this is useful when not running a tiling window manager
; (add-hook 'after-make-frame-functions 'ts-toggle-fullscreen)

;;From Herio: 2011-03-29
;Change cutting behaviour: if you press copy or cut when no region is
;active you'll copy or cut the current line:"
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun hs-copy-rectangle (start end &optional fill)
  "Save the rectangle as if killed, but don't kill it.  See
`kill-rectangle' for more information."
  (interactive "r\nP")
  (kill-rectangle start end fill)
  (goto-char start)
  (yank-rectangle))

(global-set-key (kbd "C-x r M-w") 'hs-copy-rectangle)
;;End From Herio

(provide 'tiago)
