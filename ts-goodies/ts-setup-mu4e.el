(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-show-images t)
(setq completion-ignore-case t)
(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-view-fields
      '(:from :to :cc :subject :flags :date :tags :attachments :signature))


(setq
 mu4e-maildir       "~/GMaildir"   ;; top-level Maildir
 )
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")


(setq
   mu4e-get-mail-command "/home/tiago/src/offlineimap/offlineimap.py" ;; or fetchmail, or ...
   mu4e-update-interval 1200)             ;; update every 20 minutes

;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(add-to-list 'mu4e-bookmarks
             '("size:5M..500M"
               "Big messages"
               ?B))
(add-to-list 'mu4e-bookmarks
             '("maildir:/INBOX"
               "Inbox"
               ?i))

;; ;; press 'a V'
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
        "<html>"
        "<head><meta http-equiv=\"content-type\""
        "content=\"text/html;charset=UTF-8\">"
        html))
    (browse-url (concat "file://" tmpfile))))
(add-to-list 'mu4e-view-actions
  '("View in browser" . mu4e-msgv-action-view-in-browser) t)

(add-hook 'message-mode-hook
            (lambda ()
              (setq fill-column 70)
              (turn-on-auto-fill)))

;; don't prompt for applying of marks, just apply
(setq mu4e-headers-leave-behavior 'apply)

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(provide 'ts-setup-mu4e)

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)

(when
    (not (functionp 'mu4e-shr2text))
  (defun mu4e-shr2text ()
    "Html to text using the shr engine; this can be used in
`mu4e-html2text-command' in a new enough emacs. Based on code by
Titus von der Malsburg."
    (interactive)
    (let ((dom (libxml-parse-html-region (point-min) (point-max)))
          ;; When HTML emails contain references to remote images,
          ;; retrieving these images leaks information. For example,
          ;; the sender can see when I openend the email and from which
          ;; computer (IP address). For this reason, it is preferrable
          ;; to not retrieve images.
          ;; See this discussion on mu-discuss:
          ;; https://groups.google.com/forum/#!topic/mu-discuss/gr1cwNNZnXo
          (shr-inhibit-images t))
      (erase-buffer)
      (shr-insert-document dom)
      (goto-char (point-min)))))
