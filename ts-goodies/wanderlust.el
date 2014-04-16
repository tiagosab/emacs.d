;; mode:-*-emacs-lisp-*-
;; wanderlust
; ts-link: ~/wl

(setq
  elmo-maildir-folder-path "~/var/cache/offlineimap/mail";; where i store mail

  wl-stay-folder-window nil              ;; do not show the folder pane (left)
  wl-folder-window-width 25              ;; toggle on/off with 'i'

  ;; wl-smtp-posting-server "localhost"            ;; put the smtp server here
  ;; wl-local-domain "myhost.example.com"          ;; put something here...
  ;; wl-message-id-domain "myhost.example.com"     ;; ...

  wl-from "Tiago Saboga <tiagosaboga@gmail.com>"  ;; I don't understand why it
                                                 ;; is not taken
                                                 ;; from user-mail-address

  ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path 
  ;; the '.'-prefix is for marking them as maildirs
  ;wl-fcc ".sent"                       ;; sent msgs go to the "sent"-folder
  ;wl-fcc-force-as-read t               ;; mark sent messages as read 
  wl-default-folder ".INBOX"           ;; my main inbox 
  wl-draft-folder ".[Gmail]/Sent Mail"    ;; store drafts in 'postponed'
  wl-trash-folder ".[Gmail]/Trash"             ;; put trash in 'trash'
  wl-spam-folder ".[Gmail]/Spam"              ;; ...spam as well
  ; wl-queue-folder ".queue"             ;; we don't use this

  ;; check this folder periodically, and update modeline
  ;wl-biff-check-folder-list '(".todo") ;; check every 180 seconds
                                       ;; (default: wl-biff-check-interval)

  ;; hide many fields from message buffers
  wl-message-ignored-field-list '("^.*:")
  wl-message-visible-field-list
  '("^\\(To\\|Cc\\):"
    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    "^Organization:"
    "^Message-Id:"
    "^\\(Posted\\|Date\\):"
    )
  wl-message-sort-field-list
  '("^From"
    "^Organization:"
    "^X-Attribution:"
     "^Subject"
     "^Date"
     "^To"
     "^Cc"))
