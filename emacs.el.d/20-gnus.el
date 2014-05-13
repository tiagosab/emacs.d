;; ===========================
;; Gnus
;; ===========================

;; Basics:
; http://sachachua.com/blog/2008/05/geek-how-to-use-offlineimap-and-the-dovecot-mail-server-to-read-your-gmail-in-emacs-efficiently/
;; More:
; http://roland.entierement.nu/blog/2010/09/08/gnus-dovecot-offlineimap-search-a-howto.html

(setq gnus-select-method
      '(nnimap "Mail"
               (nnimap-stream shell)
               (nnimap-shell-program ; for emacs24
                "MAIL=maildir:$HOME/Maildir /usr/lib/dovecot/imap")
               (imap-shell-program ; for emacs23
                "MAIL=maildir:$HOME/Maildir /usr/lib/dovecot/imap")
               (nnimap-record-commands t)))

(setq gnus-summary-line-format
      (concat "%U" ; status (unread mark)
              "%R" ; replied mark
              "|%z" ; zcore
              "%~(cut-left 2)~(max-right 6)~(pad 6)o" ; date
              "%(%[%4L: %-18,18f%]%)" ; from
              " %>\\\\%s\n" ; subject
              ))

(setq gnus-group-line-format
      "%M%S%5y:%B%(%g%)\n") ; default
      ;"%M%S%L%N/%I/%R/%U/%t:%B%(%g%)\n")
