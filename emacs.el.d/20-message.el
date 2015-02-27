;; How to choose, and what one am I using?
(setq send-mail-function
      'sendmail-send-it)

(setq message-send-mail-function
      'message-send-mail-with-sendmail)
                                        
;; Do not wait for sendmail: sendmail version
(setq message-interactive nil)

;; Do not wait for sendmail: message-sendmail version
(setq mail-interactive nil)
