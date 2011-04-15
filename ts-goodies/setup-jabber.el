;; ==============================
;; Jabber
;; ==============================

(eval-after-load "jabber"
  '(progn
     (setq jabber-account-list '(
                                 ("tiagosaboga@gmail.com"
                                  (:network-server . "talk.google.com")
                                  (:port . 443)
                                  (:connection-type . ssl))))
     (setq jabber-show-offline-contacts nil)
     (set-face-attribute 'jabber-title-large nil
                         :width 'expanded
                         :height 1.5)
     (set-face-attribute 'jabber-title-medium nil
                         :height 1.5)))

(provide 'setup-jabber)
