;; ==============================
;; BBdb
;; ==============================

;(require 'bbdb)
; (bbdb-initialize 'gnus 'message) ; 'w3)
(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-legal-zip-codes '("^$"
                             "^[ 	\n]*[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[ 	\n]*$"
                             "^[ 	\n]*\\([0-9][0-9][0-9][0-9][0-9][0-9]?\\)[ 	\n]*-?[ 	\n]*\\([0-9][0-9][0-9][0-9]?\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Za-z0-9]+\\)[ 	\n]+\\([A-Za-z0-9]+\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Z]+\\)[ 	\n]*-?[ 	\n]*\\([0-9]+ ?[A-Z]*\\)[ 	\n]*$"
                             "^[ 	\n]*\\([A-Z]+\\)[ 	\n]*-?[ 	\n]*\\([0-9]+\\)[ 	\n]+\\([0-9]+\\)[ 	\n]*$"))
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (bbdb-insinuate-message)

; moy-bbdb is a library to add addresses to which I sent mail to bbdb.
;; (autoload 'bbdb/send-hook "moy-bbdb"
;;   "Function to be added to `message-send-hook' to notice records
;; when sending messages" t)
;; (add-hook 'message-send-hook 'bbdb/send-hook) ; If you use Gnus
;(add-hook 'mail-send-hook 'bbdb/send-hook) ; For other mailers
;                                           ; (VM, Rmail)
