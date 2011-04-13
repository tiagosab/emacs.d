;;; bbdb-snarf-abook.el --- import an abook-style addressbook into bbdb
;; Time-stamp: <2004-12-04 11:00:35 anselm>
;; this was developed for gnu emacs 21 and bbdb 2.34.cvs20030503-1
;; Author: Anselm Helbig <anselm [at] chemie [dot] fu-berlin [dot] de>
(require 'bbdb)
(require 'bbdb-com)
(defun bbdb-snarf-abook (filename)
  "tries to parse an abook-style addressbook and writes the entries
found directly into the bbdb. your .bbdb file may be empty, but has to exist."
  (interactive "fLocation of abook's addressbook: ")
  (save-excursion
    (let ((key) (value) (count 0))
      (set-buffer (find-file-noselect filename))
      (goto-char (point-min))
      (while (re-search-forward "\\[[0-9]*\\]\\s-*$" nil t)
        (let ((name "") (address "") (city "") (zip "") (phone "")
              (notes "") (email "") (workphone "") (mobile "") (fax "")
              (address-vector (vector "address" () "" "" "" ""))
              (address-vector-valid nil)
              phone-list)
          (while (progn
                   (next-line 1) (beginning-of-line)
                   (re-search-forward "\\s-*\\([^\\s-=]*\\)=\\(.*?\\)\\s-*$" (save-excursion (end-of-line) (point)) t))
            (if (not (string= (match-string 2) ""))
                (progn
                  (setq key (match-string 1))
                  (setq value (match-string 2))
                  (if (string= key "phone")
                      (setq phone-list (cons (vector "home" value) phone-list)))
                  (if (string= key "mobile")
                      (setq phone-list (cons (vector "mobile" value) phone-list)))
                  (if (string= key "fax")
                      (setq phone-list (cons (vector "fax" value) phone-list)))
                  (if (string= key "workphone")
                      (setq phone-list (cons (vector "work" value) phone-list)))
                  (if (string= key "address")
                      (progn
                        (aset address-vector 1 (cons value ()))
                        (setq address-vector-valid t)))
                  (if (string= key "city")
                      (progn
                        (aset address-vector 2 value)
                        (setq address-vector-valid t)))
                  (if (string= key "zip")
                      (progn
                        (aset address-vector 4 value)
                        (setq address-vector-valid t)))
                  (set (read key) value))))
          (message "converting entry no. %d" (setq count (1+ count)))
          (bbdb-create-internal
           name
           nil
           email
           (if address-vector-valid (list address-vector))
           phone-list
           notes)))
      (message "abook's addressbook successfully imported."))))
(provide 'bbdb-snarf-abook)