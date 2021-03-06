;;; a2r.el --- Convert between Arabic numbers and Roman numerals.
;; From: http://www.ph.ed.ac.uk/~s0198183/a2r.el
;; $Id: a2r.el,v 1.3 2003/04/21 23:29:14 lawrence Exp $

;; Copyright (C) 2002, 2003, 2004, 2005 Lawrence Mitchell <wence@gmx.li>
;; Filename: a2r.el
;; Version: $Revision: 1.3 $
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Maintainer: Lawrence Mitchell <wence@gmx.li>
;; Created: 2002-08-03
;; Keywords: numbers fun

;;; Commentary:
;;


;;; History:
;; $Log: a2r.el,v $
;; Revision 1.3  2003/04/21 23:29:14  lawrence
;; Update copyright.
;;
;; Revision 1.2  2002/10/24 20:22:45  lawrence
;; Added file header and corrected minor docstring typos.
;;

;;; Code:

(defconst arabic-to-roman
  '((1000 .   "M") (900  .  "CM") (500  .   "D") (400  .  "CD")
    (100  .   "C") (90   .  "XC") (50   .   "L") (40   .  "XL")
    (10   .   "X") (9    .  "IX") (5    .   "V") (4    .  "IV")
    (1    .   "I"))
  "List of maps between Arabic numbers and their Roman numeral equivalents.")

(defun arabic-to-roman (num)
  (let ((map arabic-to-roman)
        res)
    (while (and map (> num 0))
      (if (or (= num (caar map))
              (> num (caar map)))
          (setq res (concat res (cdar map))
                num (- num (caar map)))
        (setq map (cdr map))))
    res))


(defun convert-arabic-to-roman (num &optional arg)
  "Convert Arabic number NUM to its Roman numeral representation.

Obviously, NUM must be greater than zero.  Don't blame me, blame the
Romans, I mean \"what have the Romans ever _done_ for /us/?\" (with
apologies to Monty Python).
If optional prefix ARG is non-nil, insert in current buffer."
  (interactive "nNumber: \nP")
  (if (< num 0)
      (message (concat "Error, NUM must be a positive number, the Romans had no"
                       "notion of\n"
                       "zero or negative numbers."))
    (let ((res (arabic-to-roman num)))
      (if arg
          (insert "\n" res "\n")
        (message "%s" res)))))

(defconst roman-to-arabic
  '(("M"  .  1000) ("CM" .   900) ("D"  .   500) ("CD" .   400)
    ("C"  .   100) ("XC" .    90) ("L"  .    50) ("XL" .    40)
    ("X"  .    10) ("IX" .     9) ("V"  .     5) ("IV" .     4)
    ("I"  .     1))
  "List of maps between Roman numerals and their Arabic equivalents.")

(defun roman-to-arabic (string &optional arg)
  "Convert STRING of Roman numerals to an Arabic number.

If STRING contains a letter which isn't a valid Roman numeral, the rest
of the string from that point onwards is ignored.

Hence:
MMD == 2500
and
MMDFLXXVI == 2500.
If optional ARG is non-nil, insert in current buffer."
  (interactive "sRoman numeral: \nP")
  (let ((res 0)
        (map roman-to-arabic))
    (while map
      (if (string-match (concat "^" (caar map)) string)
          (setq res (+ res (cdar map))
                string (replace-match "" nil t string))
        (setq map (cdr map))))
    (if arg
        (insert "\n" (number-to-string res) "\n")
      (message "%d" res))))

(provide 'a2r)

;;; a2r.el ends here
