(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)

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
    (goto-char (point-min))))
