(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq ts-my-packages
      '(adoc-mode
        gh
        gist
        git-blame
        key-chord
        bts-github
        )
      )


; It must exist a way to say to package-install/paradox to install
; these packages and its dependencies. For now I am adding 'all,
; but I'd rather not.

(let ((packlist
       (mapcar (lambda (value) "Doc" `(,value t)) ts-my-packages)))
  (setq package-load-list (add-to-list 'packlist 'all t)))

(package-initialize)
(setq package-enable-at-startup nil)

(if (not (require 'paradox "paradox" t))
    (package-install 'paradox))

(paradox-require 'adoc-mode)
(paradox-require 'gh)
(paradox-require 'gist)
(paradox-require 'git-blame)
(paradox-require 'key-chord)
(paradox-require 'bts-github)
(paradox-require 'adoc-mode)
