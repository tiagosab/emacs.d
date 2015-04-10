(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("SC"  . "http://joseito.republika.pl/sunrise-commander/")))

(setq ts-my-packages
      '(adoc-mode
        gh
        gist
        git-blame
        key-chord
        bts-github
        git-commit-mode
        git-rebase-mode
        gitconfig-mode
        gitignore-mode
        magit
        markdown-mode
        rainbow-delimiters
        rainbow-blocks
        org
        mediawiki
        async
        sunrise-commander
        ecb
        )
      )


; It must exist a way to say to package-install/paradox to install
; these packages and its dependencies. For now I let just '(all),
; but I'd rather not.

(setq package-load-list
      (mapcar (lambda (value) "Doc" `(,value t)) ts-my-packages))
(setq package-load-list '(all))

(package-initialize)
(setq package-enable-at-startup nil)

(if (not (require 'paradox "paradox" t))
    (package-install 'paradox))

(mapcar 'paradox-require ts-my-packages)
