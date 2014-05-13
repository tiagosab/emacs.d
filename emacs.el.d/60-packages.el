(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-load-list
      '((key-chord t)))
(package-initialize)
(setq package-enable-at-startup nil)

