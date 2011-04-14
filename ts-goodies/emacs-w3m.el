; ~/.emacs-w3m

(eval-after-load "w3m-search"
  '(add-to-list 'w3m-search-engine-alist
                '("pt.wikipedia"
                  "http://pt.wikipedia.org/wiki/Especial:Search?search=%s"
                  utf-8)))
