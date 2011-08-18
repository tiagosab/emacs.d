(setq org-jekyll-category "subject")

(setq org-publish-project-alist
      '(
        ("org-hack"
         ;; Path to your org files.
         :base-directory "~/h/hack/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/h/hack/html/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>

         ;; org-jekyll extensions
         :blog-publishing-directory "~/h/hack/"
         :site-root "http://tiagosab.github.com"
         :jekyll-sanitize-permalinks t
         )
        

        ("org-static-hack"
         :base-directory "~/h/hack"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/h/hack/html"
         :recursive t
         :publishing-function org-publish-attachment)

    ("ts" :components ("org-hack" "org-static-hack"))
))
