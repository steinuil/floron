(html
  (head
    (meta (@ (charset UTF-8)))
    (title page-title)
    (link (@ (rel stylesheet)
             (href /style.css))))
  (body
    (header (a (@ (href "/")) blog-title))
    (main blog-content)
    (footer "(c) " blog-author " 2016, powered by "
            (a (@ (href "https://github.com/steinuil/floron"))
               "Floron"))))
