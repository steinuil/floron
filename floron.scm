(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme cxr)
        (steen markdown)
        (chibi filesystem)
        (chibi time)
        (chibi sxml))

;; Generic helpers
(define-syntax λ
  (syntax-rules ()
    ((_ args body) (lambda args body))))

(define (read-file f)
  (with-input-from-file f read))

(define (write-to-file file proc)
  (call-with-output-file file
    (λ (f) (display proc f))))

(define (atom? x)
  (not (or (pair? x) (null? x) (vector? x))))

(define (fetch key table)
  (cond ((null? table)
         (error (string-append "Key not found: "
                               (symbol->string key))))
        ((eq? (caar table) key) (cadar table))
        (else (fetch key (cdr table)))))

(define (replace old new ls)
  (cond ((null? ls) '())
        ((atom? ls)
         (if (eq? old ls) new ls))
        ((list? ls)
         (cons (replace old new (car ls))
               (replace old new (cdr ls))))))

(define (multireplace old new ls)
  (cond ((and (null? (cdr old)) (null? (cdr new)))
         (replace (car old) (car new) ls))
        ((or (null? (cdr old)) (null? (cdr new)))
         (error "Unequal length lists given"))
        (else
          (replace (car old) (car new)
                   (multireplace (cdr old) (cdr new) ls)))))

;; Post helpers
(define-record-type <post>
  (make-post-info title date id body)
  post?
  (title post-title)
  (date  post-date)
  (id    post-id)
  (body  post-body))

(define (padded num)
  (let ((str (number->string num)))
    (if (= 1 (string-length str))
      (string-append "0" str) str)))

(define (time->ymd time-t)
  (string-append
    (number->string (+ 2000 (- (time-year time-t) 100))) "/" ; wtf, chibi
    (padded (time-month time-t)) "/"
    (padded (time-day time-t))))

(define (make-post post)
  (let ((title (fetch 'title post))
        (date  (time->ymd (seconds->time (fetch 'date post))))
        (id    (fetch 'id post)))
    (make-post-info title date id
      (call-with-input-file
        (string-append (post-dir config) "/" id "/post.md")
        markdown->sxml))))

;; Blog helpers
(define-record-type <blog>
  (make-blog-info title author)
  blog?
  (title    blog-title)
  (author   blog-author))

(define (make-blog config)
  (let ((title  (fetch 'title config))
        (author (fetch 'author config)))
    (make-blog-info title author)))

;; Configuration
(define-record-type <config>
  (make-config blog posts post-dir out-dir)
  config?
  (blog     config-blog)
  (posts    config-posts)
  (post-dir post-dir)
  (out-dir  out-dir))

(define (load-config)
  (let ((cfg (read-file "config.scm")))
    (let ((config (fetch 'config cfg))
          (posts (fetch 'posts cfg)))
      (make-config (make-blog config) posts
                   (fetch 'post-dir config)
                   (fetch 'out-dir config)))))

;; Interesting stuff
(define (render-post post)
  (let ((body   (post-body post))
        (date   (post-date post))
        (title  (post-title post))
        (layout (read-file "templates/post.scm")))
    (multireplace
      '(post-title post-date post-body)
      (list title date body)
      layout)))

(define (render-post-page blog post)
  (let ((ptitle (string-append (post-title post) " | " (blog-title blog)))
        (title  (blog-title blog))
        (author (blog-author blog))
        (post   (render-post post))
        (layout (read-file "templates/layout.scm")))
    (multireplace
      '(blog-title page-title blog-author blog-content)
      (list title ptitle author post)
      layout)))

(define (render-index blog posts)
  (let ((ptitle (string-append "Index | " (blog-title blog)))
        (title  (blog-title blog))
        (author (blog-author blog))
        (layout (read-file "templates/index.scm")))

    (define (render-index-item post)
      (let ((title (fetch 'title post))
            (date  (time->ymd (seconds->time (fetch 'date post))))
            (link  (string-append "/" (fetch 'id post))))
        (multireplace '(post-title post-date post-link)
                      (list title date link)
                      layout)))

    (let ((posts (map render-index-item posts))
          (layout (read-file "templates/layout.scm")))
      (multireplace '(blog-title page-title blog-author blog-content)
                    (list title ptitle author posts)
                    layout))))

;; Start
(define config (load-config))

(define (render-blog)
  (let ((posts (map make-post (config-posts config)))
        (out   (out-dir config)))
    (create-directory out 493)
    (write-to-file (string-append out "/index.html")
                   (sxml->xml (render-index (config-blog config)
                                            (config-posts config))))
    (for-each
      (λ (post)
        (let ((path (string-append out "/" (post-id post)))
              (page (render-post-page (config-blog config) post)))
          (create-directory path 493)
          (write-to-file (string-append path "/index.html")
                         (sxml->xml page))))
      posts)))

(render-blog)
