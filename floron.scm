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

(define key car)
(define value cadr)

(define (fetch k table)
  (cond ((null? table); '())
         (error (string-append "Key not found: " (symbol->string k))))
        ((eq? (key (car table)) k) (value (car table)))
        (else (fetch k (cdr table)))))

(define (replace old new ls)
  (cond ((null? ls) '())
        ((atom? ls)
         (if (eq? old ls) new ls))
        ((list? ls)
         (cons (replace old new (car ls))
               (replace old new (cdr ls))))))

(define (multireplace table ls)
  (if (null? table) ls
    (multireplace (cdr table)
      (let ((entry (car table)))
        (replace (key entry) (value entry) ls)))))

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

(define (seconds->ymd seconds)
  (let ((time-t (seconds->time seconds)))
    (string-append
      (number->string (+ 2000 (- (time-year time-t) 100))) "/" ; wtf, chibi
      (padded (time-month time-t)) "/"
      (padded (time-day time-t)))))

(define (make-post post)
  (let ((title (fetch 'title post))
        (date  (seconds->ymd (fetch 'date post)))
        (id    (fetch 'id post)))
    (make-post-info title date id
      (call-with-input-file
        (string-append (post-dir config) "/" id "/post.md")
        markdown->sxml))))

;; Blog helpers
(define-record-type <blog>
  (make-blog-info title author)
  blog?
  (title  blog-title)
  (author blog-author))

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
(define (render page bindings)
  (multireplace bindings (read-file page)))

(define (page-title page root)
  (string-append page " | " root))

(define (make-link file)
  (string-append "/" file))

(define (render-post post)
  (render "templates/post.scm"
    `((post-body  ,(post-body post))
      (post-date  ,(post-date post))
      (post-title ,(post-title post)))))

(define (render-post-page blog post)
  (render "templates/layout.scm"
    `((page-title   ,(page-title (post-title post) (blog-title blog)))
      (blog-title   ,(blog-title blog))
      (blog-author  ,(blog-author blog))
      (blog-content ,(render-post post)))))

(define (render-index-item post)
  (render "templates/index.scm"
    `((post-title ,(fetch 'title post))
      (post-date  ,(seconds->ymd (fetch 'date post)))
      (post-desc  ,(fetch 'description post))
      (post-link  ,(make-link (fetch 'id post))))))

(define (render-index blog posts)
  (render "templates/layout.scm"
    `((page-title   ,(page-title "Index" (blog-title blog)))
      (blog-title   ,(blog-title blog))
      (blog-author  ,(blog-author blog))
      (blog-content ,(map render-index-item posts)))))

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
