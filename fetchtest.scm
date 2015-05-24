(use-modules (web client)
             (ice-9 receive)
             (ice-9 hash-table)
             (sxml simple)
             (sxml match)
             (web client)
             (ice-9 rdelim)
             (srfi srfi-1)
             (json))

(define (get-dustycloud-data)
  (xml->sxml
   (receive (response body)
       (http-get "http://dustycloud.org/blog/index.xml") body)
   #:namespaces '((atom . "http://www.w3.org/2005/Atom"))
   #:trim-whitespace? #t))


(define dumb-example-blog
  "<blog><post><title>Yeah man</title><body>Omg I am so tired</body><butt /></post><post><title>What's for dinner</title><body>Cookies?</body></post></blog>")

(scm->json-string 
 (sxml-match (cadr (xml->sxml dumb-example-blog))
             [(blog (post (title ,title) (body ,body) . ,yo) ...)
              (list (alist->hash-table (list (cons "title" title) (cons "body" body))) ...)]
             [,otherwise #f]))


(define dustycloud-data
  (xml->sxml
   (call-with-input-file "dustycloud_less_feed.xml"
     (lambda (f)
       (read-string f)))
   #:namespaces '((atom . "http://www.w3.org/2005/Atom"))
   #:trim-whitespace? #t))


(define (feed->activitystream feed actor)
  (define (clean-result result)
    (delete #nil result))

  (define (process-entry . entry-pieces)
    (fold
     (lambda (piece activity)
       (sxml-match
        piece
        [(atom:title ,title . ,_)
         (hash-set! activity "title" title)]
        [(atom:link (@ (rel "alternate") (href ,href) . ,_))
         (hash-set!
          (hash-ref activity "object")
          "@id" href)]
        [,else
         (begin
           (format #t "TODO: ~a\n" else)
           #nil)

         ;; (hash-set!
         ;;  activity "TODO"
         ;;  (cons else (hash-ref activity "TODO" '())))
         ])
       activity)
     (alist->hash-table
      `(("@context" . "http://www.w3.org/ns/activitystreams")
        ("@type" . "Post")
        ("actor" . ,actor)
        ("object" .
         ,(alist->hash-table
           '(("@type" . "Article"))))))
     entry-pieces))

  (clean-result
   (sxml-match
    (caddr feed)
    [(atom:feed ,[feed-item] ...)
     feed-item]
    [(atom:entry ,entry-part ...)
     (process-entry entry-part ...)]
    [,else #nil])))

(display (scm->json-string (feed->activitystream dustycloud-data "http://dustycloud.org/")
                           #:pretty #t))

(map (lambda (x) (hash-map->list cons x)) (car (feed->activitystream dustycloud-data "derp")))
