(use-modules (ice-9 match)
             (ice-9 format)
             (ice-9 and-let-star)
             (ice-9 receive)
             (oop goops)
             (8sync)
             (8sync repl)
             (8sync systems web)
             (web uri)
             (web request)
             (pubstrate apclient)
             (pubstrate webapp utils)
             (pubstrate webapp ctx)
             (pubstrate webapp form-widgets)
             ((pubstrate webapp views)
              #:renamer (symbol-prefix-proc 'ps-view:)))


(define* (base-template body #:key title)
  `((doctype html)
    (head
     (meta (@ (charset "utf-8")))
     (title ,(if title
                 (string-append title " -- Pubstrate")
                 "Pubstrate"))
     ;; css
     (link (@ (rel "stylesheet")
              (href ,(local-uri "static" "css" "main.css")))))
    (body
     (div (@ (id "main-wrapper"))
          (header (@ (id "site-header"))
                  ;; @@: Not semantic naming!
                  (span (@ (id "site-header-left-stuff"))
                        (b (a (@ (href ,(local-uri)))
                              "ActivityPub test suite"))))
          (div (@ (id "site-main-content"))
               ,body))
     (div (@ (id "site-footer"))
          "ActivityPub test suite powered by "
          (a (@ (href "https://gitlab.com/dustyweb/pubstrate"))
             "Pubstrate")
          ", which is released under the "
          (a (@ (href "https://www.gnu.org/copyleft/gpl.html"))
             "GNU General Public License")
          ", version 3 or later."))))

(define (view:main-display request body)
  (respond-html
   (base-template
    "A whole lotta nothin"
    #:title "Hello!")))

(define (route request)
  (match (split-and-decode-uri-path (uri-path (request-uri request)))
    (() (values view:main-display '()))

    (("static" static-path ...)
     ;; TODO: make this toggle'able
     (values ps-view:render-static
             (list (string-append "/" (string-join
                                       static-path "/")))))

    ;; Not found!
    (_ (values ps-view:standard-four-oh-four '()))))



;; (define-class <report> ()
;;   (last-updated #:init-thunk current-time
;;                 #:accessor .last-updated)
;;   ;; This is the state of, what's next, etc?
;;   (state)
;;   ;; Here's all the data we've collected so far
;;   ;; It's a hashq of symbol -> response
;;   (responses #:init-thunk make-hash-table))






;;; Test infrastructure


(define-class <tester> (<actor>)
  (reports #:init-thunk make-hash-table
           #:getter .reports)
  (actions
   #:allocation #:each-subclass
   #:init-thunk
   (build-actions
    (run-tests tester-run-tests)
    (cleanup-old tester-cleanup-old))))


(define* (tester-run-tests message client-id)
  ;; Add client id to test states
  ;;
  'TODO)


(define (tester-cleanup-old message)
  'TODO)


;;; Web application launching stuff


(define (http-handler request body)
  (receive (view args)
      (route request)
    (apply view request body args)))

(define (main . args)
  (define hive (make-hive))
  (with-extended-ctx
   ;; TODO: fixme...
   `((base-uri . ,(string->uri "http://localhost:8080/")))
   (lambda ()
     (bootstrap-actor hive <web-server>
                      #:http-handler (wrap-apply http-handler))
     (bootstrap-actor hive <repl-manager>)
     (run-hive hive '()))))
