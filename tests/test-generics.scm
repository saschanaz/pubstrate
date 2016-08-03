(define-module (tests test-asobj)
  #:use-module (srfi srfi-64)
  #:use-module (tests utils)
  #:use-module (pubstrate vocab)
  #:use-module (pubstrate generics)
  #:use-module ((pubstrate shorthand)
                #:renamer (symbol-prefix-proc 'as:)))

(test-begin "test-generics")

(define-as-generic generic-foo "foo the bar")
(define-as-method (generic-foo (asobj ^Object) x)
  (list 'object-time asobj x))
(define-as-method (generic-foo (asobj ^Activity) x)
  (list 'activity-time asobj x))

(let ((test-object (as:object)))
  (test-equal (generic-foo test-object 'yup)
    (list 'object-time test-object 'yup)))
(let ((test-object (as:like)))
  (test-equal (generic-foo test-object 'yup)
    (list 'activity-time test-object 'yup)))

(test-end "test-generics")

(test-exit)
