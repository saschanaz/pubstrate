(use-modules (pubstrate webapp app)
             (pubstrate webapp storage)
             (pubstrate webapp storage-gdbm)
             (ice-9 getopt-long)
             (ice-9 match)
             ((system repl server)
              #:renamer (symbol-prefix-proc 'repl:)))

;; I guess for now we just want to be able to set a prefix
;; and a storage location.

(define option-spec
  '((help (single-char #\h) (value #f))
    (db-path (single-char #\d) (value #t))
    (url-prefix (single-char #\u) (value #t))
    (host (single-char #\h) (value #t))
    (port (single-char #\p) (value #t))
    (repl-server (single-char #\r) (value #f))))

(define* (get-storage-from-db-path db-path #:key (warn-if-memory-store #t))
  (if db-path
      (make-gdbm-store db-path)
      (begin
        (if warn-if-memory-store
            (display
             "WARNING: db-path not provided, so using memory store only\n"
             (current-error-port)))
        (make-memory-store))))

(define *help-text*
  "\
pubstrate-webapp [options]
  -h, --help           Display this help
  -d, --db-path=PATH   Path to a GDBM database.  Will be created if does not
                       exist.  If not supplied, a temporary in-memory database
                       will be used.
  -h, --host=HOST      Server host address.  Defaults to localhost.
  -p, --port=PORT      Server port.  Defaults to 8080.

  -r, --repl-server[=REPL-PORT]
                       Run a REPL server (on REPL-PORT if provided, otherwise
                       on port 37146)")

(define (webapp-cli args)
  ;; Maybe append a keyword argument to a list of existing
  ;; keyword arguments
  (define (maybe-kwarg kwarg val)
    (lambda (current-kwargs)
      (if val
          (append (list kwarg val)
                  current-kwargs)
          current-kwargs)))
  (let ((options (getopt-long args option-spec)))
    (cond
     ((option-ref options 'help #f)
      (display *help-text*) (newline))
     (else
      (let* ((storage (get-storage-from-db-path
                       (option-ref options 'db-path #f)))
             (get-option
              (lambda (option)
                (option-ref options option #f)))
             ;; Build up all the keyword arguments sent to run-server
             ;; depending on the options passed in
             (kwargs
              ((compose
                (maybe-kwarg #:host (get-option 'host))
                (maybe-kwarg #:port (get-option 'port))
                (maybe-kwarg #:url-prefix (get-option 'url-refix)))
               (list #:storage storage))))
        ;; Enable the REPL server if requested
        (match (option-ref options 'repl-server #f)
          ;; If false, do nothing
          (#f #f)
          ;; If it's a string and a number, serve the repl on this
          ;; port number on localhost
          ((and (? string? _) (= string->number repl-port-number))
           (repl:spawn-server (make-tcp-server-socket #:port repl-port-number)))
          ;; Anything else, start with the default port
          (_ (repl:spawn-server)))
        ;; Now run the server! :)
        (apply run-webapp
               ;; Announce that we're running on start-up
               #:announce (current-output-port)  ; @@: maybe STDERR instead?
               kwargs))))))
