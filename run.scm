(cond-expand 
  (chicken-4
    (import chicken)
    (use awful awful-postgresql
         postgresql sql-null
         spiffy spiffy-request-vars
         medea intarweb))
  (chicken-5
    (import (chicken base) (chicken io) (chicken format))
    (import awful awful-postgresql 
            postgresql sql-null 
            spiffy spiffy-request-vars 
            medea intarweb)))

(enable-db)
(db-credentials '((dbname   . "dbas1vp8gkllu9")
                  (user     . "zuqecytnilorph")
                  (password . "6f81f26818ecf95518cec24cfc4fda9e1d3a1b81e86cafaeb08836128c624196")
                  (host     . "ec2-34-246-227-219.eu-west-1.compute.amazonaws.com")))

(define connection (connect (db-credentials)))
(on-exit (lambda () (disconnect connection)))

(define array-as-list-parser
  (cons 'array (lambda (x) x)))
(json-parsers (cons array-as-list-parser (json-parsers)))

(define (sql-null-unparser v)
  (write-json 'null))
(json-unparsers (cons (cons sql-null? sql-null-unparser) 
                      (json-unparsers)))

(define (content-length) 
  (header-value 'content-length
                (request-headers (current-request))))

(define (content)
  (read-string (content-length)
               (request-port (current-request))))

(define (json-request-vars)
  (read-json (content)))

(define-syntax debug-macro
  (syntax-rules ()
    ((_ macro)
     (print (strip-syntax (expand (quote macro)))))))

(define (request-var var vars)
  (cond ((assoc (quote var) vars) => cdr)
        (else #f)))

(define-syntax with-json-request-vars
  (syntax-rules ()
    ((_ (var ...) expr . rest) 
     (let ((request-vars (json-request-vars)))
       (let ((var (request-var (quote var) request-vars)) ...)
         (begin expr . rest))))))

(define (json-response obj)
  `(literal ,(json->string obj)))

(define-syntax define-route
  (syntax-rules ()
    ((_ route method (var . vars) expr . rest)
     (define-page route
       (lambda ()
         (awful-response-headers '((content-type "application/json")))
         (with-json-request-vars (var . vars)
           (json-response (begin expr . rest))))
       no-template: #t
       method: `(,method)))))

(define-syntax get
  (syntax-rules ()
    ((_ route expr . rest)
     (define-route route 'GET expr . rest))))

(define-syntax post
  (syntax-rules ()
    ((_ route expr . rest)
     (define-route route 'POST expr . rest))))

(define (name-cols names row)
  (map cons names row))

(define (fetch-users)
  (row-fold 
    (lambda (row table) (cons (name-cols '(username email) row) table))
    '()
    (query connection "select username, email from app_user")))

(define get-user
  (begin
    (query connection "prepare get_user as select email from app_user where username = $1 or email = $2")
    (lambda (username email)
      (query connection 'get_user username email))))

(define post-user
  (begin
    (query connection "prepare post_user as insert into app_user (username, email) values ($1, $2)")
    (lambda (username email)
      (query connection 'post_user username email))))

(define (available? username email)
  (zero? (row-count (get-user username email))))

(define (exists? username email)
  (not (available? username email)))

(get "api/available" (username email password)
  `((available . ,(available? username email))))

(get "api/verify" (username email password)
  `((verified . ,(exists? username email))))

(post "api/register" (username email password first-name)
  (post-user username email))
