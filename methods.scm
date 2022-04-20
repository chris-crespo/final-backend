(module methods 
  (json-request-vars request-var json-response
   get post)

  (cond-expand 
    (chicken-4
      (import scheme chicken)
      (use awful spiffy spiffy-request-vars
           medea intarweb))
    (chicken-5
      (import scheme (chicken io) (chicken syntax))
      (import awful spiffy spiffy-request-vars 
              medea intarweb)))

  (define (json-request-vars)
    (let* ((header (request-headers (current-request)))
           (len (header-value 'content-length header))
           (content (read-string len (request-port (current-request)))))
      (read-json content)))

  (define (request-var var vars)
    (cond ((assoc (quote var) vars) => cdr)
          (else #f)))

  (define (json-response obj)
    `(literal ,(json->string obj)))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((_ (name . rest-of-pattern) expr . rest)
       (define-syntax name
         (syntax-rules ()
           ((name . rest-of-pattern) expr . rest))))))
      
  (define-syntax debug-macro
    (syntax-rules ()
      ((_ macro)
       (print (strip-syntax (expand (quote macro)))))))

  (define-syntax with-json-request-vars
    (syntax-rules ()
      ((_ (var ...) expr . rest) 
       (let ((request-vars (json-request-vars)))
         (let ((var (request-var (quote var) request-vars)) ...)
           (begin expr . rest))))))

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
      ((_ route (var . vars) expr . rest)
       (define-route route 'GET (var . vars) expr . rest))))

  (define-syntax post
    (syntax-rules ()
      ((_ route (var . vars) expr . rest)
       (define-route route 'POST (var . vars) expr . rest))))

)
