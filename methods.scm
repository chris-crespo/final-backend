(module methods 
  (json-request-vars request-var define-json
   get post)

  (import scheme)
  (cond-expand 
    (chicken-4
      (import (except chicken get) extras)
      (use awful spiffy spiffy-request-vars
           medea intarweb))
    (chicken-5
      (import (chicken io))
      (import awful spiffy spiffy-request-vars 
              medea intarweb)))

  (define (json-request-vars)
    (let* ((header (request-headers (current-request)))
           (len (header-value 'content-length header))
           (content (read-string len (request-port (current-request)))))
      (read-json content)))

  (define (request-var var vars)
    (cond ((assoc var vars) => cdr)
          (else #f)))

  (define (json-response obj)
    `(literal ,(json->string obj)))

  (define (define-json route method thunk)
    (define-page route
      (lambda ()
        (awful-response-headers '((allow "*")
                                  (content-type "application/json")
                                  (access-control-allow-origin "*")
                                  (access-control-allow-methods "*")
                                  (allow-headers "content-type")))
        (json-response (thunk)))
      no-template: #t
      method: `(,method)))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((_ (name . rest-of-pattern) expr . rest)
       (define-syntax name
         (syntax-rules ()
           ((name . rest-of-pattern) 
            (begin expr . rest)))))))
      
  (define-syntax-rule (debug-macro macro)
    (print (strip-syntax (expand (quote macro)))))

  (define-syntax-rule (with-json-request-vars (var ...) expr . rest) 
    (let ((request-vars (json-request-vars)))
      (let ((var (request-var (quote var) request-vars)) ...)
        (begin expr . rest))))

  (define-syntax-rule (get route (var . vars) expr . rest)
    (begin
      (define-json route 'GET 
        (lambda ()
          (with-request-vars (var . vars)
            (begin expr . rest))))))

  (define-syntax-rule (post route (var . vars) expr . rest)
    (define-json route 'OPTIONS (lambda () #t))
    (define-json route 'POST
      (lambda ()
        (with-json-request-vars (var . vars)
          (begin expr . rest)))))
)
