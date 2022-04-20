(cond-expand 
  (chicken-4
    (import chicken)
    (use awful awful-postgresql
         postgresql sql-null
         medea)
    (use methods))
  (chicken-5
    (import (chicken base) 
            (chicken format) 
            (chicken process-context))
    (import awful awful-postgresql 
            postgresql sql-null 
            medea)))

(enable-db)
(db-credentials (get-environment-variable "DATABASE_URL"))

(define connection (connect (db-credentials)))
(on-exit (lambda () (disconnect connection)))

(query connection "prepare get_user as select email from app_user 
                   where username = $1 or email = $2")
(query connection "prepare post_user as 
                   insert into app_user (username, email) values ($1, $2)")
(query connection "prepare username_available as 
                   select username from app_user where username = $1")
(query connection "prepare email_available as 
                   select email from app_user where email = $1")

(define array-as-list-parser
  (cons 'array (lambda (x) x)))
(json-parsers (cons array-as-list-parser (json-parsers)))

(define (sql-null-unparser v)
  (write-json 'null))
(json-unparsers (cons (cons sql-null? sql-null-unparser) 
                      (json-unparsers)))

(define (name-cols names row)
  (map cons names row))

(define get-user
  (lambda (username email)
    (query connection 'get_user username email)))

(define post-user
  (lambda (username email)
    (query connection 'post_user username email)))

(define (available? colname)
  (let ((statement-name 
          (string->symbol (format "~a_available" colname))))
    (lambda (value) 
      (zero? (row-count (query connection statement-name value))))))

(define username-available? (available? "username"))
(define email-available? (available? "email")) 

(define (exists? username email)
  (not (available? username email)))

(get "api/available" (username email)
  `((username . ,(username-available? username))
    (emai . ,(email-available? email))))

(get "api/verify" (username email password)
  `((verified . ,(exists? username email))))

(post "api/register" (username email password first-name)
  (post-user username email))
