(cond-expand 
  (chicken-4
    (import (except chicken get))
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
            medea)
    (import methods)))

(enable-db)
(db-credentials (get-environment-variable "DATABASE_URL"))

(define connection (connect (db-credentials)))
(on-exit (lambda () (disconnect connection)))

(query connection "prepare get_user as select email from app_user 
                   where username = $1 or email = $2")
(query connection "prepare post_user as 
                   insert into app_user values ($1, $2, $3, $4, $5, $6)")
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
    (row-alist (query connection 'get_user username email))))

(define (password-matches? user password)
  ;; User must be an alist obtained by calling row-alist
  (string=? (cdr (assoc 'password user )) password))

(define post-user
  (lambda user-data
    ;; TODO: Hash password before storing in db
    (apply query connection 'post_user user-data)))

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
    (email . ,(email-available? email))))

(get "api/verify" (username email password)
  `((verified . ,(exists? username email))))

(get "api/auth" (username email password)
  (let ((user (get-user username email)))
  `((user . (not (sql-null? user)))
    (password . (password-matches? user password)))))

(post "api/register" (username email password first-name last-name phone-number)
  ;; TODO: Success should depend on user being stored successfully
  (post-user username email password first-name last-name phone-number)
  `((success . #t)))
