(cond-expand 
  (chicken-4
    (import (except chicken get))
    (use awful awful-postgresql
         postgresql sql-null
         medea crypt)
    (use methods))
  (chicken-5
    (import (chicken base) 
            (chicken format) 
            (chicken process-context))
    (import awful awful-postgresql 
            postgresql sql-null 
            medea crypt)
    (import methods)))

(enable-db)
(db-credentials (get-environment-variable "DATABASE_URL"))

(define connection (connect (db-credentials)))
(on-exit (lambda () (disconnect connection)))

(query connection "prepare get_user as select * from app_user 
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

(define (full-name user)
  (let ((first-name (cdr (assoc 'first_name user)))
        (last-name  (cdr (assoc 'last_name  user))))
    (string-append first-name " " last-name)))

(define (get-user username email)
  (query connection 'get_user username email))

(define (password-matches? user password)
  ;; User must be an alist obtained by calling row-alist
  (let ((hash (cdr (assoc 'password user))))
    (string=? hash (crypt password hash))))

(define (post-user username email password first-name last-name phone)
  (query connection 'post_user
         username email (crypt password) first-name last-name phone))

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

(get "api/user" (username email)
  (let ((user (row-alist (get-user username email))))
    `(,(assoc 'username user)
      ,(assoc 'email user)
      (name . ,(full-name user)))))

(get "api/auth" (username email password)
  (let* ((user (get-user username email))
         (user-exists? (> (row-count user) 0)))
  `((user . ,user-exists?)
    (password . ,(and user-exists? (password-matches? 
                                     (row-alist user) 
                                     password))))))

(post "api/register" (username email password first-name last-name phone-number)
  ;; TODO: Success should depend on user being stored successfully
  (post-user username email password first-name last-name phone-number)
  `((success . #t)))
