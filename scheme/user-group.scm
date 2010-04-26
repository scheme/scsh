;;; User info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (user-info uid/name)
  ((cond ((string?  uid/name) name->user-info)
         ((integer? uid/name) (lambda (uid)
                                (user-id->user-info (integer->user-id uid))))
         (else (error "user-info arg must be string or integer" uid/name)))
   uid/name))

(define (user-info:name user-info)
  (os-string->string (user-info-name user-info)))

(define (user-info:uid user-info)
  (user-id->integer (user-info-id user-info)))

(define (user-info:gid user-info)
  (group-id->integer (user-info-group user-info)))

(define (user-info:home-dir user-info)
  (os-string->string (user-info-home-directory user-info)))

(define (user-info:shell user-info)
  (os-string->string (user-info-shell user-info)))

;;; Derived functions

(define (->uid uid/name)
  (user-info:uid (user-info uid/name)))

(define (->username uid/name)
  (user-info:name (user-info uid/name)))

(define (%homedir uid/name)
  (user-info:home-dir (user-info uid/name)))

(define home-directory "")

(define (init-home-directory home)
  (set! home-directory home))

(define (home-dir . maybe-user)
  (if (pair? maybe-user)
      (let ((user (car maybe-user)))
	(ensure-file-name-is-nondirectory
	    (or (%homedir user)
		(error "Cannot get user's home directory"
		       user))))
      home-directory))

;;; (home-file [user] fname)

(define (home-file arg1 . maybe-arg2)
  (receive (dir fname)
	   (if (pair? maybe-arg2)
	       (values (home-dir arg1) (car maybe-arg2))
	       (values home-directory  arg1))
    (string-append (file-name-as-directory dir) fname)))

;;; Group info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (group-info gid/name)
  ((cond ((string?  gid/name) name->group-info)
         ((integer? gid/name) (lambda (gid)
                                (group-id->group-info (integer->group-id gid))))
         (else (error "group-info arg must be string or integer" gid/name)))
   gid/name))

(define (group-info:name group-info)
  (os-string->string (group-info-name group-info)))

(define (group-info:gid group-info)
  (group-id->integer (group-info-id group-info)))

(define (group-info:members group-info)
  (map os-string->string (group-info-members group-info)))

;;; Derived functions

(define (->gid name)
  (group-info:gid (group-info name)))

(define (->groupname gid)
  (group-info:name (group-info gid)))
