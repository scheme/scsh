(define (make-lock-file-name file-name)
  (string-append file-name ".lock"))

(define (release-dot-lock file-name)
  (with-errno-handler
   ((errno packet)
    (else #f))
   (delete-file (make-lock-file-name file-name))
   #t))

(define (maybe-obtain-dot-lock file-name)
  (let ((temp-name (create-temp-file file-name)))
    (with-errno-handler
     ((errno packet)
      ((exist)
       (delete-file temp-name)
       #f))
     (create-hard-link temp-name (make-lock-file-name file-name))
     (delete-file temp-name)
     #t)))

(define random
  (let ((crank (make-random (modulo (time) (- (expt 2 27) 1)))))
    (lambda (limit)
      (quotient (* (modulo (crank) 314159265)
		   limit)
		314159265))))

;; STALE-TIME is the minimum age of a lock to be broken
;; if #f, don't break the lock

(define (obtain-dot-lock file-name . args)
  (let-optionals args ((retry-seconds 1)
		       (retry-number #f)
		       (stale-time 300))
    (let ((lock-file-name (make-lock-file-name file-name))
	  (retry-interval (* retry-seconds 1000)))
      (let loop ((retry-number retry-number)
		 (broken? #f))
	(cond
	 ((maybe-obtain-dot-lock file-name)
	  (if broken?
	      'broken
	      #t))
	 ((and stale-time
	       (with-errno-handler
		((errno packet)
		 (else #f))
		(> (time)
		   (+ (file-last-status-change (make-lock-file-name file-name))
		      stale-time))))
	  (break-dot-lock file-name)
	  (loop retry-number #t))
	 (else
	  (sleep (+ (quotient (* retry-interval 3) 4)
		    (random (quotient retry-interval 2))))
	  (cond ((not retry-number)
		 (loop retry-number broken?))
		((> retry-number 0)
		 (loop (- retry-number 1) broken?))
		(else
		 #f))))))))

(define (break-dot-lock file-name)
  (with-errno-handler
   ((errno packet)
    ((noent) 'dont-care))
   (delete-file (make-lock-file-name file-name))))

(define (with-dot-lock* file-name thunk)
  (dynamic-wind
   (lambda ()
     (obtain-dot-lock file-name))
   (lambda ()
     (call-with-values thunk
		       (lambda a
			 (release-dot-lock file-name)
			 (apply values a))))
   (lambda ()
     (release-dot-lock file-name))))

(define-syntax with-dot-lock
  (syntax-rules ()
   ((with-dot-lock file-name body ...)
    (with-dot-lock* file-name (lambda () body ...)))))


