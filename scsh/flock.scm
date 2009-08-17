;;; Scsh
;;; Posix advisory record-locking for file descriptors.
;;; These procs may only be applied to integer file descriptors; 
;;; they may not be applied to ports.
;;; Copyright (c) 1995 by David Albertz and Olin Shivers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; C syscall interface
;;;;;;;;;;;;;;;;;;;;;;;

(import-os-error-syscall %set-lock (fd cmd type whence start len) "set_lock")
(import-os-error-syscall %get-lock (fd cmd type whence start len) "get_lock")

;;; The LOCK record type
;;;;;;;;;;;;;;;;;;;;;;;;

(define-record %lock-region
  exclusive?
  start			; integer
  len			; Positive integer or #f
  whence		; seek/set, seek/delta, or seek/end.
  proc      		; Process holding lock
  )

(define lock-region?               %lock-region?)
(define lock-region:exclusive?     %lock-region:exclusive?)
(define lock-region:whence         %lock-region:whence)
(define lock-region:start          %lock-region:start)
(define lock-region:len            %lock-region:len)
(define lock-region:proc           %lock-region:proc)
(define set-lock-region:exclusive? set-%lock-region:exclusive?)
(define set-lock-region:whence     set-%lock-region:whence)
(define set-lock-region:start      set-%lock-region:start)
(define set-lock-region:len        set-%lock-region:len)
(define set-lock-region:proc       set-%lock-region:proc)

;;; Backwards compatibility for one or two releases.
(define lock-region:pid
  (deprecated-proc (lambda (lr)
		     (cond ((lock-region:proc lr) => proc:pid)
			   (else #f)))
		   'lock-region:pid
		   "Use lock-region:proc instead."))

(define (make-lock-region exclusive? start len . maybe-whence)
  (let ((whence (:optional maybe-whence seek/set)))
    (make-%lock-region exclusive? start len whence #f)))


;;; Internal middleman routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (call-lock-region proc cmd fdes lock)
  (check-arg lock-region? lock proc)
  (let ((lock-type (if (lock-region:exclusive? lock) lock/write lock/read)))
    (proc fdes cmd lock-type
	  (lock-region:whence lock)
	  (lock-region:start lock)
	  (lock-region:len lock))))


;;; The main routines
;;;;;;;;;;;;;;;;;;;;;

(define (lock-region fdes lock)
  (if (not (lock-region/no-block fdes lock))
      (begin
	(relinquish-timeslice)
	(lock-region fdes lock))))

;;; Return true/false indicating success/failure.

(define (lock-region/no-block fdes lock)
  (with-errno-handler
   ((errno data)
    ((errno/again errno/acces) #f))
   (call-lock-region %set-lock fcntl/set-record-lock-no-block fdes lock)
   #t))

;;; Return first lock that conflicts w/LOCK; if none, return #f.

(define (get-lock-region fdes lock)
  (apply (lambda (type whence start len pid)
	   (and (not (= type lock/release))
		(make-%lock-region (= type lock/write) start len whence
				   (pid->proc pid 'create))))
	 (call-lock-region %get-lock fcntl/get-record-lock fdes lock)))


(define (unlock-region fdes lock)
  (%set-lock fdes fcntl/set-record-lock lock/release
	     (lock-region:whence lock)
	     (lock-region:start lock)
	     (lock-region:len lock)))


;;; Locks with dynamic extent -- with and without sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Throwing out frees the lock. Don't throw back in.

(define (with-region-lock* fd lock thunk)
  (let ((returned? #f))
    (dynamic-wind (lambda ()
		    (if returned?
			(error "Can't throw back into a with-region-lock" lock)
			(lock-region fd lock)))
		  thunk
		  (lambda ()
		    (unlock-region fd lock)
		    (set! returned? #t)))))

(define-syntax with-region-lock
  (syntax-rules ()
    ((with-region-lock fd lock body ...)
     (with-region-lock* fd lock (lambda () body ...)))))
