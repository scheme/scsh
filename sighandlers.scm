;;; Copyright (c) 1993 by Olin Shivers.
;;; Signal handler system

;;; The principal trickiness here is that we have to interface to Unix signals
;;; *through* an intermediate interface, the S48 vm's idea of interrupts.
;;; So there is a difference between delivering a signal to the underlying
;;; Unix process and delivering it to the program that runs on the VM.
;;;
;;; One effect is that we have two separate codes for the same thing -- the
;;; Unix signal code, and the S48 interrupt value. E.g., SIGNAL/TSTP and
;;; INTERRUPT/TSTP.

;;; These system calls can return EINTR or restart. In order for the S48 vm's
;;; interrupt system to detect a signal and invoke the handler, they *must*
;;; return EINTR, and this must cause a return from C to Scheme.
;;;
;;; open close dup2 accept connect
;;; read recv recvfrom recvmsg
;;; write send sendto sendmsg
;;; select
;;; wait
;;; fcntl* ioctl
;;; sigsuspend
;;; HP-UX, but I don't use: poll lockf msem_lock msgsnd msgrcv semop
;;;
;;; * Only during a F_SETLKW
;;;
;;; From rts/interrupt.scm (package interrupts, interface interrupts-interface)
;;;     WITH-INTERRUPTS INTERRUPT-HANDLERS SET-ENABLED-INTERRUPTS !
;;;	ENABLED-INTERRUPTS
;;; Must define WITH-INTERRUPTS* and WITH-INTERRUPTS.

;;; Map a Unix async signal to its S48 interrupt value.
;;; -1 => Not defined.
(import-dynamic-externals "/home/roderic/scsh/sighandlers1")
(import-lambda-definition-2 %signal->interrupt (sig) "sig2interrupt")

(define (signal->interrupt sig)
  (let ((int (%signal->interrupt sig)))
    (if (>= int 0) int
	(error "Unix signal has no Scheme 48 interrupt." sig))))


(define (interrupt-enabled? int mask)
  (interrupt-in-set? int mask))

(define (interrupt-enable int mask)
  (insert-interrupt int mask))

(define *enabled-interrupts*
   (let lp ((i 0) (mask 0))
     (if (= i number-of-interrupts)
	 mask
	 (lp (+ i 1) (interrupt-enable i mask)))))

(define (enabled-interrupts) *enabled-interrupts*)

(define *pending-interrupts* 0)

(define (interrupt-pending? int)
  (interrupt-in-set? int *pending-interrupts*))

(define (make-interrupt-pending int)
  (set! *pending-interrupts* (insert-interrupt int *pending-interrupts*)))

(define (remove-pending-interrupt int)
  (set! *pending-interrupts* (remove-interrupt int *pending-interrupts*)))

;;; I'm trying to be consistent about the ! suffix -- I don't use it
;;; when frobbing process state. This is not a great rule; perhaps I
;;; should change it.
;;;
;;; I think you should...
(define (set-enabled-interrupts new-enabled-interrupts)
  (let ((old-enabled-interrupts *enabled-interrupts*))
    ;;; set it here so the handlers see the correct value
    (set! *enabled-interrupts* new-enabled-interrupts)
    (do ((int 0 (+ int 1)))
	((= int number-of-interrupts) new-enabled-interrupts)
      (let ((old-state (interrupt-enabled? int old-enabled-interrupts))
	    (new-state (interrupt-enabled? int new-enabled-interrupts)))
	(if (and (not old-state) new-state (interrupt-pending? int))
	    (begin
	      (remove-pending-interrupt int)
	      (call-interrupt-handler int)))))))

(define-simple-syntax (with-enabled-interrupts interrupt-set body ...)
   (begin
     (with-enabled-interrupts* interrupt-set (lambda () body ...))))

(define (with-enabled-interrupts* interrupt-set thunk)
  (let ((before *enabled-interrupts*))
    (set-enabled-interrupts interrupt-set)
    (let ((return (thunk)))
      (set-enabled-interrupts before)
      return)))

(define *interrupt-handlers-vector*)

(define (install-fresh-interrupt-handlers-vector!)
  (set! *interrupt-handlers-vector* (make-vector number-of-interrupts #t)))

(define (interrupt-handlers-vector)
  *interrupt-handlers-vector*)

(define (interrupt-handler-ref int)
  (if (or (< int 0) (>= int number-of-interrupts))
      (error "ill signum in interrupt-handler-ref" int)
      (vector-ref *interrupt-handlers-vector* int)))

(define (call-interrupt-handler int)
  (let ((handler (interrupt-handler-ref int)))
    (case handler
      ((#t) ((vector-ref default-int-handler-vec int) (enabled-interrupts)))
      ((#f) (if #f #f))
      (else (handler (enabled-interrupts))))))


;;; Get/Set signal handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When you set a signal's handler to "default," if the default for that
;;; signal is something other than "ignore," we actually install this guy.
;;; When he is called by the S48 interrupt system, he'll magically make
;;; the default action happen (by calling C code that *really* sets the
;;; handler to SIGDFL, and then re-sending the signal). This basically
;;; terminates the process, since if the default isn't "ignore," it's always
;;; "terminate" of some kind. Doing it this way means the exit code given
;;; to our waiting parent proc correctly reflects how we died, and also
;;; makes the core dump happen if it should. Details, details.

(import-lambda-definition %do-default-sigaction (signal) "do_default_sigaction")

(define default-int-handler-vec
  ;; Non-Unix-signal interrupts just get their default values from
  ;; the current value of I-H.
  (let ((v (make-vector 32)))
    (do ((sig 31 (- sig 1)))			; For each Unix signal
	((< sig 0))				; make & install a default
      (let ((i (%signal->interrupt sig)))	; signal handler.
	(if (>= i 0)	; Don't mess with non-signal interrupts.
	    (vector-set! v i (if (memv sig signals-ignored-by-default)
				 (lambda (enabled-interrupts) #f)
				 (lambda (enabled-interrupts)
				   (%do-default-sigaction sig)))))))
    v))


;;; HANDLER is #f (ignore), #t (default), or a procedure taking an integer
;;; argument. The interrupt is delivered to a procedure by (1) setting the
;;; ENABLED-INTERRUPTS register to 0 (i.e., blocking all interrupts), and (2)
;;; applying the procedure to the previous value of the ENABLED-INTERRUPTS
;;; register. If the procedure returns normally, the ENABLED-INTERRUPTS
;;; register will be restored to its previous value.

(define (set-interrupt-handler int handler)
  (if (or (< int 0) (>= int number-of-interrupts))
      (error "ill signum in set-interrupt-handler!" int)
      (let ((old-handler (vector-ref *interrupt-handlers-vector* int)))
	(vector-set! *interrupt-handlers-vector* int handler)
	old-handler)))

(define (interrupt-handler int)
  (interrupt-handler-ref int))

(define (with-scsh-sighandlers interactive? thunk)
  (install-fresh-interrupt-handlers-vector!)
  (do ((sig 32 (- sig 1)))
      ((< sig 0))
    (let ((i (%signal->interrupt sig)))
      (if (not (or (= i -1)
		   (= sig signal/alrm)))	; Leave alarm handler alone.
	  (set-interrupt-handler
	   i
	   #t))))
  (let ((scsh-initial-thread  ((structure-ref threads current-thread))))
    (if (not (eq? (thread-name scsh-initial-thread)
		  'scsh-initial-thread))
	(error "sighandler did not find scsh-initial-thread, but"
	       scsh-initial-thread))

    ;; Note: this will prevent any other system to work, since it pushes
    ;; a new command level !
    (if interactive?
	(set-interrupt-handler interrupt/keyboard
			       (lambda stuff
				 ((structure-ref threads-internal schedule-event)
				  scsh-initial-thread
				  (enum
				   (structure-ref threads-internal event-type)
				   interrupt)
				  (enum interrupt keyboard))))))
  (run-as-long-as
   deliver-interrupts
   thunk
   (structure-ref threads-internal spawn-on-root)
   'deliver-interrupts))

(define (deliver-interrupts)
  (let lp ((last ((structure-ref sigevents most-recent-sigevent))))
    (let* ((event ((structure-ref sigevents next-sigevent-set)
		   last full-interrupt-set))
	   (interrupt ((structure-ref sigevents sigevent-type) event)))
      (if (interrupt-enabled? interrupt (enabled-interrupts))
	  (call-interrupt-handler interrupt)
	  (make-interrupt-pending interrupt))
      (lp event))))

;;; Dealing with synchronous signals

(import-lambda-definition ignore-signal (sig) "ignore_signal")

(import-lambda-definition handle-signal-default (sig) "handle_signal_default")

;;; I am ashamed to say the 33 below is completely bogus.
;;; What we want is a value that is 1 + max interrupt value.

(define int->sig-vec
  (let ((v (make-vector 33 #f)))
    (do ((sig 32 (- sig 1)))
	((< sig 0))
      (let ((i (%signal->interrupt sig)))
	(if (not (= i -1)) (vector-set! v i sig))))
    v))

(define (int->signal i) (and (<= 0 i 32) (vector-ref int->sig-vec i)))
