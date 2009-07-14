; Copyright (c) 1999-2001 by Martin Gasbichler. See file COPYING.

;;; Extend the functions of the RTS
;;; Functional event system.
;;; System by Olin Shivers, implementation by Martin Gasbichler

(define-record-type sigevent :sigevent
  (really-make-sigevent type next)
  sigevent?
  (type sigevent-type set-sigevent-type!)
  (next sigevent-next set-sigevent-next!))

(define (make-sigevent type)
  (really-make-sigevent type #f))

(define empty-sigevent (make-sigevent #f))

(define *most-recent-sigevent* empty-sigevent)

(define (most-recent-sigevent) *most-recent-sigevent*)

(define sigevent-thread-queue #f)

;Wait for an sigevent of a certain type.
(define (rts-next-sigevent pre-sigevent set type-in-set?)
  (with-interrupts-inhibited
   (lambda ()
     (let lp ((pre-sigevent pre-sigevent))
       (let ((sigevent (sigevent-next pre-sigevent)))
	 (if sigevent
	     (if (type-in-set? (sigevent-type sigevent) set)
		 sigevent
		 (lp sigevent))
	     (begin (block-on-queue sigevent-thread-queue)
		    (lp pre-sigevent))))))))

; same as above, but don't block
(define (rts-next-sigevent/no-wait pre-sigevent set type-in-set?)
  (let ((sigevent (sigevent-next pre-sigevent)))
    (if sigevent
	(if (type-in-set? (sigevent-type sigevent) set)
	    sigevent
	    (rts-next-sigevent/no-wait (sigevent-next sigevent)
				       set
				       type-in-set?))
	#f)))


;Called when the interrupt actually happened.
;;; TODO w-i-i is problaly not necessary since they're off already
(define (register-interrupt type)
  (let ((waiters (with-interrupts-inhibited
		  (lambda ()
		    (set-sigevent-next! *most-recent-sigevent* (make-sigevent type))
		    (set! *most-recent-sigevent* (sigevent-next *most-recent-sigevent*))
		    (do ((waiters '() (cons (maybe-dequeue-thread! sigevent-thread-queue)
					    waiters)))
			((thread-queue-empty? sigevent-thread-queue)
			 waiters))))))
    (for-each make-ready waiters)))


;;; Records whether the sigevent system is running.
;;; If set to #f we ignore threads waiting for a sigevent.
(define sigevents-running? #f)

;;; has to be called with interrupts disabled
(define (waiting-for-sigevent?)
  (if sigevents-running?
      (not (thread-queue-empty? sigevent-thread-queue))
      #f))

(define (with-sigevents thunk)
  (set! sigevent-thread-queue (make-queue))
  (set-interrupt-handler! (enum interrupt os-signal)
			  (lambda (type enabled-interrupts)
					; type is already set in the unix signal handler
			    (register-interrupt type)))
  (set-interrupt-handler! (enum interrupt keyboard)
			  (lambda (enabled-interrupts)
			    (register-interrupt (enum interrupt keyboard))))
  (dynamic-wind
   (lambda ()
     (set! sigevents-running? #t))
   thunk
   (lambda ()
    (set! sigevents-running? #f))))


;;; the vm uses the timer for the scheduler
(define (schedule-timer-interrupt! msec)
  (spawn (lambda ()
	   (sleep msec)
	   (register-interrupt (enum interrupt alarm)))))

(define (next-sigevent pre-event type)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent pre-event type eq?))

(define (next-sigevent-set pre-event set)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent pre-event set interrupt-in-set?))

(define (next-sigevent/no-wait pre-event type)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent/no-wait pre-event type eq?))

(define (next-sigevent-set/no-wait set pre-event)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent/no-wait pre-event set interrupt-in-set?))

