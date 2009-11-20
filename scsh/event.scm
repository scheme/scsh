; Copyright (c) 1999-2001 by Martin Gasbichler. See file COPYING.

;;; Extend the functions of the RTS
;;; Functional event system.
;;; System by Olin Shivers, implementation by Martin Gasbichler

(define-record-type sigevent :sigevent
  (really-make-sigevent type next)
  sigevent?
  (type sigevent-type set-sigevent-type!)
  (next sigevent-next set-sigevent-next!))

(define *s48-signals*
  ;; kill and stop are omitted because they can't be caught.
  ;; alrm seem to be used by scheme48
  (let ((potential (list 'abrt 'fpe
                         'hup 'ill 'int
                         'pipe 'quit
                         'segv 'term 'usr1
                         'usr2 'chld 'cont
                         'tstp 'ttin
                         'ttou 'bus 'trap
                         'iot 'emt 'sys
                         'stkflt 'urg 'io
                         'poll 'cld 'xcpu
                         'xfsz 'vtalrm 'prof
                         'pwr 'info 'lost
                         'winch 'unused)))
    ;; This omits any signals that aren't on the current platform.
    (filter (lambda (x) x)
            (map (lambda (name)
                   (let ((signal (name->signal name)))
                     (if (signal-os-number signal)
                         signal #f)))
                 potential))))

(define *signal-queue* (make-signal-queue *s48-signals*))

(define (deliver-signals)
  (let loop ()
    (let ((signal (dequeue-signal! *signal-queue*)))
      (register-signal signal)
      (loop))))

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
       (with-new-proposal (lose)
        (let ((sigevent (sigevent-next pre-sigevent)))
          (if sigevent
              (if (type-in-set? (sigevent-type sigevent) set)
                  sigevent
                  (lp sigevent))
              (if (maybe-commit-and-block-on-queue sigevent-thread-queue)
                  (lp pre-sigevent)
                  (lose)))))))))

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
(define (register-signal type)
  (let ((waiters (with-interrupts-inhibited
                  (lambda ()
                    (set-sigevent-next! *most-recent-sigevent* (make-sigevent type))
                    (set! *most-recent-sigevent* (sigevent-next *most-recent-sigevent*))
                    (do ((waiters '() (cons (maybe-dequeue-thread! sigevent-thread-queue)
                                            waiters)))
                        ((thread-queue-empty? sigevent-thread-queue)
                         waiters))))))
    (for-each (lambda (x)
                (with-new-proposal (lose)
                 (if (not (maybe-commit-and-make-ready x))
                     (lose))))
              waiters)))


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
  (dynamic-wind
   (lambda ()
     (set! sigevents-running? #t))
   (lambda ()
     (run-as-long-as deliver-signals thunk spawn 'deliver-signals))
   (lambda ()
    (set! sigevents-running? #f))))

(define (next-sigevent pre-event type)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent pre-event type signal=?))

;; (define (next-sigevent-set pre-event set)
;;   (if (not (sigevent? pre-event))
;;       (error "pre-event is not an event"))
;;   (rts-next-sigevent pre-event set interrupt-in-set?))

(define (next-sigevent/no-wait pre-event type)
  (if (not (sigevent? pre-event))
      (error "pre-event is not an event"))
  (rts-next-sigevent/no-wait pre-event type signal=?))

;; (define (next-sigevent-set/no-wait set pre-event)
;;   (if (not (sigevent? pre-event))
;;       (error "pre-event is not an event"))
;;   (rts-next-sigevent/no-wait pre-event set interrupt-in-set?))

