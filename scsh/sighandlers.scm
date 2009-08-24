;;; Copyright (c) 2009 by Roderic Morris
;;; Signal handler system

;;; This is a rewrite of the original scsh signal handler system, using the
;;; posix api in scheme48.

;;;; Signal Operations

;;; (ENABLED-SIGNALS) -> list
;;; (SET-ENABLED-RESTARTS! list)
;;;   ENABLED-SIGNALS returns a list of the signals currently enabled, which
;;;   includes all of the signals recognized by scheme48 by default.
;;;   SET-ENABLED-RESTARTS sets the currently enabled signals to the given
;;;   list. Enabling a signal allows it to be delivered to its signal handler.
;;;   When a disabled signal is delivered, it is held pending until it becomes
;;;   enabled, at which time it's handler is invoked
;;;
;;; (WITH-ENABLED-SIGNALS list . body) -> values  (syntax)
;;; (WITH-ENABLED-SIGNALS* list thunk) -> values
;;;   These both run the given code with the list of signals in list enabled.
;;;
;;; (SET-SIGNAL-HANDLER! signal handler) -> old-handler
;;;   handlers can be #f (ignore), #t (default), or a procedure taking a signal
;;;   argument.

;;; When you set a signal's handler to "default," if the default for that
;;; signal is something other than "ignore," we actually install this guy.
;;; When he is called, he'll magically make the default action happen (by
;;; calling C code that *really* sets the handler to SIGDFL, and then re-sending
;;; the signal). This basically terminates the process, since if the default
;;; isn't "ignore," it's always "terminate" of some kind. Doing it this way
;;; means the exit code given to our waiting parent proc correctly reflects
;;; how we died, and also makes the core dump happen if it should. Details, details.

(import-lambda-definition-2 %do-default-sigaction (signal) "do_default_sigaction")

(define *s48-signals*
  ;; kill and stop are omitted because they can't be caught.
  (let ((potential (list 'abrt 'alrm 'fpe
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


(define *enabled-signals*
  (list-copy *s48-signals*))

(define (enabled-signals) *enabled-signals*)

(define *pending-signals* '())

(define (make-signal-pending signal)
  (set! *pending-signals* (cons signal *pending-signals*)))

(define (signal-enabled signal signal-list)
  (memq signal signal-list))

(define (set-enabled-signals! new-enabled-signals)
  (let ((old-enabled-signals *enabled-signals*))
    (set! *enabled-signals* new-enabled-signals)
    (let loop ((pending-signals *pending-signals*)
               (new-pending '()))
      (if (null? pending-signals)
          (set! *pending-signals* new-pending)
          (let ((pending (car pending-signals)))
            (if (and (signal-enabled pending new-enabled-signals)
                     (not (signal-enabled pending old-enabled-signals)))
                (begin
                  (call-signal-handler pending)
                  (loop (cdr pending-signals) new-pending))
                (loop (cdr pending-signals) (cons pending new-pending))))))))

(define (with-enabled-signals* new-signals thunk)
  (let ((before *enabled-signals*))
    (set-enabled-signals! new-signals)
    (let ((return (thunk)))
      (set-enabled-signals! before)
      return)))

(define-syntax with-enabled-signals
  (syntax-rules ()
    ((with-enabled-signals signal-list body ...)
     (with-enabled-signals* signal-list (lambda () body ...)))))

(define *signal-handlers*
  (map (lambda (signal) (cons signal #t)) *s48-signals*))

(define (install-fresh-signal-handlers!)
  (set! *signal-handlers*
        (map (lambda (signal) (cons signal #t)) *s48-signals*)))

(define *signal-queue* (make-signal-queue *s48-signals*))

(define (call-signal-handler signal)
  (let ((handler-pair (assoc signal *signal-handlers*)))
    (if handler-pair
        ((car handler-pair) signal)
        (error "call-signal-handler"
               "no signal handler set for the given signal" signal))))

(define (set-signal-handler! signal handler)
  (let ((handler-pair (assoc signal *signal-handlers*))
        (handler (case handler
                   ((#t) (lambda (signal)
                           (%do-default-sigaction (signal-os-number signal))))
                   ((#f) (lambda (signal) #f))
                   (else handler))))
    (if handler-pair
        (let ((old-handler (car handler-pair)))
          (set-cdr! handler-pair handler)
          old-handler)
        ;; The original code threw an error in it's equivalent of this point,
        ;; but I think since we know signal is valid by virtue of being a signal
        ;; type, adding a new handler and watching for this new signal is a betteer
        ;; policy.
        (begin
          (set! *signal-handlers* (alist-cons signal handler *signal-handlers*))
          (add-signal-queue-signal! *signal-queue* signal)
          ;; Since we weren't following this signal before, it was probably doing
          ;; the default.
          #t))))

(define (with-scsh-sighandlers interactive? thunk)
  (install-fresh-signal-handlers!)
  (let ((scsh-initial-thread  (current-thread)))
    (if (not (eq? (thread-name scsh-initial-thread)
                  'scsh-initial-thread))
        (error "with-scsh-sighandlers"
               "sighandler did not find scsh-initial-thread"
               scsh-initial-thread))

    ;; Note: this will prevent any other system to work, since it pushes
    ;; a new command level !
    (if interactive?
        (set-signal-handler! (signal int)
                             (lambda (signal)
                               (schedule-event
                                scsh-initial-thread
                                (enum event-type interrupt)
                                (enum interrupt keyboard))))))
  (run-as-long-as
   deliver-interrupts
   thunk
   spawn-on-root
   'deliver-interrupts))

(define (deliver-interrupts)
  (let loop ((signal (dequeue-signal! *signal-queue*)))
    (if (signal-enabled signal *enabled-signals*)
        (call-signal-handler signal)
        (make-signal-pending signal))
    (loop (dequeue-signal! *signal-queue*))))
