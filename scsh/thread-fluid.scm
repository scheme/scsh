; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees. See file COPYING.
;;; This is based on code from the scsh 0.6 distribution. There was not much
;;; documentation behind the implementation or uses of thread-fluids, but
;;; I tried to make sure that this emulates the original behavior. Unfortunately,
;;; at the moment, this implementation is very dangerous and space inefficient.
;;; It'll be used for now however, just to get things rolling.
;;;
;;; When a thread-fluid is made with MAKE-THREAD-FLUID, the given value is set
;;; as the default. Setting new values to the thread-fluid will not change
;;; the default's value. When a new thread is created using SPAWN, the
;;; thread-fluid's value will be the default, reguardless of what that
;;; thread-fluid's value was set to in any other thread. A thread-fluid's value
;;; can only be preserved across threads if it is created with
;;; MAKE-PRESERVED-THREAD-FLUID, and the child thread is created with
;;; FORK-THREAD or SPOON.
;;;
;;; Thread cells were a part of the implementation of thread-fluids in scsh,
;;; but they were actually a field in the thread record type, and so unique
;;; to that thread and safe to update. They are replaced here with a global
;;; list associating thread uids with values. These values are never collected
;;; because the global list can't be automatically updated once a thread dies.
;;; There is no mechanism used to keep the list safe for use by multiple
;;; threads either. I'm not spending time thinking of a better solution for now,
;;; because I'd like to have scsh stop using thread-fluids altogether if I can
;;; later.
;;;
;;; -Roderic

(define-record-type :thread-fluid
  (really-make-thread-fluid cell)
  thread-fluid?
  (cell thread-fluid-cell set-thread-fluid-cell!))

(define *no-fluid-value* (list 'no-fluid-value))

(define (thread-fluid thread-fluid)
  (thread-cell-ref (thread-fluid-cell thread-fluid)))

(define (set-thread-fluid! thread-fluid val)
  (thread-cell-set! (thread-fluid-cell thread-fluid) val))

(define (let-thread-fluid t-fluid val thunk)
  (let ((old-val (thread-fluid t-fluid)))
    (dynamic-wind
     (lambda () (set-thread-fluid! t-fluid val))
     thunk
     (lambda () (set-thread-fluid! t-fluid old-val)))))

(define (let-thread-fluids . args)
  (call-with-values
   (lambda ()
     (let loop ((args args) (rev-old-vals '()))
       (if (null? (cdr args))
           (values (car args) (reverse rev-old-vals))
           (loop (cddr args)
                 (cons (thread-fluid (car args))
                       rev-old-vals)))))
   (lambda (thunk old-vals)
    (dynamic-wind
     (lambda ()
       (let loop ((args args))
         (if (not (null? (cdr args)))
             (begin
               (set-thread-fluid! (car args) (cadr args))
               (loop (cddr args))))))
     thunk
     (lambda ()
       (let loop ((args args) (old-vals old-vals))
         (if (not (null? (cdr args)))
             (begin
               (set-thread-fluid! (car args) (car old-vals))
               (loop (cddr args) (cdr old-vals))))))))))

(define (make-thread-fluid top)
  (really-make-thread-fluid (make-thread-cell top)))

(define *preserved-fluids* (make-population))

(define (make-preserved-thread-fluid top)
  (let* ((t-fluid (make-thread-fluid top)))
    (add-to-population! t-fluid *preserved-fluids*)
    t-fluid))

(define (preserve-thread-fluids thunk)
  (let ((args (list thunk)))
    (walk-population
     (lambda (t-fluid)
       (set! args
             (cons t-fluid
                   (cons (thread-fluid t-fluid)
                         args))))
     *preserved-fluids*)
    (lambda ()
      (apply let-thread-fluids args))))

(define (fork-thread thunk . rest)
  (apply spawn (preserve-thread-fluids thunk) rest))

(define spoon fork-thread)

;;; Thread cells

(define-record-type :thread-cell
  (make-thread-cell default)
  (default thread-cell-default))

(define *thread-cell-envs* '())

(define (get-thread-cell-env)
  (cond
   ((assq (thread-uid (current-thread)) *thread-cell-envs*) => cdr)
   (else '())))

(define (set-thread-cell-env! value)
  (let ((probe (assq (thread-uid (current-thread)) *thread-cell-envs*)))
    (if probe
        (set-cdr! probe value)
        (set! *thread-cell-envs* (cons (cons (thread-uid (current-thread)) value)
                                       *thread-cell-envs*)))))

(define (empty-thread-cell-env) '())

(define (thread-cell-ref thread-cell)
  (let ((probe (assq thread-cell (get-thread-cell-env))))
    (if probe
        (cdr probe)
        (thread-cell-default thread-cell))))

(define (thread-cell-set! thread-cell value)
  (let ((probe (assq thread-cell (get-thread-cell-env))))
    (if probe
        (set-cdr! probe value)
        (set-thread-cell-env! (cons (cons thread-cell
                                           value)
                                     (get-thread-cell-env))))))
