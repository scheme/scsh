; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees. See file COPYING.

(define-record-type thread-fluid :thread-fluid
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

; Thread cells

(define-record-type thread-cell :thread-cell
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
