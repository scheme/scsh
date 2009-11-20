;;; Random useful utilities.
;;; Copyright (c) 1993 by Olin Shivers.

(define (mapv f v)
  (let* ((len (vector-length v))
	 (ans (make-vector len)))
    (do ((i 0 (+ i 1)))
	((= i len) ans)
      (vector-set! ans i (f (vector-ref v i))))))

(define (mapv! f v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1)))
	((= i len) v)
      (vector-set! v i (f (vector-ref v i))))))

(define (vector-every? pred v)
  (let lp ((i (- (vector-length v) 1)))
    (or (< i 0)
	(and (pred (vector-ref v i))
	     (lp (- i 1))))))

(define (copy-vector v)
  (let* ((len (vector-length v))
	 (ans (make-vector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0) ans)
      (vector-set! ans i (vector-ref v i)))))

(define (initialize-vector len init)
  (let ((v (make-vector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0) v)
      (vector-set! v i (init i)))))

(define (vector-append . vecs)
  (let* ((vlen (fold (lambda (v len) (+ (vector-length v) len)) 0 vecs))
	 (ans (make-vector vlen)))
    (let lp1 ((vecs vecs) (to 0))
      (if (pair? vecs)
	  (let* ((vec (car vecs))
		 (len (vector-length vec)))
	    (let lp2 ((from 0) (to to))
	      (cond ((< from len)
		     (vector-set! ans to (vector-ref vec from))
		     (lp2 (+ from 1) (+ to 1)))
		    (else (lp1 (cdr vecs) to)))))))
    ans))


(define (vfold kons knil v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1))
	 (ans knil (kons (vector-ref v i) ans)))
	((>= i len) ans))))

(define (vfold-right kons knil v)
  (do ((i (- (vector-length v) 1) (- i 1))
       (ans knil (kons (vector-ref v i) ans)))
      ((< i 0) ans)))


;;; We loophole the call to ERROR -- the point is that perhaps the
;;; user will interact with a breakpoint, and proceed with a new
;;; value, which we will then pass to a new invocation of CHECK-ARG
;;; for approval.
(define (check-arg pred val caller)
  (if (pred val) val
      (check-arg pred (error "Bad argument" val pred caller) caller)))

(define (deprecated-proc proc name . maybe-preferred-msg)
  (let ((warned? #f))
    (lambda args
      (cond ((not warned?)
	     (set! warned? #t)
	     (apply warn
		    "Deprecated procedure (may not be supported in a future release)"
		    name
		    maybe-preferred-msg)))
      (apply proc args))))


(define (real->exact-integer x)
  (let ((f (round x)))
    (if (inexact? f) (inexact->exact f) f)))

;----------------
; A record type whose only purpose is to run some code when we start up an
; image.

(define-record-type :reinitializer
  (make-reinitializer thunk)
  reinitializer?
  (thunk reinitializer-thunk))

(define-record-discloser :reinitializer
  (lambda (r)
    (list 'reinitializer (reinitializer-thunk r))))

(define-record-resumer :reinitializer
  (lambda (r)
    ((reinitializer-thunk r))))

;--------------
; Run thunk1 until thunk2 escapes
; This is *extremly* low level
; Don't use unless you know what you are doing

(define (run-as-long-as thunk1 thunk2 spawn-thread . name)
  (let ((thread (make-placeholder)))
    (apply spawn-thread
	   (lambda ()
	     (placeholder-set! thread (current-thread))
	     (thunk1))
	   name)
    (dynamic-wind
     (lambda () #t)
     thunk2
     (lambda ()
       (terminate-thread! (placeholder-value thread))))))

(define (obtain-all-or-none . locks)
  (let lp ((obtained '()) (needed locks))
    (if (not (null? needed))
	(let ((next (car needed)))
	  (if (maybe-obtain-lock next)
	      (lp (cons next obtained)
		  (cdr needed))
	      (begin
		(for-each release-lock obtained)
		(obtain-lock next)
		(lp (list next) (delete next locks eq?))))))))


