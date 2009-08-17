;;; Tail-weak tables. This is for internal use only, for real life
;;; applications we have to provide a more general interface with the
;;; 3 combinations of head and tail weakness.

(define make-weak-table make-table)

(define (weak-table-set! table number set-me)
  (table-set! table number (if set-me (make-weak-pointer set-me) #f)))

(define (weak-table-ref table number)
  (let ((ref (table-ref table number)))
    (if (weak-pointer? ref)
	(let ((val (weak-pointer-ref ref)))
	  (if val
	      val
	      (begin (table-set! table number #f)
		     #f)))
	  ref)))

(define (weak-table-walk proc table)
  (table-walk
   (lambda (number value)
     (proc number (if (weak-pointer? value) (weak-pointer-ref value) value)))
   table))

(define (strengthen-weak-table-ref table number)
  (table-set! table number (weak-table-ref table number)))

(define (weaken-weak-table-ref table number)
  (weak-table-set! table number (weak-table-ref table number)))
