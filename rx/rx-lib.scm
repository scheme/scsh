;;; Procedures that appear in code produced by (RX ...).

;;; In sexp syntax, a ,<exp> or ,@<exp> form may evaluate to a string, char,
;;; char-set, or regexp value. Coerce one of these to a regexp value.

(define (coerce-dynamic-regexp x)
  (cond
   ((string? x)   (make-re-string x))
   ((char? x)     (make-re-string (string x)))
   ((char-set? x) (make-re-char-set x))
   ((regexp? x) x)
   (else (error "Cannot coerce value to regular expression." x))))

;;; In a char-set context (e.g., as an operand of the SRE - operator), 
;;; a ,<exp> or form must be coercable to a char-set.

(define (coerce-dynamic-charset x)
  (cond
   ((string? x)
    (if (= 1 (string-length x)) (string->char-set x)
	(error "Multi-char string not allowed as ,<exp> or ,@<exp> SRE in char-class context."
	       x)))
   ((char? x)     (char-set x))
   ((char-set? x) x)
   ((re-char-set? x) (re-char-set:cset x))
   (else (error "Cannot coerce value to character set" x))))


(define (spec->char-set in? loose ranges)
  (let ((doit (lambda (loose ranges)
		(fold (lambda (r cset)
			(let ((from (char->ascii (car r)))
			      (to (char->ascii (cdr r))))
			  (do ((i from (+ i 1))
			       (cs cset (char-set-adjoin! cs (ascii->char i))))
			      ((> i to) cs))))
		      (string->char-set loose)
		      ranges))))
    (if in?
	(doit loose ranges)
	(char-set-complement! (doit loose ranges)))))

