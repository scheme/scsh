;;; Copyright (c) 1994 by Olin Shivers.

;;; Handy for defining random flags and constants.
;;; (define-enum-constant "errno" intr 9) => (define errno/intr 9)

;;; This is deeply bogus code. It merely serves to demonstrate what a loser
;;; I am when it comes to serious modern-tech macrology.

;;; The question: is / the best separator? Alternates: $ . |


;;; (define-enum-constant fruit apple 1) =>
;;; (define fruit/apple 1)

(define-syntax define-enum-constant
  (lambda (form rename compare)
    (let* ((%define (rename 'define))
	   (base (let ((b (cadr form)))
		   (cond ((string? b) b)
			 ((symbol? b) (symbol->string b))
			 (else (error "Enum constant base must be symbol or string"
				      b )))))
	  (var (string->symbol (string-append base "/"
					      (symbol->string (caddr form)))))
	  (value (cadddr form)))
      `(,%define ,var ,value))))

;;; (define-enum-constants fruit (apple 1) (orange 2))
;;; => (begin (define-enum-constant fruit apple  1)
;;;           (define-enum-constant fruit orange 2))

(define-syntax define-enum-constants
  (syntax-rules ()
    ((define-enum-constants set (elt val) ...)
     (begin (define-enum-constant set elt val) ...))))

(define-syntax define-enum-constants-from-x
  (syntax-rules 
   ()
   ((define-enum-constants-from-zero x set ())
    (begin))
   ((define-enum-constants-from-zero x set (elt1 elt2 ...))
    (begin (define-enum-constant set elt1 x)
	   (define-enum-constants-from-x (+ x 1) set (elt2 ...))))))

(define-syntax define-enum-constants-from-zero
  (syntax-rules 
   ()
   ((define-enum-constants-from-zero set (elt ...))
    (define-enum-constants-from-x 0 set (elt ...)))))
