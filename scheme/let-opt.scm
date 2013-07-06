;;; Here is where we define the macros, using the expanders from let-opt-expanders.

;;; (LET-OPTIONALS  args ((var1 default1 [arg-test supplied?]) ...) body1 ...)
;;; (LET-OPTIONALS* args ((var1 default1 [arg-test supplied?]) ...) body1 ...)

(define-syntax let-optionals  expand-let-optionals)
(define-syntax let-optionals* expand-let-optionals*)

;;; (:optional rest-arg default-exp [test-pred])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for evaluating optional arguments and their defaults
;;; in simple procedures that take a *single* optional argument. It is
;;; a macro so that the default will not be computed unless it is needed.
;;;
;;; REST-ARG is a rest list from a lambda -- e.g., R in
;;;     (lambda (a b . r) ...)
;;; - If REST-ARG has 0 elements, evaluate DEFAULT-EXP and return that.
;;; - If REST-ARG has 1 element, return that element.
;;; - If REST-ARG has >1 element, error.
;;;
;;; If there is an TEST-PRED form, it is a predicate that is used to test
;;; a non-default value. If the predicate returns false, an error is raised.

(define-syntax :optional
  (syntax-rules ()
    ((:optional rest default-exp)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
	   (if (null? (cdr maybe-arg)) (car maybe-arg)
	       (error "too many optional arguments" maybe-arg))
	   default-exp)))

    ((:optional rest default-exp arg-test)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
	   (if (null? (cdr maybe-arg))
	       (let ((val (car maybe-arg)))
		 (if (arg-test val) val
		     (error "Optional argument failed test"
			    'arg-test val)))
	       (error "too many optional arguments" maybe-arg))
	   default-exp)))))
