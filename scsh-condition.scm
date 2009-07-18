;;; Copyright (c) 1994 by Olin Shivers
;;; Add scsh conditions to s48.

;;; A syscall-error condition-type:

(define-condition-type &syscall-error &error
  make-syscall-error syscall-error?
  (errno condition-errno)
  (syscall condition-syscall))

(define (errno-error errno syscall . irritants)
  (errno-error-with-message errno (errno-msg errno) syscall irritants))

(define (errno-error-with-message errno msg syscall . irritants)
  (raise
   (condition
    (make-syscall-error errno syscall)
    (make-message-condition msg)
    (make-irritants-condition irritants))))

(define (with-errno-handler* handler thunk)
  (with-handler
    (lambda (condition more)
      (if (syscall-error? condition)
	  (let ((irritants (condition-irritants condition)))
	    (handler (condition-errno condition)	; errno
		     (list (condition-message condition)
                           (condition-syscall condition)
                           (condition-irritants condition))))	; (msg syscall . packet)
	  ;; capture VM exceptions (currently only prim-io.scm)
	  (if (and (vm-exception? condition)
		   (eq? (vm-exception-reason condition)
			'os-error))
              ;; this information wouldn't be in the irritants in the new
              ;; condition system. This should be changed later.
	      (let ((irritants (condition-irritants condition)))
		(if (> (length irritants) 3)
		    (handler (caddr irritants) ; errno
			     (cons
			      (last irritants) ; msg
			      (cons
			       (enumerand->name ; syscall (almost ...)
				(exception-opcode condition) op)
			        ; packet:
			       (drop-right (cdddr irritants) 1))))))))
      (more))
    thunk))

;;; (with-errno-handler
;;;   ((errno data) ; These are vars bound in this scope.
;;;    ((errno/exist) . body1)
;;;    ((errno/wouldblock errno/again) . body2)
;;;    (else . body3))
;;;
;;;   . body)

(define-syntax with-errno-handler
  (lambda (exp rename compare)
    (let* ((%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   (%else (rename 'else))
	   (%weh (rename 'with-errno-handler*))
	   (%= (rename '=))
	   (%begin (rename `begin))
	   (%or (rename `or))
	   (%call/cc (rename 'call-with-current-continuation))
	   (%cwv (rename 'call-with-values))

	   (%ret (rename 'ret)) ; I think this is the way to gensym.

	   (err-var (caaadr exp))
	   (data-var (car (cdaadr exp)))
	   (clauses (cdadr exp))
	   (body (cddr exp))

	   (arms (map (lambda (clause)
			(let ((test (if (compare (car clause) %else)
					%else
					(let ((errs (car clause)))
					  `(,%or . ,(map (lambda (err)
							   `(,%= ,err ,err-var))
							 errs))))))
			  `(,test
			    (,%cwv (,%lambda () . ,(cdr clause)) ,%ret))))
		      clauses)))

      `(,%call/cc (,%lambda (,%ret)
         (,%weh
	    (,%lambda (,err-var ,data-var)
	      (,%cond . ,arms))
	    (,%lambda () . ,body)))))))

;;;; S48 already has this machinery, i.e., (SET-INTERACTIVE?! flag)
;;;; Interactive => breakpoint on errors.
;;;; Noninteractive => exit on errors.
;
;(define $interactive-errors? (make-fluid #f))
;
;(define (with-interactive-errors val thunk)
;  (let-fluid $interactive-errors? val thunk))
;
;(define (set-interactive-errors! val)
;  (set-fluid! $interactive-errors? val))
;
;;;; Just quit if non-interactive. Otherwise, punt to next handler.
;;;; A hack, because we use the default handler for the interactive
;;;; case.
;
;(define (scsh-error-handler condition more)
;  (if (and (error? condition)
;	   (not (fluid $interactive-errors?)))
;      (begin (display condition (current-error-port))
;	     (exit -1))
;      (more)))
