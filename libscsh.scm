;;; This file is part of scsh.

(define (return-from-vm n)
  (with-continuation (if #t #f) (lambda () n)))

(define (startup user-context)
  (lambda (args)
    (start-new-session user-context
		       (current-input-port)
		       (current-output-port)
		       (current-error-port)
		       args
		       #t) ;batch?
    (with-interaction-environment
     (user-environment)
     (lambda ()
       (return-from-vm 0)))))

(define (s48-command command-string)
  (let* ((in (make-string-input-port command-string))
	 (s-exp (read in)))
    (if (and (not (eof-object? s-exp))
	     (eof-object? (read in)))
	(call-with-values
	 (lambda ()
	   (call-with-current-continuation
	    (lambda (k)
	      (with-handler
	       (lambda (cond more)
; 		 (display "error is "(current-error-port))
; 		 (display cond (current-error-port))
; 		 (newline (current-error-port))
		 (k cond))
	       (lambda ()
		 (eval s-exp (user-command-environment)))))))
	 (lambda args
	   (cond ((null? args) 
; 		  (display "null as result" 
; 			   (current-error-port)))
		 ((null? (cdr args))
; 		  (display "evaluated to:" (current-error-port))
; 		  (display (car args)(current-error-port))
; 		  (newline (current-error-port))
		  (car args))
		 (else
		  (display "multiple return values in s48-command" 
			   (current-error-port))
		  ))))
	(display "s48-command got not exactly one s-exp" 
		 (current-error-port)))))

;; TODO write a procedure to be called time by time to let the
;; administrative threads run

;; must be called from a running command processor
(define (dump-libscsh-image filename)
  (dump-scsh-program (startup (user-context)) filename))

(define-exported-binding "s48-command" s48-command)

