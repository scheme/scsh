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
      (cond ((syscall-error? condition)
             (handler (condition-errno condition)
                      (list (condition-message condition)
                            (condition-syscall condition)
                            (condition-irritants condition))))
            ((os-error? condition)
             (handler (os-error-code condition)
                      (list (condition-message condition)
                            (condition-who condtion)
                            (condition-irritants condition))))
            (else (more))))
    thunk))

;;; (with-errno-handler
;;;   ((errno data) ; These are vars bound in this scope.
;;;    ((errno/exist) . body1)
;;;    ((errno/wouldblock errno/again) . body2)
;;;    (else . body3))
;;;
;;;   . body)

(define-syntax with-errno-handler
  (syntax-rules ()
    ((with-errno-handler ((errno data) clause1 clause2 ...) body1 body2 ...)
     (call-with-current-continuation
      (lambda (ret)
        (with-errno-handler* (lambda (errno data)
                               (error-arms (errno data ret)
                                           clause1 clause2 ...))
                             (lambda () body1 body2 ...)))))))

(define-syntax error-arms
  (syntax-rules ()
    ((error-arms (errno data ret) (else body1 body2 ...))
     (call-with-values (lambda () body1 body2 ...) ret))
    ((error-arms (errno data ret) ((error-type1 error-type2 ...) body1 body2 ...)
                 clause1 clause2 ...)
     (if (or (equal? error-type1 errno) (equal? error-type2 errno) ...)
         (call-with-values (lambda () body1 body2 ...) ret)
         (error-arms (errno data ret) clause1 clause2 ...)))))
