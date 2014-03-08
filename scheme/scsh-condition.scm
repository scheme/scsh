;;; Copyright (c) 1994 by Olin Shivers
;;; Add scsh conditions to s48.

(define (with-errno-handler* handler thunk)
  (with-exception-handler
    (lambda (condition)
      (if (os-error? condition)
          (handler (os-error-code condition)
                   (list (condition-message condition)
                         (condition-who condition)
                         (condition-irritants condition)))
          (raise condition)))
    thunk))

(define (errno-error errno who message . irritants)
  (raise (condition
          (make-os-error errno)
          (make-who-condition who)
          (make-message-condition message)
          (make-irritants-condition irritants))))

(define-syntax weh-cond
  (syntax-rules (else)
    ((weh-cond () ((cond-condition cond-body) ...) error-number return)
     (cond (cond-condition cond-body) ...))
    ((weh-cond (((errno-name ...) clause-body ...) . other-clauses) (cond-clause ...) error-number return)
     (weh-cond other-clauses
               (cond-clause ... ((or (errno=? (errno errno-name) (integer->errno error-number)) ...)
                                 (call-with-values (lambda () clause-body ...) return)))
               error-number return))
    ((weh-cond ((else clause-body ...) . other-clauses) (cond-clause ...) error-number return)
     (weh-cond other-clauses
               (cond-clause ... (else (call-with-values (lambda () clause-body ...) return)))
               error-number return))))

(define-syntax with-errno-handler
  (syntax-rules ()
    ((with-errno-handler
      ((err data)
       (clause-condition clause-body ...) ...)
      body ...)
     (call-with-current-continuation
      (lambda (return)
        (with-errno-handler*
         (lambda (err data)
           (weh-cond ((clause-condition clause-body ...) ...) () err return))
         (lambda () body ...)))))))
