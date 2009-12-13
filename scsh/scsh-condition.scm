;;; Copyright (c) 1994 by Olin Shivers
;;; Add scsh conditions to s48.

(define (with-errno-handler* handler thunk)
  (with-handler
    (lambda (condition more)
      (if (os-error? condition)
          (handler (os-error-code condition)
                   (list (condition-message condition)
                         (condition-who condition)
                         (condition-irritants condition)))
          (more)))
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
    ((with-errno-handler ((the-errno data) clause1 clause2 ...) body1 body2 ...)
     (call-with-current-continuation
      (lambda (ret)
        (with-errno-handler* (lambda (the-errno data)
                               (error-arms (the-errno data ret)
                                           clause1 clause2 ...))
                             (lambda () body1 body2 ...)))))))

(define-syntax error-arms
  (syntax-rules ()
    ((error-arms (the-errno data ret) (else body1 body2 ...))
     (call-with-values (lambda () body1 body2 ...) ret))
    ((error-arms (the-errno data ret) ((error-name1 error-name2 ...) body1 body2 ...)
                 clause1 clause2 ...)
     (if (or (errno=? (errno error-name1) the-errno) (errno=? (errno error-name2) the-errno) ...)
         (call-with-values (lambda () body1 body2 ...) ret)
         (error-arms (the-errno data ret) clause1 clause2 ...)))))
