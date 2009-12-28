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
           (%errno (rename 'errno))
           (%int->errno (rename 'integer->errno))
           (%= (rename 'errno=?))
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
                                                           `(,%= (,%errno ,err)
                                                                 (,%int->errno ,err-var)))
                                                         errs))))))
                          `(,test
                            (,%cwv (,%lambda () . ,(cdr clause)) ,%ret))))
                      clauses)))

      `(,%call/cc (,%lambda (,%ret)
                            (,%weh
                             (,%lambda (,err-var ,data-var)
                                       (,%cond . ,arms))
                             (,%lambda () . ,body)))))))
