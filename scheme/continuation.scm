;;; Call THUNK, then die.
;;; A clever definition in a clever implementation allows the caller's stack
;;; and dynamic env to be gc'd away, since this procedure never returns.
(define (call-terminally thunk)
  (with-continuation
   null-continuation
   (lambda ()
     (with-handler
      (lambda (c more)
        (display-condition c (current-error-port))
        (exit 1))
      (lambda ()
        (dynamic-wind
            (lambda () (values))
            thunk
            (lambda () (exit 0))))))))

;; from shift-reset.scm:
(define null-continuation #f)
