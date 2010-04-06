; Simple condition handler for stand-alone programs.

(define (simple-condition-handler halt port)
  (lambda (c punt)
    (cond ((error? c)
           (display-condition c port)
           (halt 1))
          ((warning? c)
           (display-condition c port)) ;Proceed
          ((interrupt-condition? c)
           (halt 2))
          ((vm-exception? c)
           (display-condition c port)
           (halt 1))
          (else
           (punt)))))
