;;; Stdio/stdport sync procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (stdio->stdports)
;;   (set-current-input-port!  (fdes->inport 0))
;;   (set-current-output-port! (fdes->outport 1))
;;   (set-current-error-port!   (fdes->outport 2)))

(define (with-stdio-ports* thunk)
  (with-current-input-port (fdes->inport 0)
    (with-current-output-port (fdes->outport 1)
      (with-current-error-port (fdes->outport 2)
        (thunk)))))

(define-simple-syntax (with-stdio-ports body ...)
  (with-stdio-ports* (lambda () body ...)))


(define (stdports->stdio)
  (dup (current-input-port)  0)
  (dup (current-output-port) 1)
  (dup (current-error-port)   2))
