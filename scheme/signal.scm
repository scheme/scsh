;;; Signals (rather incomplete)
;;; ---------------------------

(import-lambda-definition-2 signal-pid (pid signal) "scsh_kill")

(define (signal-process proc signal)
  (signal-pid (cond ((proc? proc)    (proc:pid proc))
                    ((integer? proc) proc)
                    (else (error "Illegal proc passed to signal-process" proc)))
              (signal-os-number signal)))

(define (signal-process-group proc-group signal)
  (signal-pid (- (cond ((proc? proc-group)    (proc:pid proc-group))
                       ((integer? proc-group) proc-group)
                       (else (error "Illegal proc passed to signal-process-group"
                                    proc-group))))
              (signal-os-number signal)))
