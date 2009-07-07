;;; Scsh start-up code.
;;; Copyright (c) 1995 by Olin Shivers.

;;; A scsh starter takes the command line args, parses them, 
;;; initialises the scsh system, and either starts up a repl loop
;;; or executes the -s script.

(define (make-scsh-starter)
  (let ((context (user-context)))
    (lambda (args)
      (parse-switches-and-execute args context))))


;;; Had to define these as the ones in s48's build.scm do not properly
;;; initialise ERROR-OUTPUT-PORT to stderr -- this is a bug in the vm's
;;; handoff to the very first Scheme form (it passes two ports -- not three).
;;; Until Kelsey fixes these, we hack it with these replacements, which
;;; invoke INIT-SCSH-HINDBRAIN, which re-initialises the I/O system to be 
;;; what you wanted.

;;; WRITE-IMAGE calls the starter after installing a fatal top-level
;;; error handler. MAKE-SCSH-STARTER shadows it in the interactive case.

(define (really-dump-scsh-program start filename)
  (let ((filename (translate filename)))
    (display (string-append "Writing " filename) (command-output))
    (newline (command-output))
    ;JMG: it is set to #f in the vm, so I omit it now
;;;(flush-the-symbol-table!)	;Gets restored at next use of string->symbol
    (write-image filename
		 (scsh-stand-alone-resumer start)
		 "Scsh 0.6")
    #t))


;;; This one relies on the scsh top-level command-line switch parser
;;; to decide whether to do the scsh-var inits quietly or with warnings.

(define (dump-scsh fname)
  (really-dump-scsh-program (make-scsh-starter) fname))

;;; Init the scsh run-time's vars quietly before running the program.
;;; This is what we export to the user for his programs.

(define (dump-scsh-program start filename)
  (let ((context (user-context)))
    (really-dump-scsh-program (lambda (args)
				(with-scsh-initialized
				 #f context args 
				 (lambda ()
				   (start args))))
			      filename)))

(define (scsh-stand-alone-resumer start)
  (usual-resumer			; sets up exceptions, interrupts,
					; and current input & output
   (lambda (args)			; VM gives us our args, but not our program.
     (init-fdports!)
     (call-with-current-continuation
      (lambda (halt)
        (set! %vm-prog-args args)
	(set-command-line-args! %vm-prog-args)
	(with-handler 
	 (simple-condition-handler halt (current-error-port))
	 (lambda ()
	   (let ((dynamic-env (get-dynamic-env))
		 (*result* 4711))
	     (let ((runnable (make-queue))
		   (thread (make-thread (lambda ()
					  (set! *result*
						(start (command-line))))
					dynamic-env
					'scsh-initial-thread))
		   (thread-count (make-counter)))
	      
	       (enqueue! runnable thread)
	       (increment-counter! thread-count)
	      
	       (run-threads
		(round-robin-event-handler runnable 200 dynamic-env thread-count
					   (lambda args #f)
					   (lambda (thread token args) ; upcall handler
					     (propogate-upcall thread token args))
					   (lambda ()
					     (if (positive? (counter-value thread-count))
						 ((structure-ref threads-internal wait))
						 #f))))
	       (if (integer? *result*) *result* 0)))))))))) ; work around bug.

(define %vm-prog-args #f)
 
