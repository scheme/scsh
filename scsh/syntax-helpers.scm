;;; Macro expanding procs for scsh.
;;; Written for Clinger/Rees explicit renaming macros.
;;; Needs name-export and receive-syntax S48 packages.
;;; Also needs scsh's utilities package (for CHECK-ARG).
;;; Must be loaded into for-syntax package.
;;; Copyright (c) 1993 by Olin Shivers.

(define-syntax define-simple-syntax
  (syntax-rules ()
    ((define-simple-syntax (name . pattern) result)
     (define-syntax name (syntax-rules () ((name . pattern) result))))))

(define (name? thing)
  (or (symbol? thing)
      (generated? thing)))

;;; Debugging macro:
(define-simple-syntax (assert exp)
  (if (not exp) (error "Assertion failed" (quote exp))))

;;; Some process forms and redirections are implicitly backquoted.

(define (backq form rename)
  (list (rename 'quasiquote) form)) 	; form -> `form
(define (unq form rename)
  (list (rename 'unquote) form)) 	; form -> ,form

(define (make-backquoter rename)
  (lambda (form) (list (rename 'quasiquote) form)))
(define (make-unquoter rename)
  (lambda (form) (list (rename 'unquote) form)))

;; DEBLOCK maps an expression to a list of expressions, flattening BEGINS.
;; (deblock '(begin (begin 3 4) 5 6 (begin 7 8))) => (3 4 5 6 7 8)

(define (deblock exp rename compare)
  (let ((%block (rename 'begin)))
    (let deblock1 ((exp exp))
      (if (and (pair? exp)
	       (name? (car exp))
	       (compare %block (car exp)))
	  (apply append (map deblock1 (cdr exp)))
	  (list exp)))))

;; BLOCKIFY maps an expression list to a BEGIN form, flattening nested BEGINS.
;; (blockify '( (begin 3 4) 5 (begin 6) )) => (begin 3 4 5 6)

(define (blockify exps rename compare)
  (let ((new-exps (apply append
			 (map (lambda (exp) (deblock exp rename compare))
			      exps))))
    (cond ((null? new-exps)
	   (error "Empty BEGIN" exps))
	  ((null? (cdr new-exps))	; (begin exp) => exp
	   (car new-exps))
	  (else `(,(rename 'begin) . ,new-exps)))))

(define (thunkate code rename compare)
  (let ((%lambda (rename 'lambda)))
    `(,%lambda () ,@(deblock code rename compare))))

;;; Process forms are rewritten into code that causes them to execute
;;; in the current process.
;;; (BEGIN . scheme-code)	=> (STDIO->STDPORTS (LAMBDA () . scheme-code))
;;; (| pf1 pf2)			=> (BEGIN (FORK/PIPE (LAMBDA () pf1-code))
;;;                                       pf2-code)
;;; (|+ conns pf1 pf2)		=> (BEGIN
;;;                                  (FORK/PIPE+ `conns (LAMBDA () pf1-code))
;;;                                  pf2-code)
;;; (epf . epf)			=> epf-code
;;; (prog arg1 ... argn)	=> (APPLY EXEC-PATH `(prog arg1 ... argn))
;;; [note the implicit backquoting of PROG, ARG1, ...]

;;; NOTE: | and |+ won't read into many Scheme's as a symbol. If your
;;; Scheme doesn't handle it, kill them, and just use the PIPE, PIPE+
;;; aliases.

(define (transcribe-process-form pf rename compare)
  (if (and (list? pf) (pair? pf))
      (let ((head (car pf)))
	(cond
	 ((compare head (rename 'begin))
	  (transcribe-begin-process-form (cdr pf) rename compare))

	 ((compare head (rename 'epf))
	  (transcribe-extended-process-form (cdr pf) rename compare))

	 ((compare head (rename 'pipe))
	  (transcribe-simple-pipeline (cdr pf) rename compare))

	 ((compare head (rename '|))
	  (transcribe-simple-pipeline (cdr pf) rename compare))

	 ((compare head (rename '|+))
	  (let ((conns (backq (cadr pf) rename))
		(pfs (cddr pf)))
	    (transcribe-complex-pipeline conns pfs rename compare)))

	 ((compare head (rename 'pipe+))
	  (let ((conns (backq (cadr pf) rename))
		(pfs (cddr pf)))
	    (transcribe-complex-pipeline conns pfs rename compare)))

	 (else	(let ((%apply (rename 'apply))
		      (%exec-path (rename 'exec-path))
		      (pf (backq pf rename)))
		  `(,%apply ,%exec-path ,pf)))))
      (error "Illegal process form" pf)))


(define (transcribe-begin-process-form body rename compare)
  (let ((%with-stdio-ports* (rename 'with-stdio-ports*))
	(%lambda          (rename 'lambda)))
    `(,%with-stdio-ports* (,%lambda () . ,body))))


(define (transcribe-simple-pipeline pfs rename compare)
  (if (null? pfs) (error "Empty pipeline")
      (let* ((%fork/pipe (rename 'fork/pipe))
	     (trans-pf (lambda (pf)
			  (transcribe-process-form pf rename compare)))
	     (chunks (reverse (map trans-pf pfs)))
	     (last-chunk (car chunks))
	     (first-chunks (reverse (cdr chunks)))
	     (forkers (map (lambda (chunk)
			     `(,%fork/pipe ,(thunkate chunk rename compare)))
			   first-chunks)))
	(blockify `(,@forkers ,last-chunk) rename compare))))


;;; Should let-bind CONNS in case it's a computed form.

(define (transcribe-complex-pipeline conns pfs rename compare)
  (if (null? pfs) (error "Empty pipeline")
      (let* ((%fork/pipe+ (rename 'fork/pipe+))
	     (trans-pf (lambda (pf)
			 (transcribe-process-form pf rename compare)))
	     (chunks (reverse (map trans-pf pfs)))
	     (last-chunk (car chunks))
	     (first-chunks (reverse (cdr chunks)))
	     (forkers (map (lambda (chunk)
			     `(,%fork/pipe+ ,conns
					    ,(thunkate chunk rename compare)))
			   first-chunks)))
	(blockify `(,@forkers ,last-chunk) rename compare))))


(define (transcribe-extended-process-form epf rename compare)
  (let* ((pf (car epf))		; First form is the process form.
	 (redirs (cdr epf)) 	; Others are redirection forms.
	 (trans-redir (lambda (r) (transcribe-redirection r rename compare)))
	 (redir-chunks (map trans-redir redirs))
	 (pf-chunk (transcribe-process-form pf rename compare)))
    (blockify `(,@redir-chunks ,pf-chunk) rename compare)))


(define (transcribe-redirection redir rename compare)
  (let* ((backq (make-backquoter rename))
	 (parse-spec (lambda (x default-fdes) ; Parse an ([fdes] arg) form.
		       ;; X  must be a list of 1 or 2 elts.
		       (check-arg (lambda (x) (and (list? x)
						   (< 0 (length x) 3)))
				  x transcribe-redirection)
		       (let ((a (car x))
			     (b (cdr x)))
			 (if (null? b) (values default-fdes (backq a))
			     (values (backq a) (backq (car b)))))))
	 (oops (lambda () (error "unknown i/o redirection" redir)))
	 (%open (rename 'shell-open))
;	 (%dup-port (rename 'dup-port))
	 (%dup->fdes (rename 'dup->fdes))
;	 (%run/port (rename 'run/port))
	 (%open-string-source (rename 'open-string-source))
	 (%create+trunc (rename 'create+trunc))
	 (%write+append+create (rename 'write+append+create))
	 (%q (lambda (x) (list (rename 'quote) x)))
	 (%close (rename 'close))
	 (%move->fdes (rename 'move->fdes))
	 (%set! (rename 'set!))
	 (%<<-port-holder (rename '<<-port-holder))
	 (%let (rename 'let))
	 (%port (rename 'port))
	 (%stdports->stdio (rename 'stdports->stdio)))
    (cond ((pair? redir)
	   (let ((args (cdr redir))
		 (op (car redir)))
	     (cond
	      ((compare op (rename '<))
	       (receive (fdes fname) (parse-spec args 0)
		 `(,%open ,fname 0 ,fdes)))

	      ((compare op (rename '>))
	       (receive (fdes fname) (parse-spec args 1)
		 `(,%open ,fname ,%create+trunc ,fdes)))

	       ;;; BUG BUG -- EPF is backquoted by parse-spec.
;	       ((<<<) ; Just a RUN/PORT with a specific target fdes.
;		(receive (fdes epf) (parse-spec args 0)
;		  `(,%dup-port (,%run/port . ,epf) ,fdes))) ; Add a WITH-PORT.

	      ;; We save the port in the global variable <<-port-holder to prevent a
	      ;; GC from closing the port before the exec().
	      ((compare op (rename '<<))
	       (receive (fdes exp) (parse-spec args 0)
		 `(,%let ((,%port (,%open-string-source ,exp)))
			 (,%set! ,%<<-port-holder ,%port)
			 (,%move->fdes ,%port ,fdes))))

	      ((compare op (rename '>>))
	       (receive (fdes fname) (parse-spec args 1)
		 `(,%open ,fname ,%write+append+create ,fdes)))

	      ((compare op (rename '=))
	       (assert (= 2 (length args))) ; Syntax check.
	       `(,%dup->fdes ,(backq (cadr args)) ,(backq (car args))))

	      ((compare op (rename '-))	; (- fdes) => close the fdes.
	       (assert (= 1 (length args))) ; Syntax check.
	       `(,%close ,(backq (car args))))

	      (else (oops)))))

	  ((compare redir (rename 'stdports))
	   `(,%stdports->stdio))
	  (else (oops)))))

;;; <<< should be {
