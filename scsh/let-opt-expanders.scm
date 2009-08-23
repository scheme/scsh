;;; LET-OPTIONALS macros
;;; Copyright (c) 2001 by Olin Shivers.
;;; See file COPYING.

;;; This file defines three macros for parsing optional arguments to procs:
;;; 	(LET-OPTIONALS  arg-list (opt-clause1 ... opt-clauseN [rest])
;;;       body ...)
;;; 	(LET-OPTIONALS* arg-list (opt-clause1 ... opt-clauseN [rest])
;;;       body ...)
;;; 	(:OPTIONAL rest-arg default-exp [arg-check])
;;; where
;;;     <opt-clause> ::= (var default [arg-check supplied?])
;;;                  |   ((var1 ... varN) external-arg-parser)
;;;
;;; LET-OPTIONALS* has LET* scope -- each arg clause sees the bindings of
;;; the previous clauses. LET-OPTIONALS has LET scope -- each arg clause
;;; sees the outer scope (an ARG-CHECK expression sees the outer scope
;;; *plus* the variable being bound by that clause, by necessity).
;;;
;;; In practice, LET-OPTIONALS* is the one you want.
;;;
;;; The only interesting module that is exported by this file is
;;; 	LET-OPT
;;; which obeys the following interface:
;;;     (exports (let-optionals  :syntax)
;;;              (let-optionals* :syntax)
;;;		 (:optional      :syntax))
;;;
;;; The LET-OPTIONALS macro is defined using the Clinger/Rees
;;; explicit-renaming low-level macro system. You'll have to do some work to
;;; port it to another macro system.
;;;
;;; The :OPTIONAL macro is defined with simple high-level macros,
;;; and should be portable to any R4RS system.
;;;
;;; These macros are all careful to evaluate their default forms *only* if
;;; their values are needed.
;;;
;;; The LET-OPTIONALS expander is pretty hairy. Sorry. It does produce
;;; very good code.
;;;
;;; The top-level forms in this file are Scheme 48 module expressions.
;;; I use the module system to help me break up the expander code for
;;; LET-OPTIONALS into three procedures, which makes it easier to understand
;;; and test. But if you wanted to port this code to a module-less Scheme
;;; system, you'd probably have to inline the auxiliary procs into the actual
;;; macro definition.
;;;
;;; To repeat: This code is not simple Scheme code; it is module code.
;;; It must be loaded into the Scheme 48 ,config package, not the ,user
;;; package.
;;;
;;; The only non-R4RS dependencies in the macros are ERROR, RECEIVE,
;;; and CALL-WITH-VALUES.
;;;
;;; See below for details on each macro.
;;; 	-Olin

;;; (LET-OPTIONALS* arg-list (clause ... [rest]) body ...)
;;; (LET-OPTIONALS  arg-list (clause ... [rest]) body ...)
;;;
;;; clause ::= (var default [arg-test supplied?])	; The simple case
;;;        |   ((var1 ...) external-arg-parser)		; external hook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for binding a procedure's optional arguments to either
;;; the passed-in values or a default.
;;;
;;; The expression takes a rest list ARG-LIST and binds the VARi to
;;; the elements of the rest list. When there are no more elements, then
;;; the remaining VARi are bound to their corresponding DEFAULTi values.
;;; It is an error if there are more args than variables.
;;;
;;; Simple example:
;;;     (let-optionals* args ((in     (current-input-port))
;;;                           (out    (current-output-port))
;;;                           (nbytes (string-length s)))
;;;       ...)
;;;
;;; - The default expressions are *not* evaluated unless needed.
;;;
;;; - When a LET-OPTIONALS* form is evaluated, the default expressions are
;;;   carried out in a "sequential" LET*-style scope -- each clause is
;;;   evaluated in a scope that sees the bindings introduced by the previous
;;;   clauses.
;;;
;;; - LET-OPTIONALS, in contrast, evaluates all clauses in the *outer*
;;;   environment. Each ARG-TEST form, however, does see the variable
;;;   bound by that clause (see below).
;;;
;;; - If there's an ARG-TEST form, it is evaluated when an argument is
;;;   passed in; it is not evaluated when the argument is defaulted.
;;;   If it produces false, an error is raised. You can stick an arg-checking
;;;   expression here. Here's the above example with full arg-checking:
;;;     (let ((strlen (string-length s)))
;;;       (let-optionals args ((in  (current-input-port)  (input-port? in))
;;;                            (out (current-output-port) (output-port? out))
;;;                            (nbytes strlen (and (integer? nbytes)
;;;                                                (< -1 nbytes strlen))))
;;;         ...))
;;;
;;;   The ARG-TEST expression is evaluated in the outer scope of the LET,
;;;   plus a binding for the parameter being checked.
;;;
;;; - A SUPPLIED? variable is bound to true/false depending on whether or
;;;   not a value was passed in by the caller for this parameter.
;;;
;;; - If there's a final REST variable in the binding list, it is bound
;;;   to any leftover unparsed values from ARG-LIST. If there isn't a final
;;;   REST var, it is an error to have extra values left. You can use this
;;;   feature to parse a couple of arguments with LET-OPTIONALS, and handle
;;;   following args with some other mechanism. It is also useful for
;;;   procedures whose final arguments are homogeneous.
;;;
;;; - A clause of the form ((var1 ... varn) external-arg-parser) allows you
;;;   to parse & arg-check a group of arguments together. EXTERNAL-ARG-PARSER
;;;   is applied to the argument list. It returns n+1 values: one
;;;   for the leftover argument list, and one for each VARi.
;;;
;;;   This facility is intended for things like substring start/end index
;;;   pairs. You can abstract out the code for parsing the pair of arguments
;;;   in a separate procedure (parse-substring-index-args args string proc)
;;;   and then a function such as READ-STRING! can simply invoke the procedure
;;;   with a
;;;     ((start end) (lambda (args) (parse-substring-index-args args s read-string!)))
;;;   clause. That is, the external-arg parser facility is a hook
;;;   that lets you interface other arg parsers into LET-OPTIONALS.

;;; Expanding the form
;;;;;;;;;;;;;;;;;;;;;;
;;; We expand the form into a code DAG that avoids repeatedly testing the
;;; arg list once it runs out, but still shares code. For example,
;;;
;;; (define (read-string! str . maybe-args)
;;;   (let-optionals* maybe-args ((port (current-input-port))
;;;                               (start 0)
;;;                               (end (string-length str)))
;;;     ...))
;;;
;;; expands to:
;;;
;;; (let* ((body (lambda (port start end) ...))
;;;        (end-def (lambda (port start) (body port start <end-default>)))
;;;        (start-def (lambda (port) (end-def port <start-default>)))
;;;        (port-def  (lambda () (start-def <port-def>))))
;;;   (if (pair? tail)
;;;       (let ((port (car tail))
;;;             (tail (cdr tail)))
;;;         (if (pair? tail)
;;;             (let ((start (car tail))
;;;                   (tail (cdr tail)))
;;;               (if (pair? tail)
;;;                   (let ((end (car tail))
;;;                         (tail (cdr tail)))
;;;                     (if (pair? tail)
;;;                         (error ...)
;;;                         (body port start end)))
;;;                   (end-def port start)))
;;;             (start-def port)))
;;;       (port-def)))
;;;
;;; Note that the defaulter code (the chain of ...-DEF procs) is just a
;;; linear sequence of machine code into which the IF-tree branches. Once
;;; we jump into the defaulter chain, we never test the arg list again.
;;; A reasonable compiler can turn this into optimal parameter-parsing code.
(define (make-gensym prefix)
 (let ((counter 0))
   (lambda ()
     (set! counter (+ counter 1))
     (string->symbol (string-append prefix (number->string counter))))))

;;; This guy makes the END-DEF, START-DEF, PORT-DEF definitions above.
;;; If an elt of VARS is a list, we are dealing with a group-parser clause.
;;; In this case, the corresponding element of DEFS is the name of
;;; the parser.
;;; I wish I had a reasonable loop macro.
;;;
;;; DEFAULTER-NAMES also holds the xparser expressions
;;; - STAR? true
;;;   LET* scope semantics -- default I & xparser I are evaluated in
;;;   a scope that sees vars 1 ... I-1.
;;; - STAR? false
;;;   LET scope semantics -- default and xparser forms don't see any of the
;;;   vars.
;;;
;;; I considered documenting this procedure better, but finally decided
;;; that if it was this hard for me to write, it should be hard for you
;;; to read. -Olin

(define (make-default-procs vars body-proc defaulter-names defs
			    sup-vars rest-var star? rename)
  (receive (defaulters ignore-me and-me-too)
      (really-make-default-procs vars body-proc defaulter-names defs
				 sup-vars rest-var star? rename)
    (reverse defaulters)))

(define (really-make-default-procs vars body-proc defaulter-names defs
				   sup-vars rest-var star? rename)
  (let ((%lambda (rename 'lambda))
	(%let (rename 'let))
	(%ignore (rename '_))
	(%call/values (rename 'call-with-values))
	(tail (rename 'tail))
	(make-rv (let ((g (make-gensym "%ov.")))
		   (lambda x (rename (g)))))
	(make-sv (let ((g (make-gensym "%sv.")))
		   (lambda () (rename (g))))))

    ;; RECUR returns 2 values: a LET*-binding list of defaulter proc
    ;; bindings, and an expression to evaluate in their scope.
    (let recur ((vars vars)
		(rev-params '())	; These guys
		(rev-vals '())		; have these values.
		(sup-vars sup-vars)
		(rev-sup-params '())	; These guys
		(rev-sup-vals '())	; have these values.
		(defaulter-names defaulter-names)
		(defs defs))
      ;; Note that the #F's bound to the SUPPLIED? parameters have no
      ;; effects, and so commute with the evaluation of the defaults.
      ;; Hence we don't need the VALS-EVALED? trick for them, just for the
      ;; default forms & their parameters.
      (if (pair? vars)
	  (let* ((var (car vars)) (vars (cdr vars)) ; "VAR" is really a list
		 (def (car defs)) (defs (cdr defs)) ; in xparser case...
		 (rvar (if star? var	; scope control
			   (if (pair? var) (map make-rv var) (make-rv))))
		 (rev-params1 (if (pair? rvar)
				  (append (reverse rvar) rev-params)
				  (cons rvar rev-params)))
		 (rev-vals1 (if (pair? rvar) rev-params1
				(cons def rev-params)))
		 (sv (car sup-vars))
		 (sv (if (or star? (not sv)) sv (make-sv)))
		 (rev-sup-params1 (if sv (cons sv rev-sup-params)
				      rev-sup-params))
		 (rev-sup-vals1 (cond (sv (cons #f rev-sup-params))
				      ((pair? var) rev-sup-vals)
				      (else rev-sup-params)))
		 (defaulter (car defaulter-names))
		 (defaulter-names (cdr defaulter-names)))
	    (receive (procs exp vals-evaled?)
		     (recur vars rev-params1 rev-vals1 (cdr sup-vars)
			    rev-sup-params1 rev-sup-vals1
			    defaulter-names defs)
	      (if (pair? var)
		  ;; Return #f for VALS-EVALED? so we'll force any prior
		  ;; default to be eval'd & not pushed below this default eval.
		  (values procs
			  `(,%call/values (,%lambda () (,defaulter '()))
			     (,%lambda ,(cons %ignore rvar) ,exp))
			  #f)

		  (let ((params (reverse (append rev-sup-params rev-params)))
			(exp (if vals-evaled? exp
				 `(,%let ((,rvar ,def)) ,exp))))
		    (values `((,defaulter (,%lambda ,params ,exp))
			      . ,procs)
			    `(,defaulter ,@(reverse rev-vals)
			                 ,@(reverse rev-sup-vals))
			    #t)))))

	  (values '() `(,body-proc ,@(if rest-var '('()) '())
				   ,@(reverse rev-vals)
				   . ,(reverse rev-sup-vals))
		  #t)))))


;;; This guy makes the (IF (PAIR? TAIL) ... (PORT-DEF)) tree above.
;;; DEFAULTERS is a list of the names of the defaulter procs & the xparser
;;; forms.

(define (make-if-tree vars defaulters arg-tests body-proc
		      tail supvars rest-var star? rename)
  (let ((%if (rename 'if))
	(%pair? (rename 'pair?))
	(%not (rename 'not))
	(%error (rename 'error))
	(%let (rename 'let))
	(%lambda (rename 'lambda))
	(%call/values (rename 'call-with-values))
	(%car (rename 'car))
	(%cdr (rename 'cdr))
	(make-rv (let ((g (make-gensym "%ov.")))
		   (lambda x (rename (g))))))

    (let recur ((vars vars) (defaulters defaulters)
		(ats arg-tests) (non-defaults '())
		(supvars supvars) (sup-trues '()))
      (if (null? vars)
	  (if rest-var
	      `(,body-proc  ,tail ,@(reverse non-defaults) . ,sup-trues)
	      `(,%if (,%pair? ,tail)
		     (,%error "Too many optional arguments." ,tail)
		     (,body-proc ,@(reverse non-defaults) . ,sup-trues)))

	  (let* ((v (car vars))
		 (rv (if star? v	; Scope control
			 (if (pair? v) (map make-rv v) (make-rv))))
		 (at (car ats))
		 (sup-trues1 (if (car supvars) (cons #t sup-trues) sup-trues))

		 (body `(,@(if (not (eq? at #t))
			       (let ((test (if star? at
					       `(,%let ((,v ,rv)) ,at))))
				 `((,%if (,%not ,test)
					 (,%error "Optional argument failed test"
						  ',at ',v ,rv))))
			       '())	; No arg test
			 ,(recur (cdr vars)
				 (cdr defaulters)
				 (cdr ats)
				 (if (pair? rv)
				     (append (reverse rv) non-defaults)
				     (cons rv non-defaults))
				 (cdr supvars) sup-trues1))))
	    (if (pair? rv)
		`(,%call/values (,%lambda ()
			          (,(car defaulters) ,tail))
		   (,%lambda (,tail . ,rv) . ,body))

		`(,%if (,%pair? ,tail)
		       (,%let ((,rv (,%car ,tail))
			       (,tail (,%cdr ,tail)))
		         . ,body)
		       (,(car defaulters) ,@(reverse non-defaults) . ,sup-trues))))))))


;;; Parse the clauses into
;;; - a list of vars,
;;; - a list of defaults,
;;; - a list of possible arg-tests. No arg-test is represented as #T.
;;; - a list of possible SUPPLIED? vars. An elt is either (var) or #f.
;;; - either the rest var or #f
;;;
;;; This is written out in painful detail so that we can do a lot of
;;; syntax checking.

(define (parse-clauses bindings)
  ;; LIST-LIB defines EVERY... but uses LET-OPTIONALS.
  ;; Define here to break the dependency loop:
  (define (every pred lis)
    (or (not (pair? lis)) (and (pred (car lis)) (every pred (car lis)))))

  (cond ((pair? bindings)
	 (let ((rev (reverse bindings)))
	   (receive (rest-var rev) (if (symbol? (car rev))
				       (values (car rev) (cdr rev))
				       (values #f rev))
	     (receive (vars defs ats supvars)
		 (let recur ((bindings (reverse rev)))
		   (if (not (pair? bindings))
		       (values '() '() '() '())
		       (receive (vars defs ats supvars) (recur (cdr bindings))
			 (let ((binding  (car bindings)))
			   (if (not (and (list? binding) (<= 2 (length binding) 4)))
			       (error "Illegal binding form in LET-OPTIONAL or LET-OPTIONAL*"
				      binding))

			   (let* ((var (car binding))
				  (vars (cons var vars))
				  (defs (cons (cadr binding) defs))
				  (stuff (cddr binding)))
			     (if (not (or (symbol? var)
					  (and (list? var)
					       (= 2 (length binding))
					       (every symbol? var))))
				 (error "Illegal parameter in LET-OPTIONAL or LET-OPTIONAL* binding"
					binding))
			     (receive (at sup-var)
				 (if (not (pair? stuff)) (values #t #f)
				     (let ((at (car stuff))
					   (stuff (cdr stuff)))
				       (if (not (pair? stuff))
					   (values at #f)
					   (let ((sv (car stuff)))
					     (if (not (symbol? sv))
						 (error "Illegal SUPPLIED? parameter in LET-OPTIONAL or LET-OPTIONAL*"
							binding sv))
					     (values at sv)))))
			       (values vars defs (cons at ats) (cons sup-var supvars))))))))
	       (values vars defs ats supvars rest-var)))))

	((null? bindings) (values '() '() '() '() #f))
	(else (error "Illegal bindings to LET-OPTIONAL or LET-OPTIONAL* form"
		     bindings))))

(define (really-expand-let-optionals exp star? rename compare?)
  (let* ((arg-list (cadr exp))
	 (var/defs (caddr exp))
	 (body (cdddr exp))

	 (body-proc  (rename 'body))
	 (tail-var (rename '%tail))	; Bound to remaining args to be parsed.

	 (%let* (rename 'let*))
	 (%lambda (rename 'lambda))

	 (prefix-sym (lambda (prefix sym)
		       (string->symbol (string-append prefix (symbol->string sym))))))

    (receive (vars defs arg-tests maybe-supvars maybe-rest)
	     (parse-clauses var/defs)
      (let* ((defaulter-names (map (lambda (var def)
				     (if (pair? var)
					 def	; xparser
					 (rename (prefix-sym "def-" var))))
				   vars defs))
	     (rsupvars (if star? maybe-supvars
			   (let ((g (make-gensym "%sv.")))
			     (map (lambda (x) (and x (rename (g))))
				  maybe-supvars))))
	     (just-supvars (let recur ((svs maybe-supvars))	; filter
			     (if (not (pair? svs)) '()
				 (let ((sv (car svs))
				       (tail (recur (cdr svs))))
				   (if sv (cons sv tail) tail)))))

	     (defaulters (make-default-procs vars body-proc defaulter-names
					     defs rsupvars maybe-rest
					     star? rename))

	     (if-tree (make-if-tree vars defaulter-names arg-tests body-proc
				    tail-var rsupvars maybe-rest star? rename))

	     ;; Flatten out the multi-arg items.
	     (allvars (apply append (map (lambda (v) (if (pair? v) v
							 (list v)))
					 vars))))

	`(,%let* ((,tail-var ,arg-list)
		  (,body-proc (,%lambda ,(append (if maybe-rest
						     (cons maybe-rest allvars)
						     allvars)
						 just-supvars)
			        . ,body))
		  . ,defaulters)
		 ,if-tree)))))

(define (expand-let-optionals exp rename compare?)
  (really-expand-let-optionals exp #f rename compare?))
(define (expand-let-optionals* exp rename compare?)
  (really-expand-let-optionals exp #t rename compare?))

