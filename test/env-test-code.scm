

; TEST-CODE ***********************************************************************
; all procedures return either #t or #f

; Deleting a variable via (setenv var #f) cannot be tested, since 
; getenv returns #f in both cases: a) deleted variable or b) variable set to #f
; OK
(define (setenv-test var val)
  (setenv var val)
  (equal? (getenv var) val))

; getenv is tested with the same procedure than setenv
; OK 2001-04-09 16:21
(define getenv-test setenv-test)

; env->alist-test test, if
; env->alist is an alist of pairs of string and if
; a previously set variable has the set value
; OK 2001-04-09 16:21
(define (env->alist-test var val)
  (setenv var val)
  (let ((alist (env->alist)))
    (and (every (lambda (pair)                     ; syntactical correctness
		       (and (pair? pair)           ; entry is a pair
			    (not (list? pair))     ; and not a list
			    (string? (car pair))   ; car is a string...
			    (or (string? (cdr pair)) ; ...cdr is either a string... 
				(string-list? (cdr pair))))) ; ...or a string-list
		alist)
	 (equal? (cdr (assoc var alist))
		 val))))                           ; previously set variable correctly present

; checks if alist->enc really sets a new environment
; by this way, it checks if a string list is transformed correctly to a colon list, too

; COMMENTED OUT BECAUSE STRING-TOKENIZE IS MISSING (WILL BE FIXED)
; OK
(define (alist->env-test alist)
  (let ((old-env (env->alist)))       ; save old environment
    (alist->env alist)                ; set new environment
    (let (            		; compare values of alist with values of the environment
	  (result (every
		   (lambda (var-pair)
		     (let ((var-pair-value (cdr var-pair))
			   (env-var-value (getenv (car var-pair))))
;		       (begin (display var-pair) (newline)
;			      (display var-pair-value) (newline)
;			      (display env-var-value) (newline)
;			      (display "---------------------") (newline))
		       (if (string-list? var-pair-value)
			   (equal? var-pair-value
				   (string-tokenize env-var-value #\:))
			   (equal? var-pair-value env-var-value))))
		   (alist-compress alist))))
      (alist->env old-env)       ; restore old environment
      result)))


; NOTE: since alist-bla works only on alists, string-list / colon-list-conversion is not implemented
; OK 2001-04-09 16:21
(define (alist-delete-test key alist)
  (not (member key (map car (alist-delete key alist)))))


; results #t, if the first occurance of the variable has the expected (new) value, else #f
; OK 2001-04-09 16:15
(define (alist-update-test key val alist)
  (letrec ((check-update (lambda (alist)
			   (if (null? alist)         ; if alist is empty key wasn't inserted
			       #f
			       (if (equal? key (caar alist))   ; key found
				   (if (equal? val (cdar alist)) ; value ok?
					; key must not be in the cdr of alist
				       (not (member key (map car (cdr alist)))) 
				       #f)
				   (check-update (cdr alist)))))))
    (check-update (alist-update key val alist))))


; checks compression of every variable
; OK 2001-04-09 15:46
(define (alist-compress-test alist)
  (letrec ((check-compress (lambda (alist known-vars)
			     (if (null? alist)
				 #t
				 (if (member (caar alist) known-vars)
				     #f
				     (check-compress (cdr alist)
						     (cons (caar alist)
							   known-vars)))))))
    (check-compress (alist-compress alist) '())))



; OK 2001-04-09 15:46
(define (with-env*-test env-alist-delta)
  (with-env*-test-generator with-env*
			    env-alist-delta
			    (update-env (env->alist) env-alist-delta)))

; OK 2001-04-09 15:45
(define (with-total-env*-test env-alist)
  (with-env*-test-generator with-total-env*
			    env-alist
			    env-alist))

; generator:
; There are three tests for each circumstance (s. scsh manual for details)
; * simple thunk:             returns usually
; * non-local-return thunk:   returns using escape-procedure
; * reinvoking-thunk:         returns non-local, is reinvoked and returns
; each thunk return the result of the env-var-test, which is #t if the current
;   environment is as expected
; the tunks are called via run-test that first runs the test and,
; if the test returned #t, returns #t if the current environment is as expected.
; (there are two test for the current-environment necessary since the environment
; during the call and after the call differ (s. manual for details))
; OK 2001-04-09 15:45
; the generator generates test-procedures for with-total-env* and with-env*
; parameters are:
; - call:                 either with-total-env* or with-env*
; - call-argument:        either an env-alist (for with-total-env*) or an env-alist-delta (for with-env*)
; - expected-process-env: expected content of the environment during the with-env-call 
(define (with-env*-test-generator call call-argument expected-process-env)
  (let* ((old-env-alist (env->alist))
         (env-var-test (lambda ()                ; checks, if the changed environment is as expected
			 (equal-to-current-env? expected-process-env)))

	; store places for continuations:
	 (non-local-exit-cc #f)         ; exit poit for a thunk
	 (reinvoking-cc #f)             ; entry point to reinvoking thunk
	 (thunk-finished-cc #f)         ; exit point for finished reinvoking thunk

	; thunks for testing:
	 (thunk-local-return (lambda () (env-var-test)))
	 (cc-thunk-non-local-return
	  (lambda ()
	    (non-local-exit-cc (env-var-test))     ; non-local return
	    #f))                    ; if non-local return fails (?), return #f
	 
	 (cc-reinvoking-thunk
	  (lambda ()
	    (call-with-current-continuation 
	     (lambda (k)
	       (set! reinvoking-cc k)
	       (non-local-exit-cc #f)))			; non-local-return
	    (thunk-finished-cc (env-var-test))         ; finish with result of env-var-test
	    #f))                    ; if continuation-call fails (?), return #f
	 
	 ; procedure to perform tests (run test and check content of current environment)
	 (run-test
	  (lambda (thunk)
	    (and (thunk)
		 (equal-to-current-env? old-env-alist)))))

    (and (run-test (lambda ()
		     (call call-argument thunk-local-return)))
	 (run-test (lambda ()
		     (call-with-current-continuation (lambda (k)
				(set! non-local-exit-cc k)   ; possibility of non-local return
				(call call-argument
				      cc-thunk-non-local-return)))))
	 (run-test (lambda ()
	 		(call-with-current-continuation
	 		  (lambda (finished)
		             (set! thunk-finished-cc finished)
		             (call-with-current-continuation 
			       (lambda (k)
			         (set! non-local-exit-cc k) ; possibility of non-local return
			         (call call-argument cc-reinvoking-thunk)))
	                     (reinvoking-cc #f))))))))   ; reinvoke thunk
; the old environment needn't to be restored because with-env* and with-total-env*
; don't change the current environment (if successful)

; checks if home-directory is a string
; OK 2001-04-09 15:45
(define (home-directory-test)
  (string? home-directory))

; checks if exec-path-list is a string-list
; OK 2001-04-09 15:45
(define (exec-path-list-test)
  (string-list? (thread-fluid exec-path-list)))


; OK 2001-04-09 15:45
(define (add-tester elt mark original-list add-result)
  (letrec ((correct-insert
            (lambda (add-result)		; checks if elt was inserted correctly
              (if (null? add-result)	                ; empty list -> element wasn't inserted
                  #f
                  (cond
                    ((equal? (car add-result) mark) #f)  ; first occurance of mark without elt => #f
                    ((equal? (car add-result) elt)         ; found elt
                     (or (null? (cdr add-result))          ; either the list terminates or...
                         (equal? (cadr add-result) mark))) ; ...the following string is mark
                    (else                                  ; otherwise the rest of the list has to be correct
                     (correct-insert (cdr add-result)))))))
           (correct-order
            (lambda (add-result original-list) ; checks, if order was respected
              (cond
                ((null? add-result)                        ; if the result is empty,...
                 (null? original-list))                    ; ...so the original list has to be empty, too
                ((null? original-list)                     ; if the original list is empty...
                 (or (null? add-result)                    ; ...either the result list has to be empty, ...
                     (and (equal? (car add-result) elt)    ; or contains only the inserted element
                          (null? (cdr add-result)))))
                ((equal? (car add-result) (car original-list))  ; cars equal => continue with cdrs
                 (correct-order (cdr add-result) (cdr original-list)))
                ((equal? (car add-result) elt)             ; => (car original-list) =/= elt !
                 (correct-order (cdr add-result) original-list)) ; found elt -> skip
                (else                                            ; lists are unequal
                 #f)))))
    (and (correct-insert add-result)
         (correct-order add-result original-list))))

; OK 2001-04-09 15:44
(define (add-before-test elt before liste)
  (add-tester elt before liste (add-before elt before liste)))

; add-after operates as add-before on reverse list
; OK 2001-04-09 15:44
(define (add-after-test elt after liste) 
  (add-tester elt after liste (reverse (add-after elt after (reverse liste)))))

; helping procedures *************************************************************
; returns #t if liste is a list containing only strings, else #f
; OK
(define string-list?
  (lambda (liste)
    (and (list? liste)
	 (every (lambda (elt)
		  (string? elt))
		liste))))

; deletes equal-to-this once in list, if present
; OK
(define (delete-once equal-to-this list)
  (if (null? list)
      '()
      (if (equal? (car list) equal-to-this)
          (cdr list)
          (cons (car list) (delete-once equal-to-this (cdr list))))))

; compares to lists
; order is unimportant, but count of each element is
; examlpes:
; (list-equal? '(1 2) '(2 1)
; => #t
; (list-equal? '(1 2) '(2 1 1)
; #f
; OK
(define (list-equal? list1 list2)
  (if (null? list1)
      (null? list2)
      (if (member (car list1) list2)
          (list-equal? (cdr list1)
		       (delete-once (car list1) list2))
          #f)))

; updates the environment env-alist via env-alist-delta
; NOTE: Test alist-update first (run alist-update-test key val alist)
; OK
(define update-env
  (lambda (env-alist env-alist-delta)
    (if (null? env-alist-delta)
	env-alist
	(update-env (alist-update (car (car env-alist-delta))
				  (cdr (car env-alist-delta))
				  env-alist)
		    (cdr env-alist-delta)))))


; compares old-env-alist with actual environment (env->alist)
; OK
(define	equal-to-current-env?
  (lambda (old-env-alist)
    (list-equal? old-env-alist (env->alist))))
