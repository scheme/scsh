;;; Basic functions for the scsh-test-suite
;;; Author: 2001 David Frese

;; --- The list to store the tests ---

(define *test-list* '())

;; --- add-test! ------------------------------------------------
;; This is the main function to add a test to the test-suite
;; name  - a symbol naming the test uniquely
;; group - a symbol for the group of this test
;; proc  - the function that does the test
;; args  - the arguments for proc
;; add-test deletes all previously added tests that have the same
;;   name (group is ignored)!
;; proc should return #f or signal an error, if the test failed.
;; Every other value means, that the test succeeded.

(define (add-test! name group proc . args)
  (let ((test (make-testdt name group proc args)))
    (let ((other (filter (lambda (test)
			   (equal? (testdt-name test)
				   name))
			 *test-list*)))
      (for-each (lambda (test)
		  (set! *test-list* (delete! test *test-list*)))
		other))
    (set! *test-list* (cons test *test-list*))))

(define (find-test name)
  (find (lambda (test)
	  (eq? (testdt-name test) name))
	*test-list*))

;; --- add-test-multiple! ----------------------------------------
;; This function calls add-test! multiple times, with the same proc,
;;   but different arguments.
;; name, group, proc see add-test! above
;; input-lists - each additional parameter has to be a list, specifying
;;   alternative operands for proc.
;; Now add-test! is called for each permutation of input-lists.
;;   If there's more than 1 permutation, the name is appended with
;;   "-1"..."-n" respectively.
;; Example:
;; (add-test-multiple! 'test 'general proc '(a b) '(1 2))
;; results in 4 tests, that could have been generated with
;; (add-test 'test-1 'general proc 'a 1)
;; (add-test 'test-2 'general proc 'b 1)
;; (add-test 'test-3 'general proc 'a 2)
;; (add-test 'test-4 'general proc 'b 2)
;; Note: In future versions, these tests will run simultanously
;; with multi-threading.

(define (add-test-multiple! name group proc . input-lists)
  (let* ((permutations (permute-lists input-lists))
	 (single? (and (not (null? permutations))
		       (null? (cdr permutations)))))
    (let loop ((i 0)
	       (permutations permutations))
      (if (not (null? permutations))
	  (let ((input-params (car permutations))
		(new-name (if single?
			      name
			      (string->symbol (string-append
					       (symbol->string name)
					       "-"
					       (number->string i))))))
	    (apply add-test!
		   new-name
		   group
		   proc
		   input-params)
	    (loop (+ i 1) (cdr permutations)))))))

(define (permute-lists lists)
  (cond
   ((null? lists) lists)
   ((null? (cdr lists)) (map list (car lists)))
   (else
    (let ((first-list (car lists))
	  (rest-perm (permute-lists (cdr lists))))
      (fold-right (lambda (elem result)
		    (append
		     (map (lambda (new-param)
			    (cons new-param elem))
			  first-list)
		     result))
		  '()
		  rest-perm)))))

;; --- Functions for the test-datatype ---

(define-record-type testdt :testdt
  (make-testdt  name group proc args)
  testdt?
  (name testdt-name)
  (group testdt-group)
  (proc testdt-proc)
  (args testdt-args))


;; --- Basic function to make a test ---

(define (run-test test . rest)
  (let ((silent (if (null? rest) #f (car rest)))

	(name (testdt-name test))
	(group (testdt-group test))
	(proc (testdt-proc test))
	(args (testdt-args test)))

    (let ((display-start (lambda ()
			   (display "Testing ")
			   (display group)
			   (display ":")
			   (display name)
			   (display " ... "))))
      (if (not silent)
	  (display-start))

      (call-with-current-continuation
       (lambda (k)
         (if (with-handler
              (lambda (cond more)
                (display "Error: ")
                (display cond)
                (newline)
                (k #f))
              (lambda ()
                (apply proc args)))
             (begin
               (if silent
                   (display ".")
		(display "OK\n"))
               #t)
             (begin
               (if silent
                   (begin (newline)
                          (display-start)))
               (display "Error! Input was ")
               (display args)
               (newline)
               #f)))))))

;; --- Exported functions to make a test -------------------------------
;; The following 3 functions start the testing. They all have an
;; optional parameter >silent< with default #f. if silent is #t,
;; only those tests that signaled an error are printed on the screen.
;; test-single - runs the test with that name, returns the result of proc.
;; test-group  - runs all tests that are part of that group. the result
;;               is unspecified.
;; test-all    - runs all tests in the test-suite.

(define (test-single name . rest)
  (let ((test (find-test name)))
    (if test
	(apply run-test test rest)
	(begin
	  (display "Test ") (display name)
	  (display " not found")
	  (newline)))))

(define (test-single/args name . args)
  (let* ((test (find-test name))
	 (group (testdt-group test))
	 (proc (testdt-proc test)))
    (run-test (apply make-testdt name group proc args))))

(define (test-group group . rest)
  (let ((tests (filter (lambda (test)
			 (eq? (testdt-group test)
			      group))
		       *test-list*)))
    (if (null? tests)
	(begin
	  (display "Group ") (display group)
	  (display " doesn't contain any tests")
	  (newline))
	(for-each (lambda (test)
		    (apply run-test
			   test rest))
		  tests))))

(define (test-all . rest)
  (for-each (lambda (test)
	      (apply run-test
		     test rest))
	    *test-list*))

;; --- Summary functions -------------------------------------------
;; test-summary displays all registered tests in the test-suite, if
;; called with no arguments. Calling it with the additional parameter
;; group, displays only those tests that belong to that group.

(define (test-summary . rest)
  (let ((group (if (null? rest) #f (car rest))))
    (if group
	(begin
	  (display "Listing group: ") (display group) (newline)
	  (for-each (lambda (test)
		      (if (eq? (testdt-group test) group)
			  (begin
			    (display (testdt-name test))
			    (newline))))
		    *test-list*))
	(begin
	  (display "Listing all tests in format: group:name") (newline)
	  (for-each (lambda (test)
		      (display (testdt-group test))
		      (display ":")
		      (display (testdt-name test))
		      (newline))
		    *test-list*)))))
