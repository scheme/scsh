(define-interface test-base-interface
  (export add-test!
	  add-test-multiple!
	  test-all
	  test-group
	  test-single
	  test-single/args
	  test-summary))

(define-structure test-base test-base-interface
  (open scheme-with-scsh
        handle
	list-lib
	define-record-types)
  (files test-base))

(define-structure file-system-test (export)
  (open	scheme-with-scsh
	posix-files
	test-base)
  (files file-system-tests))

(define-structure process-state-test (export)
  (open	scheme-with-scsh
	test-base)
  (files process-state-tests))

(define-structure env-test
  (export
   setenv-test
   getenv-test
   env->alist-test
   alist->env-test
   alist-delete-test
   alist-update-test
   alist-compress-test
   with-env*-test
   with-total-env*-test
   home-directory-test
   exec-path-list-test
   add-before-test
   add-after-test)
  (open scheme-with-scsh
 	thread-fluids
	list-lib
	string-lib)
  (files env-test-code))

(define-structure add-env-test
  (export)
  (open scheme-with-scsh
	test-base
        env-test)
  (files env-test-add))

(define-structure system-parameter-tests (export)
  (open scheme-with-scsh
	test-base)
  (begin
    (add-test! 'uname 'system-parameter
	       (lambda ()
		 (let ((uname-rec (uname)))
		   (> (string-length (uname:node-name uname-rec)) 0))))

    (add-test! 'system-name 'system-parameter
	       (lambda ()
		 (> (string-length (system-name)) 0)))))

(define-structure strings-and-chars-test (export)
  (open scheme-with-scsh
	test-base)
  (files strings-and-chars-test))

(define-structure file-name-maniplation-test (export)
  (open scheme-with-scsh
	test-base)
  (files file-name-manipulation-test))

(define-structure read-delimited-strings-test (export)
  (open scheme-with-scsh
	test-base)
  (files read-delimited-strings))

(define-structure bitwise-ops-test (export)
  (open scheme-with-scsh
        test-base)
  (files bitwise-ops-test))

(define-structure terminal-device-control-test (export)
  (open scheme-with-scsh
        test-base)
  (files terminal-device-control-test))

(define-structure user-and-group-db-access-test (export)
  (open scheme-with-scsh
        test-base)
  (files user-and-group-db-access))

(define-structure test-all
  (export test-all)
  (open scheme
	test-base
	file-system-test
	process-state-test
	add-env-test
	system-parameter-tests
	strings-and-chars-test
        file-name-maniplation-test
        read-delimited-strings-test
        bitwise-ops-test
        user-and-group-db-access-test))

