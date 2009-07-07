;;; Tests for the functions in section 3.5 of the scsh-manual "Process state"   
;;; Author: David Frese

;; Notes: Only umask and cwd stuff, everything else isn't implemented yet.

;; --- umask stuff ---

(add-test! 'with-umask 'process-state
	   (lambda (new-umask)
	     (let ((old-umask (umask)))
	       (and
		(with-umask new-umask
			    (= (umask) new-umask))
		(= (umask) old-umask))))
	   0)

(add-test! 'set-umask 'process-state
	   (lambda (new-umask)
	     (let ((old-umask (umask)))
	       (set-umask new-umask)
	       (let ((res (umask)))
		 (set-umask old-umask)
		 (= res new-umask))))
	   7)

;; --- cwd stuff ---

(add-test! 'with-cwd 'process-state
	   (lambda (new-cwd)
	     (let ((old-cwd (cwd)))
	       (and
		(with-cwd new-cwd
			  (equal? (cwd) new-cwd))
		(equal? (cwd) old-cwd))))
	   "/")

(add-test! 'chdir 'process-state
	   (lambda (new-cwd)
	     (let ((old-cwd (cwd)))
	       (chdir new-cwd)
	       (let ((res (cwd)))
		 (chdir old-cwd)
		 (equal? res new-cwd))))
	   "/")

