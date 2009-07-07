;;; Test for the function in section 10.1 of the scsh-manual "Miscellaneous routines - Integer bitwise ops"
;;; Author: Christoph Hetz

;; for  testing: (certainly the path will be an other on other systems...)

;; ,open define-record-types handle
;; ,config ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-packages.scm
;; ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-base.scm
;; load this file
;; (test-all)

;; FIXX it - negative integers should be included

;; *** help-functions ***
;; these <bin-list>s are in reverse  order!

(define int->bin-list
  (lambda (i)
    (if (zero? i)
	'()
	(if (even? i)
	    (cons 0 (int->bin-list (/ i 2)))
	    (cons 1 (int->bin-list (/ (- i 1) 2)))))))

(define bin-list->int
  (lambda (b-l)
    (bin-list->int-1 b-l 1)))

(define bin-list->int-1
  (lambda (b-l c)
    (if (null? b-l)
	0
	(if (= 1 (car b-l))
	    (+ c (bin-list->int-1 (cdr b-l)
				  (* c 2)))
	    (bin-list->int-1 (cdr b-l)
			     (* c 2))))))

;; ---------------------------------------------------------

(define my-b-l-arithmetic-shift
  (lambda (i j)
    (if (or (null? i)
	    (zero? j))
	i
	(if (> j 0)
	    (append (let loop ((j j))
		      (if (zero? j)
			  '()
			  (cons 0 (loop (- j 1)))))
		    i)
	    (let loop ((i i)
		       (j j))
	      (if (zero? j)
		  i
		  (if (null? i)
		      '()
		      (loop (cdr i)
			    (+ j 1)))))))))

(define my-arithmetic-shift
  (lambda (i j)
    (bin-list->int (my-b-l-arithmetic-shift (int->bin-list i)
					    j))))

;; -------------------------------------------------------------

(define my-b-l-bitwise-and
  (lambda (i j)
    (let* ((i-length (length i))
	   (j-length (length j))
	   (max-length (if (> i-length j-length)
			   i-length
			   j-length)))
      (my-b-l-bitwise-and-1 (blow-up-by i (- max-length i-length))
			    (blow-up-by j (- max-length j-length))))))

(define my-b-l-bitwise-and-1
  (lambda (i j)
    (if (null? i)
	'()
	(cons (if (and (= 1 (car i))
		       (= 1 (car j)))
		  1
		  0)
	      (my-b-l-bitwise-and-1 (cdr i)
				    (cdr j))))))

(define my-bitwise-and
  (lambda (i j)
    (bin-list->int (my-b-l-bitwise-and (int->bin-list i)
				       (int->bin-list j)))))

;; -----------------------------------------------------------------

(define blow-up-by
  (lambda (l i)
    (if (zero? i)
	l
	(append l
		(let loop ((i i))
		  (if (zero? i)
		      '()
		      (cons 0 (loop (- i 1)))))))))

;; ------------------------------------------------------------------

(define my-b-l-bitwise-or
  (lambda (i j)
    (let* ((i-length (length i))
	   (j-length (length j))
	   (max-length (if (> i-length j-length)
			   i-length
			   j-length)))
      (my-b-l-bitwise-or-1 (blow-up-by i (- max-length i-length))
			   (blow-up-by j (- max-length j-length))))))

(define my-b-l-bitwise-or-1
  (lambda (i j)
    (if (null? i)
	'()
	(cons (if (or (= 1 (car i))
		      (= 1 (car j)))
		  1
		  0)
	      (my-b-l-bitwise-or-1 (cdr i)
				   (cdr j))))))

(define my-bitwise-or
  (lambda (i j)
    (bin-list->int (my-b-l-bitwise-or (int->bin-list i)
				      (int->bin-list j)))))

;; --------------------------------------------------------------------------
;; FIXX it - there should be done something
(define p-my-b-l-bitwise-not
  (lambda (i)
    (if (null? i)
	'()
	(cons (if (= 1 (car i))
		  0
		  1)
	      (p-my-b-l-bitwise-not (cdr i))))))
;
;(define my-bitwise-not
;  (lambda (i)
;    (bin-list->int (my-b-l-bitwise-not (int->bin-list i)))))

(define my-bitwise-not
  (lambda (i)
    (- (+ 1 i))))

;; -----------------------------------------------------------

(define my-b-l-bitwise-xor
  (lambda (i j)
     (let* ((i-length (length i))
	    (j-length (length j))
	    (max-length (if (> i-length j-length)
			    i-length
			    j-length)))
      (my-b-l-bitwise-xor-1 (blow-up-by i (- max-length i-length))
			    (blow-up-by j (- max-length j-length))))))

(define my-b-l-bitwise-xor-1
  (lambda (i j)
    (my-b-l-bitwise-or (my-b-l-bitwise-and i
					   (p-my-b-l-bitwise-not j))
		       (my-b-l-bitwise-and (p-my-b-l-bitwise-not i)
					   j))))

(define my-bitwise-xor
  (lambda (i j)
    (bin-list->int (my-b-l-bitwise-xor (int->bin-list i)
				       (int->bin-list j)))))

;; -----------------------------------------------------------

;; *** tests ***

(add-test-multiple! 'arithmetic-shift-test 'bitwise-ops
  (lambda (i j)
    (equal? (my-arithmetic-shift i j)
	    (arithmetic-shift i j)))
  '(1 2 3 63 64 65 127 128 129 1023 1024 1025 4294967295 4294967296 4294967297 18446744073709551615 18446744073709551616 18446744073709551617)
  '(-10 -5 -2 -1 0 1 2 5 10 15))

(add-test-multiple! 'bitwise-and-test 'bitwise-ops
  (lambda (i j)
    (equal? (my-bitwise-and i j)
	    (bitwise-and i j)))
  '(1 2 3 63 64 65 127 128 129 1023 1024 1025 4294967295 4294967296 4294967297 18446744073709551615 18446744073709551616 18446744073709551617)
  '(1 2 3 63 64 65 127 128 129 1023 1024 1025 4294967295 4294967296 4294967297 18446744073709551615 18446744073709551616 18446744073709551617))

(add-test-multiple! 'bitwise-ior-test 'bitwise-ops
  (lambda (i j)
    (equal? (my-bitwise-or i j)
	    (bitwise-ior i j)))
  '(1 2 3 63 64 65 127 128 129 1023 1024 1025 4294967295 4294967296 4294967297 18446744073709551615 18446744073709551616 18446744073709551617)
  '(1 2 3 63 64 65 127 128 129 1023 1024 1025 4294967295 4294967296 4294967297 18446744073709551615 18446744073709551616 18446744073709551617))

(add-test-multiple! 'bitwise-xor-test 'bitwise-ops
  (lambda (i j)
    (equal? (my-bitwise-xor i j)
	    (bitwise-xor i j)))
  '(1 2 3 63 64 65 127 128 129 1023 1024 1025 4294967295 4294967296 4294967297 18446744073709551615 18446744073709551616 18446744073709551617)
  '(1 2 3 63 64 65 127 128 129 1023 1024 1025 4294967295 4294967296 4294967297 18446744073709551615 18446744073709551616 18446744073709551617))

(add-test-multiple! 'bitwise-not-test 'bitwise-ops
  (lambda (i)
    (equal? (my-bitwise-not i)
	    (bitwise-not i)))
  '(1 2 3 63 64 65 127 128 129 1023 1024 1025 4294967295 4294967296 4294967297 18446744073709551615 18446744073709551616 18446744073709551617))