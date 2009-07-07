;;; Test for the function in section 8.2 of the scsh-manual "awk"
;;; Author: Christoph Hetz

;; for  testing: (certainly the path will be an other on other systems...)

;; ,open define-record-types handle
;; ,config ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-packages.scm
;; ,load C:/cygwin/home/mephisto/cvs-scsh/scsh/scsh/test/test-base.scm
;; load this file
;; (test-all)



;; *** basic help-functions ***

(define ascii->string
  (lambda (i)
    (list->string (list (ascii->char i)))))

(define char->string
  (lambda (ch)
    (list->string (list ch))))

;; *** tests ***

;; --- is the <counter> incremented correct ---

(add-test! 'counter-inc-test 'awk
  (lambda ()
    (let ((read '())
	  (string (let loop ((i 0))
		    (if (not (= 9 i))
			(begin
			  (string-append "test-zeile\n"  
					 (loop (+ i 1))))
			""))))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      (#t (set! read (cons counter read)))))
       (make-string-input-port string))
      (equal? read '(9 8 7 6 5 4 3 2 1)))))

;; --- does the "int-test" work properly ---

(add-test! 'int-test-test 'awk
  (lambda ()
    (let ((read '())
	  (string (let loop ((i 0))
		    (if (not (= 9 i))
			(begin
			  (string-append "test-zeile\n"  
					 (loop (+ i 1))))
			""))))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      (1 (set! read (cons 1 read)))
	      (2 (set! read (cons 2 read)))
	      (3 (set! read (cons 3 read)))
	      (4 (set! read (cons 4 read)))
	      (5 (set! read (cons 5 read)))
	      (6 (set! read (cons 6 read)))
	      (7 (set! read (cons 7 read)))
	      (8 (set! read (cons 8 read)))
	      (9 (set! read (cons 9 read)))
	      (0 (set! read (cons 0 read)))))
       (make-string-input-port string))
      (equal? read '(9 8 7 6 5 4 3 2 1)))))

;; --- big line ---

;(add-test! 'read-one-mb-line-from-file 'awk
;	   (lambda ()
;	     (let* ((one-kb-line (let loop ((i 0))
;				   (if (= 1024 i)
;				       ""
;				       (string-append "a" (loop (+ i 1))))))
;		    (one-mb-line (let loop ((i 0))
;				   (if (= 1024 i)
;				       ""
;				       (string-append one-kb-line (loop (+ i 1))))))	     
;		    (read '()))
;	       ((lambda (in-port)
;		  (awk (read-line in-port) (line) c ()
;		       (#t (set! read line))))
;		(make-string-input-port one-mb-line))
;	       (equal? read one-mb-line))))

;; --- special signs --- 

(add-test! 'read-special-signs 'awk
  (lambda ()
    (let (( strange-sign-line
	    (let loop ((i 0))
	      (if (= i 256)
		  ""
		  (if (= i 10)       ;; works with everything but line-feed
		      (loop (+ i 1))
		      (string-append (ascii->string i)
				     (loop (+ i 1)))))))
	  (read '()))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) ()
	      (#t (set! read line)))) 
       (make-string-input-port strange-sign-line))
      (equal? read strange-sign-line))))

;; --- sre-expr-test ---

(add-test! 'sre-expr-test-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "sre-expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) ()
	      (("sre" "zu") (set! read (cons 'sre-zu read)))
	      ("eine" (set! read (cons 'eine read)))
	      ("EINE" (set! read (cons 'EINE read)))
	      ((* "3") (set! read (cons '*3 read)))
	      ((? "s") (set! read (cons '?s read)))
	      ((+ "+") (set! read (cons '++ read)))))
       (make-string-input-port str))
;;                 |z6         |z5                   |z4                   
      (equal? (list '++ '?s '*3 '?s '*3 'eine 'sre-zu '?s '*3 'eine 'sre-zu
;;                 |z3           |z2             |z1             |
		    '?s '*3 'EINE '?s '*3 'sre-zu '?s '*3 'sre-zu)
	      read))))

;; --- when-test ---

(add-test! 'when-bool-exp-test-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "when-bool-expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      ((when (= counter 1)) 
	       (set! read (cons 'first-clause read)))
	      ((when (equal? line 
			     "when-bool-expr-test zu prüfen:"))
	       (set! read (cons 'second-clause read)))
	      ((when (> counter 2))
	       (set! read (cons 'third-clause read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'third-clause 'third-clause 'third-clause 
		    'third-clause 'second-clause 'first-clause)))))

;; --- expr-test-test ---

(add-test! 'expr-test-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      ("paar" (set! read (cons 'first-clause read)))
	      ((equal? line
		       "expr-test zu prüfen:")
	       (set! read (cons 'second-clause read)))
	      ((> counter 5)
	       (set! read (cons 'third-clause read)))
	      ((+ "3")                                  
	       (set! read (cons 'fourth-clause read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'third-clause 'fourth-clause 'second-clause 'first-clause)))))

;; --- several-bodys-in-clause-test --- 
;; XX to do: only for <test>s that were ok till now (int, when)

(add-test! 'several-bodys-in-clause-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      (1 (set! read (cons 'clause-one-body-one read))
		 (set! read (cons 'clause-one-body-two read))
		 (set! read (cons 'clause-one-body-three read)))
	      ((when (equal? line
			     "eine zeile klein..."))
	       (set! read (cons 'clause-two-body-one read))
	       (set! read (cons 'clause-two-body-two read))
	       (set! read (cons 'clause-two-body-three read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'clause-two-body-three 'clause-two-body-two 'clause-two-body-one 
		    'clause-one-body-three 'clause-one-body-two 'clause-one-body-one)))))

;; --- range-wo-begin-wo-end-test --- 
;; XX to do: only ok <test>s ... s.u.

(add-test! 'range-wo-begin-wo-end-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      (range 1 3 (set! read (cons 'first-clause read)))
	      (range (when (equal? line 
				   "EINE ZEILE GRO/3..."))
		     (when (equal? line
				   "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}"))
		     (set! read (cons 'second-clause read)))
	      (range (when (equal? line
				   "expr-test zu prüfen:"))
		     4
		     (set! read (cons 'third-clause read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'second-clause 'second-clause 'third-clause 'first-clause)))))

;; --- range-w-begin-wo-end-test --- 
;; XX to do: only ok <test>s ... s.u.

(add-test! 'range-w-begin-wo-end-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      (:range 1 3 (set! read (cons 'first-clause read)))
	      (:range (when (equal? line 
				    "EINE ZEILE GRO/3..."))
		      (when (equal? line
				    "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}"))
		      (set! read (cons 'second-clause read)))
	      (:range (when (equal? line
				    "expr-test zu prüfen:"))
		      4
		      (set! read (cons 'third-clause read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'second-clause 'second-clause 'third-clause 'second-clause 
		    'third-clause 'first-clause 'first-clause)))))

;; --- range-wo-begin-w-end-test --- 
;; XX to do: only ok <test>s ... s.u.

(add-test! 'range-wo-begin-w-end-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      (range: 1 3 (set! read (cons 'first-clause read)))
	      (range: (when (equal? line 
				    "EINE ZEILE GRO/3..."))
		      (when (equal? line
				    "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}"))
		      (set! read (cons 'second-clause read)))
	      (range: (when (equal? line
				    "expr-test zu prüfen:"))
		      4
		      (set! read (cons 'third-clause read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'second-clause 'second-clause 'third-clause 'second-clause 
		    'third-clause 'first-clause 'first-clause)))))

;; --- range-w-begin-w-end-test --- 
;; XX to do: only ok <test>s ... s.u.

(add-test! 'range-w-begin-w-end-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      (:range: 1 3 (set! read (cons 'first-clause read)))
	      (:range: (when (equal? line 
				     "EINE ZEILE GRO/3..."))
		       (when (equal? line
				     "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}"))
		       (set! read (cons 'second-clause read)))
	      (:range: (when (equal? line
				     "expr-test zu prüfen:"))
		       4
		       (set! read (cons 'third-clause read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'second-clause 'second-clause 'third-clause 'second-clause 'third-clause 
		    'second-clause 'first-clause 'third-clause 'first-clause 'first-clause)))))

;; --- else-test ---

(add-test! 'else-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) ()
	      (1 (set! read (cons 'first-clause read)))
	      (else (set! read (cons 'second-clause read)))
	      (4 (set! read (cons 'third-clause read)))
	      (5 (set! read (cons 'fourth-clause read)))
	      (else (set! read (cons 'fifth-clause read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'fifth-clause 'second-clause 'fourth-clause 'second-clause 'third-clause  
		    'second-clause 'fifth-clause 'second-clause 'fifth-clause 'second-clause 
		    'fifth-clause 'first-clause)))))


;; --- test=>expr-test ---

(add-test! 'test=>expr-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ()
	      (counter => (lambda (c)
			    (set! read (cons c read))))
	      (#f  => (lambda (c)
			(set! read (cons c read))))))
       (make-string-input-port str))
      (equal? read (list 6 5 4 3 2 1)))))


;; --- after-test ---

(add-test! 'after-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      (set! read
	    ((lambda (in-port)
	       (awk (read-line in-port) (line) ()
		    (1 (set! read 1))
		    (2 (set! read 2))
		    (after 'return)))
	     (make-string-input-port str)))
      (equal? read 'return))))


;; --- var-decl-test ---

(add-test! 'var-decl-test 'awk
  (lambda ()
    (let ((read 0)
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk (read-line in-port) (line) counter ((i 0)
						  (x 2)
						  (y 3))
	      (1 (values (+ x y) x y))
	      (2 (values i (+ i y) y))
	      (3 (values (* i 2) x y))
	      (4 (values (- i y) x y))
	      (5 (values (* i x) x y))
	      (6 (set! read i)
		 (values i x y))))
       (make-string-input-port str))
      (= read 56))))


;; --- multiple-return-values-of-next-record-test ---

(add-test! 'multiple-return-values-of-next-record-test 'awk
  (lambda ()
    (let ((read '())
	  (str (string-append "ein paar testzeilen, um\n" 
			      "expr-test zu prüfen:\n"
			      "EINE ZEILE GRO/3...\n" 
			      "eine zeile klein...\n"
			      "eine zeile mit zeichen...\n"
			      "*+#'~,;:.-_<>|!§$%&/()=?\"\\[]{}\n")))
      ((lambda (in-port)
	 (awk ((lambda ()
		 (values (read-line in-port)1 2 'a 'b))) (line x y a b) counter ()
		 (1 (set! read (cons x read)))
		 (2 (set! read (cons y read)))
		 (3 (set! read (cons a read)))
		 (4 (set! read (cons b read)))))
       (make-string-input-port str))
      (equal? read
	      (list 'b 'a 2 1)))))
