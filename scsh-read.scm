;;; Copyright (c) 1993, 1994 by Olin Shivers.
;;; #! comment read-macro
;;; no case-folding
;;; -flag is a symbol

;;; #! means: skip chars until newline-bang-splat-newline. 
;;; For Unix script headers.

(define script-skip
  (lambda (c port)
    (read-char port)
    (let lp ((state 0))
      (let ((advance-if (lambda (look-for)
			  (let ((c (read-char port)))
			    (if (eof-object? c)
				(error 
			 "EOF inside block comment -- #! missing a closing !#")
				(lp (cond ((char=? c look-for) (+ state 1))
					  ((char=? c #\newline) 1)
					  (else 0))))))))
	(case state
	  ((0) (advance-if #\newline))
	  ((1) (advance-if #\!))	; Found \n
	  ((2) (advance-if #\#))	; Found \n!
	  ((3) (advance-if #\newline))	; Found \n!#
	  ((4) (read port)))))))	; Found \n!#\n -- done.
;         was sub-read ^

(define-sharp-macro #\! script-skip)


;;; Readme and readme are distinct symbols.

(define preferred-case (lambda (x) x))

;;; These are now OK symbols: .. -geometry -O2 9x15 80x5+5+5 +Wn

(define (parse-token string port)
  (if (let ((c (string-ref string 0)))
	(or (char-numeric? c) (char=? c #\+) (char=? c #\-) (char=? c #\.)))
      (cond ((string->number string))
	    ((string=? string ".") dot)
	    (else (string->symbol (make-immutable! string))))
      (string->symbol (make-immutable! string))))


;;; | is now an OK symbol (for pipes).

(set-standard-syntax! #\| #f
		      (lambda (c port)
			(parse-token (sub-read-token c port) port)))

(define bel (ascii->char 7))
(define bs  (ascii->char  8))
(define ff  (ascii->char 12))
(define cr  (ascii->char 13))
(define ht  (ascii->char  9))
(define vt  (ascii->char 11))

;;; Full ANSI C strings:
;;; - read as themselves: \\ \? \" \'
;;; - control chars:
;;;   \a alert (bell -- ^g)
;;;   \b backspace (^h)
;;;   \f form feed (^l)
;;;   \n newline (^j)
;;;   \r carriage return (^m)
;;;   \t tab (^i)
;;;   \v vertical tab (^k)
;;; - octal escapes \nnn
;;; - hex escapes \xnn

;;; Is this the elegant thing to do? Too much might make it hard to shift
;;; to Unicode implementations. How about \^g for embedding control chars?
;;; And I haven't done anything about chars (as opposed to strings).

(set-standard-read-macro! #\" #t
  (lambda (c port)
    c ;ignored
    (let* ((readc (lambda ()
		    (let ((c (read-char port)))
		      (if (eof-object? c)
			  (reading-error port "end of file within a string")
			  c))))
	   (read-digit (lambda (base base-name)
			 (let* ((c (readc))
				(d (- (char->ascii c) (char->ascii #\0))))
			   (if (and (<= 0 d) (< d base)) d
			       (reading-error port
					      (string-append "invalid "
							     base-name
							     " code in string.")
					      d))))))

      (let loop ((l '()) (i 0))
	(let ((c (readc)))
	  (cond ((char=? c #\\)
		 (let* ((c (readc))
			(rc (case c
			      ((#\\ #\" #\? #\') c)
			      ((#\a) bel)
			      ((#\b) bs)
			      ((#\f) ff)
			      ((#\n) #\newline)
			      ((#\r) cr)
			      ((#\t) ht)
			      ((#\v) vt)
			      ((#\0 #\1 #\2 #\3)
			       (let* ((d1 (- (char->ascii c) (char->ascii #\0)))
				      (d2 (read-digit 8 "octal"))
				      (d3 (read-digit 8 "octal")))
				 (ascii->char (+ (* 64 d1) (+ (* 8 d2) d3)))))
			      ((#\x)
			       (let ((d1 (read-digit 16 "hex"))
				     (d2 (read-digit 16 "hex")))
				 (ascii->char (+ (* 16 d1) d2))))
			      (else
			       (reading-error port
					      "invalid escaped character in string"
					      c)))))
		   (loop (cons rc l) (+ i 1))))
		((char=? c #\")
		 (reverse-list->string l i))
		(else
		 (loop (cons c l) (+ i 1)))))))))
