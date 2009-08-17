;;; Meta-arg argv processor in Scheme.
;;; Copyright (c) 1995 by Olin Shivers.
;;;
;;; This is a Scheme analog of the proc2.c meta-arg expander.

;;; Syntax of the line 2 argument line:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - The only special chars are space, tab, newline, and \.
;;; - Every space char terminates an argument. 
;;;   Multiple spaces therefore introduce empty-string arguments.
;;; - A newline terminates the argument list, and will also terminate a
;;;   non-empty argument (but a newline following a space does not introduce
;;;   a final "" argument; it only terminates the argument list).
;;; - Tab is not allowed.
;;;   This is to prevent you from being screwed by thinking you had several
;;;   spaces where you really had a tab, and vice-versa.
;;; - The only other special character is \, the knock-down character. 
;;;   \ escapes \, space, tab, and newline, turning off their special 
;;;   functions. The ANSI C escapes sequences, such as \n and \t are 
;;;   supported; these also produce argument-constituents -- \n doesn't act 
;;;   like a terminating newline. \nnn for *exactly* three octal digits reads 
;;;   as the char whose ASCII code is nnn. It is an error if \ is followed by 
;;;   just 1 or 2 octal digits: \3Q is an error. Octal-escapes are always 
;;;   constituent chars. \ followed by other chars is not allowed (so we can
;;;   extend the escape-code space later if we like).
;;;
;;; You have to construct these line-2 arg lines carefully. For example,
;;; beware of trailing spaces at the end of the line. They'll give you
;;; extra trailing empty-string args.

;;; (meta-arg-process-arglist args)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expand out meta-args in argument lists.
;;;
;;; ARGS is an argument list -- a list of strings. If the first two elements
;;; are of the form ("\\" <filename> ...), then parse secondary arguments
;;; from line two of file <filename>, change the argument list to
;;;     (,@<secondary-args> <filename> ...)
;;; and loop.

(define (meta-arg-process-arglist args)
  (let lp ((args args))
    (if (and (pair? args)
	     (string=? (car args) "\\"))
	(lp (append (read-files-secondary-args (cadr args))
		    (cdr args)))
	args)))

;;; (read-files-secondary-args fname)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open file FNAME, skip the first line, and read secondary args off of
;;; line two. Return these as a list of strings.

(define read-files-secondary-args
  (let ((non-newline (char-set-complement! (char-set #\newline))))
    (lambda (fname)
      (call-with-input-file fname
	(lambda (port)
	  (skip-char-set non-newline port)	; Skip the first line
	  (read-char port)  ;read the newline
	  (read-secondary-args port))))))


;;; Read in a line of secondary args from PORT.

(define (read-secondary-args port)
  (let lp ((args '()))
    (let* ((args (cons (read-secondary-arg port) args))
	   (c (read-char port)))
      (if (or (eof-object? c) (char=? c #\newline))
	  (reverse args)
	  (lp args)))))

;;; Read in one secondary arg, but not its delimiting space or newline.

(define (read-secondary-arg port)
  (let lp ((chars '()))
    (let ((c (peek-char port)))
      (cond ((or (eof-object? c)
		 (char=? c #\newline)
		 (char=? c #\space))
	     (apply string (reverse chars)))

	    ((char=? c tab)
	     (error "Illegal tab character in meta-arg argument line."))

	    (else (lp (cons ((cond ((char=? c #\\)
				    (read-char port)
				    read-backslash-sequence)
				   (else read-char))
			     port)
			    chars)))))))


(define (read-backslash-sequence port)
  (let* ((c1 (read-char port))
	 (eof-lose (lambda () (error "Premature EOF within backslash-sequence in meta-arg argument line")))
	 (octet->int (lambda (c)
		       (cond ((eof-object? c) (eof-lose))
			     ((char-set-contains? char-set:octal-digits c)
			      (- (char->ascii c) (char->ascii #\0)))
			     (else (error "Non-octal-digit in \\nnn escape sequence in meta-arg argument line." c))))))
    
    (cond ((eof-object? c1) (eof-lose))

	  ;; This would be better handled by a char-map abstraction.
	  ((char=? c1 #\n) #\newline)
	  ((char=? c1 #\r) carriage-return)
	  ((char=? c1 #\t) tab)
	  ((char=? c1 #\b) backspace)
	  ((char=? c1 #\a) alert)
	  ((char=? c1 #\f) form-feed)
	  ((char=? c1 #\v) vertical-tab)

	  ;; \, space, tab, newline.
	  ((char-set-contains? char-set:simple-knockdown c1) c1)

	  ((char-set-contains? char-set:octal-digits c1)
	   (let* ((o64 (octet->int c1))
		  (o8  (octet->int (read-char port)))
		  (o1  (octet->int (read-char port))))
	     (ascii->char (+ o1 (* 8 (+ o8 (* 8 o64)))))))
		   
	  
	  (else (error "Illegal \\ escape sequence in meta-arg argument line."
		       c1)))))

(define char-set:octal-digits (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))

(define char-set:simple-knockdown (string->char-set "\\ \n\t"))

;;; Yechh.
(define tab (ascii->char 9))
(define carriage-return (ascii->char 13))
(define backspace (ascii->char 8))
(define alert (ascii->char 7))
(define form-feed (ascii->char 12))
(define vertical-tab (ascii->char 11))
