;;; Parse Spencer-style regexps into the regexp ADT.
;;; Olin Shivers, July 1998.

;;; One export: (posix-string->regexp s)

;;; Need better error checking on {m,n} brace parsing.

(define (parse-posix-regexp-string s)
  (receive (re i) (parse-posix-exp s 0)
    (if (= i (string-length s)) re
	(error "Illegal Posix regexp -- terminated early" s i))))

(define posix-string->regexp parse-posix-regexp-string)

;;; An complete expression is a sequence of |-separated branches.

(define (parse-posix-exp s i)
  (let ((len (string-length s)))
    (if (< i len)
	(let lp ((i i) (branches '()))
	  (receive (branch i) (parse-posix-branch s i)
	    (let ((branches (cons branch branches)))
	      (if (and (< i len)
		       (char=? #\| (string-ref s i)))
		  (lp (+ i 1) branches)
		  (values (re-choice (reverse branches)) i)))))
	(values re-trivial i))))


;;; A branch is a sequence of pieces -- stuff that goes in-between |'s.

(define (parse-posix-branch s i)
  (let ((len (string-length s)))
    (let lp ((i i) (pieces '()))
      (if (< i len)
	  (receive (piece i) (parse-posix-piece s i)
	    (let ((pieces (cons piece pieces)))
	      (if (< i len)
		  (case (string-ref s i)
		    ((#\) #\|) (values (re-seq (reverse pieces)) i))
		    (else (lp i pieces)))
		  (values (re-seq (reverse pieces)) i))))

	  (values (re-seq (reverse pieces)) i)))))


;;; A piece is an atom possibly followed by a * ? + or {...} multiplier.
;;; I.e. an element of a branch sequence.

(define (parse-posix-piece s i)
  (let ((len (string-length s)))
    (receive (atom i) (parse-posix-atom s i)
      (if (< i len)
	  (case (string-ref s i)
	    ((#\* #\+ #\?)
	     (receive (from to) (case (string-ref s i)
				  ((#\*) (values 0 #f))
				  ((#\+) (values 1 #f))
				  ((#\?) (values 0 1)))
	       (values (re-repeat from to atom) (+ i 1))))

	    ((#\{) (receive (from to i) (parse-posix-braces s (+ i 1))
		     (values (re-repeat from to atom) i)))

	    (else (values atom i)))

	  (values atom i)))))


;;; An atom is something that would bind to a following * operator --
;;; a letter, [...] charset, ^, $, or (...).

(define (parse-posix-atom s i)
  (let ((len (string-length s)))
    (if (< i (string-length s))
	(let ((c (string-ref s i)))
	  (case c
	    ((#\^) (values re-bos (+ i 1)))
	    ((#\$) (values re-eos (+ i 1)))
	    ((#\.) (values re-any (+ i 1)))
	
	    ((#\[) (parse-posix-bracket s (+ i 1)))

	    ((#\() (receive (re i) (parse-posix-exp s (+ i 1))
		     (if (and (< i len) (char=? #\) (string-ref s i)))
			 (values (re-submatch re) (+ i 1))
			 (error "Regexp subexpression has no terminating close parenthesis" s i))))

	    ((#\\) (let ((i (+ i 1)))
		     (if (< i len)
			 (values (make-re-string (string (string-ref s i)))
				 (+ i 1))
			 (error "Regexps may not terminate with a backslash" s))))

	    ((#\) #\| #\* #\+ #\? #\{)  (values re-trivial i))
	
	    (else (values (make-re-string (string c)) (+ i 1)))))

	(values re-trivial i))))


;;; Parse a [...] or [^...] bracket expression into a regexp.
;;; I is the index of the char following the left bracket.

(define db-cset (char-set #\. #\= #\:)) ; Not allowed after a #\[.

(define (parse-posix-bracket s i)
  (let ((len (string-length s)))
    (if (>= i len) (error "Missing close right bracket in regexp" s i)

	(receive (negate? i0) (let ((c (string-ref s i)))
				(if (char=? c #\^)
				    (values #t (+ i 1))
				    (values #f i)))
	  (let lp ((i i0) (cset (char-set-copy char-set:empty)))
	    (if (>= i len) (error "Missing close right bracket in regexp" s i)

		(let ((c (string-ref s i))
		      (i1 (+ i 1)))
		  (case c
		    ((#\[)
		     ;; We don't handle [..] [==] [::] frobs.
		     (if (and (< i1 len)
			      (char-set-contains? db-cset (string-ref s i1)))
			 (error "double-bracket regexps not supported." s i)
			 (lp i1 (char-set-adjoin! cset #\[))))

		    ((#\]) (if (= i i0)
			       (lp i1 (char-set-adjoin! cset #\]))
			       (let ((cset (if negate?
					       (char-set-complement! cset)
					       cset)))
				 (values (make-re-char-set cset) i1))))

		    ((#\-) (if (or (= i i0) ; first char or last char
				   (and (< i1 len)
					(char=? #\] (string-ref s i1))))
			       (lp i1 (char-set-adjoin! cset #\-))
			       (error "Illegal - in [...] regexp" s i)))

		    ;; Regular letter -- either alone, or startpoint of a range.
		    (else (if (and (< (+ i1 1) len)
				   (char=? #\- (string-ref s i1))
				   (not (char=? #\] (string-ref s (+ i1 1)))))

			      ;; Range
			      (let* ((i-tochar (+ i1 1))
				     (to (char->ascii (string-ref s i-tochar))))
				(do ((j (char->ascii c) (+ j 1))
				     (cset cset (char-set-adjoin! cset (ascii->char j))))
				    ((> j to) (lp (+ i-tochar 1) cset))))

			      ;; Just a letter
			      (lp i1 (char-set-adjoin! cset c))))))))))))


;;; Parse out a [from,to] repetition pair from a {m,n} {m} or {m,} expression.
;;; I is the index of the char following the left brace.

(define (parse-posix-braces s i)
  (let ((comma (string-index s #\, i))
	(rb (string-index s #\} i)))
    (if rb
	(if (and comma (< comma rb))
	    (values (string->number (substring s i comma))
		    (and (not (= (+ comma 1) rb))
			 (string->number (substring s (+ comma 1) rb)))
		    (+ rb 1))
	    (let ((m (string->number (substring s i rb))))
	      (values m m (+ rb 1))))
	(error "Missing close brace in regexp" s i))))
	
