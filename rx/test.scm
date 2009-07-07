;;; Test routines
;;; ,open re-posix-parsers sre-parser-package re-simp-package pp

(define (test-string)
  (let lp ()
    (write-char #\newline)
    (let ((re-s (read-line)))
      (if (not (eof-object? re-s))
	  (let ((re (posix-string->regexp re-s)))
	    (print-re re)
	    (lp))))))

(define (test-sre)
  (let lp ()
    (write-char #\newline)
    (let ((sre (read)))
      (if (not (eof-object? sre))
	  (let ((re (sre->regexp sre)))
	    (print-re re)
	    (lp))))))


(define (print-re re)
  (let ((simp-re (simplify-regexp re)))
    (cond ((static-regexp? re)
	   (receive (s lev pcount tvec) (regexp->posix-string re)
	     (format #t "plain: ~a\n       lev=~a pcount=~a tvec=~a\n"
		     s lev pcount tvec))
	   (receive (s lev pcount tvec) (regexp->posix-string simp-re)
	     (format #t "simp: ~a\n      lev=~a pcount=~a tvec=~a\n"
		     s lev pcount tvec))))
    (p (regexp->sre re))
    (p (regexp->sre simp-re))))

(define (test-match)
  (let lp ()
    (write-string "sre: ")
    (let ((sre (read)))
      (if (not (eof-object? sre))
	  (let ((re (sre->regexp sre)))
	    (let lp2 ()
	      (let ((line (read-line)))
		(cond ((not (eof-object? line))
		       (cond ((regexp-search re line) =>
			      (lambda (m)
				(format #t "Hit at [~a,~a).\n"
					(match:start m)
					(match:end m)))))
		       (lp2))
		      (else (lp))))))))))
