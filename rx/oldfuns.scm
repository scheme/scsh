;;; These functions were dropped from the regexp API when I shifted scsh's
;;; regexps over to SREs. They are retained for backwards compatibility.
;;; 	-Olin 8/98

(define (string-match re str . maybe-start)
  (apply regexp-search (->regexp re) str maybe-start))

(define make-regexp posix-string->regexp)

(define regexp-exec regexp-search)

(define (->regexp str-or-re)
  (cond ((string? str-or-re) (posix-string->regexp str-or-re))
	((regexp? str-or-re) str-or-re)
	(else (error ->regexp
		     "Value must be either a Posix regexp string or a regexp value"
		     str-or-re))))

(define (regexp-quote str)
  (receive (s lev pcount tvec) (regexp->posix-string (re-string str))
    s))
