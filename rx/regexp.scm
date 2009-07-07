; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees. See file COPYING.

; Exports:
;   make-regexp
;   regexp?
;   regexp-match
;   regexp-match?
;   regexp-match-start
;   regexp-match-end
;   regexp-option

; The compiled version of the expression is produced when needed.

(define-record-type regexp :regexp
  (really-make-regexp pattern compiled
		      extended? ignore-case? submatches? newline?)
  regexp?
  (pattern regexp-pattern)                             ; immutable string
  (compiled real-regexp-compiled set-regexp-compiled!) ; #f or a c-record
  (extended?    regexp-extended?)		       ; four flags
  (ignore-case? regexp-ignore-case?)
  (submatches?  regexp-submatches?)
  (newline?     regexp-newline?))

; Drop the compiled version when resuming.  We may be resuming on a different
; architecture, or version of the library, or whatever.

(define-record-resumer :regexp
  (lambda (regexp)
    (set-regexp-compiled! regexp #f)))

; There are four options when making a regular expression.

(define-enumerated-type regexp-option :regexp-option
  regexp-option?
  regexp-options
  regexp-option-name
  regexp-option-index
  (extended ignore-case submatches newline))

; Loop down finding which options are present and checking for duplicates.
; This is not specific to regular expressions.
;
; It would be nice if this could handle values as well, as in
; (make-regexp "sldkjf" (regexp-option size 10))

(define (decode-boolean-options options all-options predicate indexer)
  (let ((map (make-vector (vector-length all-options) #f)))
    (let loop ((options options))
      (if (null? options)
	  (vector->list map)
	  (let ((option (car options)))
	    (if (predicate option)
		(let ((index (indexer option)))
		  (if (vector-ref map index)
		      'duplicates
		      (begin
			(vector-set! map index #t)
			(loop (cdr options)))))
		'bad-value))))))

; The only thing we do here is to decode the options and make sure that the
; pattern is immutable, as it won't be used until later.

(define (make-regexp pattern . options)
  (let ((options (decode-boolean-options options
					 regexp-options
					 regexp-option?
					 regexp-option-index)))
    (if (and (string? pattern)
	     (pair? options))
	(let* ((pattern (immutable-copy-string pattern))
	       (regexp (apply really-make-regexp pattern #f options)))
	  (add-finalizer! regexp free-compiled-regexp)
	  regexp)
	(apply call-error "invalid argument(s)"
	                  make-regexp
		          pattern
			  options))))

; Free up the C-heap storage used for the compiled regexp.

(define (free-compiled-regexp regexp)
  (let ((compiled (real-regexp-compiled regexp)))
    (if compiled
	(call-imported-binding posix-free-regexp compiled))))

; We compile the pattern if that hasn't already been done, raising an error
; if anything goes wrong.

(define (regexp-compiled regexp)
  (or (real-regexp-compiled regexp)
      (let ((compiled (call-imported-binding posix-compile-regexp
					     (regexp-pattern regexp)
					     (regexp-extended? regexp)
					     (regexp-ignore-case? regexp)
					     (regexp-submatches? regexp)
					     (regexp-newline? regexp))))
	(if (not (integer? compiled))
	    (begin
	      (set-regexp-compiled! regexp compiled)
	      compiled)
	    (let ((message (call-imported-binding posix-regexp-error-message
						  (regexp-pattern regexp)
						  (regexp-extended? regexp)
						  (regexp-ignore-case? regexp)
						  (regexp-submatches? regexp)
						  (regexp-newline? regexp))))
	      (error (if message
			 (string-append "Posix regexp ("
					(regexp-pattern regexp)
					") : " message)
			 "inconsistent results from Posix regexp compiler")
		     regexp))))))

; Call the pattern matcher.  We return #F if the match fails.  On a successful
; match, we either return #T or a list of match records, depending on the value
; of SUBMATCHES?.

(define (regexp-match regexp string submatches? starts-line? ends-line? . rest)
  (if (and (regexp? regexp)
	   (string? string))
      (let ((start (if (null? rest)
		       0
		       (car rest))))
	(call-imported-binding posix-regexp-match
			       (regexp-compiled regexp)
			       string
			       submatches?
			       starts-line?
			       ends-line?
			       start))
      (call-error "invalid argument"
		  regexp-match
		  regexp string starts-line? ends-line?)))
  
; These are made by the C code.  The SUBMATCHES field is not used by us,
; but is used by the functional interface.

(define-record-type match :match
  (make-match start end submatches)
  match?
  (start match-start)
  (end match-end)
  (submatches match-submatches))

(define-record-discloser :match
  (lambda (rem)
    (list 'match
	  (match-start rem)
	  (match-end rem))))

(define-exported-binding "posix-regexp-match-type" :match)

; The various C functions we use.

(import-definition posix-compile-regexp)
(import-definition posix-regexp-match)
(import-definition posix-regexp-error-message)
(import-definition posix-free-regexp)

