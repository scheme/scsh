#!/usr/local/bin/scsh -s
!#

(define (main args)
  (for-each (lambda (f) (display f) (write-char #\ ))
	    args)
  (newline))

(define (ekko)
  (main command-line-arguments)
  )

(ekko)
