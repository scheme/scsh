;;; These defs are things for characters *not* in SRFIs 13 & 14.
;;; It includes some R5RS defs that are not correct in S48 in a Latin-1 world.

(define-interface char-predicates-interface
  (export
   ((char-lower-case?		; R5RS
     char-upper-case?		; R5RS
     char-alphabetic?		; R5RS
     char-numeric?		; R5RS
     char-whitespace?		; R5RS

     char-alphanumeric?		; For compatibility w/old code

     char-letter?		; Scsh
     char-digit?
     char-letter+digit?
     char-graphic?
     char-printing?
     char-blank?
     char-iso-control?
     char-punctuation?
     char-symbol?
     char-hex-digit?
     char-ascii?)		(proc (:char) :boolean))))


(define-structure char-predicates-lib char-predicates-interface
  (open error-package	; ERROR
	scsh-utilities	; DEPRECATED-PROC
	srfi-14
	(modify scheme (hide char-lower-case?
                             char-upper-case?
                             char-alphabetic?
                             char-numeric?
                             char-whitespace?)))
  (begin
    ;; These are R5RS. We can't use the native S48 ones, because they
    ;; don't handle full Latin-1.
    (define (char-lower-case? c) (char-set-contains? char-set:lower-case c))
    (define (char-upper-case? c) (char-set-contains? char-set:upper-case c))
    (define (char-alphabetic? c) (char-set-contains? char-set:letter c))
    (define (char-numeric?    c) (char-set-contains? char-set:digit    c))
    (define (char-whitespace? c) (char-set-contains? char-set:whitespace c))

    ;; These are scsh extensions to R5RS.
    (define (char-letter?       c) (char-set-contains? char-set:letter       c))
    (define (char-digit?        c) (char-set-contains? char-set:digit        c))
    (define (char-letter+digit? c) (char-set-contains? char-set:letter+digit c))
    (define (char-graphic?      c) (char-set-contains? char-set:graphic      c))
    (define (char-printing?     c) (char-set-contains? char-set:printing     c))
    (define (char-blank?        c) (char-set-contains? char-set:blank        c))
    (define (char-iso-control?  c) (char-set-contains? char-set:iso-control  c))
    (define (char-punctuation?  c) (char-set-contains? char-set:punctuation  c))
    (define (char-symbol?       c) (char-set-contains? char-set:symbol       c))
    (define (char-hex-digit?    c) (char-set-contains? char-set:hex-digit    c))
    (define (char-ascii?        c) (char-set-contains? char-set:ascii        c))

    ;; Obsolete scsh.
    (define char-alphanumeric?
      (deprecated-proc char-letter+digit? 'char-alphanumeric?
		       "Use CHAR-LETTER+DIGIT? instead.")))
  (optimize auto-integrate))
