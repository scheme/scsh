;;; SRE syntax support for regular expressions
;;; Olin Shivers, June 1998.

;;; Export SRE-FORM?, EXPAND-RX

;;; Is the form an SRE expression?
;;; We only shallowly check the initial keyword of a compound form.

(define (sre-form? exp r same?)			; An SRE is
  (let ((kw? (lambda (x kw) (same? x (r kw)))))		
    (or (string? exp)				; "foo"
	(and (pair? exp)
	     (let ((head (car exp)))
	       (or (every string? exp)		; ("aeiou")
		   (kw? head '*)		; (*  re ...)
		   (kw? head '+)		; (+  re ...)
		   (kw? head '?)		; (?  re ...)
		   (kw? head '=)		; (=  n re ...)
		   (kw? head '>=)		; (>= n re ...)
		   (kw? head '**)		; (** m n re ...)

		   (kw? head '|)		; (| re ...)
		   (kw? head 'or)		; (| re ...)
		   (kw? head ':)		; (: re ...)
		   (kw? head 'seq)		; (: re ...)

		   (kw? head '-)		; (- re ...)
		   (kw? head '&)		; (& re ...)
		   (kw? head '~)		; (~ re ...)

		   (kw? head 'submatch)		; (submatch re ...)
		   (kw? head 'dsm)		; (dsm pre post re ...)

		   (kw? head 'uncase)		; (uncase re ...)
		   (kw? head 'w/case)		; (w/case re ...)
		   (kw? head 'w/nocase)		; (w/nocase re ...)

		   (kw? head 'unquote)		; ,exp
		   (kw? head 'unquote-splicing)	; ,@exp

		   (kw? head 'posix-string))))	; (posix-string string)

	(kw? exp 'any)				; any
	(kw? exp 'nonl)				; nonl
	(kw? exp 'bos) (kw? exp 'eos)		; bos / eos
	(kw? exp 'bol) (kw? exp 'eol)		; bol / eol

	(kw? exp 'lower-case)	(kw? exp 'lower); The char class names
	(kw? exp 'upper-case)	(kw? exp 'upper)
	(kw? exp 'alphabetic)	(kw? exp 'alpha)
	(kw? exp 'numeric)	(kw? exp 'num)		(kw? exp 'digit)
	(kw? exp 'alphanumeric)	(kw? exp 'alphanum)	(kw? exp 'alnum)
	(kw? exp 'blank)
	(kw? exp 'control)	(kw? exp 'cntrl)
	(kw? exp 'printing)	(kw? exp 'print)
	(kw? exp 'punctuation)	(kw? exp 'punct)
	(kw? exp 'hex-digit)	(kw? exp 'hex)		(kw? exp 'xdigit)
	(kw? exp 'graphic)	(kw? exp 'graph)
	(kw? exp 'whitespace)	(kw? exp 'white)	(kw? exp 'space)
	(kw? exp 'ascii))))


;;; (if-sre-form form conseq-form alt-form)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If FORM is an SRE, expand into CONSEQ-FORM, otherwise ALT-FORM.
;;; This is useful for expanding a subform of a macro that can
;;; be either a regexp or something else, e.g.
;;;     (if-sre-form test			; If TEST is a regexp,
;;;       (regexp-search? (rx test) line)	; match it against the line,
;;;       (test line))				; otw it's a predicate.

;;; The macro is actually defined directly in the module file.
;;; (define-syntax if-sre-form
;;;   (lambda (exp r c)
;;;     (if (sre-form? (cadr exp) r c)
;;;         (caddr exp)
;;;         (cadddr exp))))


;;; (RX re ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The basic SRE form.

(define (expand-rx exp r c)
    (let ((re (simplify-regexp (parse-sres (cdr exp) r c))))

      ;; If it's static, pre-compute the Posix string & tvec now,
      ;; so the re->scheme unparser will find it and toss it into 
      ;; the constructor. We do this only for the top-level regexp.
      (if (static-regexp? re) (compile-regexp re))

      (regexp->scheme re r)))


;(define-syntax rx (syntax-rules () ((rx stuff ...) (really-rx stuff ...))))
;(define-syntax really-rx
;  (syntax-rules () ((really-rx stuff ...) (rx/cs stuff ...))))
;
;(define-syntax rx/cs (lambda (exp r c) (expand-rx exp #t r c)))
;(define-syntax rx/ci (lambda (exp r c) (expand-rx exp #f r c)))
;
;(define-syntax case-sensitive
;  (lambda (exp r c)
;    (let ((%ls (r 'let-syntax))
;	  (%really-rx (r 'really-rx))
;	  (%sr (r 'syntax-rules))
;	  (%rx/cs (r 'rx/cs)))
;    `(,ls ((,%really-rx (,sr () ((,%really-rx stuff ...) (,%rx/cs stuff ...)))))
;       . ,(cdr exp)))))
				 
