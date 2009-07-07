;;; Module definitions for the scsh regexp system.
;;; This is a sleazy modularisation -- we just load everything into
;;; scsh-level-0, and export from there.
;;;     -Olin <shivers@ai.mit.edu> 8/98

;; From Scheme 48, only here temporarily

(define-structure external-util (export immutable-copy-string)
  (open scheme
	primitives	;copy-bytes!
	features)	;immutable? make-immutable!
  (begin
    (define (immutable-copy-string string)
      (if (immutable? string)
	  string
	  (let ((copy (copy-string string)))
	    (make-immutable! copy)
	    copy)))

    ; Why isn't this available elsewhere?

    (define (copy-string string)
      (let* ((length (string-length string))
	     (new (make-string length #\?)))
	(copy-bytes! string 0 new 0 length)
	new))))

(define-interface posix-regexps-interface
  (export make-regexp
	  (regexp-option :syntax)
	  regexp?
	  regexp-match

	  match?
	  match-start
	  match-end
	  match-submatches
	  ))

(define-structures ((posix-regexps posix-regexps-interface)
		    (posix-regexps-internal (export make-match)))
  (open scheme define-record-types finite-types external-calls
	signals
	external-util)
  (files regexp))

(define-interface basic-re-interface
  (export (re-dsm? (proc (:value) :boolean))
	  (make-re-dsm (proc (:value :exact-integer :exact-integer) :value))
	  (re-dsm:body (proc (:value) :value))
	  (re-dsm:pre-dsm (proc (:value) :exact-integer))
	  (re-dsm:tsm (proc (:value) :exact-integer))
	  (re-dsm:posix (proc (:value) :value))
	  (set-re-dsm:posix (proc (:value :value) :unspecific))
	  ((re-dsm:post-dsm re-dsm) (proc (:value) :exact-integer))
	  (open-dsm (proc (:value) (some-values :value :exact-integer)))

	  (re-seq? (proc (:value) :boolean))
	  (really-make-re-seq (proc (:value :exact-integer :value) :value))
	  (make-re-seq/tsm (proc (:value :exact-integer) :value))
	  ((re-seq make-re-seq) (proc (:value) :value))
	  (re-seq:elts (proc (:value) :value))
	  (re-seq:tsm (proc (:value) :exact-integer))
	  (re-seq:posix (proc (:value) :value))
	  (set-re-seq:posix (proc (:value :value) :unspecific))

	  (re-choice? (proc (:value) :boolean))
	  (really-make-re-choice (proc (:value :exact-integer :value) :value))
	  (make-re-choice/tsm (proc (:value :exact-integer) :value))
	  ((make-re-choice re-choice) (proc (:value) :value))
	  (re-choice:elts (proc (:value) :value))
	  (re-choice:tsm (proc (:value) :exact-integer))
	  (re-choice:posix (proc (:value) :value))
	  (set-re-choice:posix (proc (:value :value) :unspecific))

	  (re-repeat? (proc (:value) :boolean))
	  (really-make-re-repeat (proc (:exact-integer
					:value :value
					:exact-integer :value)
				  :value))
	  (make-re-repeat/tsm (proc (:exact-integer :value :value :exact-integer )
				    :value))
	  ((re-repeat make-re-repeat)
	   (proc (:exact-integer :value :value) :value))
	  ((re-repeat:from re-repeat:tsm)
	   (proc (:value) :exact-integer))
	  (re-repeat:to (proc (:value) :value))
	  ((re-repeat:body re-repeat:posix)
	   (proc (:value) :value))
	  (set-re-repeat:posix (proc (:value :value) :unspecific))

	  (re-submatch? (proc (:value) :boolean))
	  (really-make-re-submatch (proc (:value :exact-integer :exact-integer :value)
					 :value))
	  (make-re-submatch/tsm (proc (:value :exact-integer :exact-integer) :value))
	  ((make-re-submatch re-submatch)
	   (proc (:value &opt :exact-integer :exact-integer) :value))
 
	  (re-submatch:body (proc (:value) :value))
	  ((re-submatch:pre-dsm re-submatch:tsm re-submatch:post-dsm)
	   (proc (:value) :exact-integer))
	  (re-submatch:posix (proc (:value) :value))
	  (set-re-submatch:posix (proc (:value :value) :unspecific))

	  (re-string? (proc (:value) :boolean))
	  ((make-re-string re-string) (proc (:string) :value))
	  (re-string:chars (proc (:value) :string))
	  (set-re-string:chars (proc (:value :string) :unspecific))
	  (re-string:posix (proc (:value) :value))
	  (set-re-string:posix (proc (:value :value) :unspecific))

	  re-trivial
	  (re-trivial? (proc (:value) :boolean))

	  (re-char-set? (proc (:value) :boolean))
	  ((make-re-char-set re-char-set) (proc (:value) :value))
	  (re-char-set:cset (proc (:value) :value))
	  (set-re-char-set:cset (proc (:value :value) :unspecific))
	  (re-char-set:posix (proc (:value) :value))
	  (set-re-char-set:posix (proc (:value :value) :unspecific))

	  re-empty
	  (re-empty? (proc (:value) :boolean))
	  re-bos	  re-eos
	  re-bol 	  re-eol

	  ((re-bos? re-eos? re-bol? re-eol? re-any?)
	   (proc (:value) :boolean))

	  re-any
	  re-nonl

	  (regexp? (proc (:value) :boolean))
	  (re-tsm (proc (:value) :exact-integer))

	  ;; These guys can be in code produced by RX expander.
	  (flush-submatches (proc (:value) :value))
	  (uncase (proc (:value) :value))
	  (uncase-char-set (proc (:value) :value))
	  (uncase-string (proc (:string) :value))
	  ))


;;; These guys were made obsolete by the new SRE package and exist for
;;; backwards compatibility only.
(define-interface re-old-funs-interface
  (export
   (string-match (proc (:value :string &opt :exact-integer) :value))
   (make-regexp  (proc (:string) :value))
   (regexp-exec  (proc (:value :string &opt :exact-integer) :value))
   (->regexp     (proc (:value) :value))
   (regexp-quote (proc (:string) :value))))


(define-interface re-internals-interface
  ;; These are constructors for the Scheme unparser
  (export
   (make-re-string/posix (proc (:string :string :vector) :value))
   ((make-re-seq/posix make-re-choice/posix)
    (proc (:value :exact-integer :string :vector) :value))
   (make-re-char-set/posix (proc (:value :string :vector) :value))
   (make-re-repeat/posix (proc (:exact-integer :value :value :exact-integer :string :vector)
				:value))
   (make-re-dsm/posix (proc (:value :exact-integer :exact-integer :string :vector)
			     :value))
   (make-re-submatch/posix (proc (:value :exact-integer :exact-integer :string :vector) :value))))


(define re-match-internals-interface
  (export (regexp-match:string (proc (:value) :string))
	  (regexp-match:submatches  (proc (:value) :vector))))


(define-interface posix-re-interface
  (export (regexp->posix-string (proc (:value) :string))	; posixstr.scm
	  (posix-string->regexp (proc (:string) :value))	; spencer
	  ))

(define-interface re-subst-interface
  (export
   (regexp-substitute (proc (:value :value &rest :value) :value))
   (regexp-substitute/global (proc (:value :value :string &rest :value) :value))))

(define-interface re-folders-interface
  (export
   (regexp-fold (proc (:value (proc (:exact-integer :value :value) :value)
			      :value
			      :string
			      &opt (proc (:exact-integer :value) :value)
			      :exact-integer)
		      :value))
   (regexp-fold-right (proc (:value (proc (:value :exact-integer :value) :value)
				    :value
				    :string
				    &opt (proc (:exact-integer :value) :value)
				    :exact-integer)
			    :value))
   (regexp-for-each (proc (:value (proc (:value) :unspecific)
				  :string &opt :exact-integer)
			  :unspecific))))

(define-interface re-level-0-interface
  (compound-interface posix-re-interface
		      basic-re-interface
		      (export (regexp-match? (proc (:value) :boolean))
			      (match:start (proc (:value &opt :exact-integer) :value))
			      (match:end   (proc (:value &opt :exact-integer) :value))
			      (match:substring (proc (:value &opt :exact-integer) :value))
			      (regexp-search (proc (:value :string &opt :exact-integer)
						   :value))
			      (regexp-search? (proc (:value :string &opt :exact-integer)
						   :boolean))
			      (sre->regexp (proc (:value) :value))
			      (regexp->sre (proc (:value) :value))
			      (simplify-regexp (proc (:value) :value))
			      )))


(define-structures ((re-level-0 re-level-0-interface)
		    (re-match-internals re-match-internals-interface)
		    (re-internals re-internals-interface)
		    (sre-syntax-tools (export (if-sre-form :syntax)
					      sre-form?
					      parse-sre parse-sres
					      regexp->scheme
					      static-regexp?))
		    (standard-char-sets (export nonl-chars word-chars))
		    (sre-internal-syntax-tools (export expand-rx)))
  (open defrec-package
	weak		
	;; re-posix-parsers	; regexp->posix-string
	let-opt
	sort				; Posix renderer
	define-record-types
	defrec-package
	receiving
	scsh-utilities
	(subset srfi-1 (fold every fold-right))
	srfi-14
	error-package
	ascii
	primitives			; JMG add-finalizer!
	define-record-types		; JMG debugging
	external-calls
	srfi-13				; string-fold
	posix-regexps
	scheme)

  (files re-low re simp re-high
	 parse posixstr spencer re-syntax)

  (begin (define-syntax if-sre-form
	   (lambda (exp r c)
	     (if (sre-form? (cadr exp) r c)
		 (caddr exp)
		 (cadddr exp)))))

 ; (optimize auto-integrate)
  )


;;; Stuff that could appear in code produced by (rx ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-interface rx-lib-interface
  (compound-interface (export coerce-dynamic-regexp
			      coerce-dynamic-charset
			      spec->char-set
			      flush-submatches
			      uncase
			      uncase-char-set
			      uncase-string)
		      re-internals-interface))

(define-structure rx-lib rx-lib-interface
  (open re-internals
	re-level-0
	(subset srfi-1 (fold))
	srfi-14
	error-package
	ascii
	scheme)
  (files rx-lib)
;  (optimize auto-integrate)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-interface rx-syntax-interface (export (rx :syntax)))

(define-structure rx-syntax rx-syntax-interface
  (open re-level-0
	srfi-14
	rx-lib
	standard-char-sets
	scheme)
  (for-syntax (open sre-internal-syntax-tools scheme))
  (begin (define-syntax rx expand-rx))
;  (optimize auto-integrate)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-interface re-match-syntax-interface
  (export (let-match  :syntax)
	  (if-match   :syntax)
	  (match-cond :syntax)))

(define-structure re-match-syntax re-match-syntax-interface
  (for-syntax (open scheme
		    signals))	; For ERROR
  (open re-level-0 scheme)
  (access signals) ; for ERROR
  (files re-match-syntax))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-structure re-old-funs re-old-funs-interface
  (open re-level-0 error-package receiving scheme)
  (files oldfuns)
;  (optimize auto-integrate)
)



(define-structure re-subst re-subst-interface
  (open re-level-0
	re-match-internals
	posix-regexps
	(subset srfi-1 (fold))
	scsh-level-0	; write-string
	srfi-13		; string-copy!
	scheme)
  (files re-subst)
;  (optimize auto-integrate)
)


(define-structure re-folders re-folders-interface
  (open re-level-0 let-opt error-package scheme)
  (files re-fold)
;  (optimize auto-integrate)
)


(define-interface re-exports-interface
  (compound-interface re-level-0-interface
		      rx-syntax-interface
		      re-subst-interface
		      re-match-syntax-interface
		      re-folders-interface))

(define-structure re-exports re-exports-interface
  (open rx-syntax
	re-level-0
	re-subst
	re-match-syntax
	re-folders)
;  (optimize auto-integrate)
)


;;; File	Exports
;;; ----	-------
;;; parse	sre->regexp regexp->sre  
;;;             parse-sre parse-sres regexp->scheme
;;;             char-set->in-pair static-regexp?
;;; posixstr	regexp->posix-string
;;; re-high	compile-regexp regexp-search regexp-search? 
;;; re-subst	regexp-substitute regexp-substitute/global
;;; re-low	match:start match:end match:substring
;;;             CRE record, new-cre
;;;             cre-search cre-search?
;;; re-syntax	sre-form? if-sre-form expand-rx
;;; re.scm	The ADT. flush-submatches uncase uncase-char-set
;;;             char-set-full? char-set-empty?
;;;             re-char-class? static-char-class?
;;; rx-lib	coerce-dynamic-regexp coerce-dynamic-charset spec->char-set
;;; simp	simplify-regexp
;;; spencer	posix-string->regexp
