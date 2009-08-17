;;; Char->char partial maps					-*- Scheme -*-
;;; Copyright (C) 1998 by Olin Shivers.

;;; CCPs are an efficient data structure for doing simple string transforms,
;;; similar to the kinds of things you would do with the tr(1) program.
;;;
;;; This code is tuned for a 7- or 8-bit character type. Large, 16-bit
;;; character types would need a more sophisticated data structure, tuned
;;; for sparseness. I would suggest something like this:
;;;     (define-record ccp
;;; 	    domain		; The domain char-set
;;; 	    map			; Sorted vector of (char . string) pairs 
;;; 				;   specifying the map.
;;; 	    id?)		; If true, mappings not specified by MAP are 
;;; 	    			;   identity mapping. If false, MAP must 
;;; 				;   specify a mapping for every char in DOMAIN.
;;; 
;;; A (char . string) elements in MAP specifies a mapping for the contiguous
;;; sequence of L chars beginning with CHAR (in the sequence of the underlying
;;; char type representation), where L is the length of STRING. These MAP elements
;;; are sorted by CHAR, so that binary search can be used to get from an input
;;; character C to the right MAP element quickly.
;;; 
;;; This representation should be reasonably compact for standard mappings on,
;;; say, a Unicode CCP. An implementation might wish to have a cache field
;;; in the record for storing the full 8kb bitset when performing ccp-map
;;; operations. Or, an implementation might want to store the Latin-1 subset
;;; of the map in a dense format, and keep the remainder in a sparse format.

(define num-chars (char-set-size char-set:full)) ; AKA 256.

(define-record ccp
  domain		; The domain char-set
  dshared?		; Is the domain value shared or linear?
  map			; 256-elt string
  mshared?)		; Is the map string shared or linear?


;;; Accessors and setters that manage the linear bookkeeping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ccp-domain ccp)
  (set-ccp:dshared? ccp #t)
  (ccp:domain ccp))

;;; CCP is a linear ccp. PROC is a domain->domain function; it must be
;;; linear in its parameter and result.
;;;
;;; Updates the domain of the CCP with PROC, returns the resulting
;;; CCP; reuses the old one to construct the new one.

(define (restrict-linear-ccp-domain ccp proc)
  (let ((new-d (proc (if (ccp:dshared? ccp)
			 (begin (set-ccp:dshared? ccp #f)
				(char-set-copy (ccp:domain ccp)))
			 (ccp:domain ccp)))))
    (set-ccp:domain ccp new-d)
    ccp))

;;; CCP is a linear CCP. PROC is a domain x cmap -> domain function.
;;; It is passed a linear domain and cmap string. It may side-effect
;;; the cmap string, and returns the resulting updated domain.
;;; We return the resulting CCP, reusing the parameter to construct it.

(define (linear-update-ccp ccp proc)
  (let* ((cmap (if (ccp:mshared? ccp)
		   (begin (set-ccp:mshared? ccp #f)
			  (string-copy (ccp:map ccp)))
		   (ccp:map ccp)))

	 (new-d (proc (if (ccp:dshared? ccp)
			  (begin (set-ccp:dshared? ccp #f)
				 (char-set-copy (ccp:domain ccp)))
			  (ccp:domain ccp))
		      cmap)))
    (set-ccp:domain ccp new-d)
    ccp))



;;; Return CCP's map field, and mark it as shared. CCP functions that
;;; restrict a ccp's domain share map strings, so they use this guy.
(define (ccp:map/shared ccp)
  (set-ccp:mshared? ccp #t)
  (ccp:map ccp))

(define (ccp-copy ccp) (make-ccp (char-set-copy (ccp:domain ccp)) #f
				 (string-copy (ccp:map ccp))      #f))

;;; N-ary equality relation for partial maps

(define (ccp= ccp1 . rest)
  (let ((domain (ccp:domain ccp1))
	(cmap (ccp:map ccp1)))
    (every (lambda (ccp2)
	     (and (char-set= domain (ccp:domain ccp2))
		  (let ((cmap2 (ccp:map ccp2)))
		    (char-set-every (lambda (c)
				      (let ((i (char->ascii c)))
					(char=? (string-ref cmap  i)
						(string-ref cmap2 i))))
				    domain))))
	   rest)))


;;; N-ary subset relation for partial maps

(define (ccp<= ccp1 . rest)
  (let lp ((domain1 (ccp:domain ccp1))
	   (cmap1 (ccp:map ccp1))
	   (rest rest))
    (or (not (pair? rest))
	(let* ((ccp2 (car rest))
	       (domain2 (ccp:domain ccp2))
	       (cmap2   (ccp:map    ccp2))
	       (rest (cdr rest)))
	  (and (char-set<= domain1 domain2)
	       (let ((cmap2 (ccp:map ccp2)))
		 (char-set-every (lambda (c)
				   (let ((i (char->ascii c)))
				     (char=? (string-ref cmap1 i)
					     (string-ref cmap2 i))))
				 domain1))
	       (lp domain2 cmap2 rest))))))


;;; CCP iterators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ccp-fold kons knil ccp)
  (let ((cmap (ccp:map ccp)))
    (char-set-fold (lambda (c v) (kons c (string-ref cmap (char->ascii c)) v))
		   knil
		   (ccp:domain ccp))))

(define (ccp-for-each proc ccp)
  (let ((cmap (ccp:map ccp)))
    (char-set-for-each (lambda (c) (proc c (string-ref cmap (char->ascii c))))
		       (ccp:domain ccp))))

(define (ccp->alist ccp)
  (ccp-fold (lambda (from to alist) (cons (cons from to) alist))
	    '()
	    ccp))


;;; CCP-RESTRICT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restrict a ccp's domain.

(define (ccp-restrict ccp cset)
  (make-ccp (char-set-intersection cset (ccp:domain ccp))
	    #f
	    (ccp:map/shared ccp)
	    #t))

(define (ccp-restrict! ccp cset)
  (restrict-linear-ccp-domain ccp (lambda (d) (char-set-intersection! d cset))))
					      

;;; CCP-ADJOIN ccp from-char1 to-char1 ... 
;;; CCP-DELETE ccp char1 ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add & delete mappings to/from a ccp.

(define (ccp-delete ccp . chars)
  (make-ccp (apply char-set-delete (ccp:domain ccp) chars)
	    #f
	    (ccp:map/shared ccp)
	    #t))

(define (ccp-delete! ccp . chars)
  (restrict-linear-ccp-domain ccp (lambda (d) (apply char-set-delete! d chars))))


(define (ccp-adjoin ccp . chars)
  (let ((cmap (string-copy (ccp:map ccp))))
    (make-ccp (install-ccp-adjoin! cmap (char-set-copy (ccp:domain ccp)) chars)
	      #f
	      cmap
	      #f)))

(define (ccp-adjoin! ccp . chars)
  (linear-update-ccp ccp (lambda (d cmap) (install-ccp-adjoin! cmap d chars))))

(define (install-ccp-adjoin! cmap domain chars)
  (let lp ((chars chars) (d domain))
    (if (pair? chars)
	(let ((from  (car  chars))
	      (to    (cadr chars))
	      (chars (cddr chars)))
	  (string-set! cmap (char->ascii from) to)
	  (lp chars (char-set-adjoin! d from)))
	d)))


;;; CCP-EXTEND ccp1 ...
;;; CCP-EXTEND! ccp1 ccp2 ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extend ccp1 with ccp2, etc.

(define (ccp-extend . ccps)
  (if (pair? ccps)
      (let ((ccp0 (car ccps))
	    (ccps (cdr ccps)))
	(if (pair? ccps)
	    (let ((cmap (string-copy (ccp:map ccp0)))) ; Copy cmap.
	      ;; The FOLD installs each ccp in CCPS into CMAP and produces
	      ;; the new domain.
	      (make-ccp (fold (lambda (ccp d)
				(install-ccp-extension! cmap d ccp))
			      (char-set-copy (ccp:domain ccp0))
			      ccps)
			#f cmap #f))

	    ccp0))	; Only 1 parameter

      ccp:0))	; 0 parameters

(define (ccp-extend! ccp0 . ccps)
  (linear-update-ccp ccp0
    (lambda (domain cmap)
      (fold (lambda (ccp d) (install-ccp-extension! cmap d ccp))
	    domain
	    ccps))))


;;; Side-effect CMAP, linear-update and return DOMAIN.
(define (install-ccp-extension! cmap domain ccp)
  (let ((cmap1 (ccp:map ccp))
	(domain1 (ccp:domain ccp)))
    (char-set-for-each (lambda (c)
			 (let ((i (char->ascii c)))
			   (string-set! cmap i (string-ref cmap1 i))))
		       domain1)
    (char-set-union! domain domain1)))


;;; Compose the CCPs. 0-ary case: (ccp-compose) = ccp:1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For each character C-IN in the original domain, we push it
;;; through the pipeline of CCPs. If we ever land outside the
;;; domain of a ccp, we punt C-IN. If we push it all the way
;;; through, we add C-IN to our result domain, and add the mapping
;;; into the cmap we are assembling.
;;;
;;; Looping this way avoids building up intermediate temporary
;;; CCPs.  If CCP's were small bitsets, we might be better off
;;; slicing the double-nested loops the other way around.

(define (ccp-compose . ccps)
  (cond ((not (pair? ccps))       ccp:1)	; 0 args => ccp:1
	((not (pair? (cdr ccps))) (car ccps))	; 1 arg
	(else
	 (let* ((v (list->vector ccps))
		(vlen-2 (- (vector-length v) 2))
		(cmap (make-string num-chars))
		(d1 (ccp:domain (vector-ref v (+ vlen-2 1))))
		(d (char-set-fold (lambda (c-in d)
				    (let lp ((c c-in) (i vlen-2))
				      (if (>= i 0)
					  (let ((ccp (vector-ref v i)))
					    (if (char-set-contains? (ccp:domain ccp) c)
						(lp (string-ref (ccp:map ccp)
								(char->ascii c))
						    (- i 1))

						;; Lose: remove c-in from d.
						(char-set-delete! d c-in)))

					  ;; Win: C-IN -> C
					  (begin (string-set! cmap
							      (char->ascii c-in)
							      c)
						 d))))
				  (char-set-copy d1)
				  d1)))
	   (make-ccp d #f cmap #f)))))



;;; ALIST->CPP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alist->ccp cc-alist . maybe-base-ccp)
  (let ((base (:optional maybe-base-ccp ccp:0)))
    (if (pair? cc-alist)
	(let ((cmap (string-copy (ccp:map base))))
	  (make-ccp (install-ccp-alist! cmap
					(char-set-copy (ccp:domain base))
					cc-alist)
		    #f cmap #f))
	base)))

(define (alist->ccp! alist base)
  (linear-update-ccp base (lambda (d cmap) (install-ccp-alist! cmap d alist))))

;;; Side-effect CMAP, linear-update and return DOMAIN.
(define (install-ccp-alist! cmap domain alist)
  (fold (lambda (from/to d) (let ((from (car from/to))
				  (to (cdr from/to)))
			      (string-set! cmap (char->ascii from) to)
			      (char-set-adjoin! domain from)))
	domain
	alist))


;;; PROC->CCP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (proc->ccp proc [domain base-ccp])

(define (proc->ccp proc . args)
  (let-optionals args ((proc-domain char-set:full)
		       (base ccp:0))
    (let ((cmap (string-copy (ccp:map base))))
      (make-ccp (install-ccp-proc! cmap (char-set-copy (ccp:domain base))
				   proc proc-domain)
		#f cmap #f))))

(define (proc->ccp! proc proc-domain base)
  (linear-update-ccp base
    (lambda (d cmap) (install-ccp-proc! cmap d proc proc-domain))))

(define (install-ccp-proc! cmap domain proc proc-domain)
  (char-set-for-each (lambda (c) (string-set! cmap (char->ascii c) (proc c)))
		     proc-domain)
  (char-set-union! domain proc-domain))


;;; CONSTANT-CCP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (constant-ccp char [domain base-ccp])
;;; Extend BASE-CCP with the a map taking every char in DOMAIN to CHAR.
;;; DOMAIN defaults to char-set:full. BASE-CCP defaults to CCP:0.

(define (constant-ccp char . args)
  (let-optionals args ((char-domain char-set:full) (base ccp:0))
    (let ((cmap (string-copy (ccp:map base))))
      (make-ccp (install-constant-ccp! cmap (char-set-copy (ccp:domain base))
				       char char-domain)
		#f cmap #f))))

(define (constant-ccp! char char-domain base)
  (linear-update-ccp base
    (lambda (d cmap) (install-constant-ccp! cmap d char char-domain))))

;;; Install the constant mapping into CMAP0 by side-effect,
;;; linear-update & return DOMAIN0 with the constant-mapping's domain.
(define (install-constant-ccp! cmap0 domain0 char char-domain)
  (char-set-for-each (lambda (c) (string-set! cmap0 (char->ascii c) char))
		     char-domain)
  (char-set-union! domain0 char-domain))


;;; CCP/MAPPINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (ccp/mappings from1 to1 from2 to2 ...) -> ccp
;;; (extend-ccp/mappings  base-ccp from1 to1 from2 to2 ...) -> ccp
;;; (extend-ccp/mappings! base-ccp from1 to1 from2 to2 ...) -> ccp
;;; Each FROM element is either a string or a (lo-char . hi-char) range.
;;; Each TO element is either a string or a lo-char. Strings are replicated
;;;     to match the length of the corresponding FROM element.
;;; CCP/MAPPINGS's base CCP is CCP:0
;;;
;;; Tedious code.

;;; Internal utility.
;;; Install the FROM->TO mapping pair into DOMAIN & CMAP by side-effect.
;;; Return the new domain.

(define (install-ccp-mapping-pair! cmap domain from to)
  ;; Tedium -- four possibilities here:
  ;;   str->str, str->lo-char,
  ;;   range->str, range->lo-char.
  (if (string? from)
      (if (string? to)
	  ;; "abc" -> "ABC"
	  (let ((len1 (string-length from))
		(len2 (string-length to)))
	    (let lp2 ((i (- len1 1))
		      (j (modulo (- len2 1) len1))
		      (d domain))
	      (if (>= i 0)
		  (let ((c (string-ref from i)))
		    (string-set! cmap
				 (char->ascii c)
				 (string-ref to i))
		    (lp2 (- i 1)
			 (- (if (> j 0) j len2) 1)
			 (char-set-adjoin! d c)))
		  d)))

	  ;; "abc" -> #\A
	  (let lp2 ((i (- (string-length from) 1))
		    (j (char->ascii to))
		    (d domain))
	    (if (>= i 0)
		(let ((c (string-ref from i)))
		  (string-set! cmap
			       (char->ascii c)
			       (ascii->char j))
		  (lp2 (- i 1)
		       (- j 1)
		       (char-set-adjoin! d c)))
		d)))

      (let ((from-start (char->ascii (car from)))
	    (from-end   (char->ascii (cdr from))))
	(if (string? to)
	    (let ((len2-1 (- (string-length to) 1)))
	      ;; (#\a . #\c) -> "ABC"
	      (let lp2 ((i from-start) (j 0) (d domain))
		(if (<= i from-end)
		    (let ((c (string-ref to j)))
		      (string-set! cmap i c)
		      (lp2 (+ i 1)
			   (if (= j len2-1) 0 (+ j 1))
			   (char-set-adjoin! d c)))
		    d)))
				  
	    ;; (#\a . #\c) -> #\A
	    (do ((i from-start       (+ i 1))
		 (j (char->ascii to) (+ j 1))
		 (d domain (begin (string-set! cmap i (ascii->char j))
				  (char-set-adjoin d (ascii->char i)))))
		((> i from-end) d))))))

;;; Internal utility -- side-effects CMAP; linear-updates & returns DOMAIN.
(define (install-mapping-pairs cmap domain args)
  (let lp ((domain domain) (args args))
    (if (pair? args)
	(lp (install-ccp-mapping-pair! cmap domain (car args) (cadr args))
	    (cddr args))
	domain)))

(define (ccp/mappings . args)
  (let ((cmap (make-string num-chars)))
    (make-ccp (install-mapping-pairs (make-string num-chars)
				     (char-set-copy char-set:empty)
				     args)
	      #f cmap #f)))

(define (extend-ccp/mappings base . args)
  (let ((cmap (string-copy (ccp:map base))))
    (make-ccp (install-mapping-pairs cmap (char-set-copy (ccp:domain base)) args)
	      #f cmap #f)))

(define (extend-ccp/mappings! base . args)
  (linear-update-ccp base (lambda (d cmap) (install-mapping-pairs cmap d args))))
  

;;; CONSTRUCT-CCP! ccp elt ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The kitchen-sink constructor; static typing be damned. 
;;; ELTS are interpreted as follows:
;;;	(lo-char . hi-char) to-string|lo-char		; ccp/range
;;; 	from-string to-string|lo-char			; ccp/range
;;; 	ccp						; ccp-extend
;;; 	alist						; alist->ccp
;;; 	domain char					; ccp-constant
;;; 	domain proc					; proc->ccp

(define (construct-ccp! ccp . elts)
  (linear-update-ccp ccp (lambda (d cmap) (install-ccp-construct! cmap d elts))))

(define (construct-ccp base . elts)
  (let ((cmap (string-copy (ccp:map base))))
    (make-ccp (install-ccp-construct! cmap (char-set-copy (ccp:domain base)) elts)
	      #f cmap #f)))

;;; Install the mappings into CMAP by side-effect,
;;; linear-update & return DOMAIN with the final domain.

(define (install-ccp-construct! cmap domain elts)
  (let lp ((d domain) (elts elts))
    ;(format #t "d=~s elts=~s\n" d elts)
    (if (not (pair? elts)) d
	(let ((elt (car elts))
	      (elts (cdr elts)))
	  (cond ((pair? elt)
		 (cond ((pair? (car elt)) ; ELT is an alist.
			(lp (install-ccp-alist! cmap d elt) elts))
		       ((char? (car elt)) ; ELT is (lo-char . hi-char) range.
			(lp (install-ccp-mapping-pair! cmap d elt (car elts))
			    (cdr elts)))
		       (else (error "Illegal elt to construct-ccp" elt))))

		((string? elt)
		 (lp (install-ccp-mapping-pair! cmap d elt (car elts))
		     (cdr elts)))

		((ccp? elt) (lp (install-ccp-extension! cmap d elt) elts))

		((char-set? elt)
		 (let ((elt2 (car elts))
		       (elts (cdr elts)))
		   (lp (cond ((char? elt2)
			      (install-constant-ccp! cmap d elt2 elt))
			     ((procedure? elt2)
			      (install-ccp-proc! cmap d elt2 elt))
			     (else (error "Illegal elt-pair to construct-ccp"
					  elt elt2)))
		       elts)))

		(else (error "Illegal elt to construct-ccp" elt)))))))


;;; CCP unfold

(define (ccp-unfold p f g seed)
  (let lp ((seed seed) (ccp (ccp-copy ccp:0)))
    (if (p seed) ccp
	(lp (g seed)
	    (receive (from to) (f seed)
	      (lp (g seed) (ccp-adjoin! ccp from to)))))))
      


;;; Using CCPs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TR ccp string [start end] -> string
;;; CCP-MAP  ccp string [start end] -> string
;;; CCP-MAP! ccp string [start end] -> undefined
;;; CCP-APP  ccp char -> char or false

;;; If a char in S is not in CCP's domain, it is dropped from the result.
;;; You can use this to map and delete chars from a string.

(define (tr ccp s . maybe-start+end)
  (let-optionals maybe-start+end ((start 0) (end (string-length s)))
    ;; Count up the chars in S that are in the domain,
    ;; and allocate the answer string ANS:
    (let* ((len (- end start))
	   (domain (ccp:domain ccp))
	   (ans-len (string-fold (lambda (c numchars)
				   (if (char-set-contains? domain c)
				       (+ numchars 1)
				       numchars))
				 0 s start end))
	   (ans (make-string ans-len)))

      ;; Apply the map, installing the resulting chars into ANS:
      (string-fold (lambda (c i) (cond ((ccp-app ccp c) =>
					(lambda (c)
					  (string-set! ans i c)
					  (+ i 1)))
				       (else i))) ; Not in domain -- drop it.
		   0 s start end)
      ans)))

(define (ccp-map  ccp s . maybe-start+end)
  (apply string-map (lambda (c) (ccp-app ccp c)) s maybe-start+end))

(define (ccp-map! ccp s . maybe-start+end)
  (apply string-map! (lambda (c) (ccp-app ccp c)) s maybe-start+end))

(define (ccp-app ccp char)
  (and (char-set-contains? (ccp:domain ccp) char)
       (string-ref (ccp:map ccp) (char->ascii char))))


;;; Primitive CCPs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define id-cmap
  (let ((m (make-string num-chars)))
    (do ((i (- num-chars 1) (- i 1)))
	((< i 0))
      (string-set! m i (ascii->char i)))
    m))
  
(define ccp:0 (make-ccp char-set:empty #t id-cmap #t))
(define ccp:1 (make-ccp char-set:full  #t id-cmap #t))

(define ccp:upcase   (proc->ccp char-upcase  char-set:full))
(define ccp:downcase (proc->ccp char-downcase char-set:full))
