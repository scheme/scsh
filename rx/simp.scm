;;; Olin Shivers, June 1998
;;; Copyright (c) 1998 by the Scheme Underground.

;;; One export: (simplify-regexp re) -> re

;;; Regexp simplifier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (| (in c1 ...) (in c2 ...) re ...) => (| (in c1 ... c2 ...) re ...)
;;; (| (not-in c1 ...) (not-in c2 ...)) => (| (not-in [intersect (c1 ...)
;;; 							     (c2 ...)])
;;; A run of BOS's or a run of EOS's in a sequence may be elided.
;;; Nested exponents can be collapsed (*, +, ?) -- multiply the "from's"
;;;   together; multiply the "to's" together.
;;; Exponent range [1,1] simplifies, as does [0,0].
;;; Uniquify branches
;;; Adjacent literals in a sequence can be collapsed
;;; A singleton-char char class can be collapsed into a constant
;;; Nested choices can be collapsed
;;; Nested sequences can be collapsed
;;; An empty sequence (:) can be turned into an empty-string match "".
;;; Singleton choices and sequences can be reduced to their body.
;;;
;;; The simplifier is carefully written so that it won't blow up
;;; when applied to a dynamic regexp -- that is, 
;;; - a chunk of Scheme code that produces a regexp instead of
;;;   an actual regexp value;
;;; - a repeat regexp whose FROM or TO fields are chunks of Scheme code
;;;   rather than integers; 
;;; - a char-set regexp whose CSET field is a chunk of Scheme code rather
;;;   than an actual char-set value.
;;; This is useful because the RX macro can build such a regexp as part
;;; of its expansion process.

(define (simplify-regexp re)
  (if (and (regexp? re)
	   (number? (re-tsm re)))
      (receive (simp-re pre-dsm) (simp-re re)
	(re-dsm simp-re pre-dsm (- (re-tsm re) (+ (re-tsm simp-re) pre-dsm))))
      re))

(define (simp-re re)
  (cond
   ((re-string? re) (values re 0))
   ((re-seq? re)    (simp-seq re))
   ((re-choice? re) (simp-choice re))

   ;; Singleton char-sets reduce to the character.
   ;; Bear in mind the cset field might be Scheme code instead 
   ;; of an actual char set if the regexp is dynamic.
   ((re-char-set? re)
    (values (let ((cs (re-char-set:cset re)))
	      (if (and (char-set? cs)
		       (= 1 (char-set-size cs)))
		  (make-re-string (string (car (char-set->list cs))))
		  re))
	    0))

   ((re-repeat? re) (simp-repeat re))

   ((re-submatch? re) (simp-submatch re))
   ((re-dsm?      re) (simp-dsm      re))

   (else (values re 0))))



;;; If the body of a submatch is the empty re, reduce it to the empty re.

(define (simp-submatch re)
  (let ((tsm     (re-submatch:tsm     re))
	(pre-dsm (re-submatch:pre-dsm re)))
    (receive (body1 pre-dsm1) (simp-re (re-submatch:body re))
      (if (re-empty? body1)
	  (values re-empty tsm)
	  (values (make-re-submatch/tsm body1 (+ pre-dsm pre-dsm1) tsm)
		  0)))))

;;; - Flatten nested DSM's.
;;; - Return pre-dsm field and body field as the two return values.

(define (simp-dsm re)
  (receive (body pre-dsm1) (simp-re (re-dsm:body re))
    (values body (+ (re-dsm:pre-dsm re) pre-dsm1))))



;;; Simplifying sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - Collapse nested sequences and DSM's.
;;; - Merge adjacent strings, identical adjacent anchors (bos, eos, etc.).
;;; - Bubble DSM's forwards past elts that don't contain live submatches.
;;;   (Going past live submatches would switch the submatch indexes around,
;;;   which would be an error). This helps to coalesce DSMs and if we bring
;;;   them all the way to the front, we can pop them off and make them a 
;;;   pre-dsm for the entire seq record. 
;;; - If an elt is the re-empty, reduce the whole re to the empty re.
;;; - Reduce singleton and empty seq.

(define (simp-seq re)
  (let ((tsm (re-seq:tsm re))
	(elts (map simplify-regexp (re-seq:elts re))))
    (cond
     ((null? elts)
      (values re-trivial 0))		; Empty seq
     ((number? tsm)
      (call-with-current-continuation
       (lambda (abort)
	 (receive (pre-dsm head tail) (simp-seq1 elts abort tsm)
	   (values (if (pair? tail)
		       (make-re-seq/tsm (cons head tail) (- tsm pre-dsm))
		       head)		; Singleton seq
		   pre-dsm)))))
     (else (values re tsm)))))		; dynamic components


;;; Simplify the non-empty sequence ELTS.
;;; - Return the result split out into three values: 
;;;   [head-elt-pre-dsm, head-elt, tail].
;;; - If any elt is the empty (impossible) re, abort by calling
;;;   (abort elt tsm). TSM is otherwise unused.
;;; - If any elt is dynamic, abort as well.

(define (simp-seq1 elts abort tsm)
  (let recur ((elt (car elts)) (elts (cdr elts)))
    (receive (elt pre-dsm) (open-dsm elt)
      (cond
       ((re-seq? elt)			; Flatten nested seqs.
	(let ((sub-elts (re-seq:elts elt)))
	  (recur (re-dsm (car sub-elts) pre-dsm 0)
		 (append (cdr sub-elts) elts))))
		  					
       ((re-empty? elt) (abort elt tsm)) ; Bomb out on the empty
					; (impossible) re.
       ((pair? elts)
	(receive (next-pre-dsm next tail) ; Simplify the tail,
	    (recur (car elts) (cdr elts)) ; then think about
					; the head:
	  ;; This guy is called when we couldn't find any other
	  ;; simplification. If ELT contains live submatches, then
	  ;; there really is nothing to be done at this step -- just
	  ;; assemble the pieces together and return them. If ELT
	  ;; *doesn't* contain any live submatches, do the same, but
	  ;; bubble its following next-pre-dsm submatches forwards.
	  (define (no-simp)
	    (if (has-live-submatches? elt)
		(values pre-dsm elt (cons (re-dsm next next-pre-dsm 0) tail))
		(values (+ pre-dsm next-pre-dsm) elt (cons next tail))))

	  ;; Coalesces two adjacent bol's, two adjacent eol's, etc.
	  (define (coalesce-anchor anchor?)
	    (if (and (anchor? elt) (anchor? next))
		(values (+ pre-dsm next-pre-dsm) elt tail)
		(no-simp)))

	  (cond
	   ((re-trivial? elt)		; Drop trivial re's.
	    (values (+ pre-dsm next-pre-dsm) next tail))

	   ;; Coalesce adjacent strings
	   ((re-string? elt)
	    (if (re-string? next)
		(values (+ pre-dsm next-pre-dsm)
			(make-re-string (string-append (re-string:chars elt)
						       (re-string:chars next)))
			tail)
		(no-simp)))

	   ;; Coalesce adjacent bol/eol/bos/eos's.
	   ((re-bol? elt) (coalesce-anchor re-bol?))
	   ((re-eol? elt) (coalesce-anchor re-eol?))
	   ((re-bos? elt) (coalesce-anchor re-bos?))
	   ((re-eos? elt) (coalesce-anchor re-eos?))
	   (else (no-simp)))))

       (else (values pre-dsm elt '()))))))



;;; Simplifying choices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - Collapse nested choices and DSM's.
;;; - Delete re-empty's.
;;; - Merge sets; merge identical anchors (bos, eos, etc.).
;;;   But you can't merge across an element that contains a live submatch,
;;;   see below.
;;; - A singleton string "c" is included into the char-set merge as a 
;;;   singleton set.
;;; - Bubble DSM's forwards past elts that don't contain live submatches.
;;;   (Going past live submatches would switch the submatch indexes around,
;;;   which would be an error). This helps to coalesce DSMs and if we bring
;;;   them all the way to the front, we can pop them off and make them a 
;;;   pre-dsm for the entire seq record. 
;;; - Reduce singleton and empty choice.
;;;
;;; You have to be careful simplifying choices -- you can't merge two sets
;;; that appear on different sides of an element containing a live submatch.
;;; The problem is that the assignment of submatches breaks ties left-to-right.
;;; So these aren't the same:
;;;     (| (submatch "x") any)    (| any (submatch "x"))
;;; The first assigns the submatch, the second doesn't -- the ANY gets credit.
;;; We want to collapse multiple char-sets, bos's, and eos's, but we have
;;; to deal with this issue. So
;;; - When we coalesce anchors, we retain the *leftmost* one.
;;; - We coalesce sets that appear between live-submatch boundaries.
;;;   When we do this, we subtract from the set any char that was in
;;;   an earlier coalesced char-set. If this gets us down to the empty set,
;;;   we drop it. If it gets us down to a singleton set, we convert it into
;;;   a singleton string.
;;; Whew. I had to think about this one.

(define (simp-choice re)
  (let ((tsm (re-choice:tsm re)))

    (receive (pre-dsm cset bos? eos? bol? eol? tail)
	     (simp-choice1 (map simplify-regexp (re-choice:elts re)))

      (let ((tail (assemble-boundary-tail char-set:empty cset
					  bos? eos? bol? eol?
					  #f #f #f #f
					  tail)))
	(values (if (pair? tail)
		    (if (pair? (cdr tail))
			(make-re-choice/tsm tail (- tsm pre-dsm))
			(car tail))		; Singleton choice
		    re-empty)			; Empty choice
		pre-dsm)))))		



;;; Given the return values from simp-choice1, this tacks all
;;; the various pieces (CSET, BOS?, EOS?, etc.) onto the front of
;;; TAIL. However, elements are not added onto TAIL that are already
;;; described by PREV-CSET, PREV-BOS?, etc. -- they will be added onto
;;; some earlier bit of the final result.

(define (assemble-boundary-tail prev-cset cset
				bos? eos? bol? eol?
				prev-bos? prev-eos?
				prev-bol? prev-eol?
				tail)
  (let* ((cset (char-set-difference cset prev-cset))
	 (numchars (char-set-size cset))
	 (tail (if (and eos? (not prev-eos?)) (cons re-eos tail) tail))
	 (tail (if (and eol? (not prev-eol?)) (cons re-eol tail) tail))
	 (tail (if (and bol? (not prev-bol?)) (cons re-bol tail) tail))
	 (tail (if (and bos? (not prev-bos?)) (cons re-bos tail) tail))
	 (tail (cond
		((zero? numchars) tail)	; Drop empty char set.
		((= 1 numchars)		; {c} => "c"
		 (cons (make-re-string (string (car (char-set->list cset))))
		       tail))
		(else (cons (make-re-char-set cset) tail)))))
    tail))


;;; Simplify the non-empty list of choices ELTS.
;;; Return the result split out into the values
;;;     [pre-dsm, cset, bos?, eos?, bol?, eol?, tail]

(define (simp-choice1 elts)
  (let recur ((elts elts)

	      (prev-cset char-set:empty) ; Chars we've already seen.

	      (prev-bos? #f) (prev-eos? #f) ; These flags say if we've
	      (prev-bol? #f) (prev-eol? #f)) ; already seen one of these anchors.
			       
    
    (if (pair? elts)
	(let ((elt  (car elts))
	      (elts (cdr elts)))
	  (receive (elt pre-dsm) (open-dsm elt)
	    (if (re-choice? elt)

		;; Flatten nested choices.
		(let ((sub-elts (re-seq:elts elt)))
		  (receive (tail-pre-dsm cset bos? eos? bol? eol? tail)
		      (recur (append sub-elts elts)
			     prev-cset
			     prev-bos? prev-eos?
			     prev-bol? prev-eol?)
		    (values (+ pre-dsm tail-pre-dsm)
			    cset bos? eos? bol? eol? tail)))
		  
		;; Simplify the tail, then think about the head.
		(receive (tail-pre-dsm cset bos? eos? bol? eol? tail)
		    (recur elts
			   (cond
			    ((and (re-string? elt)
				  (= 1 (string-length (re-string:chars elt))))
			     (char-set-union prev-cset
					     (string->char-set (re-string:chars elt))))

			    ;; The cset might be a Scheme exp.
			    ((and (re-char-set? elt)
				  (char-set? (re-char-set:cset elt)))
			     (char-set-union prev-cset
					     (re-char-set:cset elt)))

			    (else prev-cset))
			   (or prev-bos? (re-bos? elt))
			   (or prev-eos? (re-eos? elt))
			   (or prev-bol? (re-bol? elt))
			   (or prev-eol? (re-eol? elt)))

		  ;; This guy is called when we couldn't find any other
		  ;; simplification. If ELT contains live submatches, then we
		  ;; are at a merge boundary, and have to take all the
		  ;; TAIL-PRE-DSM, CSET, BOS?, EOS?, ... stuff we've collected
		  ;; and tack them onto TAIL as elements, then put ELT on
		  ;; front.  Otherwise, we can commute TAIL-PRE-DSM, CSET,
		  ;; BOS?, etc. with ELT, since it contains no live
		  ;; submatches, so just tack ELT onto TAIL.

		  (define (no-simp)
		    (if (has-live-submatches? elt)
			(let ((tail (assemble-boundary-tail prev-cset cset
							    bos? eos?
							    bol? eol?
							    prev-bos? prev-eos?
							    prev-bol? prev-eol?
							    tail)))
			  (values pre-dsm char-set:empty #f #f #f #f
				  (if (pair? tail)
				      ;; Tack tail-pre-dsm onto
				      ;; TAIL's first elt.
				      (cons elt
					    (cons (re-dsm (car tail)
							  tail-pre-dsm 0)
						  (cdr tail)))

				      ;; Squirrel case: TAIL is empty, so use 
				      ;; TAIL-PRE-DSM as ELT's post-dsm.
				      (list (re-dsm elt 0 tail-pre-dsm)))))

			;; ELT has no live submatches, so we can commute all
			;; the recursion state forwards past it.
			(values (+ pre-dsm tail-pre-dsm)
				cset bos? eos? bol? eol?
				(cons elt tail))))

		  (cond
		   ((and (re-char-set? elt)
			 (char-set? (re-char-set:cset elt))) ; Might be Scheme code
		    (values (+ pre-dsm tail-pre-dsm)
			    (char-set-union cset (re-char-set:cset elt))
			    bos? eos? bol? eol? tail))

		   ;; Treat a singleton string "c" as a singleton set {c}.
		   ((and (re-string? elt) (= 1 (string-length (re-string:chars elt))))
		    (values (+ pre-dsm tail-pre-dsm)
			    (char-set-union cset (string->char-set (re-string:chars elt)))
			    bos? eos? bol? eol? tail))

		   ;; Coalesce bol/eol/bos/eos's.
		   ((re-bos? elt) (values (+ pre-dsm tail-pre-dsm) cset
					  #t   eos? bol? eol? tail))
		   ((re-eos? elt) (values (+ pre-dsm tail-pre-dsm) cset
					  bos? #t   bol? eol? tail))
		   ((re-bol? elt) (values (+ pre-dsm tail-pre-dsm) cset
					  bos? eos? #t   eol? tail))
		   ((re-eol? elt) (values (+ pre-dsm tail-pre-dsm) cset
					  bos? eos? bol? #t   tail))

		   (else (no-simp)))))))

	(values 0 char-set:empty #f #f #f #f '()))))



(define (simp-repeat re)
  (let ((from (re-repeat:from re))
	(to   (re-repeat:to   re))
	(body (re-repeat:body re)))
    (receive (simp-body pre-dsm) (simp-re body) ; Simplify body.
      ;; The fancy reductions are all handled by REDUCE-REPEAT.
      (reduce-repeat from to simp-body pre-dsm))))



;;; Does RE contain a live submatch?
;;; If RE is dynamic, we can't tell, so we err conservatively, 
;;; which means we say "yes."

(define (has-live-submatches? re)
  (or (re-submatch? re)
      (cond ((re-seq?    re) (every has-live-submatches? (re-seq:elts    re)))
	    ((re-choice? re) (every has-live-submatches? (re-choice:elts re)))
	    ((re-repeat? re) (has-live-submatches? (re-repeat:body re)))
	    ((re-dsm?    re) (has-live-submatches? (re-dsm:body    re)))

	    ;; If it's not one of these things, then this isn't a regexp -- it's
	    ;; a chunk of Scheme code producing a regexp, and we conservatively
	    ;; return #T -- the expression *might* produce a regexp containing
	    ;; a live submatch:
	    (else (not (or (re-char-set? re) (re-string? re)
			   (re-bos? re) (re-eos? re) 
			   (re-bol? re) (re-eol? re)))))))
