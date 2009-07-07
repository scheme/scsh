;;; Regexp-ADT -> Posix-string translator.
;;; Olin Shivers January 1997, May 1998.

;;; - If the regexp value contains nul character constants, or character sets 
;;;   that contain the nul character, they will show up in the Posix string
;;;   we produce. Spencer's C regexp engine can handle regexp strings that
;;;   contain nul bytes, but this might blow up other implementations -- that
;;;   is, the nul byte might prematurely terminate the C string passed to the
;;;   regexp engine.
;;; 
;;; - The code is ASCII-specific in only one place: the expression for
;;;   a regexp that matches nothing is the 6-char pattern "[^\000-\177]",
;;;   which assumes a 7-bit character code. Note that the static simplifier
;;;   can remove *all* occurences of this "empty regexp" except for the 
;;;   un-simplifiable case of a single, top-level empty regexp, e.g. 
;;;       (rx (in))
;;;   We can handle this one special case specially, so we shouldn't *ever*
;;;   have to produce this ASCII-specific pattern.

;;; Exports: regexp->posix-string

;;; Todo: A dumb, simple char-set renderer.

;;; These functions translate static regular expressions into Posix regexp
;;; strings. They generally return four values:
;;;   - string (regexp)
;;; 
;;;   - syntax level: 0 parenthesized exp, 1 piece, 2 branch, 3 top
;;;     ("piece", "branch" and "top" are Spencer's terms):
;;;     + A parenthesized exp is syntactically equivalent to a piece.
;;;       (But it's useful to know when an exp is parenthesized for
;;;       eliminating redundant submatch-generated parens.)
;;;     + A piece is something that would bind to a following * 
;;;       ("a" but not "aa").
;;;     + A branch is a sequence of pieces -- something that would bind to a |
;;;       ("ab*d" but not "ab*|d"). That is, a branch is not allowed to contain
;;;       top-level |'s.
;;;     + Top is for a sequence of branches -- "a|b*c|d".
;;; 
;;;   - paren count in the returned string.
;;;
;;;   [This is a newer description; is it correct?]
;;;   - A vector mapping submatches (vector index 0 is submatch 1)
;;;     to the paren for that submatch (the first paren is paren #1).
;;;
;;;   [This is my original description.]
;;;   - Vector of parens numbers used for submatching. The first paren is
;;;     numbered 1. #F means a dead submatch -- one we can tell statically
;;;     will never match anything.

;;; Non-R4RS imports:
;;; ? = COND
;;; Multiple-value return: VALUES RECEIVE CALL-WITH-VALUES
;;; SORT-LIST


;;; Useful little utility -- pad vector V with 
;;; PRE initial and POST following #f's.

(define (pad-vector pre post v)
  (if (= pre post 0) v
      (let* ((vlen (vector-length v))
	     (alen (+ pre post vlen))
	     (ans (make-vector alen #f)))
	(do ((from (- vlen 1)      (- from 1))
	     (to   (+ pre vlen -1) (- to 1)))
	    ((< from 0))
	  (vector-set! ans to (vector-ref v from)))
	ans)))

(define (n-falses n) (make-vector n #f))


;;; There's no representation for regexps that never match anything (e.g.,
;;; (|)) in strict Posix notation. When we get one of these, we treat it
;;; specially, producing [#f #f #f #f].
;;;
;;; We can always detect these empty regexps, because they always simplify
;;; to one of these two values:
;;; - (make-re-char-set char-set:empty)
;;; - (dsm m n (make-re-char-set char-set:empty))

(define (simple-empty-re? re)
  (or (and (re-char-set? re)
	   (char-set-empty? (re-char-set:cset re)))
      (and (re-dsm? re)
	   (simple-empty-re? (re-dsm:body re)))))


;;; Top-level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regexp->posix-string re)
  ;; We *must* simplify, to guarantee correct translation.
  (let ((re (simplify-regexp re))) 
    (if (simple-empty-re? re) (values #f #f #f '#())
	(translate-regexp re))))


(define (translate-regexp re)
  (cond
   ((re-string? re) (translate-string (re-string:chars re)))

   ((re-repeat? re)   (translate-repeat   re))
   ((re-choice? re)   (translate-choice   re))
   ((re-seq? re)      (translate-seq      re))
   ((re-char-set? re) (translate-char-set (re-char-set:cset re)))

   ((re-submatch? re) (translate-submatch re))

   ((re-bos? re) (values "^" 1 0 '#()))
   ((re-eos? re) (values "$" 1 0 '#()))

   ((re-bol? re) (error "Beginning-of-line regexp not supported in this implementation."))
   ((re-eol? re) (error "End-of-line regexp not supported in this implementation."))

   ((re-dsm? re) (let ((pre-dsm (re-dsm:pre-dsm re))
		       (body    (re-dsm:body re)))
		   (translate-dsm body pre-dsm
				  (- (re-dsm:tsm re)
				     (+ pre-dsm (re-tsm body))))))

   (else (error "Illegal regular expression" re))))


;;; Translate reloc-elt ELT = (N . RE) from a sequence or choice
;;; into a Posix string.
;;; - Relocate the submatch indices by PREV-PCOUNT.
;;;   (That is, assume rendering preceding elts used PREV-PCOUNT parens.)
;;; - Assume preceding elements allocated PREV-SMCOUNT submatches
;;;   (we may have to pad our returned submatches string with some
;;;   initial #F's to account for dead submatches PREV-SMCOUNT through N.)
;;; - If SUB-LEV3? is true, the result string is guaranteed to be < level 3.
;;;   This is used by the & and | translators.
;;; - Returns the usual 4 values plus the final submatch count including
;;;   this regexp.

(define (translate-elt elt prev-pcount prev-smcount sub-lev3?)
  (let ((offset (car elt))
	(re     (cdr elt)))

    (receive (s level pcount submatches) (translate-regexp re)

      ;; Relocate submatch indices by OFFSET and force level <3, if needed:
      (receive (s level pcount submatches)
               (if (and sub-lev3? (= level 3))
		   (values (string-append "(" s ")")
			   0
			   (+ pcount 1)
			   (mapv (lambda (sm) (and sm (+ prev-pcount 1 sm)))
				 submatches))
		   (values s level pcount
			   (mapv (lambda (sm) (and sm (+ prev-pcount sm)))
				 submatches)))

	;; Tack onto submatches as many initial #F's as needed to bump
	;; the previous submatches count from PREV-SMCOUNT to OFFSET.
	(values s level pcount
		(pad-vector (- offset prev-smcount) 0 submatches)
		(+ offset (re-tsm re)))))))
      


;;; Force the string to be level < 3 by parenthesizing it if necessary.

(define (paren-if-necessary s lev pcount submatches)
  (if (< lev 3)
      (values s lev pcount submatches)
      (values (string-append "(" s ")")
	      0
	      (+ pcount 1)
	      (mapv (lambda (sm) (and sm (+ 1 sm)))
		    submatches))))



;;; (: re1 ... ren)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translate-seq re)
  (let ((elts (re-seq:elts re))
	(tsm  (re-seq:tsm  re)))
    (let recur ((elts elts) (prev-pcount 0) (prev-smcount 0))
      ;; Render a sequence tail ELTS, assuming the previous elements translated
      ;; to a string with PREV-PCOUNT parens, and allocated PREV-SMCOUNT
      ;; submatches.
      (if (pair? elts)
	  (let* ((elt  (car elts))
		 (elts (cdr elts)))

	    (receive (s1 level1 pcount1 submatches1)
		     (translate-regexp elt)

	      (receive (s1 level1 pcount1 submatches1)
		       (paren-if-necessary s1 level1 pcount1 submatches1)

		(receive (s level pcount submatches)
		         (recur elts
				(+ pcount1 prev-pcount)
				(+ prev-smcount (re-tsm elt)))

		  (values (string-append s1 s)
			  2
			  (+ pcount1 pcount)
			  (vector-append (mapv (lambda (p) (and p (+ p prev-pcount)))
					       submatches1)
					 submatches))))))

	    (values "" 2 0 '#()))))) ; Empty seq



;;; (| re1 ... ren)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translate-choice re)
  (let ((elts (re-choice:elts re))
	(tsm  (re-choice:tsm  re)))
    (if (pair? elts)
	(let recur ((elts elts) (prev-pcount 0) (prev-smcount 0))
	  ;; ELTS is a non-empty choice tail. Render it, assuming the
	  ;; previous elements translated to a string with PREV-PCOUNT parens,
          ;; and allocated PREV-SMCOUNT submatches.
	  (let ((elt (car elts))  (tail (cdr elts)))
	    (receive (s1 level1 pcount1 submatches1) (translate-regexp elt)
	      (let ((submatches1 (mapv (lambda (sm) (and sm (+ sm prev-pcount)))
				       submatches1)))
		(if (pair? tail)
		    (receive (s level pcount submatches)
			(recur tail
			       (+ pcount1 prev-pcount)
			       (+ prev-smcount (re-tsm elt)))
		      (values (string-append s1 "|" s) 3
			      (+ pcount1 pcount)
			      (vector-append submatches1 submatches)))
		    
		    (values s1 level1 pcount1 submatches1))))))

	(values "[^\000-\377]" 1 0 (n-falses tsm)))))	; Empty choice.



;;; Repeated cases: * + ? and {n,m} ranges.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translate-repeat re)
  (let ((from (re-repeat:from re))
	(to   (re-repeat:to   re))
	(body (re-repeat:body re))
	(tsm  (re-repeat:tsm  re)))

    (cond
     ((and to (> from to))		; Unsatisfiable
      (values "[^\000-\377]" 1 0 (n-falses tsm))) 

     ((and to (= from to 1)) (translate-seq body)) ; RE{1,1} => RE

     ((and to (= to 0))			; RE{0,0} => ""
      (values "" 2 0 (n-falses tsm)))

     (else				; General case
      (receive (s level pcount submatches) (translate-regexp body)
	(receive (s level pcount submatches) ; Coerce S to level <2.
	    (if (> level 1)
		(values (string-append "(" s ")")
			0
			(+ pcount 1)
			(mapv (lambda (i) (and i (+ i 1))) submatches))
		(values s level pcount submatches))

	  (values (if to
		      (cond ((and (= from 0) (= to 1)) (string-append s "?"))
			    ((= from to)
			     (string-append s "{" (number->string to) "}"))
			    (else
			     (string-append s "{" (number->string from)
					    "," (number->string to) "}")))
		      (cond ((= from 0) (string-append s "*"))
			    ((= from 1) (string-append s "+"))
			    (else (string-append s "{" (number->string from) ",}"))))
		  1 pcount submatches)))))))



;;; Submatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translate-submatch re)
  (let ((body    (re-submatch:body re))
	(pre-dsm (re-submatch:pre-dsm  re)))

    ;; Translate the body, along with any leading or trailing dead submatches.
    (receive (s level pcount submatches)
	     (translate-dsm body
			    pre-dsm
			    (- (re-submatch:tsm re)
			       (+ 1 pre-dsm (re-tsm body))))
	
      ;; If the whole expression isn't already wrapped in a paren, wrap it.
      ;; This outer paren becomes the new submatch -- add to submatches list.
      (if (= level 0)
	  (values s 0 pcount (vector-append '#(1) submatches))
	  (values (string-append "(" s ")")
		  0
		  (+ pcount 1)
		  (mapv! (lambda (i) (and i (+ i 1)))		; Excuse me.
			 (vector-append '#(0) submatches)))))))

;;; Translating DSM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translate the body, and paste enough #F's before and after the submatches
;;; list to account for extra dead submatches.

(define (translate-dsm body pre-dsm post-dsm)
  (receive (s level pcount submatches) (translate-regexp body)
    (values s level pcount (pad-vector pre-dsm post-dsm submatches))))

;;; Constant regexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convert a string into a regexp pattern that matches that string exactly --
;;; quote the special chars with backslashes.

(define translate-string
  (let ((specials (string->char-set "{}[.*?()|\\$^+")))
    (lambda (s)
      (let ((len (string-length s)))
	(if (zero? len)
	    (values "()" 0 1 '#()) ; Special case ""

	    (let* ((len2 (string-fold (lambda (c len) ; Length of answer str
					(+ len (if (char-set-contains? specials c) 2 1)))
				      0 s))
		   (s2 (make-string len2)))		; Answer string

	      ;; Copy the chars over to S2.
	      (string-fold (lambda (c i)
			     ;; Write char C at index I, return the next index.
			     (let ((i (cond ((char-set-contains? specials c)
					     (string-set! s2 i #\\)
					     (+ i 1))
					    (else i))))
			       (string-set! s2 i c)
			       (+ i 1)))
			   0 s)
	      (values s2 (if (= len 1) 1 2)
		      0 '#())))))))



;;; Translating char-sets to [...] strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the nastiest code in the system. We make an effort to return
;;; succinct encodings of the char-sets, in the event these encodings are
;;; being shown to humans.
;;; - A singleton set is rendered as that char.
;;; - A full set is rendered as "."
;;; - An empty set is rendered as [^\000-\177].
;;; - Otherwise, render it both as a [...] and as a [^...] spec, and
;;;   take whichever is shortest.

;;; Take a char set, and return the standard 
;;;     [regexp-string, level, pcount, submatches]
;;; quadruple.
;;;

(define *nul* (ascii->char 0))

(define (translate-char-set cset)
  (if (char-set-full? cset)
      (values "." 1 0 '#())		; Full set
      (let* ((cset (char-set-delete cset *nul*))
	     (nchars (char-set-size cset))
	     (->bracket-string (lambda (cset in?)
				 (receive (loose ranges) (char-set->in-pair cset)
				   (hack-bracket-spec loose ranges in?)))))
	
	(cond
	 ((= 0 nchars) (values "[^\000-\177]" 1 0 '#())) ; Empty set
	     
	 ((= 1 nchars)			; Singleton set
	  (translate-string (string (car (char-set->list cset)))))

	 ;; General case. Try both [...] and [^...].
	 (else (let ((s- (->bracket-string cset #t))
		     (s+ (->bracket-string
			  (char-set-delete (char-set-complement cset) *nul*)
			  #f)))
		 (values (if (< (string-length s-) (string-length s+))
			     s- s+)
			 1 0 '#())))))))


;;; Commentary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Hacking special chars in character-class strings:
;;; ] - ^	]...^-
;;; ] -		]...-
;;; ]   ^	]...^
;;; ]   	]...
;;;   - ^	...^-	(or doubleton screw-case)
;;;   -		...-
;;;     ^	...^	(or singleton screw-case)
;;;
;;; Two screw cases: 
;;;   "^-" must be converted to "-^" for IN.
;;;   "^" must be converted to non-class "^" for IN.

;;; Rendering a general char-set into a correct Posix [...] bracket expression
;;; is a complete mess.
;;;
;;; The rules on bracket expressions:
;;; - ] terminates the exp unless it is the first char 
;;;   (after an optional leading ^).
;;; - .*[\ are not special in bracket expressions.
;;; - However, [. [= and [: *are* special, so you can't follow an
;;;   open bracket by one of .=: -- argh. See below.
;;; - ^ isn't special unless it's the first char.
;;; - - is special unless it's first (after an optional ^), last,
;;;   or as the ending char in a range (e.g., a--).

;;; This means:
;;; - You must ensure that ] doesn't begin or terminate a range.
;;; - You must ensure that .=: don't follow [
;;;   + This can happen in the loose char list;
;;;   + This can happen in the range list -- consider the pair of
;;;     ranges "x-[.-%" Handle this by prohibiting [ as a range-terminator.
;;;   + It can happen at the loose/range boundary: %[:-?

;;; First, run-length encode the set into loose and range-pairs.
;;; If the set is a singleton set, then punt the whole [...] effort,
;;; and do it as a simple char.

;;; Repeat until stable:
;;; - Sort the ranges in this order:
;;;     1. other ranges;
;;;     2. ranges that begin with ^	(not priority)
;;;     3. ranges that begin with .=:	(priority)
;;;     4. ranges that end with [	(priority)
;;;   This eliminates [. [= [: problems in the ranges, and
;;;   minimises the chances of the problem at the loose/range boundary.
;;;   and problems with initial ^ chars.
;;; - Sort the loose chars so that ] is first, then -, then .=:, then [,
;;;   then others, then ^. This eliminates [. [= [: problems in the loose 
;;;   chars, and minimises the chances of the problem at the loose/range
;;;   boundary.
;;; - Shrink ranges by moving an opening or closing range char into the
;;;   loose-char set:
;;;   + If ] opens or closes a range, shrink it out.
;;;   + If any range opens with -, shrink it out.
;;;   + If the first range opens with .=:, and the last loose char is [,
;;;     shrink it out.
;;;   + If there are no loose chars, the first range begins with ^, and
;;;     we're doing an IN range, shrink out the ^.
;;;   + Shrinking a range down to <3 chars means move it's elts into the
;;;     loose char set.
;;; - If both [ and - are in the loose char set, 
;;;   pull - out as special end-hypen.

;;; Finally, we have to hack things so that ^ doesn't begin an IN sequence.
;;; - If it's a NOT-IN sequence, no worries.
;;; - If ^ is the opening loose char, then it's the only loose char.
;;;   If there are ranges, move it to the end of the string.
;;;   If there are no ranges, then just punt the char-class and convert
;;;   it to a singleton ^. In fact, do this up-front, for any singleton 
;;;   set.
;;;
;;; If the special end-hyphen flag is set, add - to the end of the string.

;;; This general approach -- starting out with maximal ranges, and then
;;; shrinking them to avoid other syntax violations -- has the advantage
;;; of not relying on the details of the ASCII encodings.

;;; Ordering ranges:
;;;     1. other ranges (ordered by start char)
;;;     2. ranges that begin with ^	(not priority)
;;;     3. ranges that begin with .=:	
;;;     4. ranges that end with [	(priority over #2 & #3)

(define (range< r1 r2)
  (let ((r1-start (car r1)) (r1-end (cdr r1))
	(r2-start (car r2)) (r2-end (cdr r2)))
    (or (char=? r2-end #\[)	; Range ending with [ comes last.
	(and (not (char=? r1-end #\[))

	     ;; Range begin with one of .=: comes next-to-last
	     (or (char=? r2-start #\.) (char=? r2-start #\=) (char=? r2-start #\:)
		 (and (not (char=? r1-start #\.))
		      (not (char=? r1-start #\=))
		      (not (char=? r1-start #\:))

		      ;; Range beginning with ^ comes before that.
		      (or (char=? r1-start #\^)
			  (and (not (char=? r2-start #\^))
			       
			       ;; Other ranges are ordered by start char.
			       (< (char->ascii r1-start)
				  (char->ascii r2-start))))))))))

;;; Order loose chars:
;;;   ]   is first,
;;;   -   is next, 
;;;   .=: are next, 
;;;   [   is next,
;;;   then others (ordered by ascii val)
;;;   ^   is last.


(define (loose<= c1 c2)
  (or (char=? c1 #\])				; ] is first,
      (and (not (char=? c2 #\]))

	   (or (char=? c1 #\-)			; - is next,
	       (and (not (char=? c2 #\-))

		    ;; .=: are next,
		    (or (char=? c1 #\.) (char=? c1 #\=) (char=? c1 #\:)
			(and (not (char=? c2 #\.))
			     (not (char=? c2 #\=))
			     (not (char=? c2 #\:))

			     (or (char=? c1 #\[)	; [ is next,
				 (and (not (char=? c2 #\[))

				      (or (char=? c2 #\^)	; ^ is last,
					  (and (not (char=? c1 #\^))

					       ;; other chars by ASCII.
					       (<= (char->ascii c1)
						   (char->ascii c2)))))))))))))

;;; Returns (1) a list of 0-3 loose chars, (2) a list of 0 or 1 ranges.

(define (shrink-range-start r)
  (let ((start (char->ascii (car r)))
	(end   (char->ascii (cdr r))))
    (shrink-range-finish-up start (+ start 1) end)))

(define (shrink-range-end r)
  (let ((start (char->ascii (car r)))
	(end   (char->ascii (cdr r))))
    (shrink-range-finish-up end start (- end 1))))

(define (shrink-range-finish-up c start end)
  (cond
   ((> start end) (values (list (ascii->char c)) '())) ; Empty range

   ((= start end)			; Collapse singleton range.
    (values (list (ascii->char c) (ascii->char start))
	    '()))

   ((= (+ start 1) end)			; Collapse doubleton range.
    (values (list (ascii->char c) (ascii->char start) (ascii->char end))
	    '()))

   (else (values (list (ascii->char c))
		 (list (cons (ascii->char start) (ascii->char end)))))))


;;; We assume the bracket-spec is not a singleton, not empty, and not complete.
;;; (These cases get rendered as the letter, [^\000-\177], and ".", 
;;; respectively.) We assume the loose chars and the ranges are all disjoint.

(define (hack-bracket-spec loose ranges in?)
  (let lp ((loose0 loose) (ranges0 ranges) (end-hyphen? #f))
    ;; Repeat until stable:
    (let ((loose  (sort-list loose0  loose<=)) ; Sort loose chars and ranges.
	  (ranges (sort-list ranges0 range<)))
      ;; If ] opens or closes a range, shrink it out.
      ;; If - opens a range, shrink it out.
      (receive (loose ranges)
	  (let recur ((ranges ranges))
	    (if (pair? ranges)
		(let* ((range (car ranges))
		       (start (car range))
		       (end   (cdr range))
		       (ranges (cdr ranges)))
		  (receive (new-loose new-ranges) (recur ranges)
		    (receive (new-loose0 new-ranges0)
			(cond ((char=? #\] start)
			       (shrink-range-start range))

			      ((char=? #\] end)
			       (shrink-range-end range))

			      ((char=? #\- start)
			       (shrink-range-start range))

			      (else (values '() (list range))))
		      (values (append new-loose0  new-loose)
			      (append new-ranges0 new-ranges)))))
		(values loose '())))

	(let ((loose  (sort-list loose  loose<=)) ; Sort loose chars and ranges.
	      (ranges (sort-list ranges range<)))

	  (cond
	   ((or (not (equal? loose0  loose)) ; Loop if anything changed.
		(not (equal? ranges0 ranges)))
	    (lp loose ranges end-hyphen?))

	   ;; If the first range opens with .=:, and the last loose char is [,
	   ;; shrink it out & loop.
	   ((and (pair? ranges)
		 (memv (caar ranges) '(#\. #\= #\:))
		 (pair? loose)
		 (char=? #\[ (car (reverse loose))))
	    (receive (new-loose new-ranges)
		(shrink-range-start (car ranges))
	      (lp (append new-loose loose) (append new-ranges (cdr ranges)) end-hyphen?)))

	   ;; If there are no loose chars, the first range begins with ^, and
	   ;; we're doing an IN range, shrink out the ^.
	   ((and in? (null? loose) (pair? ranges) (char=? #\^ (caar ranges)))
	    (receive (new-loose new-ranges) (shrink-range-start (car ranges))
	      (lp (append new-loose loose) (append new-ranges ranges) end-hyphen?)))

	   ;; If both ] and - are in the loose char set,
	   ;; pull - out as special end-hypen.
	   ((and (pair? loose)
		 (pair? (cdr loose))
		 (char=? (car loose) #\])
		 (char=? (cadr loose) #\-))
	    (lp (cons (car loose) (cddr loose)) ranges #t))

	   ;; No change! Build the answer...
	   (else (string-append (if in? "[" "[^")
				(list->string loose)
				(apply string-append
				       (map (lambda (r) (string (car r) #\- (cdr r)))
					    ranges))
				(if end-hyphen? "-" "")
				"]"))))))))
