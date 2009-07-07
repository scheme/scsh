;;; The regexp data type
;;;     Olin Shivers, January 1997, May 1998.

;;;       A DSM around a choice gets absorbed into the choice's first elt.
;;;         But this prevents it from being moved out into a containing
;;;         choice or seq elt, or outer DSM. Fix.

;;; A regexp is a: dsm, submatch, seq, choice, repeat, 
;;;                char-set, string, bos, eos

;;; Deleted sub-match regexp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This stands for a regexp containing TSM submatches, of which
;;; PRE-DSM come first as dead submatches, then the regexp BODY with its 
;;; submatches, then POST-DSM as dead submatches.

(define-record-type re-dsm :re-dsm
  (really-make-re-dsm body pre-dsm tsm posix)
  re-dsm?
  (body     re-dsm:body)		; A Regexp
  (pre-dsm  re-dsm:pre-dsm)		; Integer -- initial dead submatches
  (tsm      re-dsm:tsm)					; Total submatch count
  (posix    re-dsm:posix set-re-dsm:posix))		; Posix bits

(define (make-re-dsm/tsm body pre-dsm tsm) (really-make-re-dsm body pre-dsm tsm #f))

;;; This is only used in code that the (RX ...) macro produces 
;;; for static regexps.
(define (make-re-dsm/posix body pre-dsm tsm posix-str tvec)
  (really-make-re-dsm body pre-dsm tsm (new-cre posix-str tvec)))

(define (make-re-dsm body pre-dsm post-dsm)
  (make-re-dsm/tsm body pre-dsm (+ post-dsm pre-dsm (re-tsm body))))

;;; "Virtual field" for the RE-DSM record -- how many dead submatches 
;;; come after the body:

(define (re-dsm:post-dsm re)		; Number of post-body DSM's =
  (- (re-dsm:tsm re)			;   total submatches
     (+ (re-dsm:pre-dsm re)		;   minus pre-body dead submatches
	(re-tsm (re-dsm:body re)))))	;   minus body's submatches.

;;; Slightly smart DSM constructor:
;;; - Absorb this DSM into an inner dsm.
;;; - Punt unnecessary DSM's.

(define (re-dsm body pre-dsm post-dsm)
  (let ((tsm (+ pre-dsm (re-tsm body) post-dsm)))
    (receive (body1 pre-dsm1) (open-dsm body)
      (let ((pre-dsm (+ pre-dsm pre-dsm1)))

	(if (= tsm (re-tsm body1)) body1		; Trivial DSM
	    (make-re-dsm/tsm body1 pre-dsm tsm))))))	; Non-trivial DSM

;;; Take a regexp RE and return an equivalent (re', pre-dsm) pair of values.
;;; Recurses into DSM records. It is the case that 
;;;   (<= (+ pre-dsm (re-tsm re')) (re-tsm re))
;;; The post-dsm value is (- (re-tsm re) (re-tsm re') pre-dsm).

(define (open-dsm re)
  (let lp ((re re) (pre-dsm 0))
    (if (re-dsm? re)
	(lp (re-dsm:body re) (+ pre-dsm (re-dsm:pre-dsm re)))
	(values re pre-dsm))))



;;; Sequence: (: re ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type re-seq :re-seq
  (really-make-re-seq elts tsm posix)
  re-seq?
  (elts   re-seq:elts)				; Regexp list 
  (tsm    re-seq:tsm)				; Total submatch count
  (posix  re-seq:posix set-re-seq:posix))	; Posix record

(define (make-re-seq/tsm elts tsm) (really-make-re-seq elts tsm #f))

;;; This is only used in code that (RE ...) macro produces for static regexps.
(define (make-re-seq/posix elts tsm posix-str tvec)
  (really-make-re-seq elts tsm (new-cre posix-str tvec)))

(define (make-re-seq res)
  (make-re-seq/tsm res
		   (fold (lambda (re sm-count)
			   (let ((maybe-tsm (re-tsm re)))
			     (if (and (number? maybe-tsm)
				      (number? sm-count))
				 (+ maybe-tsm sm-count)
				 (unspecific))))
			 0 res)))

;;; Slightly smart sequence constructor:
;;; - Flattens nested sequences
;;; - Drops trivial "" elements
;;; - Empty sequence => ""
;;; - Singleton sequence is reduced to its one element.
;;; - We don't descend into DSM's; too much work for this routine.

(define (re-seq res)
  (let ((res (let recur ((res res)) 	; Flatten nested seqs & drop ""'s.
	       (if (pair? res)
		   (let* ((re (car res))
			  (tail (recur (cdr res))))
		     (cond ((re-seq? re) ; Flatten nested seqs
			    (append (recur (re-seq:elts re)) tail))
			   ((re-trivial? re) tail) ; Drop trivial elts
			   (else (cons re tail))))
		   '()))))

    (if (pair? res)
	(if (pair? (cdr res))
	    (make-re-seq res)		; General case
	    (car res))			; Singleton sequence
	re-trivial)))			; Empty seq -- ""


;;; Choice: (| re ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type re-choice :re-choice
  (really-make-re-choice elts tsm posix)
  re-choice?
  (elts re-choice:elts)				; List of rel-items
  (tsm  re-choice:tsm)				; Total submatch count
  (posix  re-choice:posix set-re-choice:posix))	; Posix string

(define (make-re-choice/tsm elts tsm) (really-make-re-choice elts tsm #f))

;;; This is only used in code that (RE ...) macro produces for static regexps.
(define (make-re-choice/posix elts tsm posix-str tvec)
  (really-make-re-choice elts tsm (new-cre posix-str tvec)))

(define (make-re-choice res)
  (if (every re-char-set? res)
      (make-re-char-set (apply char-set-union (map re-char-set:cset res)))
      (make-re-choice/tsm res
                          (fold (lambda (re sm-count)
                                  (let ((maybe-tsm (re-tsm re)))
                                    (if (and (number? maybe-tsm)
                                             (number? sm-count))
                                        (+ maybe-tsm sm-count)
                                        (unspecific))))
                                0 res))))

;;; Slightly smart choice constructor:
;;; - Flattens nested choices
;;; - Drops empty (impossible) elements
;;; - Empty choice => empty-match
;;; - Singleton choice is reduced to its one element.
;;; - We don't descend into DSM's; too much work for this routine.
;;;
;;; This routine guarantees to preserve char-classness -- if it is applied
;;; to a list of char-class regexps (char-set and singleton-string re's),
;;; it will return a char-class regexp.

(define (re-choice res)
  (let ((res (let recur ((res res)) 	; Flatten nested choices
	       (if (pair? res)		; & drop empty re's.
		   (let* ((re (car res))
			  (tail (recur (cdr res))))
		     (cond ((re-choice? re) ; Flatten nested choices
			    (append (recur (re-choice:elts re)) tail))
			   ((re-empty? re) tail) ; Drop empty re's.
			   (else (cons re tail))))
		   '()))))
    ;; If all elts are char-class re's, fold them together.
    (if (every static-char-class? res)
	(let ((cset (apply char-set-union
			   (map (lambda (elt)
				  (if (re-char-set? elt)
				      (re-char-set:cset elt)
				      (string->char-set (re-string:chars elt))))
				res))))
	  (if (= 1 (char-set-size cset))
	      (make-re-string (apply string (char-set->list cset)))
	      (make-re-char-set cset)))

	(if (pair? res)
	    (if (pair? (cdr res))
		(make-re-choice res)	; General case
		(car res))		; Singleton sequence
	    re-empty))))		; Empty choice = ("")

;;; Repetition (*,?,+,=,>=,**)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The repeat record's body contains all of the repeat record's submatches --
;;; there is no pre-dsm field allowing for initial & trailing dead submatches.
;;; This is not a limit on expressiveness because repeat commutes with dsm --
;;; we can always move submatches that come before and after body to an outer
;;; DSM. Hence
;;;     (= (re-repeat:tsm re) (re-tsm (re-repeat:body re)))

(define-record-type re-repeat :re-repeat
  (really-make-re-repeat from to body tsm posix)
  re-repeat?
  (from  re-repeat:from)	; Integer    (Macro expander abuses.)
  (to    re-repeat:to)		; Integer or #f for infinite	(Macro expander abuses.)
  (body  re-repeat:body)	; Regexp
  (tsm   re-repeat:tsm)		; Total submatch count
  (posix re-repeat:posix set-re-repeat:posix))		; Posix record

(define (make-re-repeat/tsm from to body tsm)
  (really-make-re-repeat from to body tsm #f))

;;; This is only used in code that (RE ...) macro produces for static regexps.
(define (make-re-repeat/posix from to body tsm posix-str tvec)
  (really-make-re-repeat from to body tsm (new-cre posix-str tvec)))

(define (make-re-repeat from to body)
  (make-re-repeat/tsm  (check-arg (lambda (from)
				    (or (not (integer? from)) ; Dynamic
					(>= from 0)))
				  from
				  make-re-repeat)
		       (check-arg (lambda (to)
				    (or (not (integer? to)) ; #f or dynamic
					(and (integer? to) (>= to 0))))
				  to
				  make-re-repeat)
		       body
		       (re-tsm body)))

;;; Slightly smart repeat constructor
;;; - Flattens nested repeats.
;;; - re{1,1}, re{0,0}, and re{m,n} where m>n reduced.
;;; - If re is empty-match: from=0 => "", from>0 => empty-match.
;;; - If re is eos, bos, or "", and to <= from, reduce to simply re.
;;; - Commutes into DSM records.

(define (re-repeat from to body)
  (receive (re pre-dsm) (reduce-repeat from to body 0)
    (re-dsm re pre-dsm (- (re-tsm body) (+ pre-dsm (re-tsm re))))))

;;; This guy does all the work (and is also called by the repeat simplifier)

(define (reduce-repeat from to body pre-dsm)
  (receive (from to body1 pre-dsm)
           ;; Collapse nested repeats and dsm's:
           (let iter ((from from) (to to) (body body) (dsm0 pre-dsm))
	     (receive (body body-dsm0) (open-dsm body)
	       (let ((dsm0 (+ dsm0 body-dsm0)))
		 (if (and (integer? from)		; Stop if FROM or TO
			  (or (not to) (integer? to))	; are code.
			  (re-repeat? body))
		     (let ((bfrom (re-repeat:from body))
			   (bto (re-repeat:to body))
			   (bbody (re-repeat:body body)))
		       (if (or (not (integer? bfrom))        ; Stop if bfrom or
			       (and bto (not (integer? bto)))) ; bto are code.
			   (values from to body dsm0)
			   (iter (* from bfrom)
				 (and to bto (* to bto))
				 bbody
				 dsm0)))
		     (values from to body dsm0)))))

    (cond
     ((and (eqv? from 1) (eqv? to 1))	; re{1,1} => re
      (values body1 pre-dsm))

     ((and (eqv? from 0) (eqv? to 0))	; re{0,0} => ""
      (values re-trivial (+ (re-tsm body1) pre-dsm)))

     ;; re{m,n} => re-empty when m>n:
     ((and (integer? from) (integer? to) (> from to))
      (values re-empty (+ (re-tsm body1) pre-dsm)))

     ;; Reduce the body = re-empty case.
     ((and (re-empty? body1) (integer? from)) ; (+ (in)) => (in)
      (values (if (> from 0) re-empty re-trivial) ; (* (in)) => ""
	      pre-dsm))

     ;; If BODY1 is eos, bos, or "", and m<=n, reduce to simply BODY1.
     ((and (integer? from)
	   (or (and (integer? to) (<= from to)) (not to))
	   (or (re-eos? body1)
	       (re-bos? body1)
	       (and (re-string? body1)
		    (string=? "" (re-string:chars body1)))))
      (values body1 pre-dsm))

     (else (values (make-re-repeat from to body1) ; general case
		   pre-dsm)))))



;;; Submatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A submatch record introduces a new submatch. This is followed by
;;; PRE-DSM dead submatches (caused by simplifying the body), then the
;;; BODY, then perhaps more dead submatches, all for a total of TSM
;;; submatches.

(define-record-type re-submatch :re-submatch
  (really-make-re-submatch body pre-dsm tsm posix)
  re-submatch?
  (body re-submatch:body)	; Regexp
  (pre-dsm re-submatch:pre-dsm)	; Deleted submatches preceding the body
  (tsm  re-submatch:tsm)	; Total submatch count for the record
  (posix re-submatch:posix set-re-submatch:posix)) ; Posix string

(define (make-re-submatch/tsm body pre-dsm tsm)
  (really-make-re-submatch body pre-dsm tsm #f))

;;; This is only used in code that (RE ...) macro produces for static regexps.
(define (make-re-submatch/posix body pre-dsm tsm posix-str tvec)
  (really-make-re-submatch body pre-dsm tsm (new-cre posix-str tvec)))


;;; "Virtual field" for the RE-SUBMATCH record -- how many dead submatches 
;;; come after the body:

(define (re-submatch:post-dsm re)	 ; Number of post-body DSM's =
  (- (re-submatch:tsm re)		 ;   total submatches
     (+ 1				 ;   minus *this* submatch
	(re-submatch:pre-dsm re)	 ;   minus pre-body dead submatches
	(re-tsm (re-submatch:body re)))));   minus body's submatches.

(define (make-re-submatch body . maybe-pre+post-dsm)
  (let-optionals maybe-pre+post-dsm ((pre-dsm 0) (post-dsm 0))
    (make-re-submatch/tsm body pre-dsm (+ pre-dsm 1 (re-tsm body) post-dsm))))

;;; Slightly smart submatch constructor
;;; - DSM's unpacked
;;; - If BODY is the re-empty, we'll never match, so just produce a DSM.

(define (re-submatch body . maybe-pre+post-dsm)
  (let-optionals maybe-pre+post-dsm ((pre-dsm 0) (post-dsm 0))
    (let ((tsm (+ 1 pre-dsm (re-tsm body) post-dsm)))
      (receive (body1 pre-dsm1) (open-dsm body)
	(if (re-empty? body1)
	    (re-dsm re-empty tsm 0)
	    (make-re-submatch/tsm body1 (+ pre-dsm pre-dsm1) tsm))))))



;;; Other regexps : string, char-set, bos & eos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Also, re-empty and re-trivial.

(define-record-type re-string :re-string
  (really-make-re-string chars posix)
  re-string?
  (chars re-string:chars set-re-string:chars)
  (posix re-string:posix set-re-string:posix))

(define-record-discloser :re-string
  (lambda (r)
    (list 're-string
	  (re-string:chars r))))

;; Kludge: POSIX wants "()" for "the empty string".

(define (make-re-string chars)
  (if (string=? "" chars)
      re-trivial
      (really-make-re-string chars #f)))

(define re-string make-re-string)	; For consistency w/other re makers.

;;; This is only used in code that (RE ...) macro produces for static regexps.
(define (make-re-string/posix chars posix-str tvec)
  (if (string=? "" chars)
      re-trivial
      (really-make-re-string chars (new-cre posix-str tvec))))

(define re-empty-string (really-make-re-string "" #f))

;;; Matches the empty string anywhere.
(define re-trivial (make-re-dsm/posix re-empty-string
				      1 0 "()" '#()))

(define (re-trivial? re)
  (eq? re re-trivial))

(define-record re-char-set
  cset			; A character set	(Macro expander abuses.)
  (posix    #f))	; Posix record

(define re-char-set make-re-char-set)	; For consistency w/other re makers.

;;; This is only used in code that (RE ...) macro produces for static regexps.
(define (make-re-char-set/posix cs posix-str tvec)
  (let ((re (make-re-char-set cs)))
    (set-re-char-set:posix re (new-cre posix-str tvec))
    re))

;;; Never matches
;;; NEED TO OPTIMIZE - PRE-SET POSIX FIELD.
(define re-empty (make-re-char-set char-set:empty))

(define (re-empty? re)
  (and (re-char-set? re)
       (let ((cs (re-char-set:cset re)))
	 (and (char-set? cs) ; Might be code...
	      (char-set-empty? cs)))))

(define-record re-bos)	(define re-bos (make-re-bos))
(define-record re-eos)  (define re-eos (make-re-eos))

(define-record re-bol)  (define re-bol (make-re-bol))
(define-record re-eol)  (define re-eol (make-re-eol))

(define re-any (make-re-char-set/posix char-set:full "." '#()))

(define (re-any? re)
  (and (re-char-set? re)
       (let ((cs (re-char-set:cset re)))
	 (and (char-set? cs) ; Might be code...
	      (char-set-full? cs)))))

(define re-nonl
  (make-re-char-set/posix (char-set-complement (char-set #\newline))
			  "[^\n]"
			  '#()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regexp? x)
  (or (re-seq? x)      (re-choice? x)   (re-repeat? x)
      (re-char-set? x) (re-string? x)
      (re-bos? x)      (re-eos? x)
      (re-bol? x)      (re-eol? x)
      (re-submatch? x) (re-dsm? x)))


;;; Return the total number of submatches bound in RE.

(define (re-tsm re)
  (cond
   ((re-seq? re)      (re-seq:tsm re))
   ((re-choice? re)   (re-choice:tsm re))
   ((re-repeat? re)   (re-repeat:tsm re))
   ((re-dsm? re)      (re-dsm:tsm re))
   ((re-submatch? re) (re-submatch:tsm re))
   ((or (re-char-set? re) (re-string? re)
	(re-bos? re) (re-eos? re)
	(re-bol? re) (re-eol? re))
    0)))

;;; (flush-submatches re)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Return regular expression RE with all submatch-binding elements
;;; stripped out -- (= 0 (re-tsm (flush-submatches re))).

(define (flush-submatches re)
  (cond
   ((zero? (re-tsm re)) re)		; RE has no submatches.

   ((re-seq?    re) (re-seq    (map flush-submatches (re-seq:elts    re))))
   ((re-choice? re) (re-choice (map flush-submatches (re-choice:elts re))))

   ((re-repeat? re) (re-repeat (re-repeat:from re)
			       (re-repeat:to re)
			       (flush-submatches (re-repeat:body re))))
		  
   ((re-submatch? re) (flush-submatches (re-submatch:body re)))
   ((re-dsm? re)      (flush-submatches (re-dsm:body      re)))

   (else re)))


;;; Map F over ELTS. (F x) returns two values -- the "real" return value,
;;; and a "changed?" flag. If CHANGED? is false, then the "real" return value
;;; should be identical to the original argument X. MAP/CHANGED constructs
;;; the mapped list sharing as long an unchanged tail as possible with the
;;; list ELTS; if F changes no argument, MAP/CHANGED returns exactly the list
;;; ELTS. MAP/CHANGED returns two values: the mapped list, and a changed? 
;;; flag for the entire list.

(define (map/changed f elts)
  (let recur ((elts elts))
    (if (pair? elts)
	(let ((elt (car elts)))
	  (receive (new-elts elts-changed?) (recur (cdr elts))
	    (receive (new-elt elt-changed?) (f elt)
	      (if (or elts-changed? elt-changed?)
		  (values (cons new-elt new-elts) #t)
		  (values elts #f)))))
	  (values '() #f))))


(define (uncase re)
  (receive (new-re changed?)
      (let recur ((re re))
	(cond
	 ((re-seq? re)
	  (let ((elts (re-seq:elts re)))
	    (receive (new-elts elts-changed?)
		(map/changed recur elts)
	      (if elts-changed?
		  (values (make-re-seq/tsm new-elts (re-seq:tsm re)) #t)
		  (values re #f)))))

	 ((re-choice? re)
	  (let ((elts (re-choice:elts re)))
	    (receive (new-elts elts-changed?)
		(map/changed recur elts)
	      (if elts-changed?
		  (values (re-choice new-elts) #t)
		  (values re #f)))))

	 ((re-char-set? re)
	  (let* ((cs (re-char-set:cset re))
		 (new-cs-re (uncase-char-set cs))) ; Better not be code.
	    (if (char-set= cs  (re-char-set:cset new-cs-re))
		(values re #f)
		(values new-cs-re #t))))

	 ((re-repeat? re)
	  (receive (new-body body-changed?) (recur (re-repeat:body re))
	    (if body-changed?
		(values (re-repeat (re-repeat:from re)
				   (re-repeat:to re)
				   new-body)
			#t)
		(values re #f))))

	 ((re-submatch? re)
	  (receive (new-body body-changed?) (recur (re-submatch:body re))
	    (if body-changed?
		(values (make-re-submatch/tsm new-body
					      (re-submatch:pre-dsm re)
					      (re-submatch:tsm     re))
			#t)
		(values re #f))))
		  
	 ((re-string? re)
	  (let ((cf-re (uncase-string (re-string:chars re))))
	    (if (re-string? cf-re)
		(values re    #f)
		(values cf-re #t))))

	 (else (values re #f))))
    new-re))
		  
     
;;; (uncase-char-set cs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Return a regexp for char-set cs' such that cs' contains every char
;;; c in cs in both its upcase and downcase form.

(define (uncase-char-set cs)
  (make-re-char-set
   (char-set-fold (lambda (c new-cset)
                    (char-set-adjoin! new-cset
                                      (char-downcase c)
                                      (char-upcase c)))
                  (char-set-copy char-set:empty)
                  cs)))


;;; I actually make an effort to keep this a re-string
;;; if possible (if the string contains no case-sensitive
;;; characters). Returns a regexp matching the string in
;;; a case-insensitive fashion.

(define (uncase-string s)
  ;; SEQ is a list of chars and doubleton char-sets.
  (let* ((seq (string-fold-right (lambda (c lis)
				   (cons (cond
					  ((char-lower-case? c)
					   (char-set c (char-upcase   c)))
					  ((char-upper-case? c)
					   (char-set c (char-downcase c)))
					  (else c))
					 lis))
				 '() s))

	 ;; Coalesce adjacent chars together into a string.
	 (fixup (lambda (chars seq)
		  (if (pair? chars)
		      (cons (make-re-string (list->string (reverse chars)))
			    seq)
		      seq)))

	 (new-seq (let recur ((seq seq) (chars '()))
		    (if (pair? seq)
			(let ((elt (car seq))
			      (seq (cdr seq)))
			  (if (char? elt)
			      (recur seq (cons elt chars))
			      (fixup chars (cons (make-re-char-set elt)
						 (recur seq '())))))
			(fixup chars '())))))

    (if (= 1 (length new-seq)) (car new-seq)
	(make-re-seq new-seq))))


		     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define char-set-full?
  (let ((allchars-nchars (char-set-size char-set:full)))
    (lambda (cs) (= allchars-nchars (char-set-size cs)))))

(define (char-set-empty? cs) (zero? (char-set-size cs)))


;;; A "char-class" re is either a char-set re or a string re whose string
;;; has only one character.

(define (re-char-class? re)
  (or (re-char-set? re)
      (and (re-string? re)
	   (= 1 (string-length (re-string:chars re))))))

(define (static-char-class? re)
  (or (and (re-char-set? re)
	   (char-set? (re-char-set:cset re)))	; This might be code.
      (and (re-string? re)			; But never this, so no check.
	   (= 1 (string-length (re-string:chars re))))))
