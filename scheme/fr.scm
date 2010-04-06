;;; Field and record parsing utilities for scsh.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.

;;; Notes:
;;; - Comment on the dependencies here...
;;; - Awk should deal with case-insensitivity.
;;; - Should I change the field-splitters to return lists? It's the
;;;   right thing, and costs nothing in terms of efficiency.

;;; Looping primitives:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; It is nicer for loops that loop over a bunch of different things
;;; if you can encapsulate the idea of iterating over a data structure
;;; with a 
;;;     (next-element state) -> elt next-state
;;;     (more-elements? state) -? #t/#f
;;; generator/termination-test pair. You can use the generator with REDUCE
;;; to make a list; you can stick it into a loop macro to loop over the 
;;; elements. For example, if we had an extensible Yale-loop style loop macro,
;;; we could have a loop clause like
;;; 
;;;     (loop (for field in-infix-delimited-string ":" path)
;;;           (do (display field) (newline)))
;;; 
;;; and it would be simple to expand this into code using the generator.
;;; With procedural inlining, you can get pretty optimal loops over data
;;; structures this way.
;;;
;;; As of now, you are forced to parse fields into a buffer, and loop
;;; over that. This is inefficient of time and space.  If I ever manage to do
;;; an extensible loop macro for Scheme 48, I'll have to come back to this
;;; package and rethink how to provide this functionality.

;;; Forward-progress guarantees and empty string matches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A loop that pulls text off a string by matching a regexp against
;;; that string can conceivably get stuck in an infinite loop if the
;;; regexp matches the empty string. For example, the regexps
;;; ^, $, .*, foo|[^f]* can all match the empty string.
;;; 
;;; The regexp-loop routines in this code are careful to handle this case. 
;;; If a regexp matches the empty string, the next search starts, not from
;;; the end of the match (which in the empty string case is also the 
;;; beginning -- there's the rub), but from the next character over.
;;; This is the correct behaviour. Regexps match the longest possible
;;; string at a given location, so if the regexp matched the empty string
;;; at location i, then it is guaranteed they could not have matched
;;; a longer pattern starting with character #i. So we can safely begin
;;; our search for the next match at char i+1.
;;; 
;;; So every iteration through the loop makes some forward progress,
;;; and the loop is guaranteed to terminate.
;;; 
;;; This has the effect you want with field parsing. For example, if you split
;;; a string with the empty pattern, you will explode the string into its
;;; individual characters:
;;;     ((suffix-splitter (rx "")) "foo") -> #("" "f" "o" "o")
;;; However, even though this boundary case is handled correctly, we don't
;;; recommend using it. Say what you mean -- just use a field splitter:
;;;     ((field-splitter (rx any)) "foo") -> #("f" "o" "o")


;;; FIELD PARSERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section defines routines to split a string into fields.
;;; You can parse by specifying a pattern that *separates* fields,
;;; a pattern that *terminates* fields, or a pattern that *matches*
;;; fields.

(define (->delim-matcher x)
  (if (procedure? x) x					; matcher proc
      (let ((re (cond ((string? x) (re-string x))
		      ((char-set? x) (re-char-set x))
		      ((char? x) (re-string (string x)))
		      ((regexp? x) x)
		      (else (error "Illegal field-reader delimiter value" x)))))
	(lambda (s i)
	  (cond ((regexp-search re s i) =>
		 (lambda (m) (values (match:start m 0) (match:end m 0))))
		(else (values #f #f)))))))

;;; (infix-splitter         [re num-fields handle-delim])	-> parser
;;; (suffix-splitter        [re num-fields handle-delim])	-> parser
;;; (sloppy-suffix-splitter [re num-fields handle-delim])	-> parser
;;; (field-splitter         [re num-fields])		  	-> parser
;;;
;;; (parser string [start]) -> string-list

(define (make-field-parser-generator default-delim-matcher loop-proc)
  ;; This is the parser-generator
  (lambda args
    (let-optionals args ((delim-spec default-delim-matcher)
			 (num-fields #f)
			 (handle-delim 'trim))
      ;; Process and error-check the args
      (let ((match-delim (->delim-matcher delim-spec))
	    (cons-field (case handle-delim	 	; Field     is s[i,j).
			  ((trim)			; Delimiter is s[j,k).
			   (lambda (s i j k fields)
			     (cons (substring s i j) fields)))
			  ((split)
			   (lambda (s i j k fields)
			     (cons (substring s j k)
				   (cons (substring s i j) fields))))
			  ((concat)
			   (lambda (s i j k fields)
			     (cons (substring s i k)
				   fields)))
			  (else
			   (error "Illegal handle-delim spec"
				  handle-delim)))))

	(receive (num-fields nfields-exact?)
	         (cond ((not num-fields) (values #f #f))
		       ((not (integer? num-fields))
			(error "Illegal NUM-FIELDS value" num-fields))
		       ((<= num-fields 0) (values (- num-fields) #f))
		       (else (values num-fields #t)))

	  ;; This is the parser.
	  (lambda (s . maybe-start)
	    (reverse (loop-proc s (:optional maybe-start 0)
				match-delim cons-field
				num-fields nfields-exact?))))))))

;;; Default field spec is runs of non-whitespace chars.
(define default-field-matcher (->delim-matcher (rx (+ (~ white)))))

;;; (field-splitter [field-spec num-fields])

(define (field-splitter . args)
  (let-optionals args ((field-spec default-field-matcher)
		       (num-fields #f))

    ;; Process and error-check the args
    (let ((match-field (->delim-matcher field-spec)))
      (receive (num-fields nfields-exact?)
	       (cond ((not num-fields) (values #f #f))
		     ((not (integer? num-fields))
		      (error "Illegal NUM-FIELDS value"
			     field-splitter num-fields))
		     ((<= num-fields 0) (values (- num-fields) #f))
		     (else (values num-fields #t)))

	;; This is the parser procedure.
	(lambda (s . maybe-start)
	  (reverse (fieldspec-field-loop s (:optional maybe-start 0)
					 match-field num-fields nfields-exact?)))))))


;;; These four procedures implement the guts of each parser
;;; (field, infix, suffix, and sloppy-suffix).
;;;
;;; The CONS-FIELD argument is a procedure that parameterises the
;;; HANDLE-DELIM action for the field parser.
;;; 
;;; The MATCH-DELIM argument is used to match a delimiter. 
;;; (MATCH-DELIM S I) returns two integers [start, end] marking
;;; the next delimiter after index I in string S. If no delimiter is
;;; found, it returns [#f #f].

;;; In the main loop of each parser, the loop variable LAST-NULL? tells if the
;;; previous delimiter-match matched the empty string. If it did, we start our
;;; next delimiter search one character to the right of the match, so we won't
;;; loop forever. This means that an empty delimiter regexp "" simply splits
;;; the string at each character, which is the correct thing to do.
;;;
;;; These routines return the answer as a reversed list.


(define (fieldspec-field-loop s start match-field num-fields nfields-exact?)
  (let ((end (string-length s)))
    (let lp ((i start) (nfields 0) (fields '()) (last-null? #f))
      (let ((j (if last-null? (+ i 1) i)) ; Where to start next delim search.

	    ;; Check to see if we made our quota before returning answer.
	    (finish-up (lambda ()
			 (if (and num-fields (< nfields num-fields))
			     (error "Too few fields in record." num-fields s)
			     fields))))

	(cond ((> j end) (finish-up))	; We are done. Finish up.

	      ;; Read too many fields. Bomb out.
	      ((and nfields-exact? (> nfields num-fields))
	       (error "Too many fields in record." num-fields s))

	      ;; Made our lower-bound quota. Quit early.
	      ((and num-fields (= nfields num-fields) (not nfields-exact?))
	       (if (= i end) fields	; Special case hackery.
		   (cons (substring s i end) fields)))

	      ;; Match off another field & loop.
	      (else (receive (m0 m1) (match-field s j)
	              (if m0 (lp m1 (+ nfields 1)
				 (cons (substring s m0 m1) fields)
				 (=  m0 m1))
			  (finish-up)))))))))	; No more matches. Finish up.


(define (infix-field-loop s start match-delim cons-field
			  num-fields nfields-exact?)
  (let ((end (string-length s)))
    (if (= start end) '() ; Specially hack empty string.

	(let lp ((i start) (nfields 0) (fields '()) (last-null? #f))
	  (let ((finish-up (lambda ()
			     ;; s[i,end) is the last field. Terminate the loop.
			     (cond ((and num-fields (< (+ nfields 1) num-fields))
				    (error "Too few fields in record."
					   num-fields s))
			      
				   ((and nfields-exact?
					 (>= nfields num-fields))
				    (error "Too many fields in record."
					   num-fields s))

				   (else
				    (cons (substring s i end) fields)))))

		(j (if last-null? (+ i 1) i))) ; Where to start next search.

	    (cond
		  ;; If we've read NUM-FIELDS fields, quit early .
	          ((and num-fields (= nfields num-fields))
		   (if nfields-exact?
		       (error "Too many fields in record." num-fields s)
		       (cons (substring s i end) fields)))

	    
		  ((<= j end)		; Match off another field.
		   (receive (m0 m1) (match-delim s j)
		     (if m0
			 (lp m1 (+ nfields 1)
			     (cons-field s i m0 m1 fields)
			     (= m0 m1))
			 (finish-up)))) ; No more delimiters - finish up.

		  ;; We've run off the end of the string. This is a weird
		  ;; boundary case occuring with empty-string delimiters.
		  (else (finish-up))))))))



;;; Match off an optional initial delimiter,
;;; then jump off to the suffix parser.

(define (sloppy-suffix-field-loop s start match-delim cons-field
				  num-fields nfields-exact?)
  ;; If sloppy-suffix, skip an initial delimiter if it's there.
  (let ((start (receive (i j) (match-delim s start)
                 (if (and i (zero? i)) j start))))
    (suffix-field-loop s start match-delim cons-field
		       num-fields nfields-exact?)))


(define (suffix-field-loop s start match-delim cons-field
			   num-fields nfields-exact?)
  (let ((end (string-length s)))

    (let lp ((i start) (nfields 0) (fields '()) (last-null? #f))
      (let ((j (if last-null? (+ i 1) i))) ; Where to start next delim search.
	(cond ((= i end) ; We are done.
	       (if (and num-fields (< nfields num-fields)) ; Didn't make quota.
		   (error "Too few fields in record." num-fields s)
		   fields))

	      ;; Read too many fields. Bomb out.
	      ((and nfields-exact? (= nfields num-fields))
	       (error "Too many fields in record." num-fields s))

              ;; Made our lower-bound quota. Quit early.
	      ((and num-fields (= nfields num-fields) (not nfields-exact?))
	       (cons (substring s i end) fields))

	      (else ; Match off another field.
	       (receive (m0 m1) (match-delim s j)
		 (if m0 (lp m1 (+ nfields 1)
			    (cons-field s i m0 m1 fields)
			    (= m0 m1))
		     (error "Missing field terminator" s)))))))))


;;; Now, build the exported procedures: {infix,suffix,sloppy-suffix}-splitter.

(define default-suffix-matcher (->delim-matcher (rx (| (+ white) eos))))
(define default-infix-matcher  (->delim-matcher (rx (+ white))))

(define infix-splitter
  (make-field-parser-generator default-infix-matcher  infix-field-loop))
(define suffix-splitter
  (make-field-parser-generator default-suffix-matcher suffix-field-loop))
(define sloppy-suffix-splitter
  (make-field-parser-generator default-suffix-matcher sloppy-suffix-field-loop))



;;; Reading records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-record-delims (char-set #\newline))

;;; (record-reader [delims elide? handle-delim]) -> reader
;;; (reader [port]) -> string or eof

(define (record-reader . args)
  (let-optionals args ((delims default-record-delims)
		      (elide? #f)
		      (handle-delim 'trim))
    (let ((delims (x->char-set delims)))

      (case handle-delim
	((trim)			; TRIM-delimiter reader.
	 (lambda maybe-port
	   (let ((s (apply read-delimited delims maybe-port)))
	     (if (and (not (eof-object? s)) elide?)
		 (apply skip-char-set delims maybe-port)) ; Snarf extra delims.
	     s)))

	((concat)		; CONCAT-delimiter reader.
	 (let ((not-delims (char-set-complement delims)))
	   (lambda maybe-port
	     (let* ((p (:optional maybe-port (current-input-port)))
		    (s (read-delimited delims p 'concat)))
	       (if (or (not elide?) (eof-object? s)) s
		   (let ((extra-delims (read-delimited not-delims p 'peek)))
		     (if (eof-object? extra-delims) s
			 (string-append s extra-delims))))))))

	((split)		; SPLIT-delimiter reader.
	 (let ((not-delims (char-set-complement delims)))
	   (lambda maybe-port
	     (let ((p (:optional maybe-port (current-input-port))))
	       (receive (s delim) (read-delimited delims p 'split)
		 (if (eof-object? s) (values s s)
		     (values s
			     (if (or (not elide?) (eof-object? delim))
				 delim
				 ;; Elide: slurp in extra delims.
				 (let ((delim (string delim))
				       (extras (read-delimited not-delims
							       p 'peek)))
				   (if (eof-object? extras) delim
				       (string-append delim extras)))))))))))

	(else
	 (error "Illegal delimiter-action" handle-delim))))))


;;; Reading and parsing records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (field-reader [field-parser rec-reader]) -> reader
;;; (reader [port]) -> [raw-record parsed-record] or [eof '()]
;;; 
;;; This is the field reader, which is basically just a composition of
;;; RECORD-READER and FIELD-PARSER.

(define default-field-parser (field-splitter))

(define (field-reader . args)
  (let-optionals args ((parser    default-field-parser)
		       (rec-reader read-line))
    (lambda maybe-port
      (let ((record (apply rec-reader maybe-port)))
	(if (eof-object? record)
	    (values record '())
	    (values record (parser record)))))))



;;; Parse fields by regexp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code parses up a record into fields by matching a regexp specifying
;;; the field against the record. The regexp describes the *field*. In the
;;; other routines, the regexp describes the *delimiters*. They are
;;; complimentary.

;;; Repeatedly do (APPLY PROC M STATE) to generate new state values,
;;; where M is a regexp match structure made from matching against STRING.

;(define (regexp-fold string start regexp proc . state)
;  (let ((end (string-length string)))
;    (let lp ((i start) (state state) (last-null? #f))
;      (let ((j (if last-null? (+ i 1) i)))
;	(cond ((and (<= j end) (regexp-search regexp string j)) =>
;               (lambda (m)
;		 (receive state (apply proc m state)
;		   (lp (match:end m) state (= (match:start m) (match:end m))))))
;	      (else (apply values state)))))))
;
;(define (all-regexp-matches regexp string)
;  (reverse (regexp-fold string 0 regexp
;			 (lambda (m ans) (cons (match:substring m 0) ans))
;			 '())))
