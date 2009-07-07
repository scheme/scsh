;;; Regular expression matching for scsh
;;; Copyright (c) 1994 by Olin Shivers.

;;; Match data for regexp matches.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record regexp-match
  string
  submatches)

(define (match:start match . maybe-index)
  (match-start
   (vector-ref (regexp-match:submatches match)
	       (:optional maybe-index 0))))

(define (match:end match . maybe-index)
  (match-end
   (vector-ref (regexp-match:submatches match)
	       (:optional maybe-index 0))))

(define (match:substring match . maybe-index)
  (let* ((i (:optional maybe-index 0))
	 (submatch (vector-ref (regexp-match:submatches match) i)))
    (and submatch (substring (regexp-match:string match)
			     (match-start submatch)
			     (match-end submatch)))))

;;; Compiling regexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; There's no legal Posix string expressing the empty match (e.g., (|))
;;; that will never match anything. So when we have one of these, we set
;;; the STRING field to #f. The matchers will spot this case and handle it
;;; specially.

;;; We compile the string two ways, on demand -- one for cre-search, and
;;; one for cre-search?.

;(define-record cre	; A compiled regular expression
;  string		; The Posix string form of the regexp or #F.
;  max-paren		; Max paren in STRING needed for submatches.
;  (regexp    #f)		; Compiled form or #F.
;  (regexp/nm #f)		; Same as REGEXP, but compiled with no-submatch.
;  tvec			; Translation vector for the submatches
;  ((disclose self) (list "cre" (cre:string self))))

(define-record-type cre :cre
  (really-make-cre string max-paren regexp regexp/nm tvec debug)
  cre?
  (string cre:string set-cre:string)
  (max-paren cre:max-paren set-cre:max-paren)
  (regexp cre:regexp set-cre:regexp)
  (regexp/nm cre:regexp/nm set-cre:regexp/nm)
  (tvec cre:tvec set-cre:tvec)
  (debug cre:debug set-cre:debug))

(define-record-discloser :cre
        (lambda (self) (list "cre" (cre:string self))))

(define (make-cre str max-paren tvec)
  (really-make-cre str max-paren #f #f tvec #f))

(define (new-cre str tvec)
  (make-cre str (max-live-posix-submatch tvec) tvec))

(define (max-live-posix-submatch tvec)
  (vfold (lambda (sm mlpsm) (if sm (max mlpsm sm) mlpsm)) 0 tvec))

;;; Searching with compiled regexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cre-search returns match info; cre-search? is just a predicate.

(define (cre-search cre match-vec str start)
  (let ((re-str (cre:string cre)))	;; RE-STR = #F => empty match.
    (if (not re-str)
	#f
	(begin
	  (if (not (cre:regexp cre))
	      (set-cre:regexp cre (make-regexp re-str
					       (regexp-option extended)
					       (regexp-option submatches))))
	  (let ((ret (regexp-match (cre:regexp cre) str #t #f #f start)))
	    (if (not ret)
		#f
		(make-regexp-match str 
				   (translate-submatches ret
							 (cre:tvec cre)
							 match-vec))))))))

(define (translate-submatches matches trans-vec match-vec)
  (let ((n-virtual-submatches (vector-length trans-vec)))
    (let loop ((virtual-index 0)
	       (match-index 0)
	       (matches matches))
      (cond
       ((> virtual-index n-virtual-submatches)
	match-vec)
       ((if (zero? virtual-index)
	    0
	    (vector-ref trans-vec (- virtual-index 1)))
	=> (lambda (actual-index)
	     (if (= match-index actual-index)
		 (begin
		   (vector-set! match-vec virtual-index (car matches))
		   (loop (+ 1 virtual-index) (+ 1 match-index) (cdr matches)))
		 (loop virtual-index (+ 1 match-index) (cdr matches)))))
       (else
	(loop (+ 1 virtual-index) match-index matches))))))

(define (cre-search? cre str start)
  (let ((re-str (cre:string cre)))	;; RE-STR = #F => empty match.
    (if (not re-str)
	#f
	(begin
	  (if (not (cre:regexp/nm cre))
	      (set-cre:regexp/nm cre (make-regexp re-str
						  (regexp-option extended))))
	  (regexp-match (cre:regexp/nm cre) str #f #f #f start)))))
