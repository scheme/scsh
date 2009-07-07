;;; Regexp "fold" combinators					-*- scheme -*-
;;; Copyright (c) 1998 by Olin Shivers.

;;; REGEXP-FOLD       re kons knil s [finish start] -> value
;;; REGEXP-FOLD-RIGHT re kons knil s [finish start] -> value
;;; REGEXP-FOR-EACH re proc s [start] -> unspecific

;;; Non-R4RS imports: let-optionals :optional error ?

;;; regexp-fold re kons knil s [finish start] -> value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following definition is a bit unwieldy, but the intuition is
;;; simple: this procedure uses the regexp RE to divide up string S into
;;; non-matching/matching chunks, and then "folds" the procedure KONS
;;; across this sequence of chunks.
;;;
;;; Search from START (defaulting to 0) for a match to RE; call
;;; this match M. Let I be the index of the end of the match
;;; (that is, (match:end M 0)). Loop as follows:
;;;   (regexp-fold re kons (kons START M knil) s finish I)
;;; If there is no match, return instead 
;;;   (finish START knil)
;;; FINISH defaults to (lambda (i knil) knil)
;;;
;;; In other words, we divide up S into a sequence of non-matching/matching
;;; chunks:
;;;    NM1 M1 NM1 M2 ... NMk Mk NMlast
;;; where NM1 is the initial part of S that isn't matched by the RE, M1 is the
;;; first match, NM2 is the following part of S that isn't matched, M2 is the
;;; second match, and so forth -- NMlast is the final non-matching chunk of
;;; S. We apply KONS from left to right to build up a result, passing it one
;;; non-matching/matching chunk each time: on an application (KONS i m KNIL),
;;; the non-matching chunk goes from I to (match:begin m 0), and the following
;;; matching chunk goes from (match:begin m 0) to (match:end m 0). The last
;;; non-matching chunk NMlast is processed by FINISH. So the computation we
;;; perform is
;;;   (final q (kons Jk MTCHk ... (kons J2 MTCH2 (kons J1 MTCH1 knil))...))
;;; where Ji is the index of the start of NMi, MTCHi is a match value
;;; describing Mi, and Q is the index of the beginning of NMlast.

(define (regexp-fold re kons knil s . maybe-finish+start)
  (let-optionals maybe-finish+start ((finish (lambda (i x) x))
				     (start 0))
    (if (> start (string-length s))
	(error "Illegal START parameter"
	       regexp-fold re kons knil s finish start))
    (let lp ((i start) (val knil))
      (cond
       ((regexp-search re s i) =>
	(lambda (m)
	  (let ((next-i (match:end m 0)))
	    (if (= next-i (match:start m 0))
		(error "An empty-string regexp match has put regexp-fold into an infinite loop."
		       re s start next-i)
		(lp next-i (kons i m val))))))
       (else (finish i val))))))

;;; regexp-fold-right re kons knil s [finish start] -> value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This procedure repeatedly matches regexp RE across string S.
;;; This divides S up into a sequence of matching/non-matching chunks:
;;;    NM0 M1 NM1 M2 NM2 ... Mk NMk
;;; where NM0 is the initial part of S that isn't matched by the RE,
;;; M1 is the first match, NM1 is the following part of S that isn't
;;; matched, M2 is the second match, and so forth. We apply KONS from
;;; right to left to build up a result
;;;   (final q (kons MTCH1 J1 (kons MTCH2 J2 ...(kons MTCHk JK knil)...)))
;;; where MTCHi is a match value describing Mi, Ji is the index of the end of
;;; NMi (or, equivalently, the beginning of Mi+1), and Q is the index of the
;;; beginning of M1. In other words, KONS is passed a match, an index
;;; describing the following non-matching text, and the value produced by
;;; folding the following text. The FINAL function "polishes off" the fold
;;; operation by handling the initial chunk of non-matching text (NM0, above).
;;; FINISH defaults to (lambda (i knil) knil)

(define (regexp-fold-right re kons knil s . maybe-finish+start)
  (let-optionals maybe-finish+start ((finish (lambda (i x) x))
				     (start 0))
    (if (> start (string-length s))
	(error "Illegal START parameter" regexp-fold-right re kons knil s
	       finish start))

    (cond
     ((regexp-search re s start) =>
      (lambda (m)
	(finish (match:start m 0)
		(let recur ((last-m m))
		  (cond
		   ((regexp-search re s (match:end last-m 0)) =>
		    (lambda (m)
		      (let ((i (match:start m 0)))
			(if (= i (match:end m 0))
			    (error "An empty-string regexp match has put regexp-fold-right into an infinite loop."
				   re s start i)
			    (kons last-m i (recur m))))))
		   (else (kons last-m (string-length s) knil)))))))
     (else (finish (string-length s) knil)))))

;;; regexp-for-each re proc s [start] -> unspecific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Repeatedly match regexp RE against string S. 
;;; Apply PROC to each match that is produced.
;;; Matches do not overlap.

(define (regexp-for-each re proc s . maybe-start)
  (let ((start (:optional maybe-start 0)))
    (if (> start (string-length s))
	(apply error "Illegal START parameter" regexp-for-each re proc s start)
	(let lp ((i start))
	  (cond
	   ((regexp-search re s i) =>
	    (lambda (m)
	      (let ((next-i (match:end m 0)))
		(if (= (match:start m 0) next-i)
		    (error "An empty-string regexp match has put regexp-for-each into an infinite loop."
			   re proc s start next-i))
		(proc m)
		(lp next-i)))))))))
