;;; These are some macros to support using regexp matching.

;;; (let-match m mvars body ...)
;;; Bind the vars in MVARS to the match & submatch strings of match data M,
;;; and eval the body forms. #F is allowed in the MVARS list, as a don't-care 
;;; parameter.
;;;
;;; (if-match m mvars conseq alt)
;;; The same as LET-MATCH -- eval the CONSEQ form in the scope of the
;;; bound MVARS. However, if the match data M evaluates to false, instead
;;; of blowing up, we execute the ALT form instead.

(define-syntax let-match
  (syntax-rules ()
    ((let-match ?match-exp (?mvars ...) ?body0 ?body ...)
     (let ((?match-var ?match-exp))
       (let-match-aux ?match-var 0 (?mvars ...) ?body0 ?body ...)))))

(define-syntax let-match-aux
  (syntax-rules ()
    ((let-match-aux ?match-var ?i0 (#f ?mvars ...) ?body0 ?body ...)
     (let-match-aux ?match-var (+ 1 ?i0) (?mvars ...) ?body0 ?body ...))
    ((let-match-aux ?match-var ?i0 (?mvar0 ?mvars ...) ?body0 ?body ...)
     (let ((?mvar0 (match:substring ?match-var ?i0)))
       (let-match-aux ?match-var (+ 1 ?i0) (?mvars ...) ?body0 ?body ...)))
    ((let-match-aux ?match-var ?i0 () ?body0 ?body ...)
     (begin ?body0 ?body ...))))

(define-syntax if-match
  (syntax-rules ()
    ((if-match match-exp mvars on-match no-match)
     (cond (match-exp => (lambda (m) (let-match m mvars on-match)))
	   (else no-match)))))

;;; (MATCH-COND (<match-exp> <match-vars> <body> ...)
;;;             (TEST <exp> <body> ...)
;;;             (TEST <exp> => <proc>)
;;;             (ELSE <body> ...))
;;;
;;; The first clause is as-in IF-MATCH; the next three clauses are as-in COND.
;;;
;;; It would be slicker if we could *add* extra clauses to the syntax
;;; of COND, but Scheme macros aren't extensible this way.
 
;;; Two defs. The other expander produces prettier output -- one COND
;;; rather than a mess of nested IF's.
;(define-syntax match-cond
;  (syntax-rules (else test =>)
;    ((match-cond (else body ...) clause2 ...) (begin body ...))
;
;    ((match-cond) (cond))
;
;    ((match-cond (test exp => proc) clause2 ...)
;     (let ((v exp)) (if v (proc v) (match-cond clause2 ...))))
;
;    ((match-cond (test exp body ...) clause2 ...)
;     (if exp (begin body ...) (match-cond clause2 ...)))
;
;    ((match-cond (test exp) clause2 ...)
;     (or exp (match-cond clause2 ...)))
;
;    ((match-cond (match-exp mvars body ...) clause2 ...)
;     (if-match match-exp mvars (begin body ...)
;	       (match-cond clause2 ...)))))

(define-syntax match-cond
  (syntax-rules ()
    ((match-cond clause ...) (match-cond-aux () clause ...))))

(define-syntax match-cond-aux
  (syntax-rules (test else)

   ;; No more clauses.
   ((match-cond-aux (cond-clause ...))
    (cond cond-clause ...))

   ;; (TEST . <cond-clause>)
   ((match-cond-aux (cond-clause ...)
		    (test . another-cond-clause) clause2 ...)
    (match-cond-aux (cond-clause ... another-cond-clause)
		    clause2 ...))
   
   ;; (ELSE <body> ...)
   ((match-cond-aux (cond-clause ...)
		    (else body ...) clause2 ...)
    (match-cond-aux (cond-clause ... (else body ...))))

   ;; (<match-exp> <mvars> <body> ...)
   ((match-cond-aux (cond-clause ...)
		    (match-exp mvars body ...) clause2 ...)
    (match-cond-aux (cond-clause ... (match-exp => (lambda (m)
						     (let-match m mvars
						       body ...))))
		    clause2 ...))))
