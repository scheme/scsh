;;; An awk loop, after the design of David Albertz and Olin Shivers.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.

;;; This uses the new RX SRE syntax. Defines a Clinger-Rees expander for
;;; the old, pre-SRE syntax AWK, and one for the new SRE-syntax AWK.

;;; Imports:
;;; - Requires RECEIVE from RECEIVING package.
;;; - Would require DESTRUCTURE from DESTRUCTURING package, but it appears
;;;   to be broken, so we hack it w/cars and cdrs.
;;; - Requires STRING-MATCH and STRING-MATCH? from RE-EXPORTS package.
;;; - Requires regexp manipulation stuff from SRE-SYNTAX-TOOLS
;;; - Requires ERROR from ERROR-PACKAGE.
;;; - Requires ANY and FILTER frm SCSH-UTILITIES.
;;;
;;; Needs error-package receiving sre-syntax-tools scsh-utilities
;;;
;;; Exports:
;;; (expand-awk exp r c)                Clinger-Rees macro expander, new syntax
;;; (expand-awk/obsolete exp r c)       Clinger-Rees macro expander, old syntax
;;;
;;; next-range next-:range              These four functions are used in the
;;; next-range: next-:range:                    code output by the expander.


;;; Examples:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ;;; Filter -- pass only lines containing my name.
;;; (awk (read-line) (line) ()
;;;   ("Olin" (display line) (newline)))
;;;
;;; ;;; Count the number of non-comment lines of code in my Scheme source.
;;; (awk (read-line) (line) ((nlines 0))
;;;   ((: bos (* white) ";")    nlines)         ; A comment line.
;;;   (else                     (+ nlines 1)))  ; Not a comment line.
;;;
;;;  ;;; Read numbers, counting the evens and odds,
;;;  ;;; and printing out sign information.
;;;  (awk (read) (val) ((evens 0) (odds 0))
;;;     ((zero? val) (display "zero ") (values evens odds))
;;;     ((> val 0)   (display "pos ")  (values evens odds))
;;;     (else        (display "neg ")  (values evens odds))
;;;
;;;     ((even? val) (values (+ evens 1) odds))
;;;     (else        (values evens       (+ odds 1))))

;;; Syntax:
;;; (awk <reader-exp> <rec&field-vars> [<rec-counter>] <state-var-inits>
;;;   <clause1>
;;;       .
;;;       .
;;;   <clausen>)
;;;
;;; <clause> ::= (ELSE body ...)
;;;          |   (:RANGE test1 test2 body ...) ; RANGE :RANGE RANGE: :RANGE:
;;;          |   (AFTER body ...)
;;;          |   (test => proc)
;;;          |   (test ==> vars body ...)
;;;          |   (test body ...)
;;;
;;;  test ::= integer | sre | (WHEN exp) | exp
;;;  (sre/exp ambiguities resolved in favor of SRE)


;;; This macro is written using Clinger/Rees explicit-renaming low-level
;;; macros. So it is pretty ugly. It takes a little care to generate
;;; cosmetically attractive code, for two reasons:
;;; - It makes it easier for humans to examine the expanded code.
;;; - It helps low-tech compilers compile the code well. Some of the
;;;   optimisations the expander implements would be hard for even a
;;;   sophisticated compiler to perform automatically. For example, it doesn't
;;;   introduce a record-counter variable unless required to do so. It's a
;;;   non-trivial analysis to spot and remove an unused loop variable (I show
;;;   how to do so in my dissertation; I don't know of any production
;;;   compilers that do it). Same remarks apply to the variable that tracks
;;;   the state bit for ELSE clauses -- we don't introduce one unless the loop
;;;   actually contains ELSE clauses. The lesson here is that loop macros
;;;   by definition have information about the data-flow of their bodies that
;;;   compilers have to work hard to spot by analysis of their expanded forms.
;;;   The macro can exploit this knowledge at the high-level.
;;;
;;; Interesting research issue: Could one design a macro system that would
;;; allow the macro to communicate this knowledge to the compiler? Could
;;; the macro's assertions be verified by the compiler, as well?
;;;
;;; In any even, there's a down-side to this cosmetic clean-up:
;;; all of this optimisation definitely makes the macro a lot more hairy
;;; than it would otherwise be. The expanded code is easier to read; the
;;; macro itself is harder to read.


;;; Simple syntax-hacking utilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a form that produces multiple values.
;;; ()          => (values)
;;; (v)         => v
;;; (v1 v2 ...) => (values v1 v2 ...)

(define (mult-values vals rename)
  (if (or (not (pair? vals)) (pair? (cdr vals)))
      `(,(rename 'values) . ,vals)
      (car vals)))

;;; ()      => ()
;;; (v1)    => (v1)
;;; (v1 v2) => ((VALUES v1 v2))
;;;
;;; Return an expression list, not an expression. (Either 1 or 0 expressions.)
;;; Use this one when we don't care what happens if we are returning 0 vals.
;;; It pairs up with MV-LET below, which ignores the number of values
;;; returned to it when expecting zero values.

(define (sloppy-mult-values vals rename)
  (if (and (pair? vals) (pair? (cdr vals)))
      `((,(rename 'values) . ,vals))
      vals))

;; DEBLOCK maps an expression to a list of expressions, flattening BEGINS.
;; (deblock '(begin (begin 3 4) 5 6 (begin 7 8))) => (3 4 5 6 7 8)

(define (deblock exp rename compare)
  (let ((%block (rename 'begin)))
    (let deblock1 ((exp exp))
      (if (and (pair? exp)
;              (name? (car exp))
               (compare %block (car exp)))
          (apply append (map deblock1 (cdr exp)))
          (list exp)))))

;; BLOCKIFY maps an expression list to a BEGIN form, flattening nested BEGINS.
;; (blockify '( (begin 3 4) 5 (begin 6) )) => (begin 3 4 5 6)

(define (blockify exps rename compare)
  (let ((new-exps (apply append
                         (map (lambda (exp) (deblock exp rename compare))
                              exps))))
    (cond ((null? new-exps)
           (error "Empty BEGIN" exps))
          ((null? (cdr new-exps))       ; (begin exp) => exp
           (car new-exps))
          (else `(,(rename 'begin) . ,new-exps)))))


(define (mv-let r c vars exp body)
  (if (pair? vars)
      (if (pair? (cdr vars))
          `(,(r 'receive) ,vars ,exp . ,(deblock body r c))
          `(,(r 'let) ((,(car vars) ,exp)) . ,(deblock body r c)))
      (blockify (list exp body) r c)))


;;; Is X one of the keywords {range, :range, range:, :range:}?
(define (range-keyword? x rename compare)
  (or (compare x (rename 'range))
      (compare x (rename ':range))
      (compare x (rename 'range:))
      (compare x (rename ':range:))))

;;; Apply PRED to every element of VALS. Collect & return all the non-#f
;;; values produced.
(define (all-trues pred vals)
  (let lp ((vals vals) (ans '()))
    (if (pair? vals)
        (lp (cdr vals)
            (cond ((pred (car vals)) => (lambda (elt) (cons elt ans)))
                  (else ans)))
        (reverse ans))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If STRING-REGEXPS? is true, we use the old, obsolete syntax, where
;;; a test form that is a string, such as "shivers|bdc", is treated as
;;; a regular expression in the Posix string syntax. Otherwise, we use the
;;; new SRE syntax, where strings are treated as SRE constants.

(define (expand-awk exp r c)          (really-expand-awk exp r c #f))
(define (expand-awk/obsolete exp r c) (really-expand-awk exp r c #t))

(define (really-expand-awk exp r c string-regexps?)
  (let* ((%lambda       (r 'lambda))    ; Bind a mess of keywords.
         (%let          (r 'let))
         (%if           (r 'if))
         (%eof-object?  (r 'eof-object?))
         (%after        (r 'after))
         (%else         (r 'else))
         (%+            (r '+))
         (%rx           (r 'rx))

         (gensym (let ((i 0))
                   (lambda (s)
                     (set! i (+ i 1))
                     (string->symbol (string-append s (number->string i))))))

         ;; Is the clause a range-test clause?
         (range? (lambda (clause) (range-keyword? (car clause) r c)))

         ;; Make some standard vars we'll need.
         (lp-var (r 'lp))
         (reader (r 'read-rec))
         ;; If I throw in an abort-loop or abort-iteration macro,
         ;; I'll also need to make two vars for the continuations.

         ;; Rip the form apart.
         (reader-exp (cadr exp))
         ;; Replace #F's with gensym'd variables in the record/field vars.
         (rec/field-vars (map (lambda (v) (or v (r (gensym "anon-rfval"))))
                              (caddr exp)))
         (rec-var (car rec/field-vars)) ; The var bound to the record.
         (rest (cdddr exp)))            ; Stuff after the rec&field-vars.

    (receive (rec-counter state-inits clauses)          ; Parse out the last
             (if (list? (car rest))                     ; three parts of the
                 (values #f (car rest) (cdr rest))      ; form.
                 (values (car rest) (cadr rest) (cddr rest)))

      ;; If we are doing the old, obsolete Posix-string syntax, map
      ;; the clause tests over to the new syntax.
      (let* ((clauses (if string-regexps?
                          (map (lambda (clause)
                                 (hack-clause-for-posix-string-syntax clause r c))
                               clauses)
                          clauses))

             ;; Some analysis: what have we got?
             ;; Range clauses, else clauses, line num tests,...
             (recnum-tests?             ; Do any of the clauses test the record
              (any (lambda (clause)     ; count? (I.e., any integer tests?)
                     (let ((test (car clause)))
                       (or (integer? test)
                           (and (range? clause)
                                (or (integer? (cadr clause))
                                    (integer? (caddr clause)))))))
                   clauses))

             ;; If any ELSE clauses, bind this to the var in which we
             ;; will keep the else state, otherwise #f.
             (else-var (and (any (lambda (clause)
                                   (c (car clause) %else))
                                 clauses)
                            (r 'else-state)))

             ;; We compile all of the *static* regexp patterns into regexp
             ;; data structures outside the AWK loop. So we need to
             ;; make a list of all the regexps that are used as tests.
             (patterns (apply append
                              (map (lambda (clause)
                                     (let ((test (car clause)))
                                       (cond ((sre-form? test r c) (list test))
                                             ((range? clause)
                                              (let ((t1 (cadr clause))
                                                    (t2 (caddr clause)))
                                                (append (if (sre-form? t1 r c)
                                                            (list t1)
                                                            '())
                                                         (if (sre-form? t2 r c)
                                                             (list t2)
                                                             '()))))
                                             (else '()))))
                                   clauses)))

             ;; Gratuitous optimisation: uniquify the patterns.
             (patterns (let recur ((pats patterns))
                         (if (pair? pats)
                             (let ((pat (car pats))
                                   (ans (recur (cdr pats))))
                               (if (member pat ans) ans (cons pat ans)))
                             '())))

             (pats-static? (map (lambda (sre)
                                  (static-regexp? (parse-sre sre r c)))
                                patterns))

             ;; An alist matching each pattern with the exp that refers
             ;; to it -- a var if it's static, a Scheme (RX ...) exp otw.
             (pats/refs (map (lambda (pat static?)
                               (cons pat
                                     (if static?
                                         (r (gensym "re."))
                                         `(,%rx ,pat))))
                             patterns pats-static?))

             ;; A LET-list binding the regexp vars to their
             ;; compiled static regexps.
             (regexp-inits (apply append
                                  (map (lambda (p/r static?)
                                         (if static?
                                             `((,(cdr p/r) (,%rx ,(car p/r))))
                                             '()))
                                       pats/refs
                                       pats-static?)))

             ;; Make a list of state vars for the range clauses.
             ;; For each range clause, we need a boolean var to track
             ;; whether or not the range is activated.
             (range-vars (all-trues (lambda (clause)
                                      (and (range? clause)
                                           (r (gensym "r."))))
                                    clauses))

             (svars (map car state-inits))      ; The user's state variables.

             ;; If the user didn't declare a record-counter var,
             ;; but he is testing line numbers (with integer test forms),
             ;; go ahead and generate a record-counter of our own.
             (rec-counter (or rec-counter
                              (and recnum-tests?
                                   (r (gensym "record-count.")))))

             ;; Generate the loop vars & their inits.
             ;; These are: the record counter, the range vars,
             ;; and the user's state vars.
             ;; All of these different sets are optional.
             (loop-vars (append (if rec-counter (list rec-counter) '())
                                range-vars
                                svars))
             (loop-var-init-values (append (if rec-counter '(0)  '())
                                           (map (lambda (x) #f) range-vars)
                                           (map cadr state-inits)))
             ;; A LET list initialising all the loop vars.
             (loop-var-init (map list loop-vars loop-var-init-values))

             ;; Build the clause that computes the loop's return value.
             ;; If the user gave an AFTER clause, use its body. Otherwise,
             ;; it's (values ,@svars).
             (after-clause? (lambda (clause) (c (car clause) %after)))
             (after-exp (let ((after-clauses (filter after-clause? clauses)))
                          (cond ((null? after-clauses)
                                 (mult-values svars r))
                                ((null? (cdr after-clauses))
                                 (blockify (cdar after-clauses) r c))
                                (else (error "Multiple AFTER clauses in awk body."
                                             after-clauses exp)))))


             (loop-body (awk-loop-body lp-var rec-var else-var
                                       rec-counter range-vars svars
                                       clauses pats/refs r c))

             ;; Variables that have to be updated per-iteration, as a LET list.
             ;; Note that we are careful not to increment the record counter
             ;; until after we've verified the new record isn't EOF.
             (per-iteration-updates
                  (append (if else-var `((,else-var #t)) '())   ; Else state.
                          (if rec-counter                       ; Record count.
                              `((,rec-counter (,%+ ,rec-counter 1)))
                              '())))

             (loop-body (if (pair? per-iteration-updates)
                            `(,%let ,per-iteration-updates
                               . ,(deblock loop-body r c))
                            loop-body)))

        `(,%let ((,reader (,%lambda () ,reader-exp))
                 . ,regexp-inits)
           (,%let ,lp-var ,loop-var-init
             ,(mv-let r c rec/field-vars `(,reader)
                `(,%if (,%eof-object? ,rec-var) ,after-exp
                       ,loop-body))))))))

;;; This maps a clause in the old, obsolete syntax over to a clause
;;; in the new, SRE syntax.
(define (hack-clause-for-posix-string-syntax clause r c)
  (let ((hack-simple-test (lambda (test)
                            (cond ((string? test)
                                   `(,(r 'posix-string) ,test))
                                  ((integer? test) test)
                                  (else `(,(r 'when) ,test)))))
        (test (car clause)))
    (cond ((range-keyword? test r c)
           `(,test ,(hack-simple-test (cadr clause))
                   ,(hack-simple-test (caddr clause))
                   . ,(cdddr clause)))

          ((or (c test (r 'else))
               (c test (r 'after)))
           clause)

          (else `(,(hack-simple-test test) . ,(cdr clause))))))


;;; Expand into the body of the awk loop -- the code that tests & executes
;;; each clause, and then jumps to the top of the loop.

(define (awk-loop-body lp-var rec-var else-var rec-counter
                       range-vars svars clauses pats/refs r c)
  (let ((clause-vars (if else-var (cons else-var svars) svars))
        (loop-vars (append (if rec-counter (list rec-counter) '())
                           range-vars
                           svars))
        (range-clause? (lambda (clause) (range-keyword? (car clause) r c)))

        (%after (r 'after))
        (%else  (r 'else)))

    (let expand ((clauses clauses) (range-vars range-vars))
      (if (pair? clauses)
          (let* ((clause (car clauses))
                 (test   (car clause)))
            (cond ((range-keyword? test r c)
                   (let ((tail (expand (cdr clauses) (cdr range-vars))))
                     (expand-range-clause clause tail (car range-vars)
                                          rec-var else-var rec-counter svars
                                          pats/refs
                                          r c)))

                  ((c test %after)      ; An AFTER clause. Skip it.
                   (expand (cdr clauses) range-vars))

                  ((c test %else)       ; An ELSE clause.
                   (let ((tail (expand (cdr clauses) range-vars)))
                     (expand-else-clause clause tail else-var svars r c)))

                  (else                 ; A simple clause.
                   (let ((tail (expand (cdr clauses) range-vars)))
                     (expand-simple-clause clause tail
                                           rec-var else-var rec-counter svars
                                           pats/refs r c)))))

          ;; No clauses -- just jump to top of loop.
          `(,lp-var . ,loop-vars)))))


;;; Make a Scheme expression out of a test form.
;;; Integer i           =>  (= i <record-counter>)
;;; SRE s               =>  (regexp-search <re> <record>)
;;; (when e)            =>  e
;;; Expression e        =>  e
;;;
;;; If FOR-VALUE? is true, then we do regexp searches with REGEXP-SEARCH,
;;; otherwise, we use the cheaper REGEXP-SEARCH?.

(define (->simple-clause-test test-form for-value? rec-var rec-counter pats/refs r c)
  (cond ((integer? test-form) `(,(r '=) ,rec-counter ,test-form))

        ((sre-form? test-form r c)
         `(,(r (if for-value? 'regexp-search 'regexp-search?))
           ,(cdr (assoc test-form pats/refs))
           ,rec-var))

        ((and (pair? test-form)
              (c (r 'when) (car test-form)))
         (if (= 2 (length test-form)) (cadr test-form)
             (error "Illegal WHEN test in AWK" test-form)))

        (else test-form)))


(define (expand-simple-clause clause tail
                              rec-var else-var rec-counter svars
                              pats/refs r c)
  (let* ((%let          (r 'let))
         (%arrow        (r '=>))
         (%long-arrow   (r '==>))
         (%if           (r 'if))
         (%mss          (r 'match:substring))

         (test (car clause))
         (mktest (lambda (for-value?)
                   (->simple-clause-test test for-value? rec-var
                                         rec-counter pats/refs r c)))

         ;; Is clause of the form (test => proc)
         (arrow? (and (= 3 (length clause))
                      (c (cadr clause) %arrow)))

         ;; How about (test ==> (var ...) body ...)?
         (long-arrow? (and (< 3 (length clause))
                           (c (cadr clause) %long-arrow)))

         (null-clause-list (null-clause-action else-var svars r))

         ;; The core form conditionally executes the body.
         ;; It returns the new else var and the new state vars, if any.
         (core (cond (arrow?
                      (let* ((tv (r 'tval))                ; APP is the actual
                             (app `(,(caddr clause) ,tv))  ; body: (proc tv).
                             (test (mktest #t)))
                        `(,%let ((,tv ,test))
                           (,%if ,tv
                                 ,(clause-action (list app) else-var svars r c)
                                 . ,null-clause-list))))

                     (long-arrow?
                      (let* ((tv (r 'tval))
                             (test (mktest #t))
                             (bindings ; List of LET bindings for submatches.
                              (let lp ((i 0)
                                       (vars (caddr clause))
                                       (bindings '()))
                                (if (pair? vars)
                                    (let ((var (car vars)))
                                      (lp (+ i 1) (cdr vars)
                                          (if var
                                              `((,var (,%mss ,tv ,i)) . ,bindings)
                                              bindings))) ; #F = "don't-care"
                                    bindings))))

                        `(,%let ((,tv ,test))
                           (,%if ,tv
                                 (,%let ,bindings ; Bind submatches.
                                   . ,(deblock (clause-action (cdddr clause)
                                                              else-var svars
                                                              r c)
                                               r c))
                                 . ,null-clause-list))))

                     (else
                      `(,%if ,(mktest #f) ,(clause-action (cdr clause)
                                                          else-var svars r c)
                             . ,null-clause-list))))

         (loop-vars (if else-var (cons else-var svars) svars)))

    ;; Do the core computation, update the iteration vars,
    ;; and then do the tail in the scope of the updated environment.
    (core-then-tail loop-vars core tail r c)))

(define (core-then-tail loop-vars core tail r c)
  (mv-let r c loop-vars core tail))

(define (expand-range-clause clause tail range-var
                             rec-var else-var rec-counter svars
                             pats/refs r c)
  (let* ((start-test (cadr clause))
         (stop-test (caddr clause))
         (body (cdddr clause))

         (%receive (r 'receive))
         (%if      (r 'if))
         (%lambda  (r 'lambda))

         (keyword (car clause)) ; range or :range or range: or :range:
         (tester (r (cond ((c keyword (r 'range))   'next-range)
                          ((c keyword (r ':range))  'next-:range)
                          ((c keyword (r 'range:))  'next-range:)
                          ((c keyword (r ':range:)) 'next-:range:)
                          (else (error "Unrecognised range keyword!" clause)))))

         ;; Convert the start and stop test forms to code.
         (start-test (->simple-clause-test start-test #f rec-var
                                           rec-counter pats/refs r c))
         (stop-test  (->simple-clause-test stop-test #f rec-var
                                           rec-counter pats/refs r c))

         (start-thunk `(,%lambda () ,start-test))       ; ...and thunkate them.
         (stop-thunk  `(,%lambda () ,stop-test))

         (loop-vars (if else-var (cons else-var svars) svars))
         (this-rec (r 'this-record?))

         (core `(,%if ,this-rec
                      ,(clause-action body else-var svars r c)
                      . ,(null-clause-action else-var svars r))))

    `(,%receive (,this-rec ,range-var)
                (,tester ,start-thunk ,stop-thunk ,range-var)
       ,(core-then-tail loop-vars core tail r c))))


(define (expand-else-clause clause tail else-var svars r c)
  (let* ((body (cdr clause))
         (tail-exps (deblock tail r c))

         (%if      (r 'if))
         (%let     (r 'let))

         ;; We are hard-wiring the else var to #t after this, so the core
         ;; expression doesn't need to return it -- just the new values
         ;; of the user's state vars.
         (core `(,%if ,else-var
                      ,(clause-action body #f svars r c)
                      . ,(sloppy-mult-values svars r))))

    (mv-let r c svars core `(,%let ((,else-var #t)) . ,tail-exps))))


;;; BODY is a list of expressions from a loop clause. We want to evaluate it,
;;; under some conditions.
;;; - The body evaluates to multiple values, one for each state variable.
;;;   However, if there are no state variables, we want to *ignore* the
;;;   values produced by the body, and explicitly return 0 values,
;;;   not blow up if the body should happen not to return exactly zero values.
;;; - If we are tracking an else-variable, then the body firing will turn
;;;   it off by returning its new #f value.

(define (clause-action body else-var svars r c)
  (let ((%values  (r 'values))
        (%receive (r 'receive)))

    (blockify (if (pair? svars)

                  (if else-var
                      (if (pair? (cdr svars)) ; state vars and an else var.
                          `((,%receive ,svars ,(blockify body r c)
                              (,%values #f . ,svars)))
                          `((,%values #f ,(blockify body r c)))) ; Gratuitous.
                      body)                     ; State vars, but no else var.

                  ;; No state vars -- ignore value computed by BODY forms.
                  `(,@body . ,(if else-var '(#f) `())))
              r c)))


;;; The clause didn't execute. Return the svars unchanged, and also
;;; return the current else-value if we are tracking one. We return
;;; a 0 or 1 element expression list -- if no values are being expected
;;; this returns the empty list.

(define (null-clause-action else-var svars r)
  (sloppy-mult-values (if else-var (cons else-var svars) svars)
                      r))



;;; These procs are for handling RANGE clauses.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First return value tells whether this line is active;
;;; next value tells whether region is active after this line.
;;;
;;; (:range  0 4) = 0 1 2 3     This is the most useful one.
;;; (range:  0 4) = 1 2 3 4
;;; (range   0 4) = 1 2 3
;;; (:range: 0 4) = 0 1 2 3 4

;;; If these were inlined and the test thunks substituted, it would
;;; be acceptably efficient. But who writes Scheme compilers that good
;;; in the 90's?

(define (next-:range start-test stop-test state)
  (let ((new-state (if state
                       (or (not (stop-test))            ; Stop,
                           (start-test))                ;   but restart.

                       (and (start-test)                ; Start,
                            (not (stop-test))))))       ;   but stop, too.
    (values new-state new-state)))

(define (next-range: start-test stop-test state)
  (values state
          (if state
              (or (not (stop-test))             ; Stop,
                  (start-test))                 ;   but restart.
              (and (start-test)                 ; Start,
                   (not (stop-test))))))        ;   but stop, too.

(define (next-range start-test stop-test state)
  (if state
      (let ((not-stop (not (stop-test))))
        (values not-stop
                (or not-stop                    ; Stop,
                    (start-test))))             ;   but restart.
      (values #f
              (and (start-test)                 ; Start,
                   (not (stop-test))))))        ;   but stop, too.

(define (next-:range: start-test stop-test state)
  (if state
      (values #t
              (or (not (stop-test))             ; Stop
                  (start-test)))                ;   but restart.

      (let ((start? (start-test)))
        (values start?
                (and start?                     ; Start,
                     (not (stop-test)))))))     ;   but stop, too.
