; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1993-1999 by Richard Kelsey and Jonathan Rees. See file COPYING.


; A little Scheme reader.

; Nonstandard things used:
;  Ascii stuff: char->ascii, ascii->char, ascii-whitespaces, ascii-limit
;    (for dispatch table; portable definitions in alt/ascii.scm)
;  reverse-list->string  -- ok to define as follows:
;    (define (reverse-list->string l n)
;      (list->string (reverse l)))
;  make-immutable! -- ok to define as follows:
;    (define (make-immutable! x) x)
;  signal (only for use by reading-error; easily excised)

(define-structure scsh-reader (export scsh-read define-sharp-macro reading-error)
  (open scheme
        number-i/o
        i/o-internal    ;input-port-option
        ascii           ;for dispatch table
        unicode
        signals         ;warn, signal-condition, make-condition
        conditions      ;define-condition-type
        primitives      ;make-immutable!
        silly           ;reverse-list->string
        byte-vectors
        let-opt
        receiving
        (subset srfi-13 (string-every))
        (subset srfi-14 (char-set x->char-set char-set-contains? char-set:whitespace))
        ports)
  (begin

(define preferred-case (lambda (x) x))

(define (script-skip c port)
  (read-char port)
    (let lp ((state 0))
      (let ((advance-if (lambda (look-for)
                          (let ((c (read-char port)))
                            (if (eof-object? c)
                                (reading-error  port
                         "EOF inside block comment -- #! missing a closing !#")
                                (lp (cond ((char=? c look-for) (+ state 1))
                                          ((char=? c #\newline) 1)
                                          ((char=? c cr) state)
                                          (else 0))))))))
        (case state
          ((0) (advance-if #\newline))
          ((1) (advance-if #\!))        ; Found \n
          ((2) (advance-if #\#))        ; Found \n!
          ((3) (advance-if #\newline))  ; Found \n!#
          ((4) (scsh-read port))
          (else
           (reading-error port "case other"))))))       ; Found \n!#\n -- done.
;         was sub-read ^


(define (multi-line-comment-skip c port)
  (read-char port)
  (let lp ((state 0) (nested? #f))
    (let* ((advance-one-of-two
            (lambda (look-for1 state1 look-for2 state2 nested?)
                          (let ((c (read-char port)))
                            (if (eof-object? c)
                                (error
                         "EOF inside block comment -- #| missing a closing |#")
                                (lp (cond ((char=? c look-for1) state1)
                                          ((char=? c look-for2) state2)
                                          (else 0)) nested?)))))
           (advance-if (lambda (look-for state nested?)
                         (advance-one-of-two look-for state
                                             look-for state
                                             nested?))))
      (case state
        ((0) (advance-one-of-two #\| 1 #\# 5 nested?))
        ((1) (advance-if #\# 2 nested?))
        ((2) (if nested? #f (sub-read port)))
        ((5) (advance-if #\| 6 nested?))
        ((6) (lp 0 #t) (lp 0 nested?))))))


; scsh stop

(define (scsh-read . port-option)
  (let ((port (input-port-option port-option)))
    (let loop ()
      (let ((form (sub-read port)))
        (cond ((not (reader-token? form)) form)
              ((eq? form close-paren)
               ;; Too many right parens.
               (warn "discarding extraneous right parenthesis")
               (loop))
              (else
               (reading-error port (cdr form))))))))

(define (sub-read-carefully port)
  (let ((form (sub-read port)))
    (cond ((eof-object? form)
           (reading-error port "unexpected end of file"))
          ((reader-token? form) (reading-error port (cdr form)))
          (else form))))

(define reader-token-marker (list 'reader-token))
(define (make-reader-token message) (cons reader-token-marker message))
(define (reader-token? form)
  (and (pair? form) (eq? (car form) reader-token-marker)))

(define close-paren (make-reader-token "unexpected right parenthesis"))
(define dot         (make-reader-token "unexpected \" . \""))


; Main dispatch

(define (sub-read port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        c
        ((vector-ref read-dispatch-vector (char->ascii c))
         c port))))

(define read-dispatch-vector
  (make-vector ascii-limit
               (lambda (c port)
                 (reading-error port "illegal character read" c))))

(define read-terminating?-vector
  (make-vector ascii-limit #t))

(define (set-standard-syntax! char terminating? reader)
  (vector-set! read-dispatch-vector     (char->ascii char) reader)
  (vector-set! read-terminating?-vector (char->ascii char) terminating?))

(let ((sub-read-whitespace
       (lambda (c port)
         c                              ;ignored
         (sub-read port))))
  (for-each (lambda (c)
              (vector-set! read-dispatch-vector c sub-read-whitespace))
            ascii-whitespaces))

(let ((sub-read-constituent
       (lambda (c port)
         (parse-token (sub-read-token c port) port))))
  (for-each (lambda (c)
              (set-standard-syntax! c #f sub-read-constituent))
            (string->list
             (string-append "!$%&*+-./0123456789:<=>?@^_~ABCDEFGHIJKLM"
                            "NOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))))

; Usual read macros

(define (set-standard-read-macro! c terminating? proc)
  (set-standard-syntax! c terminating? proc))

(define (sub-read-list c port)
  (let ((form (sub-read port)))
    (if (eq? form dot)
        (reading-error port
                       "missing car -- ( immediately followed by .")
        (let recur ((form form))
          (cond ((eof-object? form)
                 (reading-error port
                                "end of file inside list -- unbalanced parentheses"))
                ((eq? form close-paren) '())
                ((eq? form dot)
                 (let* ((last-form (sub-read-carefully port))
                        (another-form (sub-read port)))
                   (if (eq? another-form close-paren)
                       last-form
                       (reading-error port
                                      "randomness after form after dot"
                                      another-form))))
                (else
                 (cons form (recur (sub-read port)))))))))

(set-standard-read-macro! #\( #t sub-read-list)

(set-standard-read-macro! #\) #t
  (lambda (c port)
    c port
    close-paren))

(set-standard-read-macro! #\' #t
  (lambda (c port)
    c
    (list 'quote (sub-read-carefully port))))

(set-standard-read-macro! #\` #t
  (lambda (c port)
    c
    (list 'quasiquote (sub-read-carefully port))))

(set-standard-read-macro! #\, #t
  (lambda (c port)
    c
    (let* ((next (peek-char port))
           ;; DO NOT beta-reduce!
           (keyword (cond ((eof-object? next)
                           (reading-error port "end of file after ,"))
                          ((char=? next #\@)
                           (read-char port)
                           'unquote-splicing)
                          (else 'unquote))))
      (list keyword
            (sub-read-carefully port)))))

;(set-standard-read-macro! #\" #t
;  (lambda (c port)
;    c ;ignored
;    (let loop ((l '()) (i 0))
;      (let ((c (read-char port)))
;        (cond ((eof-object? c)
;               (reading-error port "end of file within a string"))
;              ((char=? c #\\)
;               (let ((c (read-char port)))
;                (cond ((eof-object? c)
;                       (reading-error port "end of file within a string"))
;                      ((or (char=? c #\\) (char=? c #\"))
;                       (loop (cons c l) (+ i 1)))
;                      (else
;                       (reading-error port
;                                      "invalid escaped character in string"
;                                      c)))))
;              ((char=? c #\")
;              (reverse-list->string l i))
;              (else
;              (loop (cons c l) (+ i 1))))))))

(set-standard-read-macro! #\; #t
  (lambda (c port)
    c ;ignored
    (gobble-line port)
    (sub-read port)))

(define (gobble-line port)
  (let loop ()
    (let ((c (read-char port)))
      (cond ((eof-object? c) c)
            ((char=? c #\newline) #f)
            (else (loop))))))

(define *sharp-macros* '())

(define (define-sharp-macro c proc)
  (set! *sharp-macros* (cons (cons c proc) *sharp-macros*)))

(set-standard-read-macro! #\# #f
  (lambda (c port)
    c ;ignored
    (let* ((c (peek-char port))
           (c (if (eof-object? c)
                  (reading-error port "end of file after #")
                  (char-downcase c)))
           (probe (assq c *sharp-macros*)))
      (if probe
          ((cdr probe) c port)
          (reading-error port "unknown # syntax" c)))))

(define-sharp-macro #\f
  (lambda (c port) (read-char port) #f))

(define-sharp-macro #\t
  (lambda (c port) (read-char port) #t))

; Don't use non-R5RS char literals to avoid bootstrap circularities

(define *nul* (scalar-value->char 0))
(define *alarm* (scalar-value->char 7))
(define *backspace* (scalar-value->char 8))
(define *tab* (scalar-value->char 9))
(define *linefeed* (scalar-value->char 10))
(define *vtab* (scalar-value->char 11))
(define *page* (scalar-value->char 12))
(define *return* (scalar-value->char 13))
(define *escape* (scalar-value->char 27))
(define *rubout* (scalar-value->char 127))

(define *char-name-table*
  (list
   (cons 'space #\space)
   (cons 'newline #\newline)
   (cons 'nul *nul*)
   (cons 'alarm *alarm*)
   (cons 'backspace *backspace*)
   (cons 'tab *tab*)
   (cons 'linefeed *linefeed*)
   (cons 'vtab *vtab*)
   (cons 'page *page*)
   (cons 'return *return*)
   (cons 'escape *escape*)
   (cons 'rubout *rubout*)
   (cons 'delete *rubout*)
   (cons 'del *rubout*)))

(define-sharp-macro #\\
  (lambda (c port)
    (read-char port)
    (let ((c (peek-char port)))
      (cond ((eof-object? c)
             (reading-error port "end of file after #\\"))
            ((char-alphabetic? c)
             (let ((name (sub-read-carefully port)))
               (cond ((= (string-length (symbol->string name)) 1)
                      c)
                     ((assq name *char-name-table*)
                      => cdr)
                     (else
                      (reading-error port "unknown #\\ name" name)))))
            (else
             (read-char port)
             c)))))

(define-sharp-macro #\(
  (lambda (c port)
    (read-char port)
    (list->vector (sub-read-list c port))))

(let ((number-sharp-macro
       (lambda (c port)
         (let ((string (sub-read-token #\# port)))
           (or (string->number string)
               (reading-error port "unsupported number syntax" string))))))
  (for-each (lambda (c)
              (define-sharp-macro c number-sharp-macro))
            '(#\b #\o #\d #\x #\i #\e)))

(define-sharp-macro #\! script-skip)

(define-sharp-macro #\| multi-line-comment-skip)

(define (read-here-string port)
  (make-immutable!
   (let ((delim-char (read-char port)))
     (cond ((eof-object? delim-char)
            (reading-error port "EOF while reading #< here-string delimiter."))

           ((char=? delim-char #\<)	; It's a #<<EOF long here-string.
            (read-line-delimited-here-string port))

           ;; It's short: #<|Here's the string.|
           (else (receive (str terminator)
                          (read-delimited (char-set delim-char) port 'split)
                          (if (eof-object? terminator)
                              (reading-error port "EOF while reading #< here-string.")
                              str)))))))

(define (read-line-delimited-here-string port)
  ;; First, read in the delimiter line.
  (let ((delim (read-line port)))
    (cond ((eof-object? delim)
           (reading-error port
                          "EOF while reading #<< here-string delimiter line."))
          ((= 0 (string-length delim))
           (reading-error port "#<< here-string empty delimiter line"))

          (else
           (let lp ((text '()))
             (receive (line terminator) (read-line port 'split)
                      (cond ((equal? line delim)
                             ;; We're done. The last newline must be stripped
                             ;; off (with the CDR application).
                             (if (pair? text)
                                 (apply string-append (reverse (cdr text)))
                                 ""))

                            ((eof-object? terminator)
                             (reading-error port "EOF while reading #<< here-string."))

                            (else (lp `(,(string terminator) ,line . ,text))))))))))

(define-sharp-macro #\<
  (lambda (c port)
    (read-char port)		; Snarf the first < char.
    (read-here-string port)))


; Tokens

(define (sub-read-token c port)
  (let loop ((l (list (preferred-case c))) (n 1))
    (let ((c (peek-char port)))
      (cond ((or (eof-object? c)
                 (vector-ref read-terminating?-vector (char->ascii c)))
             (reverse-list->string l n))
            (else
             (read-char port)
             (loop (cons (preferred-case c) l)
                   (+ n 1)))))))

;(define (parse-token string port)
;  (if (let ((c (string-ref string 0)))
;       (or (char-numeric? c) (char=? c #\+) (char=? c #\-) (char=? c #\.)))
;      (cond ((string->number string))
;           ((member string strange-symbol-names)
;            (string->symbol (make-immutable! string)))
;           ((string=? string ".")
;            dot)
;           (else
;            (reading-error port "unsupported number syntax" string)))
;      (string->symbol (make-immutable! string))))

; scsh start
(define (parse-token string port)
  (if (let ((c (string-ref string 0)))
        (or (char-numeric? c) (char=? c #\+) (char=? c #\-) (char=? c #\.)))
      (cond ((string->number string))
            ((string=? string ".") dot)
            (else (string->symbol (make-immutable! string))))
      (string->symbol (make-immutable! string))))

(set-standard-syntax! #\| #f
                      (lambda (c port)
                        (parse-token (sub-read-token c port) port)))



(define bel (ascii->char 7))
(define bs  (ascii->char  8))
(define ff  (ascii->char 12))
(define cr  (ascii->char 13))
(define ht  (ascii->char  9))
(define vt  (ascii->char 11))

;;; Full ANSI C strings:
;;; - read as themselves: \\ \? \" \'
;;; - control chars:
;;;   \a alert (bell -- ^g)
;;;   \b backspace (^h)
;;;   \f form feed (^l)
;;;   \n newline (^j)
;;;   \r carriage return (^m)
;;;   \t tab (^i)
;;;   \v vertical tab (^k)
;;; - octal escapes \nnn
;;; - hex escapes \xnn

;;; Is this the elegant thing to do? Too much might make it hard to shift
;;; to Unicode implementations. How about \^g for embedding control chars?
;;; And I haven't done anything about chars (as opposed to strings).

(set-standard-read-macro! #\" #t
  (lambda (c port)
    c ;ignored
    (let* ((readc (lambda ()
                    (let ((c (read-char port)))
                      (if (eof-object? c)
                          (reading-error port "end of file within a string")
                          c))))
           (read-digit (lambda (base base-name)
                         (let* ((c (readc))
                                (d (- (char->ascii c) (char->ascii #\0))))
                           (if (and (<= 0 d) (< d base)) d
                               (reading-error port
                                              (string-append "invalid "
                                                             base-name
                                                             " code in string.")
                                              d))))))

      (let loop ((l '()) (i 0))
        (let ((c (readc)))
          (cond ((char=? c #\\)
                 (let* ((c (readc))
                        (rc (case c
                              ((#\\ #\" #\? #\') c)
                              ((#\a) bel)
                              ((#\b) bs)
                              ((#\f) ff)
                              ((#\n) #\newline)
                              ((#\r) cr)
                              ((#\t) ht)
                              ((#\v) vt)
                              ((#\0 #\1 #\2 #\3)
                               (let* ((d1 (- (char->ascii c) (char->ascii #\0)))
                                      (d2 (read-digit 8 "octal"))
                                      (d3 (read-digit 8 "octal")))
                                 (ascii->char (+ (* 64 d1) (+ (* 8 d2) d3)))))
                              ((#\x)
                               (let ((d1 (read-digit 16 "hex"))
                                     (d2 (read-digit 16 "hex")))
                                 (ascii->char (+ (* 16 d1) d2))))
                              (else
                               (reading-error port
                                              "invalid escapedcharacter in string"
                                              c)))))
                   (loop (cons rc l) (+ i 1))))
                ((char=? c #\")
                 (reverse-list->string l i))
                (else
                 (loop (cons c l) (+ i 1)))))))))

;scsh stop

(define strange-symbol-names
  '("+" "-" "..."
        "1+" "-1+"  ;Only for S&ICP support
        "->"        ;Only for JAR's thesis
        ))

;--- This loses because the compiler won't in-line it.  Hacked by hand
; because it is in READ's inner loop.
;(define preferred-case
;  (if (char=? (string-ref (symbol->string 't) 0) #\T)
;      char-upcase
;      char-downcase))

(define p-c-v (make-string ascii-limit #\0))

(let ((p-c (if (char=? (string-ref (symbol->string 't) 0) #\T)
               char-upcase
               char-downcase)))
  (do ((i 0 (+ i 1)))
      ((>= i ascii-limit))
    (string-set! p-c-v i (p-c (ascii->char i)))))

;(define (preferred-case c)
;  (string-ref p-c-v (char->ascii c)))

; Reader errors

(define (reading-error port message . irritants)
  (apply signal 'read-error message
         (append irritants (list port))))

(define (read-delimited delims . args)
  (let-optionals args ((port         (current-input-port))
                       (delim-action 'trim))
        (let ((substr (lambda (s end)           ; Smart substring.
                        (if (= end (string-length s)) s
                            (substring s 0 end))))
              (delims (x->char-set delims))
              (gobble? (not (eq? delim-action 'peek))))
      ;; BUFLEN is total amount of buffer space allocated to date.
      (let lp ((strs '()) (buflen 80) (buf (make-string 80)))
        (receive (terminator num-read)
                 (%read-delimited! delims buf gobble? port)
          (if terminator

              ;; We are done. NUM-READ is either a read count or EOF.
              (let ((retval (if (and (zero? num-read)
                                     (eof-object? terminator)
                                     (null? strs))
                                terminator              ; EOF -- got nothing.

                                ;; Got something. Stick all the strings
                                ;; together, plus the terminator if the
                                ;; client said 'CONCAT.
                                (let ((s (substr buf num-read)))
                                  (cond ((and (eq? delim-action 'concat)
                                              (char? terminator))
                                         (apply string-append
                                                (reverse `(,(string terminator)
                                                           ,s . ,strs))))

                                        ((null? strs) s)    ; Gratuitous opt.
                                        (else (apply string-append
                                                     (reverse (cons s strs)))))))))
                (if (eq? delim-action 'split)
                    (values retval terminator)
                    retval))

              ;; We are not done. Loop and read in some more.
              (lp (cons buf strs)
                  (+ buflen buflen)
                  (make-string buflen))))))))


;;; (read-delimited! delims buf [port delim-action start end])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns:
;;; - EOF if at end of file, and a non-zero read was requested.
;;; - Integer j if that many chars read into BUF.
;;; - #f if the buffer was filled w/o finding a delimiter.
;;;
;;; DELIM-ACTION determines what to do with the terminating delimiter;
;;; it is as in READ-DELIMITED.
;;;
;;; In determining the return value, there is an ambiguous case: when the
;;; buffer is full, *and* the following char is a delimiter char or EOF.
;;; Ties are broken favoring termination over #f -- after filling the buffer,
;;; READ-DELIMITED! won't return #f until it has peeked one past the end
;;; of the buffer to ensure the next char doesn't terminate input (or is EOF).
;;; However, this rule is relaxed with delim-action = CONCAT -- if the buffer
;;; is full, READ-DELIMITED! won't wait around trying to peek at the following
;;; char to determine whether or not it is a delimiter char, since it doesn't
;;; have space to store the character anyway. It simply immediately returns #f;
;;; a following read can pick up the delimiter char.

(define (read-delimited! delims buf . args) ; [port delim-action start end]
  (let-optionals args ((port         (current-input-port))
                       (delim-action 'trim)
                       (start        0)
                       (end          (string-length buf)))
    (receive (terminator num-read)
             (%read-delimited! delims buf
                               (not (eq? delim-action 'peek)) ;Gobble delim?
                               port
                               start
                               (if (eq? delim-action 'concat)
                                   (- end 1) ; Room for terminator.
                                   end))

      (if terminator    ; Check for buffer overflow.
          (let ((retval (if (and (zero? num-read)
                                 (eof-object? terminator))
                            terminator  ; EOF -- got nothing.
                            num-read))) ; Got something.

            (case delim-action
              ((peek trim)      retval)
              ((split)  (values retval terminator))
              ((concat) (cond ((char? terminator)
                               (string-set! buf (+ start num-read) terminator)
                               (+ num-read 1))
                              (else retval)))))

          ;; Buffer overflow.
          (case delim-action
            ((peek trim) #f)
            ((split)     (values #f #f))
            ((concat)    (let ((last (read-char port)))
                           (if (char? last)
                               (string-set! buf (+ start num-read) last))
                           (and (or (eof-object? last)
                                    (char-set-contains? (x->char-set delims)
                                                        last))
                                (+ num-read 1)))))))))


;;; (%read-delimited! delims buf gobble? [port start end])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This low-level routine uses a different interface. It returns two values:
;;; - TERMINATOR: A value describing why the read was terminated:
;;;   + character or eof-object => read terminated by this value;
;;;   + #f                      => filled buffer w/o terminating read.
;;; - NUM-READ: Number of chars read into buf.
;;;
;;; Note:
;;; - Invariant: TERMINATOR = #f  =>  NUM-READ = END - START.
;;; - Invariant: TERMINATOR = eof-object and NUM-READ = 0 => at EOF.
;;; - When determining the TERMINATOR return value, ties are broken
;;;   favoring character or the eof-object over #f. That is, if the buffer
;;;   fills up, %READ-DELIMITED! will peek at one more character from the
;;;   input stream to determine if it terminates the input. If so, that
;;;   is returned, not #f.
;;;
;;; If GOBBLE? is true, then a terminator character is removed from
;;; the input stream. Otherwise, it is left in place for a following input
;;; operation.


(define (port-buffer-read-delimited delims buf gobble? port start end)
  (let ((the-port-limit (port-limit port)))
    (let lp ((i start) (lp-port-index (port-index port)))
      (cond ((port-pending-eof? port)
             (set-port-index! port lp-port-index)
             (values (eof-object) (- i start)))
            ((>= i end)
             (set-port-index! port lp-port-index)
             (values #f (- i start)))
            ((< lp-port-index the-port-limit)
             (let ((the-read-char
                    (scalar-value->char (byte-vector-ref
                                         (port-buffer port) lp-port-index))))
               (if (char-set-contains? delims the-read-char)
                   (begin
                     (if gobble?
                         (set-port-index! port (+ lp-port-index 1))
                         (set-port-index! port lp-port-index))
                     (values the-read-char (- i start)))
                   (begin
                     (string-set! buf i the-read-char)
                     (lp (+ i 1) (+ lp-port-index 1))))))
            (else  (set-port-index! port 0)
                   (set-port-limit! port 0)
                   (values 'port-buffer-exhausted (- i start)))))))



(define (%read-delimited! delims buf gobble? . args)
  (let-optionals args ((port  (current-input-port))
                       (start 0)
                       (end   (string-length buf)))
    (if (immutable? buf)
        (error "Immutable buffer argument to %read-delimited!" buf))
    (let ((delims (x->char-set delims)))
      (let lp ((start start) (total 0))
        (receive (terminator num-read)
            (port-buffer-read-delimited delims buf gobble? port start end)
          (if (not (eq? terminator 'port-buffer-exhausted))
              (values terminator (+ num-read total))
              (begin (peek-char port)   ; kludge to fill the buffer
                     (lp (+ start num-read) (+ total num-read)))))))))



; overwrites port-index :-(
(define (push-back port char)
  (byte-vector-set! (port-buffer port) (port-index port) (char->scalar-value char))
  (set-port-limit! port (+ (port-limit port) 1)))


(define (skip-char-set skip-chars . maybe-port)
  (let ((port (:optional maybe-port (current-input-port)))
        (cset (x->char-set skip-chars)))

    (let lp ((total 0))
      (receive (succ num-read) (buffer-skip-char-set cset port)
        (if (not succ)
            (+ total num-read)          ; eof
            (begin (peek-char port)     ; kludge to fill the buffer
                   (lp  (+ total num-read))))))))

(define (buffer-skip-char-set cset port)
  (let ((the-port-limit (port-limit port)))
    (let lp ((lp-port-index (port-index port)) (i 0))
      (cond ((port-pending-eof? port)
             (set-port-index! port lp-port-index)
             (values #f i))
            ((< lp-port-index the-port-limit)
             (let ((the-read-char
                    (scalar-value->char (byte-vector-ref
                                         (port-buffer port) lp-port-index))))
               (cond ((char-set-contains? cset the-read-char)
                      (lp (+ lp-port-index 1) (+ i 1)))
                     (else
                      (set-port-index! port lp-port-index)
                      (values #f i)))))
            (else (set-port-index! port 0)
                  (set-port-limit! port 0)
                  (values 'port-buffer-exhausted i))))))

;;; (read-line [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read in a line of data. Input is terminated by either a newline or EOF.
;;; The newline is trimmed from the string by default.

(define charset:newline (char-set #\newline))

(define (read-line . rest) (apply read-delimited charset:newline rest))


;;; (read-paragraph [port handle-delim])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-paragraph . args)
  (let-optionals args ((port         (current-input-port))
                       (handle-delim 'trim))
    ;; First, skip all blank lines.
    (let lp ()
      (let ((line (read-line port 'concat)))
        (cond ((eof-object? line)
               (if (eq? handle-delim 'split) (values line line) line))

              ((string-every char-set:whitespace line) (lp))

              ;; Then, read in non-blank lines.
              (else
               (let lp ((lines (list line)))
                 (let ((line (read-line port 'concat)))
                   (if (and (string? line)
                            (not (string-every char-set:whitespace line)))

                       (lp (cons line lines))

                       ;; Return the paragraph
                       (let ((x->str (lambda (lns) (apply string-append (reverse lns)))))
                         (case handle-delim
                           ((trim) (x->str lines))

                           ((concat)
                            (x->str (if (eof-object? line) lines (cons line lines))))

                           ((split)
                            (values (x->str lines) line))

                           (else (error "Illegal HANDLE-DELIM parameter to READ-PARAGRAPH")))))))))))))
))
