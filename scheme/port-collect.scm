;;; Read characters from PORT until EOF, collect into a string.

(define (port->string port)
  (reduce ((input* char port read-char))
          ((sc (make-string-collector)))
    (collect-char! sc char)
    (string-collector->string sc)))

;;; Read items from PORT with READER until EOF. Collect items into a list.

(define (port->list reader port)
  (let lp ((ans '()))
    (let ((x (reader port)))
      (if (eof-object? x) (reverse! ans)
          (lp (cons x ans))))))

(define (port->sexp-list port)
  (port->list read port))

(define (port->string-list port)
  (port->list read-line port))

(define (port-fold port reader op . seeds)
  (letrec ((fold (lambda seeds
                     (let ((x (reader port)))
                       (if (eof-object? x) (apply values seeds)
                           (call-with-values (lambda () (apply op x seeds))
                                             fold))))))
    (apply fold seeds)))

(define reduce-port
  (deprecated-proc port-fold 'reduce-port "Use port-fold instead."))

;;; Not defined:
;;; (field-reader field-delims record-delims)
;;; Returns a reader that reads strings delimited by 1 or more chars from
;;; the string FIELD-DELIMS. These strings are collected in a list until
;;; eof or until 1 or more chars from RECORD-DELIMS are read. Then the
;;; accumulated list of strings is returned. For example, if we want
;;; a procedure that reads one line of input, splitting it into
;;; whitespace-delimited strings, we can use
;;;     (field-reader " \t" "\n")
;;; for a reader.


;; Loop until EOF reading characters or strings and writing (FILTER char)
;; or (FILTER string). Useful as an arg to FORK or FORK/PIPE.

(define (make-char-port-filter filter)
  (lambda ()
    (let lp ()
      (let ((c (read-char)))
        (if (not (eof-object? c))
            (begin (write-char (filter c))
                   (lp)))))))

(define (port->string/limit port limit)
  (iterate loop ((input* char port read-char)
                 (count* len 0))
                ((chars (list)))
    (let ((chars (cons char chars)))
      (if (>= len (- limit 1))
        (list->string (reverse chars))
        (loop chars)))
    (and (pair? chars) (list->string (reverse chars)))))

(define (make-string-port-filter filter . maybe-buflen)
  (let ((buflen (:optional maybe-buflen 1024)))
    (lambda ()
      (let lp ()
        (cond ((port->string/limit (current-input-port) buflen) =>
               (lambda (string)
                 (display (filter string))
                 (lp))))))))
