;;; Read characters from PORT until EOF, collect into a string.

(define (port->string port)
  (let ((sc (make-string-collector)))
    (letrec ((lp (lambda ()
                   (cond ((read-string 1024 port) =>
                          (lambda (s)
                            (collect-string! sc s)
                            (lp)))
                         (else (string-collector->string sc))))))
      (lp))))

;;; (loop (initial (sc (make-string-collector)))
;;;       (bind (s (read-string 1024 port)))
;;;       (while s)
;;;       (do (collect-string! sc s))
;;;       (result (string-collector->string sc)))

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

(define (make-string-port-filter filter . maybe-buflen)
  (let* ((buflen (:optional maybe-buflen 1024))
         (buf (make-string buflen)))
    (lambda ()
      (let lp ()
        (cond ((read-string! buf (current-input-port) 0 buflen) =>
               (lambda (nread)
                 (display (filter (if (= nread buflen) buf
                                      (substring buf 0 nread)))) ; last one.
                 (lp))))))))
