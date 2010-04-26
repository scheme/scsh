;;; Command-line argument access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some globals.
(define %command-line '())              ; Includes program.
(define command-line-arguments #f)      ; Doesn't include program.

(define (set-command-line-args! args)
  (set! %command-line args)
  (set! command-line-arguments (append (cdr args) '())))

(define (arg* arglist n . maybe-default-thunk)
  (let ((oops (lambda () (error "argument out of bounds" arglist n))))
    (if (< n 1) (oops)
        (let lp ((al arglist) (n n))
          (if (pair? al)
              (if (= n 1) (car al)
                  (lp (cdr al) (- n 1)))
              (if (and (pair? maybe-default-thunk)
                       (null? (cdr maybe-default-thunk)))
                  ((car maybe-default-thunk))
                  (oops)))))))

(define (arg arglist n . maybe-default)
  (if maybe-default (arg* arglist n (lambda () (car maybe-default)))
      (arg* arglist n)))

(define (argv n . maybe-default)
  (apply arg %command-line (+ n 1) maybe-default))

(define (command-line) (append %command-line '()))
