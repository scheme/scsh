(import-dynamic-externals "=scshexternal/scsh")

(define (byte-vector->string bytev)
  (os-string->string (byte-vector->os-string bytev)))

(define-syntax define/vector-args
  (syntax-rules ()
    ((define/vector-args new raw (string-arg ...) arg ...)
     (define (new string-arg ... arg ...)
       (raw (string->os-byte-vector string-arg) ...
            arg ...)))))
