#lang racket

(require scribble/manual)
(provide defproc/nolink defform/nolink)

(define-syntax defform/nolink
  (syntax-rules () ((_ args ...) (defform #:link-target? #f args ...))))

(define-syntax defproc/nolink
  (syntax-rules () ((_ args ...) (defproc #:link-target? #f args ...))))
