(define-syntax define-constance
  (syntax-rules ()
    ((define-constance constance-name
       initializer
       ((scheme-name c-binding) ...))
     (begin
       (define-record-type constance-name
         (maker name c-value)
         constance-name?
         (name name-accessor)
         (c-value c-value-accessor c-value-setter))
       (import-dynamic-externals (string-append "=scshexternal/" (symbol->string 'constance-name)))
       (begin (define scheme-name (maker scheme-name (shared-binding-ref (lookup-imported-binding (symbol->string 'c-binding))))) ...)
       (define (initializer)
         (c-value-settter scheme-name (shared-binding-ref (lookup-imported-binding (symbol->string 'c-binding)))) ...)))))

(define-syntax define-direct-constance
  (syntax-rules ()
    ((define-direct-constance constance-name
       initializer
       reinitializer
       ((scheme-name c-binding) ...))
     (begin
       (import-dynamic-externals (string-append "=scshexternal/" (symbol->string 'constance-name)))
       (begin (define scheme-name (shared-binding-ref (lookup-imported-binding (symbol->string 'c-binding)))) ...)
       (define constance-name (vector scheme-name ...))
       (define (initializer)
         (set! scheme-name (shared-binding-ref (lookup-imported-binding (symbol->string
                                                                         'c-binding)))) ...)
       (define-reinitializer reinitializer initializer)))))
