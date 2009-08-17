(define-record-type md5-context :md5-context
  (really-make-md5-context c-context final? init?)
  md5-context?
  (c-context md5-c-context)
  (final? md5-context-final? set-md5-context-final?!)
  (init? md5-context-init? set-md5-context-init?!))

(define (make-md5-context)
  (let ((context (really-make-md5-context (make-md5-ctx) #f #f)))
    (init-md5-context! context)
    context))

(define (init-md5-context! context)
  (md5-init! (md5-c-context context))
  (set-md5-context-init?! context #t)
  (set-md5-context-final?! context #f))

(define (update-md5-context! context string)
  (if (not (md5-context-init? context))
      (error "md5 context not initialized" context))
  (if (md5-context-final? context)
      (error "tried to update final md5 context" context))
  (md5-update! (md5-c-context context) string))

(define (md5-context->md5-digest context)
  (if (not (md5-context-init? context))
      (error "md5 context not initialized" context))
  (let((digest-as-string (md5-final! (md5-c-context context))))
    (set-md5-context-final?! context #t)
    (make-md5-digest (bits128->number digest-as-string))))

(define-record-type md5-digest :md5-digest
  (make-md5-digest value)
  md5-digest?
  (value md5-digest-value))

(define number->md5-digest
  (let ((bit128-mask (- (expt 2 128) 1)))
    (lambda (n)
      (if (and (>= n 0)
	       (<= n bit128-mask))
	  (make-md5-digest n)
	  (error "Argument to number->md5-digest not a 128 bit number" n)))))

(define md5-digest->number md5-digest-value)

(define (bits128->number bits-as-string)
  (car 
   (string-fold-right
    (lambda (c number.arity)
      (let ((number (car number.arity))
	    (arity (cdr number.arity)))
	(cons (bitwise-ior number 
			   (arithmetic-shift (char->ascii c) (* arity 8)))
	      (+ arity 1))))
    (cons 0 0)
    bits-as-string)))


(define (md5-digest-for-string s)
  (let ((context (make-md5-context)))
    (init-md5-context! context)
    (update-md5-context! context s)
    (md5-context->md5-digest context)))

(define (md5-digest-for-port port . maybe-buffer-size)
  (let* ((buffer-size (if (null? maybe-buffer-size) 1024 (car maybe-buffer-size)))
	 (buffer (make-string buffer-size))
	 (context (make-md5-context)))
    (init-md5-context! context)
    (let lp ()
      (let ((got (read-block buffer 0 buffer-size port)))
	(cond ((eof-object? got)
	       (md5-context->md5-digest context))
	      ((< got buffer-size)
	       (if (not (eof-object? (peek-char port)))
		   (error "read-block didn't read port to the end"))
	       (update-md5-context! context (substring buffer 0 got))
	       (md5-context->md5-digest context))
	      (else
	       (update-md5-context! context buffer)
	       (lp)))))))


(import-lambda-definition make-md5-ctx () "make_MD5_CTX")
(import-lambda-definition md5-init! (context) "MD5Init_stub")
(import-lambda-definition md5-update! (context string) "MD5Update_stub")
(import-lambda-definition md5-final! (context) "MD5Final_stub")

