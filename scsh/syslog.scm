; Copyright (c) 1993-2000 by Richard Kelsey and Jonathan Rees. See file COPYING.

;; Options for openlog

(define-enumerated-type syslog-option :syslog-option
  syslog-option?
  the-syslog-options
  syslog-option-name
  syslog-option-index
  ;; The order of these is known to the C code.
  (console
   delay
   no-delay
   log-pid))

(define-exported-binding "syslog-options" the-syslog-options)

(define-enum-set-type syslog-options :syslog-options
  syslog-options?
  make-syslog-options

  syslog-option
  syslog-option?
  the-syslog-options
  syslog-option-index)

(define-exported-binding "syslog-options?" syslog-options?)

(define default-syslog-options (syslog-options))

(define-enumerated-type syslog-facility :syslog-facility
  syslog-facility?
  syslog-facilities
  syslog-facility-name
  syslog-facility-index
  ;; Options for openlog
  ;; The order of these is known to the C code.
  (authorization
   cron
   daemon
   kernel
   lpr
   mail
   news
   user
   uucp
   local0 local1 local2 local3 local4 local5 local6 local7))

(define default-syslog-facility (syslog-facility user))

(define-exported-binding "syslog-facility-type" :syslog-facility)
(define-exported-binding "syslog-facilities" syslog-facilities)

(define-enumerated-type syslog-level :syslog-level
  syslog-level?
  syslog-levels
  syslog-level-name
  syslog-level-index
  ;; The order of these is known to the C code.
  (emergency
   alert
   critical
   error
   warning
   notice
   info
   debug))

(define-exported-binding "syslog-level-type" :syslog-level)
(define-exported-binding "syslog-levels" syslog-levels)

(define-enum-set-type syslog-mask :syslog-mask
  syslog-mask?
  make-syslog-mask

  syslog-level
  syslog-level?
  syslog-levels
  syslog-level-index)

(define-exported-binding "syslog-mask?" syslog-mask?)
(define-exported-binding ":syslog-mask" :syslog-mask)

(define (syslog-mask-upto level)
  (let loop ((index (syslog-level-index level)) (levels '()))
    (if (< index 0)
	(make-syslog-mask levels)
	(loop (- index 1)
	      (cons (vector-ref syslog-levels index)
		    levels)))))

(define syslog-mask-all (make-syslog-mask (vector->list syslog-levels)))

(define default-syslog-mask syslog-mask-all)

(import-lambda-definition openlog (ident options facility)
			  "sch_openlog")
(import-lambda-definition unix-syslog (level opt-facility message)
			  "sch_syslog")
(import-lambda-definition setlogmask! (logmask)
			  "sch_setlogmask")
(import-lambda-definition closelog ()
			  "sch_closelog")

(define-record-type :syslog-channel
  (make-syslog-channel ident options facility mask)
  syslog-channel?
  (ident syslog-channel-ident)
  (options syslog-channel-options)
  (facility syslog-channel-facility)
  (mask syslog-channel-mask))

(define (syslog-channel-equivalent? channel-1 channel-2)
  (and (string=? (syslog-channel-ident channel-1)
		 (syslog-channel-ident channel-2))
       (enum-set=? (syslog-channel-options channel-1)
		   (syslog-channel-options channel-2))
       ;; facility can be specified with each syslog-write
       (enum-set=? (syslog-channel-mask channel-1)
		   (syslog-channel-mask channel-2))))

(define current-syslog-channel 'unitinialized)
(define current-syslog-channel-lock 'unitinialized)

(define (initialize-syslog)
  (set! current-syslog-channel #f)
  (set! current-syslog-channel-lock (make-lock)))

(define open-syslog-channel make-syslog-channel)

(define (close-syslog-channel channel)
  (obtain-lock current-syslog-channel-lock)
  (if (syslog-channel-equivalent? channel
				  current-syslog-channel)
      (closelog))
  (release-lock current-syslog-channel-lock))

(define (with-syslog-channel channel thunk)
  (dynamic-wind
   (lambda ()
     (obtain-lock current-syslog-channel-lock))
   (lambda ()
     (if (or (not current-syslog-channel)
	     (not (syslog-channel-equivalent? channel
					      current-syslog-channel)))
	 (begin
	   (if current-syslog-channel
	       (closelog))
	   (openlog (syslog-channel-ident channel)
		    (syslog-channel-options channel)
		    (syslog-channel-facility channel))
	   (if (not (enum-set=? (syslog-channel-mask channel)
				default-syslog-mask))
	       (setlogmask! (syslog-channel-mask channel)))
	   (set! current-syslog-channel channel)))
     (thunk))
   (lambda ()
     (release-lock current-syslog-channel-lock))))

(define (syslog-write level message channel)
  (with-syslog-channel
   channel
   (lambda ()
     (unix-syslog level (syslog-channel-facility channel) message))))

(define (change-syslog-channel channel ident options facility mask)
  (make-syslog-channel (or ident
			   (syslog-channel-ident channel))
		       (or options
			   (syslog-channel-options channel))
		       (or facility
			   (syslog-channel-facility channel))
		       (or mask
			   (syslog-channel-mask channel))))

(define dynamic-syslog-channel
  (make-preserved-thread-fluid
   (make-syslog-channel "scsh"
			default-syslog-options
			default-syslog-facility
			default-syslog-mask)))

(define (syslog level message . rest)
  (syslog-write level message
		(cond
		 ((null? rest)
		  (thread-fluid dynamic-syslog-channel))
		 ((and (null? (cdr rest))
		       (syslog-channel? (car rest)))
		  (car rest))
		 (else
		  ;; this might be a little excessive allocation
		  (apply change-syslog-channel
			 (thread-fluid dynamic-syslog-channel)
			 (append rest '(#f)))))))

(define (with-syslog-destination ident options facility mask thunk)
  (let-thread-fluid dynamic-syslog-channel
		    (change-syslog-channel
		     (thread-fluid dynamic-syslog-channel)
		     ident options facility mask)
		    thunk))

(define (set-syslog-channel! channel)
  (set-thread-fluid! dynamic-syslog-channel
		    channel))

(define (set-syslog-destination! ident options facility mask)
  (set-thread-fluid! dynamic-syslog-channel
		     (change-syslog-channel
		      (thread-fluid dynamic-syslog-channel)
		      ident options facility mask)))

;----------------
; A record type whose only purpose is to run some code when we start up an
; image.

(define-record-type :reinitializer
  (make-reinitializer thunk)
  reinitializer?
  (thunk reinitializer-thunk))

(define-record-discloser :reinitializer
  (lambda (r)
    (list 'reinitializer (reinitializer-thunk r))))

(define-record-resumer :reinitializer
  (lambda (r)
    ((reinitializer-thunk r))))

(initialize-syslog)

(define syslog-reinitializer
  (make-reinitializer initialize-syslog))
