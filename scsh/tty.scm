;;; My comments:
;;; - We have a lot of NeXT-specific stuff. More importantly, what is the
;;;   Linux, Solaris, and HP-UX specific stuff?
;;;
;;; - I would suggest totally flushing the ttychars vector from the interface
;;;   in favor of individual slots in the TTY-INFO record. Keep the vec
;;;   in the implementation, and define the TTY-INFO:EOL, etc. procs by
;;;   hand as being indices into the vector. We could *also* expose the
;;;   vector if we liked.
;;;     -Olin

;;; Terminal Control for the Scheme Shell
;;; Copyright (c) 1995 by Brian D. Carlstrom.
;;; Rehacked by Olin 8/95.

;;; tty-info records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I have to fake out my record package so I can define my very own
;;; MAKE-TTY-INFO procedure. Ech. I oughta have a lower-level record macro
;;; for this kind of thing.

(define-record-type :tty-info
  (really-make-tty-info control-chars input-flags output-flags control-flags
                        local-flags input-speed input-speed-code output-speed
                        output-speed-code min time)
  tty-info?
  (control-chars tty-info:control-chars
                 set-tty-info:control-chars)
  (input-flags tty-info:input-flags
               set-tty-info:input-flags)
  (output-flags tty-info:output-flags
                set-tty-info:output-flags)
  (control-flags tty-info:control-flags
                 set-tty-info:control-flags)
  (local-flags tty-info:local-flags
               set-tty-info:local-flags)
  (input-speed tty-info:input-speed
               set-tty-info:input-speed)
  (input-speed-code tty-info:input-speed-code
                    set-tty-info:input-speed-code)
  (output-speed tty-info:output-speed
                set-tty-info:output-speed)
  (output-speed-code tty-info:output-speed-code
                     set-tty-info:output-speed-code)
  (min tty-info:min set-tty-info:min)
  (time tty-info:time set-tty-info:time))

(define-record-discloser :tty-info
  (lambda (self)
    (list 'tty-info)))

;;; Encode the speeds at assignment time.
(define (set-tty-info:input-speed info speed)
  (set-tty-info:input-speed-code info (encode-baud-rate speed))
  (set-tty-info:input-speed      info speed))

(define (set-tty-info:output-speed info speed)
  (set-tty-info:output-speed-code info (encode-baud-rate speed))
  (set-tty-info:output-speed      info speed))

(define (make-tty-info iflags oflags cflags lflags ispeed ospeed min time)
  (really-make-tty-info (make-string num-ttychars (ascii->char 0))
                        iflags oflags cflags lflags
                        ispeed (encode-baud-rate ispeed)
                        ospeed (encode-baud-rate ospeed)
                        min time))

(define (copy-tty-info info)
  (really-make-tty-info (string-copy (tty-info:control-chars info))
                        (tty-info:input-flags	       info)
                        (tty-info:output-flags       info)
                        (tty-info:control-flags      info)
                        (tty-info:local-flags	       info)
                        (tty-info:input-speed	       info)
                        (tty-info:input-speed-code  info)
                        (tty-info:output-speed       info)
                        (tty-info:output-speed-code info)
                        (tty-info:min		       info)
                        (tty-info:time	       info)))

(define (sleazy-call/file tty opener proc)
  (if (string? tty)
      (opener tty (lambda (port) (sleazy-call/fdes port proc)))
      (sleazy-call/fdes tty proc)))

;;; (tty-info [fd/port/fname])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieve tty-info bits from a tty. Arg defaults to current input port.

(define (tty-info . maybe-fdport)
  (let ((control-chars (make-string num-ttychars))
	(fdport (:optional maybe-fdport (current-input-port))))
    (if (not (tty? fdport))
        (error "Argument to tty-info is not a tty" fdport))
    (apply
     (lambda (iflag oflag cflag lflag ispeed-code ospeed-code)
      (really-make-tty-info control-chars
                            iflag
                            oflag
                            cflag
                            lflag
                            (decode-baud-rate ispeed-code) ispeed-code
                            (decode-baud-rate ospeed-code) ospeed-code
                            (char->ascii
                             (string-ref control-chars ttychar/min))
                            (char->ascii
                             (string-ref control-chars ttychar/time))))
     (sleazy-call/file fdport
                       call-with-input-file
                       (lambda (fd) (%tty-info fd control-chars))))))

(import-lambda-definition-2 %tty-info (fdes control-chars)
  "scheme_tcgetattr")


;;; JMG: I don't know what the purpose of this code is...
;(define-foreign %bogus-tty-info/errno
;  ("scheme_tcgetattrB" (fixnum fdes)
;                       (var-string control-chars)
;		       (vector-desc ivec))
;  (to-scheme fixnum errno_or_false))

;(define-errno-syscall (%bogus-tty-info fdes control-chars ivec)
;  %bogus-tty-info/errno)

;(define (%%bogus-tty-info fd control-chars)
;  (let ((ivec (make-vector 6)))
;    (%bogus-tty-info fd control-chars ivec)
;    ivec))



;(define (%tty-info fdes cc)
;  (let ((ivec (%%bogus-tty-info fdes cc)))
;    (values (vector-ref ivec 0) (vector-ref ivec 1)
;	    (vector-ref ivec 2) (vector-ref ivec 3)
;	    (vector-ref ivec 4) (vector-ref ivec 5)
;	    (vector-ref ivec 6) (vector-ref ivec 7)
;	    (vector-ref ivec 8) (vector-ref ivec 9)
;	    cc)))

;;; (set-tty-info       tty option info)	[Not exported]
;;; (set-tty-info/now   tty option info)
;;; (set-tty-info/drain tty option info)
;;; (set-tty-info/flush tty option info)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assign tty-info bits to a tty.

(define (set-tty-info fdport option info)
  (let ((if (tty-info:input-flags   info))
	(of (tty-info:output-flags  info))
	(cf (tty-info:control-flags info))
	(lf (tty-info:local-flags   info))
	(cc (tty-info:control-chars info))
	(is (tty-info:input-speed-code  info))
	(os (tty-info:output-speed-code info)))
    (sleazy-call/file
     fdport
     call-with-input-file
     (lambda (fd)
       (%set-tty-info fd option
		      cc
		      if
		      of
		      cf
		      lf
		      is        os
		      (tty-info:min  info)
		      (tty-info:time info))))))


(import-lambda-definition-2 %set-tty-info
  (fdes option control-chars iflag oflag cflag lflag ispeed-code ospeed-code
	min time)
  "scheme_tcsetattr")

;;; Exported procs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that the magic %set-tty-info/foo constants must be defined before this
;;; file is loaded due to the set-tty-info/foo definitions below.

(define (make-tty-info-setter how)
  (lambda (fdport info) (set-tty-info fdport how info)))

(define set-tty-info/now   (make-tty-info-setter %set-tty-info/now))
(define set-tty-info/drain (make-tty-info-setter %set-tty-info/drain))
(define set-tty-info/flush (make-tty-info-setter %set-tty-info/flush))


;;; Send a break on the serial line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (send-tty-break . args)
  (let-optionals args ((tty (current-output-port))
                       (duration 0))
    (sleazy-call/file tty call-with-output-file
                      (lambda (fdes)
                        (%send-tty-break-fdes fdes duration)))))

(import-lambda-definition-2 %send-tty-break-fdes (fdes duration)
  "sch_tcsendbreak")

;;; Drain the main vein.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (drain-tty . maybe-tty)
  (let ((tty (:optional maybe-tty (current-output-port))))
    (cond ((integer? tty) (%tcdrain tty)) ; File descriptor.
          ((fdport? tty)                ; Scheme port -- flush first.
           (force-output tty)
           (sleazy-call/fdes tty %tcdrain))
          ((string? tty)                ; file name
           (sleazy-call/file tty call-with-output-file %tcdrain))
          (else (error "Illegal argument to DRAIN-TTY" tty)))))

(import-lambda-definition-2 %tcdrain (fdes) "sch_tcdrain")

;;; Flushing the device queues. (tcflush)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that the magic %flush-tty/foo constants must be defined before this
;;; file is loaded due to the flush-tty/foo definitions below.

(define (make-input-tty-flusher flag)
  (lambda maybe-tty
    (sleazy-call/file (:optional maybe-tty (current-input-port))
		      call-with-input-file
		      (lambda (fdes) (%tcflush fdes flag)))))

(define (make-output-tty-flusher flag)
  (lambda maybe-tty
    (sleazy-call/file (:optional maybe-tty (current-output-port))
		      call-with-output-file
		      (lambda (fdes) (%tcflush fdes flag)))))

(define flush-tty/input  (make-input-tty-flusher  %flush-tty/input))
(define flush-tty/output (make-output-tty-flusher %flush-tty/output))
(define flush-tty/both   (make-input-tty-flusher  %flush-tty/both))

(import-lambda-definition-2 %tcflush (fdes flag) "sch_tcflush")

;;; Stopping and starting I/O (tcflow)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that the magic %tcflow/foo constants must be defined before this
;;; file is loaded due to the definitions below.

(define (make-input-flow-controller action)
  (lambda maybe-tty
    (sleazy-call/file (:optional maybe-tty (current-input-port))
		      call-with-input-file
		      (lambda (fdes) (%tcflow fdes action)))))

(define (make-output-flow-controller action)
  (lambda maybe-tty
    (sleazy-call/file (:optional maybe-tty (current-output-port))
		      call-with-output-file
		      (lambda (fdes) (%tcflow fdes action)))))

(define start-tty-output (make-output-flow-controller %tcflow/start-out))
(define stop-tty-output  (make-output-flow-controller %tcflow/stop-out))
(define start-tty-input  (make-input-flow-controller %tcflow/start-in))
(define stop-tty-input   (make-input-flow-controller %tcflow/stop-in))

(import-lambda-definition-2 %tcflow (fdes action) "sch_tcflow")

;;; Baud rate translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We should just move these guys out to the tty-consts file.
;;; We currently search a vector of (code . speed) pairs.

(define (encode-baud-rate speed)	; 9600 -> value of BAUD/9600
  (do ((i (- (vector-length baud-rates) 1) (- i 1)))
      ((eqv? (cdr (vector-ref baud-rates i)) speed)
       (car (vector-ref baud-rates i)))
    (if (< i 0) (error "Unknown baud rate." speed))))

(define (decode-baud-rate code)		; BAUD/9600 -> 9600
  (do ((i (- (vector-length baud-rates) 1) (- i 1)))
      ((eqv? (car (vector-ref baud-rates i)) code)
       (cdr (vector-ref baud-rates i)))
    (if (< i 0) (error "Unknown baud rate code." code))))


;;; Set/Get tty process group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-tty-process-group port/fd/fname proc-group)
  (sleazy-call/file port/fd/fname call-with-input-file
    (lambda (fd)
      (%set-tty-process-group fd (if (integer? proc-group)
				     proc-group
				     (proc:pid proc-group))))))

(import-lambda-definition-2 %set-tty-process-group (fdes pid) "sch_tcsetpgrp")

(define (tty-process-group port/fd/fname)
  (sleazy-call/file port/fd/fname call-with-input-file %tty-process-group))

(import-lambda-definition-2 %tty-process-group (fdes) "sch_tcgetpgrp")

;;; (open-control-tty fname [flags])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open a control tty, return a port.
;;; This procedure is only guaranteed to work when the process doesn't already
;;; have a control tty -- e.g., right after a (BECOME-PROCESS-LEADER) call.
;;; This limted functionality is about all we can provide portably across BSD,
;;; SunOS, and SVR4.

(define (open-control-tty ttyname . maybe-flags)
  (let ((flags (:optional maybe-flags (file-options read-write))))
      (let ((fd (%open-control-tty ttyname flags)))
	((if (or (file-options-on? flags (file-options read-only))
		 (file-options-on? flags (file-options read-write)))
	     make-input-fdport
	     make-output-fdport)
	 fd 1))))

(import-lambda-definition-2 %open-control-tty (ttyname flags) "open_ctty")

(define (make-control-tty fd/port)
  (sleazy-call/fdes fd/port %make-control-tty))

(import-lambda-definition-2 %make-control-tty (fd) "make_ctty")

;;; Random bits & pieces: isatty ttyname ctermid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (tty? fd/port) -> boolean
;;; (tty-file-name fd/port) -> string
;;; (control-tty-file-name) -> string

(import-lambda-definition-2 %tty? (fd) "sch_isatty")
(define (tty? fd/port) (sleazy-call/fdes fd/port %tty?))

(import-lambda-definition-2 %tty-file-name (fd) "sch_ttyname")

(define (tty-file-name fd/port) (sleazy-call/fdes fd/port %tty-file-name))

(import-lambda-definition-2 control-tty-file-name () "scm_ctermid")
