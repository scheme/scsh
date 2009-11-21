;;; A Unix file port system to completely replace S48 file ports.
;;; We use S48 extensible ports.
;;; Copyright (c) 1993 by Olin Shivers.

(define-record-type :fdport-data
  (make-fdport-data channel revealed)
  fdport-data?
  (channel fdport-data:channel set-fdport-data:channel)
  (revealed fdport-data:revealed set-fdport-data:revealed))

(define-record-discloser :fdport-data
  (lambda (r)
    (list 'fdport-data
          (fdport-data:channel r) (fdport-data:revealed r))))

; This stuff is _weak_.
; Vector of weak pointers mapping fd -> fdport.

(define fdports (make-integer-table))

(define (install-fdport fdport)
  (let* ((fdport* (fdport-data fdport))
         (ch (fdport-data:channel fdport*))
         (ch-number  (channel-os-index ch)))
    (if (not (= (fdport-data:revealed fdport*) 0))
        (table-set! fdports ch-number fdport)
        (begin
          (weak-table-set! fdports ch-number fdport)
          (%set-cloexec (fdport-data:fd (port-data fdport)) #t)))))

(define (maybe-fdes->port fdes)
  (weak-table-ref fdports fdes))

;Hmm... these shouldn't be necessary.  But still.
;Fake defrec routines for backwards compatibility.
(define (fdport-data:fd fdport*)
  (channel-os-index  (fdport-data:channel fdport*)))

(define (fdport-data:closed? fdport*)
  (eq? (channel-status (fdport-data:channel fdport*))
       (enum channel-status-option closed)))

;;; Support for channel-ready?
;;; This applies to input- and output-ports

(define (fdport-channel-ready? fdport)
  (channel-ready? (fdport-data:channel (port-data fdport))))

;Arbitrary, for now.
(define buffer-size 255)

(define open-fdchannel open-channel)

(define (make-input-fdchannel fd)
  (open-fdchannel fd (enum channel-status-option input)))

(define (make-output-fdchannel fd)
  (open-fdchannel fd (enum channel-status-option output)))

;The two following routines are to build ports from stdin and stdout channels.
(define (channel-port->input-fdport channel-port)
  (let ((p (make-buffered-input-port input-fdport-handler
                            (make-fdport-data
                             (port->channel channel-port) 1)
                            (make-byte-vector buffer-size 0) 0 0)))
    (set-port-lock! p (port-lock channel-port))
    (install-fdport p)
    p))

(define (channel-port->output-fdport channel-port)
  (let ((p (make-buffered-output-port
            output-fdport-handler
            (make-fdport-data  (port->channel channel-port) 1)
            (make-byte-vector buffer-size 0) 0 buffer-size)))
    (set-port-lock! p (port-lock channel-port))
    (install-fdport p)
    (periodically-force-output! p)
    p))

(define (channel-port->unbuffered-output-fdport channel-port)
  (let ((p (make-unbuffered-output-port unbuffered-output-fdport-handler
                             (make-fdport-data
                              (port->channel channel-port) 1))))
    (set-port-lock! p (port-lock channel-port))
    (install-fdport p)
    (periodically-force-output! p)
    p))

(define (alloc-input-fdport fd revealed)
  (make-buffered-input-port input-fdport-handler
                   (make-fdport-data (make-input-fdchannel fd) revealed)
                   (make-byte-vector buffer-size 0) 0 0))

(define (alloc-output-fdport fd revealed)
  (make-buffered-output-port output-fdport-handler
                    (make-fdport-data (make-output-fdchannel fd) revealed)
                    (make-byte-vector buffer-size 0) 0 buffer-size))

(define (make-input-fdport fd revealed)
  (let ((p (alloc-input-fdport fd revealed)))
    (install-fdport p)
    p))

(define (make-output-fdport fd revealed)
  (let ((p (alloc-output-fdport fd revealed)))
    (periodically-force-output! p)
    (install-fdport p)
    p))

(define (fdport? x)
  (cond ((or (and (input-port? x) (port-data x))
             (and (output-port? x) (port-data x)))
         => (lambda (d) (fdport-data? d)))
        (else #f)))

(define fdport-null-method (lambda (x) x #f))

(define null-func (lambda args #t))

(define (close-fdport* fdport*)
  (table-set! fdports (channel-os-index (fdport-data:channel fdport*)) #f)
  (close-channel (fdport-data:channel fdport*)))

;The handlers drop straight through to the convenient channel routines.
(define (make-input-fdport-handler bufferproc)
  (make-buffered-input-port-handler
   (lambda (fdport*)
     (list 'input-fdport (fdport-data:channel fdport*)))
   close-fdport*
   bufferproc
   fdport-channel-ready?
   ;; (lambda (fdport* owner)
   ;;   (steal-channel! (fdport-data:channel fdport*) owner))
   ))

(define input-fdport-handler
  (make-input-fdport-handler
   (lambda (fdport* buffer start needed)
     ;; I'm not sure if this is correct
     (let ((condvar (make-condvar)))
       (with-new-proposal (lose)
         (or (channel-maybe-commit-and-read
              (fdport-data:channel fdport*) buffer start needed condvar #t)
             (lose))
         (condvar-value condvar))))))

(define (make-output-fdport-handler bufferproc)
   (make-buffered-output-port-handler
   (lambda (fdport*)
     (list 'output-fdport (fdport-data:channel fdport*)))
   close-fdport*
   bufferproc
   fdport-channel-ready?
   ;; (lambda (fdport* owner)
   ;;   (steal-channel! (fdport-data:channel fdport*) owner))
   ))

(define output-fdport-handler
  (make-output-fdport-handler
   (lambda (fdport* buffer start count)
     (channel-write buffer start count (fdport-data:channel fdport*)))))

(define unbuffered-output-fdport-handler
  (let ((buffer (make-byte-vector 1 0)))
    (make-output-fdport-handler
     (lambda (fdport* char)
       (byte-vector-set! buffer 0 (char->ascii char))
       (channel-write buffer 0 1 (fdport-data:channel fdport*))))))

(define fdport-data port-data)
; That was easy.

(define (guess-output-policy port)
  (if (= 0 (port-limit port))
      bufpol/none
      bufpol/block))

(define (set-port-buffering port policy . maybe-size)
  (cond ((and (fdport? port) (open-input-port? port))
         (let ((size (if (pair? maybe-size) (car maybe-size)
                         (if (= policy bufpol/none) 1 255))))
           (if (<= size 0)
               (error "buffer size must be at least 1 for input ports"
                      port policy size))
           (set-input-port-buffering port policy size)))
        ((and (fdport? port) (open-output-port? port))
         (let ((size (if (pair? maybe-size) (car maybe-size)
                         (if (= policy bufpol/none) 0 255))))
           (if (< size 0)
               (error "buffer size must be at least 0 for output ports"
                      port policy size))
           (set-output-port-buffering port policy size)))
        (else
            (error "Not a port" port))))

(define (set-output-port-buffering port policy size)
  (cond ((eq? policy bufpol/none)
         (if (not (= size 0))
             (error "buffer size must be 0 for bufpol/none on output ports"
                    port policy size))
         (install-nullbuffer port unbuffered-output-fdport-handler))
        ((eq? policy bufpol/block)
         (if (= size 0)
             (install-nullbuffer port unbuffered-output-fdport-handler)
             (let ((old-size (byte-vector-length (port-buffer port)))
                   (new-buffer (make-byte-vector size 0)))
               (if (< size old-size)
                   (begin
                     ((port-handler-force (port-handler port)) port #t) ;really-force-output
                     (set-port-index! port 0))
                   (begin
                     (copy-bytes! (port-buffer port) 0 new-buffer 0 old-size)))
               (install-buffer port new-buffer size))))
         ((eq? policy bufpol/line)
         ;(install-nullbuffer port (make-line-output-proc size)))
         (error "bufpol/line is currently not supported"))
        (else (error "policy not supported " policy))))

(define (install-nullbuffer port handler)
 ((port-handler-force (port-handler port)) port #t) ;really-force-output
 (set-port-limit! port 0)
 (set-port-index! port 0)
 (set-port-buffer! port (make-byte-vector 0 0))
 (set-port-handler! port handler))

(define (install-buffer port new-buffer size)
  (if (eq? bufpol/none (guess-output-policy port))
      (set-port-handler! port output-fdport-handler))
  (set-port-limit! port size)
  (set-port-buffer! port new-buffer))

; TODO flush on stdinput is required but probably impossible since current-input-port is a fluid and may change without notice. One possibility would be to override (current-input-port)

;;; This port can ONLY be flushed with a newline or a close-output
;;; flush-output  won't help
(define (make-line-output-proc size)
  (let ((proc-buffer (make-byte-vector size 0))
        (proc-buffer-index 0))
    (make-buffered-output-port-handler
     (lambda (fdport*)
       (list 'output-fdport (fdport-data:channel fdport*)))
     (lambda (fdport*)
       (channel-write proc-buffer
                      0
                      proc-buffer-index
                      (fdport-data:channel fdport*))
       (close-fdport* fdport*))
     (lambda (fdport* char)
       (byte-vector-set! proc-buffer proc-buffer-index (char->ascii char))
       (set! proc-buffer-index (+ proc-buffer-index 1))
       (cond ((or (eq? char #\newline) (= proc-buffer-index size))
              (channel-write proc-buffer
                             0
                             proc-buffer-index
                             (fdport-data:channel fdport*))
              (set! proc-buffer-index 0))))
     fdport-channel-ready?
     ;; (lambda (fdport* owner)
     ;;   (steal-channel! (fdport-data:channel fdport*) owner))
     )))


(define (set-input-port-buffering port policy size)
  (cond ((eq? policy bufpol/none)
         (if (not (= size 1))
             (error "buffer size must be 1 for bufpol/none on input ports"
                    port policy size))
         (set-input-port-buffering port bufpol/block 1))
        ((eq? policy bufpol/block)
         (install-input-handler port input-fdport-handler size #t))
        ((eq? policy bufpol/line)
         (error "bufpol/line not allowed on input"))
        (else (error "policy not supported " policy))))

(define (install-input-handler port new-handler size gentle?)
         (let* ((old-limit (port-limit port))
                (old-index (port-index port))
                (old-buffer (port-buffer port))
                (old-unread (- old-limit old-index))
                (new-unread (min old-unread size))
                (throw-away (max 0 (- old-unread new-unread)))
                (new-buffer (make-byte-vector size 0)))
           (if (not gentle?)
               (let ((ret (if (> throw-away 0)
                              (let ((return-buffer
                                     (make-byte-vector throw-away 0)))
                                (copy-bytes! old-buffer old-index
                                             return-buffer 0
                                             throw-away) return-buffer)
                              #f)))
                   (copy-bytes! old-buffer (+ old-index throw-away)
                                new-buffer 0
                                new-unread)
                   (set-port-buffer! port new-buffer)
                   (set-port-index! port 0)
                   (set-port-limit! port new-unread)
                   (set-port-handler! port new-handler)
                 ret)
                (begin
                  (install-drain-port-handler
                   old-buffer old-index old-limit port new-handler)
                  (set-port-buffer! port new-buffer)
                  (set-port-index! port 0)
                  (set-port-limit! port 0)
                  #t))))

(define (install-drain-port-handler
         old-buffer old-start old-limit port new-handler)
   (if (< 0 (- old-limit old-start))
       (set-port-handler! port
                          (make-drain-port-handler
                           old-buffer old-start old-limit port new-handler))
       (set-port-handler! port new-handler)))


;;; TODO: This reference to port will prevent gc !!!
(define (make-drain-port-handler
         very-old-buffer old-start old-limit port new-handler)
  (let ((old-buffer (make-byte-vector old-limit 0)))
    (copy-bytes! very-old-buffer 0 old-buffer 0 old-limit)
    (make-input-fdport-handler
     (lambda (data buffer start needed)
       (let ((old-left (- (byte-vector-length old-buffer) old-start)))
         (let ((size (cond ((or (eq? needed 'any) (eq? needed 'immediate))
                            (min old-left
                                 (byte-vector-length buffer)))
                           (else (min needed old-left)))))
           (copy-bytes! old-buffer old-start buffer start size)
           (set! old-start (+ size old-start))

           (if (= old-start (byte-vector-length old-buffer))  ;buffer drained ?
               (begin
                 (set-port-handler! port new-handler)
                 (if (and (integer? needed) (> needed size))
                     (+ size ((port-handler-buffer-proc new-handler)
                              data buffer (+ start size) (- needed size)))
                     size))
               size)))))))


;;; Open & Close
;;; ------------

;;; replace rts/channel-port.scm begin
(define (open-file fname flags . maybe-mode)
  (let ((fd (apply open-fdes fname flags maybe-mode))
        (access (bitwise-and flags open/access-mask)))
    ((if (or (= access open/read) (= access open/read+write))
         make-input-fdport
         make-output-fdport)
     fd 0)))

(define (open-input-file fname . maybe-flags)
  (let ((flags (:optional maybe-flags 0)))
    (open-file fname (deposit-bit-field flags open/access-mask open/read))))

(define (deposit-bit-field bits mask field)
  (bitwise-ior (bitwise-and field mask)
               (bitwise-and bits  (bitwise-not mask))))

(define (open-output-file fname . rest)
  (let* ((flags (if (pair? rest) (car rest)
                    (bitwise-ior open/create open/truncate))) ; default
         (maybe-mode (if (null? rest) '() (cdr rest)))
         (flags (deposit-bit-field flags open/access-mask open/write)))
    (apply open-file fname flags maybe-mode)))

;;; replace rts/channel-port.scm end

;;; All these revealed-count-hacking procs have atomicity problems.
;;; They need to run uninterrupted.
;;; (port-locks should do the trick -df)
;;; (what else has atomicity problems? -df)

(define (increment-revealed-count port delta)
  (let* ((data (fdport-data port))
         (count (fdport-data:revealed data))
         (newcount (+ count delta)))
    (set-fdport-data:revealed data newcount)
    (if (and (zero? count) (> newcount 0))          ; We just became revealed,
        (begin
          (strengthen-weak-table-ref fdports (fdport-data:fd data))
          (%set-cloexec (fdport-data:fd data) #f))))) ; so don't close on exec().


(define (release-port-handle port)
  (check-arg fdport? port port->fdes)
  (let* ((data (fdport-data port))
         (rev (fdport-data:revealed data)))
    (if (not (zero? rev))
;       (set-fdport-data:old-revealed data
;                                     (- (fdport-data:old-revealed data) 1))
        (let ((new-rev (- rev 1)))
          (set-fdport-data:revealed data new-rev)
          (if (zero? new-rev)                   ; We just became unrevealed, so
              (begin                            ; the fd can be closed on exec.
                (weaken-weak-table-ref fdports (fdport-data:fd data))
                (%set-cloexec (fdport-data:fd data) #t)))))))

(define (port-revealed port)
  (let ((count (fdport-data:revealed
                (fdport-data
                 (check-arg fdport? port port-revealed)))))
    (and (not (zero? count)) count)))

(define (fdes->port fd port-maker) ; local proc.
  (cond  ((maybe-fdes->port fd) =>
          (lambda (p)
            (increment-revealed-count p 1)
            p))
         (else (port-maker fd 1))))

(define (fdes->inport fd)
  (let ((port (fdes->port fd make-input-fdport)))
    (if (not (input-port? port))
        (error "fdes was already assigned to an outport" fd)
        port)))

(define (fdes->outport fd)
  (let ((port (fdes->port fd make-output-fdport)))
    (if (not (output-port? port))
        (error "fdes was already assigned to an inport" fd)
        port)))

(define (port->fdes port)
  (check-arg open-fdport? port port->fdes)
  (let ((data (fdport-data port)))
    (increment-revealed-count port 1)
    (fdport-data:fd data)))

(define (call/fdes fd/port proc)
  (cond ((integer? fd/port)
         (proc fd/port))

        ((fdport? fd/port)
         (let ((port fd/port))
           (dynamic-wind
            (lambda ()
              (if (not port) (error "Can't throw back into call/fdes.")))
            (lambda () (proc (port->fdes port)))
            (lambda ()
              (release-port-handle port)
              (set! port #f)))))

        (else (error "Not a file descriptor or fdport." fd/port))))

;;; Don't mess with the revealed count in the port case
;;; -- just sneakily grab the fdes and run.

(define (sleazy-call/fdes fd/port proc)
  (proc (cond ((integer? fd/port) fd/port)
              ((fdport? fd/port) (fdport-data:fd (fdport-data fd/port)))
              (else (error "Not a file descriptor or fdport." fd/port)))))


;;; Random predicates and arg checkers
;;; ----------------------------------

(define (open-fdport-data? x)
  (and (fdport-data? x)
       (not (fdport-data:closed? x))))

(define (open-fdport? x)
  (and (fdport? x) (or (open-output-port? x) (open-input-port? x))))

(define (fdport-open? port)
  (check-arg fdport? port fdport-open?)
  (not (fdport-data:closed? (port-data port))))


;;; Initialise the system
;;; ---------------------

;;; JMG: should be deprecated-proc
(define error-output-port
  current-error-port)


(define (init-fdports!)
  (set-fluid! $current-input-port
              (channel-port->input-fdport (current-input-port)))
  (set-port-buffering (current-input-port) bufpol/none)

  (set-fluid! $current-output-port
              (channel-port->output-fdport (current-output-port)))
  (set-fluid! $current-error-port
              (channel-port->unbuffered-output-fdport (current-error-port)))
  (set-fluid! $current-noise-port
              (fluid $current-error-port)))

;;; Generic port operations
;;; -----------------------

;;; (close-after port f)
;;;     Apply F to PORT. When F returns, close PORT, then return F's result.
;;;     Does nothing special if you throw out or throw in.

(define (close-after port f)
  (receive vals (f port)
    (close port)
    (apply values vals)))

(define (close port/fd)
  ((cond ((integer? port/fd)     close-fdes)
         ((output-port? port/fd) close-output-port)
         ((input-port?  port/fd) close-input-port)
         (else (error "Not file-descriptor or port" port/fd)))  port/fd))

;;; If this fd has an associated input or output port,
;;; move it to a new fd, freeing this one up.

(define (evict-ports fd)
  (cond ((maybe-fdes->port fd) =>       ; Shouldn't bump the revealed count.
         (lambda (port)
           (%move-fdport (%dup fd) port 0)
           #t))
        (else #f)))

(define (%move-fdport fd port new-revealed)
  (let* ((fdport* (fdport-data port))
         (ch (fdport-data:channel fdport*))
         (old-fd (channel-os-index ch))
         (old-vector-ref (table-ref fdports old-fd)))
    (set-fdport-data:revealed fdport* new-revealed)
    (table-set! fdports old-fd #f)
    (close-channel ch)
    (set-fdport-data:channel
     fdport*
     (make-fd-channel port fd))
    (table-set! fdports fd old-vector-ref)
    (%set-cloexec fd (not new-revealed)))
  #f)  ; JMG: It used to return #f on succes in 0.5.1, so we do the same

(define (make-fd-channel port fd)
  ((if (input-port? port) make-input-fdchannel make-output-fdchannel) fd))

(define (close-fdes fd)
  (if (evict-ports fd)
      #t ; EBADF should not occur if there is a port
      (%close-fdes fd)))

(define (flush-fdport fdport)
  (check-arg fdport? fdport flush-fdport)
  (force-output fdport))

(define (flush-all-ports)
  (let ((thunks (output-port-forcers #f)))
    (cond ((null? thunks)
           #f)
          (else
           (let loop ((threads
                       (map spawn-thread thunks))
                      (new-threads '()))
             (cond
              ((not (null? threads))
               (if ((structure-ref threads-internal thread-continuation)
                    (car threads))
                   (loop (cdr threads)
                         (cons (car threads) new-threads))
                   (loop (cdr threads) new-threads)))
              ((not (null? new-threads))
               (loop new-threads '()))))
           #t))))

(define (spawn-thread thunk)
  (let ((placeholder (make-placeholder)))
    (spawn
     (lambda ()
       (placeholder-set!
        placeholder
        ((structure-ref threads current-thread)))
       (thunk)))
    (placeholder-value placeholder)))

(define (flush-all-ports-no-threads)
  (let ((thunks (output-port-forcers #f)))
    (for-each (lambda (thunk) (thunk)) thunks)))

;;; Extend R4RS i/o ops to handle file descriptors.
;;; -----------------------------------------------

(define s48-char-ready? (structure-ref scheme char-ready?))
(define s48-read-char   (structure-ref scheme read-char))

(define-simple-syntax
  (define-r4rs-input (name arg ...) stream s48name body ...)
  (define (name arg ... . maybe-i/o)
    (let ((stream (:optional maybe-i/o (current-input-port))))
      (cond ((input-port? stream) (s48name arg ... stream))
            ((integer? stream) body ...)
            (else (error "Not a port or file descriptor" stream))))))

(define-r4rs-input (char-ready?) input s48-char-ready?
  (%char-ready-fdes? input))

(define-r4rs-input (read-char) input s48-read-char
  (let ((port (fdes->inport input)))
    (set-port-buffering port bufpol/none)
    (s48-read-char port)))

;structure refs changed to get reference from scheme -dalbertz
(define s48-display    (structure-ref scheme display))
(define s48-newline    (structure-ref scheme newline))
(define s48-write      (structure-ref scheme write))
(define s48-write-char (structure-ref scheme write-char))
(define s48-format     (structure-ref formats format))
(define s48-force-output (structure-ref i/o force-output))

(define-simple-syntax
  (define-r4rs-output (name arg ...) stream s48name body ...)
  (define (name arg ... . maybe-i/o)
    (let ((stream (:optional maybe-i/o (current-output-port))))
      (cond ((output-port? stream) (s48name arg ... stream))
            ((integer? stream) body ...)
            (else (error "Not an outport or file descriptor" stream))))))

;;; This one depends upon S48's string ports.
(define-r4rs-output (display object) output s48-display
  (let ((sp (make-string-output-port)))
    (display object sp)
    (write-string (string-output-port-output sp) output)))

(define-r4rs-output (newline) output s48-newline
  (let ((port (fdes->outport output)))
    (set-port-buffering port bufpol/none)
    (s48-newline port)))

(define-r4rs-output (write object) output s48-write
  (let ((sp (make-string-output-port)))
    (write object sp)
    (write-string (string-output-port-output sp) output)))

(define-r4rs-output (write-char char) output s48-write-char
  (let ((port (fdes->outport output)))
    (set-port-buffering port bufpol/none)
    (s48-write-char char port)))

;;; S48's force-output doesn't default to forcing (current-output-port).
(define-r4rs-output (force-output) output s48-force-output
  (values)) ; Do nothing if applied to a file descriptor.

;;; extend channel-i/o's version to fdports
;;; WARNING: evil procedure, bypasses port-lock
(define (port->channel port)
  (fdport-data:channel (fdport-data port)))

(define (format dest cstring . args)
  (if (integer? dest)
      (write-string (apply s48-format #f cstring args) dest)
      (apply s48-format dest cstring args)))

;;; with-current-foo-port procs
;;; ---------------------------

(define (with-current-input-port* port thunk)
  (let-fluid $current-input-port port thunk))

(define (with-current-output-port* port thunk)
  (let-fluid $current-output-port port thunk))

(define (with-current-error-port* port thunk)
  (let-fluid $current-error-port port thunk))

(define (with-error-output-port* port thunk)
  (let-fluid $current-error-port port thunk))

(define-simple-syntax (with-current-input-port port body ...)
  (with-current-input-port* port (lambda () body ...)))

(define-simple-syntax (with-current-output-port port body ...)
  (with-current-output-port* port (lambda () body ...)))

(define-simple-syntax (with-current-error-port port body ...)
  (with-current-error-port* port (lambda () body ...)))

(define-simple-syntax (with-error-output-port port body ...)
  (with-error-output-port* port (lambda () body ...)))

;;; set-foo-port! procs
;;; -------------------
;;; Side-effecting variants of with-current-input-port* and friends.

(define (set-current-input-port!  port) (set-fluid! $current-input-port  port))
(define (set-current-output-port! port) (set-fluid! $current-output-port port))
(define (set-current-error-port!  port) (set-fluid! $current-error-port  port))
(define (set-error-output-port!   port) (set-fluid! $current-error-port  port))


;;; call-with-foo-file with-foo-to-file
;;; -----------------------------------

;;; Copied straight from rts/port.scm, but re-defined in this module,
;;; closed over my versions of open-input-file and open-output-file.

(define (call-with-mumble-file open close)
  (lambda (string proc)
    (let ((port #f))
      (dynamic-wind (lambda ()
                      (if port
                          (warn "throwing back into a call-with-...put-file"
                                string)
                          (set! port (open string))))
                    (lambda () (proc port))
                    (lambda ()
                      (if port
                          (close port)))))))

;;; replace rts/channel-port.scm begin
(define call-with-input-file
  (call-with-mumble-file open-input-file close-input-port))

(define call-with-output-file
  (call-with-mumble-file open-output-file close-output-port))

(define (with-input-from-file string thunk)
  (call-with-input-file string
    (lambda (port)
      (let-fluid $current-input-port port thunk))))

(define (with-output-to-file string thunk)
  (call-with-output-file string
    (lambda (port)
      (let-fluid $current-output-port port thunk))))

;;; replace rts/channel-port.scm end

;;; select
;;; -----

(define (port/fdes->input-port port/fd)
  (if (port? port/fd)
      port/fd
      (fdes->inport port/fd)))

(define (port/fdes->output-port port/fd)
  (if (port? port/fd)
      port/fd
      (fdes->outport port/fd)))

(define (input-port/fdes-ready? port/fd)
  (let ((port (port/fdes->input-port port/fd)))
    ((port-handler-ready? (port-handler port)) port)))

(define (output-port/fdes-ready? port/fd)
  (let ((port (port/fdes->output-port port/fd)))
    ((port-handler-ready? (port-handler port)) port)))

(define (make-any-ready port/fdes-ready?)
  (lambda (port/fds)
    (let loop ((port/fds port/fds))
      (if (null? port/fds)
          '()
          (let ((port/fd (car port/fds)))
            (if (port/fdes-ready? port/fd)
                ;; one is ready, get them all
                (let loop ((rest (cdr port/fds))
                           (ready (list port/fd)))
                  (cond
                   ((null? rest) (reverse ready))
                   ((port/fdes-ready? (car rest))
                    (loop (cdr rest) (cons (car rest) ready)))
                   (else
                    (loop (cdr rest) ready))))
                (loop (cdr port/fds))))))))

(define any-input-ready (make-any-ready input-port/fdes-ready?))
(define any-output-ready (make-any-ready output-port/fdes-ready?))

(define any-channel-ready (make-any-ready fdport-channel-ready?))

(define (make-port/fdes-check-unlocked input?)
  (let ((port/fdes->port
         (if input?
             port/fdes->input-port
             port/fdes->output-port)))
    (lambda (port/fd)
      (if #f                            ;I'll make this right later
          (begin
            ((structure-ref interrupts enable-interrupts!))
            (error "SELECT on port with pending operation"
                   port/fd))))))

(define input-port/fdes-check-unlocked (make-port/fdes-check-unlocked #t))
(define output-port/fdes-check-unlocked (make-port/fdes-check-unlocked #f))

(define (port/fdes->input-channel port/fd)
  (fdport-data:channel
   (fdport-data
    (port/fdes->input-port port/fd))))

(define (port/fdes->output-channel port/fd)
  (fdport-data:channel
   (fdport-data
    (port/fdes->output-port port/fd))))

(define (select read-vec write-vec exception-vec . maybe-timeout)
  (let ((read-list (vector->list read-vec))
        (write-list (vector->list write-vec))
        (timeout (:optional maybe-timeout #f)))

    ((structure-ref interrupts disable-interrupts!))

    (for-each input-port/fdes-check-unlocked read-list)
    (for-each output-port/fdes-check-unlocked write-list)

    (let ((any-read (any-input-ready (filter input-port? read-list)))
          (any-write (any-output-ready (filter output-port? write-list))))

      (if (or (eqv? timeout 0) (pair? any-read) (pair? any-write))
          (begin
            ((structure-ref interrupts enable-interrupts!))
            (values (list->vector any-read)
                    (list->vector any-write)
                    (make-vector 0)))

          ;; we need to block
          (let ((read-channels (map port/fdes->input-channel read-list))
                (write-channels (map port/fdes->output-channel write-list)))

            (for-each (lambda (channel)
                        (add-pending-channel channel #t))
                      read-channels)

            (for-each (lambda (channel)
                        (add-pending-channel channel #f))
                      write-channels)

            (call-with-values
             (lambda ()
               (wait-for-channels read-channels write-channels
                                  (and timeout (* 1000 timeout))))
             ;; re-enables interrupts
             (lambda (ready-read-channels ready-write-channels)
               (let ((ready-read-port/fds '())
                     (ready-write-port/fds '()))
                 (for-each (lambda (port/fd channel)
                             (if (memq channel ready-read-channels)
                                 (set! ready-read-port/fds
                                       (cons port/fd ready-read-port/fds))))
                           read-list read-channels)
                 (for-each (lambda (port/fd channel)
                             (if (memq channel ready-write-channels)
                                 (set! ready-write-port/fds
                                       (cons port/fd ready-write-port/fds))))
                           write-list write-channels)

                 (values (list->vector (reverse ready-read-port/fds))
                         (list->vector (reverse ready-write-port/fds))
                         (make-vector 0))))))))))

(define (select! read-vec write-vec exception-vec . maybe-timeout)
  (let ((read-list (vector->list read-vec))
        (write-list (vector->list write-vec))
        (timeout (:optional maybe-timeout #f)))

    ((structure-ref interrupts disable-interrupts!))

    (for-each input-port/fdes-check-unlocked read-list)
    (for-each output-port/fdes-check-unlocked write-list)

    (let ((any-read (any-input-ready (filter input-port? read-list)))
          (any-write (any-output-ready (filter output-port? write-list))))
      (if (or (eqv? timeout 0) (pair? any-read) (pair? any-write))
          (begin
            ((structure-ref interrupts enable-interrupts!))
            (let ((n-read-ready
                   (let ((length (vector-length read-vec)))
                     (let loop ((i 0) (n 0))
                       (cond
                        ((= i length) n)
                        ((memq (vector-ref read-vec i) any-read)
                         (loop (+ 1 i) (+ 1 n)))
                        (else
                         (vector-set! read-vec i #f)
                         (loop (+ 1 i) n))))))
                  (n-write-ready
                   (let ((length (vector-length write-vec)))
                     (let loop ((i 0) (n 0))
                       (cond
                        ((= i length) n)
                        ((memq (vector-ref write-vec i) any-write)
                         (loop (+ 1 i) (+ 1 n)))
                        (else
                         (vector-set! write-vec i #f)
                         (loop (+ 1 i) n)))))))

              ;; zero out EXCEPTION-VEC
              (let ((length (vector-length exception-vec)))
                (let loop ((i 0))
                  (if (< i length)
                      (begin
                        (vector-set! exception-vec i #f)
                        (loop (+ 1 i))))))

              (values n-read-ready n-write-ready 0)))

          ;; we need to block
          (let ((read-channels (map port/fdes->input-channel read-list))
                (write-channels (map port/fdes->output-channel write-list)))

            (for-each (lambda (channel)
                        (add-pending-channel channel #t))
                      read-channels)

            (for-each (lambda (channel)
                        (add-pending-channel channel #f))
                      write-channels)

            (call-with-values
             (lambda ()
               (wait-for-channels read-channels write-channels
                                  (and timeout (* 1000 timeout))))
             ;; re-enables interrupts
             (lambda (ready-read-channels ready-write-channels)

               (let ((n-read-ready
                      (let loop ((read-channels read-channels)
                                 (n-ready 0)
                                 (index 0))
                        (if (null? read-channels)
                            n-ready
                            (if (memq (car read-channels) ready-read-channels)
                                (loop (cdr read-channels)
                                      (+ 1 n-ready)
                                      (+ 1 index))
                                (begin
                                  (vector-set! read-vec index #f)
                                  (loop (cdr read-channels)
                                        n-ready
                                        (+ 1 index)))))))
                     (n-write-ready
                      (let loop ((write-channels write-channels)
                                 (n-ready 0)
                                 (index 0))
                        (if (null? write-channels)
                            n-ready
                            (if (memq (car write-channels) ready-write-channels)
                                (loop (cdr write-channels)
                                      (+ 1 n-ready)
                                      (+ 1 index))
                                (begin
                                  (vector-set! write-vec index #f)
                                  (loop (cdr write-channels)
                                        n-ready
                                        (+ 1 index))))))))
                 ;; zero out EXCEPTION-VEC
                 (let ((length (vector-length exception-vec)))
                   (let loop ((i 0))
                     (if (< i length)
                         (begin
                           (vector-set! exception-vec i #f)
                           (loop (+ 1 i))))))

                 (values n-read-ready n-write-ready 0)))))))))

(define (verify-select-port-arguments! proc lst)
  (for-each (lambda (p)
              (or (input-port? p)
                  (output-port? p)
                  (call-error "Argument not a port" proc p)))
            lst))

(define (select-ports timeout . ports)
  (verify-select-port-arguments! select-ports ports)
  (let ((read-list (filter input-port? ports))
        (write-list (filter output-port? ports)))

    ((structure-ref interrupts disable-interrupts!))

    (for-each input-port/fdes-check-unlocked read-list)
    (for-each output-port/fdes-check-unlocked write-list)

    (let ((any-read (any-input-ready read-list))
          (any-write (any-output-ready write-list)))

      (if (or (eqv? timeout 0) (pair? any-read) (pair? any-write))
          (begin
            ((structure-ref interrupts enable-interrupts!))
            (append any-read any-write))

          (really-select-port-channels timeout read-list write-list)))))

(define (select-port-channels timeout . ports)
  (verify-select-port-arguments! select-ports ports)
  (let ((read-list (filter input-port? ports))
        (write-list (filter output-port? ports)))

    ((structure-ref interrupts disable-interrupts!))

    (for-each input-port/fdes-check-unlocked read-list)
    (for-each output-port/fdes-check-unlocked write-list)

    (let ((any-read (any-channel-ready read-list))
          (any-write (any-channel-ready write-list)))

      (if (or (eqv? timeout 0) (pair? any-read) (pair? any-write))
          (begin
            ((structure-ref interrupts enable-interrupts!))
            (append any-read any-write))

          (really-select-port-channels timeout read-list write-list)))))

;; assumes interrupts are disabled and that ports aren't locked

(define (really-select-port-channels timeout read-list write-list)
  (let ((read-channels (map port/fdes->input-channel read-list))
        (write-channels (map port/fdes->output-channel write-list)))

    (for-each (lambda (channel)
                (add-pending-channel channel #t))
              read-channels)

    (for-each (lambda (channel)
                (add-pending-channel channel #f))
              write-channels)

    (call-with-values
     (lambda ()
       (wait-for-channels read-channels write-channels
                          (and timeout (* 1000 timeout))))
     ;; re-enables interrupts
     (lambda (ready-read-channels ready-write-channels)
       (append (filter (lambda (read-port)
                         (any (lambda (read-channel)
                                (eq? read-channel
                                     (port/fdes->input-channel read-port)))
                              ready-read-channels))
                       read-list)
               (filter (lambda (write-port)
                         (any (lambda (write-channel)
                                (eq? write-channel
                                     (port/fdes->output-channel write-port)))
                              ready-write-channels))
                       write-list))))))
