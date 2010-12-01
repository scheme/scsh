;;; A Unix file port system to completely replace S48 file ports.
;;; We use S48 extensible ports.
;;; Copyright (c) 1993 by Olin Shivers.


;;; A functional search tree mapping integer file descriptors to ports. I'm
;;; putting it all in a cell so that reffing and setting can be done provisionally
;;; and be protected by optimistic concurrency.
(define *fdports* (make-cell (make-search-tree = <)))

;;; Sets the port and reveal count for fd, and always replaces if fd was already
;;; in the table.
(define (set-fdport! fd port revealed)
  (atomically!
   (delete-fdport! fd)
   (let ((ports-table (provisional-cell-ref *fdports*)))
     (if (not (zero? revealed))
         (begin (provisional-cell-set! *fdports* (search-tree-insert ports-table fd (cons port revealed)))
                (%set-cloexec fd #f))
         (begin
           (provisional-cell-set! *fdports* (search-tree-insert ports-table fd (cons (make-weak-pointer port) revealed)))
           (%set-cloexec fd #t))))))

;;; Removes fd from the table if it was installed.
(define (delete-fdport! fd)
  (atomically!
   (provisional-cell-set!
    *fdports* (search-tree-delete (provisional-cell-ref *fdports*) fd))))

;;; Returns the port and revealed count for fd in a cons cell (port . revealed).
;;; Returns #f if fd wasn't installed.
(define (maybe-ref-fdport fd)
  (atomically!
   (let* ((ports-table (provisional-cell-ref *fdports*))
          (ref (search-tree-ref ports-table fd)))
     (and ref (if (weak-pointer? (car ref))
                  (let ((val (weak-pointer-ref (car ref))))
                    (if val
                        val
                        (begin (provisional-cell-set! *fdports* (search-tree-delete ports-table fd))
                               #f)))
                  ref)))))

;;; Uses reffer to get a desired value from the cons pair returned by
;;; (maybe-ref-fdport fd)
(define (maybe-ref-fdport-* reffer fd)
  (let ((ref (maybe-ref-fdport)))
    (if ref
        (reffer ref)
        ref)))

;;; Returns the port mapped to fd, or #f if it wasn't installed.
(define (maybe-ref-fdport-port fd)
  (maybe-ref-fdport-* car fd))

;;; Returns fd's revealed count, or #f if it wasn't installed.
(define (maybe-ref-fdport-revealed fd)
  (maybe-ref-fdport-* cdr fd))

(define (make-input-channel fd)
  (open-channel fd "input" (enum channel-status-option input) #t))

(define (make-output-channel fd)
  (open-channel fd "output" (enum channel-status-option output) #t))

(define (close-fdport-channel channel)
  (delete-fdport! (channel-os-index channel))
  (close-channel channel))

(define (make-input-fdport fd revealed)
  (let ((port (input-channel+closer->port (make-input-channel fd) close-fdport-channel)))
    (set-fdport! fd port revealed)
    port))

(define (make-output-fdport fd revealed)
  (let ((port (output-channel+closer->port (make-output-channel fd) close-fdport-channel)))
    (set-fdport! fd port revealed)
    port))

;;; This is now really just a check if x is a channel port, and is
;;; installed in *fdports*
(define (fdport? x)
  (and (or (input-port? x) (output-port? x))
       (fd-port? x)                     ;from posix-i/o
       (maybe-ref-fdport (port->fd x))
       #t))

(define (fdport:revealed fdport)
  (check-arg fdport? fdport fdport:revealed)
  (maybe-ref-fdport-revealed (port->fd fdport)))

(define (set-fdport:revealed! fdport revealed)
  (check-arg fdport? fdport set-fdport:revealed!)
  (set-fdport! (port->fd fdport) fdport revealed))

(define (fdport-channel-ready? fdport)
  (channel-ready? (port->channel fdport)))

;;; Open & Close
;;; ------------

(define (open-file fname flags . maybe-mode)
  (let ((port (s48-open-file fname flags
                             (:optional maybe-mode (integer->file-mode #o666)))))
    (set-fdport! (port->fd port) port 0)
    port))

(define (open-input-file fname . maybe-flags)
  (let ((flags (:optional maybe-flags (file-options))))
    (open-file fname (file-options-union flags (file-options read-only)))))

(define (open-output-file fname . rest)
  (let* ((flags (if (pair? rest) (car rest)
		    (file-options create truncate))) ; default
	 (maybe-mode (if (null? rest) '() (cdr rest)))
	 (flags (file-options-union flags (file-options write-only))))
    (apply open-file fname flags maybe-mode)))

(define (increment-revealed-count port delta)
  (atomically!
   (let* ((count (fdport:revealed port))
          (newcount (+ count delta)))
     (set-fdport:revealed! port newcount))))

(define (release-port-handle port)
  (check-arg fdport? port release-port-handle)
  (atomically!
   (let ((rev (fdport:revealed port)))
     (if (not (zero? rev))
         (let ((new-rev (- rev 1)))
           (set-fdport:revealed! port new-rev))))))

(define (port-revealed port)
  (let ((count (fdport:revealed (check-arg fdport? port port-revealed))))
    (and (not (zero? count)) count)))

(define (fdes->port fd port-maker) ; local proc.
  (cond  ((maybe-ref-fdport-port fd) =>
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
  (increment-revealed-count port 1)
  (port->fd port))

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
              ((fd-port? fd/port) (port->fd fd/port)) ;notice fd-port? instead of fdport?
              (else (error "Not a file descriptor or fdport." fd/port)))))

;;; Random predicates and arg checkers
;;; ----------------------------------

(define (open-fdport? x)
  (and (fdport? x) (or (open-output-port? x) (open-input-port? x))))

(define (fdport-open? port)
  (check-arg fdport? port fdport-open?)
  (eq? (channel-status (port->channel port))
       (enum channel-status-option closed)))

;;; Initialise the system
;;; ---------------------

;;; JMG: should be deprecated-proc
(define error-output-port
  current-error-port)


(define (init-fdports!)
  (set-fdport! (port->fd (current-input-port)) (current-input-port) 1)
  (set-fdport! (port->fd (current-output-port)) (current-output-port) 1)
  (set-fdport! (port->fd (current-error-port)) (current-error-port) 1))

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
  (cond ((maybe-ref-fdport-port fd) =>       ; Shouldn't bump the revealed count.
         (lambda (port)
             (%move-fdport (port->fd (dup port)) port 0)  ;s48's dup modifies port's channel for us
             #t))
        (else #f)))

(define (%move-fdport old-fd port new-revealed)
  (delete-fdport! old-fd)
  (set-fdport! (port->fd port) port new-revealed)
  #f)  ; JMG: It used to return #f on succes in 0.5.1, so we do the same

(define (close-fdes fd)
  (if (evict-ports fd)
      #t ; EBADF should not occur if there is a port
      (%close-fdes fd)))

(define (flush-fdport fdport)
  (force-output (check-arg fdport? fdport flush-fdport)))

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
               (if (thread-continuation
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
        (current-thread))
       (thunk)))
    (placeholder-value placeholder)))

(define (flush-all-ports-no-threads)
  (let ((thunks (output-port-forcers #f)))
    (for-each (lambda (thunk) (thunk)) thunks)))

;;; Extend R4RS i/o ops to handle file descriptors.
;;; -----------------------------------------------
;;; Will probably need to limit buffers somehow..

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
    (s48-read-char port)))

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
    (s48-newline port)))

(define-r4rs-output (write object) output s48-write
  (let ((sp (make-string-output-port)))
    (write object sp)
    (write-string (string-output-port-output sp) output)))

(define-r4rs-output (write-char char) output s48-write-char
  (let ((port (fdes->outport output)))
    (s48-write-char char port)))

;;; S48's force-output doesn't default to forcing (current-output-port).
(define-r4rs-output (force-output) output s48-force-output
  (values)) ; Do nothing if applied to a file descriptor.

(define (format dest cstring . args)
  (if (integer? dest)
      (write-string (apply s48-format #f cstring args) dest)
      (apply s48-format dest cstring args)))

;;; with-current-foo-port procs
;;; ---------------------------

(define (with-current-input-port* port thunk)
  (call-with-current-input-port port thunk))

(define (with-current-output-port* port thunk)
  (call-with-current-output-port port thunk))

(define (with-current-error-port* port thunk)
  (call-with-current-noise-port port thunk))

(define (with-error-output-port* port thunk)
  (call-with-current-noise-port port thunk))

(define-simple-syntax (with-current-input-port port body ...)
  (with-current-input-port* port (lambda () body ...)))

(define-simple-syntax (with-current-output-port port body ...)
  (with-current-output-port* port (lambda () body ...)))

(define-simple-syntax (with-current-error-port port body ...)
  (with-current-error-port* port (lambda () body ...)))

(define-simple-syntax (with-error-output-port port body ...)
  (with-error-output-port* port (lambda () body ...)))

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

(define (port/fdes->input-channel port/fd)
  (port->channel
   (port/fdes->input-port port/fd)))

(define (port/fdes->output-channel port/fd)
  (port->channel
   (port/fdes->output-port port/fd)))

(define (select read-vec write-vec exception-vec . maybe-timeout)
  (let ((read-list (vector->list read-vec))
        (write-list (vector->list write-vec))
        (timeout (:optional maybe-timeout #f)))

    (disable-interrupts!)

    (let ((any-read (any-input-ready (filter input-port? read-list)))
          (any-write (any-output-ready (filter output-port? write-list))))

      (if (or (eqv? timeout 0) (pair? any-read) (pair? any-write))
          (begin
            (enable-interrupts!)
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

    (disable-interrupts!)

    (let ((any-read (any-input-ready (filter input-port? read-list)))
          (any-write (any-output-ready (filter output-port? write-list))))
      (if (or (eqv? timeout 0) (pair? any-read) (pair? any-write))
          (begin
            (enable-interrupts!)
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

    (disable-interrupts!)

    (let ((any-read (any-input-ready read-list))
          (any-write (any-output-ready write-list)))

      (if (or (eqv? timeout 0) (pair? any-read) (pair? any-write))
          (begin
            (enable-interrupts!)
            (append any-read any-write))

          (really-select-port-channels timeout read-list write-list)))))

(define (select-port-channels timeout . ports)
  (verify-select-port-arguments! select-ports ports)
  (let ((read-list (filter input-port? ports))
        (write-list (filter output-port? ports)))

    (disable-interrupts!)

    (let ((any-read (any-channel-ready read-list))
          (any-write (any-channel-ready write-list)))

      (if (or (eqv? timeout 0) (pair? any-read) (pair? any-write))
          (begin
            (enable-interrupts!)
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

;;; I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define seek/set 0)                     ;Unix codes for "whence"
(define seek/delta 1)
(define seek/end 2)

(define (seek fd/port offset . maybe-whence)
  (if (and (open-input-port? fd/port)
           (> (byte-vector-length (port-buffer fd/port)) 1))
      (error "Seek does currently not work on buffered ports" fd/port))
  (if (and (open-output-port? fd/port)
           (> (byte-vector-length (port-buffer fd/port)) 0))
      (error "Seek does currently not work on buffered ports" fd/port))
  (let ((whence (:optional maybe-whence seek/set))
        (fd (if (integer? fd/port) fd/port (port->fdes fd/port))))
    (%fd-seek fd offset whence)))

(define (tell fd/port)
  (let ((fd (if (integer? fd/port) fd/port (port->fdes fd/port))))
    (%fd-seek fd 0 seek/delta)))

(define (open-fdes path flags . maybe-mode) ; mode defaults to 0666
    (with-resources-aligned
     (list cwd-resource umask-resource euid-resource egid-resource)
     (lambda ()
      (%open path flags (:optional maybe-mode #o666)))))

(import-lambda-definition-2 pipe-fdes () "scheme_pipe")

(define (pipe)
  (apply (lambda (r-fd w-fd)
           (let ((r (fdes->inport  r-fd))
                 (w (fdes->outport w-fd)))
             (release-port-handle r)
             (release-port-handle w)
             (values r w)))
         (pipe-fdes)))
