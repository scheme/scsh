;;;; Process->Scheme interface forms: run/collecting, run/port, run/string, ...

;;; (run/collecting FDS . EPF)
;;; --------------------------
;;; RUN/COLLECTING and RUN/COLLECTING* run processes that produce multiple
;;; output streams and return ports open on these streams.
;;;
;;; To avoid issues of deadlock, RUN/COLLECTING first runs the process
;;; with output to temp files, then returns the ports open on the temp files.
;;;
;;; (run/collecting (1 2) (ls))
;;; runs ls with stdout (fd 1) and stderr (fd 2) redirected to temporary files.
;;; When ls is done, RUN/COLLECTING returns two ports open on the temporary
;;; files. The files are deleted before RUN/COLLECTING returns, so when
;;; the ports are closed, they vanish.
;;;
;;; The FDS list of file descriptors is implicitly backquoted.
;;;
;;; RUN/COLLECTING* is the procedural abstraction of RUN/COLLECTING.

(define (run/collecting* fds thunk)
  ;; First, generate a pair of ports for each communications channel.
  ;; Each channel buffers through a temp file.
  (let* ((channels (map (lambda (ignore)
                          (call-with-values temp-file-channel cons))
                       fds))
         (read-ports (map car channels))
         (write-ports (map cdr channels))

         ;; In a subprocess, close the read ports, redirect input from
         ;; the write ports, and run THUNK.
         (status (run (begin (for-each close-input-port read-ports)
                             (for-each move->fdes write-ports fds)
                             (thunk)))))

    ;; In this process, close the write ports and return the exit status
    ;; and all the the read ports.
    (for-each close-output-port write-ports)
    (apply values status read-ports)))


;;; Single-stream collectors:
;;; Syntax: run/port, run/file, run/string, run/strings, run/sexp, run/sexps
;;; Procedures: run/port*, run/file*, run/string*, run/strings*, run/sexp*,
;;;             run/sexps*
;;;             port->string, port->string-list, port->sexp-list,
;;;             port->list
;;;
;;; Syntax:
;;; (run/port . epf)
;;;     Fork off the process EPF and return a port on its stdout.
;;; (run/file . epf)
;;;     Run process EPF with stdout redirected into a temp file.
;;;     When the process exits, return the name of the file.
;;; (run/string . epf)
;;;     Read the process' stdout into a string and return it.
;;; (run/strings . epf)
;;;     Run process EPF, reading newline-terminated strings from its stdout
;;;     until EOF. After process exits, return list of strings read. Delimiting
;;;     newlines are trimmed from the strings.
;;; (run/sexp . epf)
;;;     Run process EPF, read and return one sexp from its stdout with READ.
;;; (run/sexps . epf)
;;;     Run process EPF, read sexps from its stdout with READ until EOF.
;;;     After process exits, return list of items read.
;;;
;;; Procedural abstractions:
;;; run/port*, run/file*, run/string*, run/strings*, run/sexp*, run/sexps*
;;;
;;; These are all procedural equivalents for the macros. They all take
;;; one argument: the process to be executed passed as a thunk. For example,
;;; (RUN/PORT . epf) expands into (RUN/PORT* (LAMBDA () (EXEC-EPF . epf)))
;;;
;;; Other useful procedures:
;;;
;;; (port->string port)
;;;     Read characters from port until EOF; return string collected.
;;; (port->string-list port)
;;;     Read newline-terminated strings from port until EOF. Return
;;;     the list of strings collected.
;;; (port->sexp-list port)
;;;     Read sexps from port with READ until EOF. Return list of items read.
;;; (port->list reader port)
;;;     Repeatedly applies READER to PORT, accumulating results into a list.
;;;     On EOF, returns the list of items thus collected.
;;; (port-fold port reader op . seeds)
;;;     Repeatedly read things from PORT with READER. Each time you read
;;;     some value V, compute a new set of seeds with (apply OP V SEEDS).
;;;     (More than 1 seed means OP must return multiple values).
;;;     On eof, return the seeds: (apply value SEEDS).
;;;     PORT->LIST is just (PORT-FOLD PORT READ CONS '())

(define (run/port+proc* thunk)
  (receive (read write) (open-pipe)
    (let ((proc (fork (lambda ()
                        (close-input-port read)
                        (dup2 write 1)
                        (thunk)))))
      (close-output-port write)
      (values read proc))))

(define (run/port* thunk)
  (receive (port proc) (run/port+proc* thunk)
    port))

(define (run/file* thunk)
  (let ((fname (create-temp-file)))
    (run (begin (thunk)) (> ,fname))
    fname))

(define (run/string* thunk)
  (close-after (run/port* thunk) port->string))

(define (run/sexp* thunk)
  (close-after (run/port* thunk) read))

(define (run/sexps* thunk)
  (close-after (run/port* thunk) port->sexp-list))

(define (run/strings* thunk)
  (close-after (run/port* thunk) port->string-list))
