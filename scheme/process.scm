;;; Process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we can't algin env here, because exec-path/env calls
;; %%exec/errno directly  F*&% *P

(define (%exec prog arg-list env)
  (let ((env (if env (alist->env-list env) env)))
    (exec-with-alias prog #f env arg-list)))


(import-lambda-definition-2 exit/errno ; errno -- misnomer.
  (status) "scsh_exit")

(import-lambda-definition-2 %exit/errno ; errno -- misnomer
  (status) "scsh__exit")

(define (%exit . maybe-status)
  (%exit/errno (:optional maybe-status 0))
  (error "Yikes! %exit returned."))


(import-lambda-definition-2 %%fork () "scsh_fork")

;;; EXEC support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assumes a low-level %exec procedure:
;;; (%exec prog arglist env)
;;;   ENV is either #t, meaning the current environment, or a string->string
;;;       alist.
;;;   %EXEC stringifies PROG and the elements of ARGLIST.


(define (exec-path-search prog path-list)
  (cond
   ((not (file-name-absolute? prog))
    (let loop ((path-list path-list))
      (if (not (null? path-list))
          (let* ((dir (car path-list))
                 (fname (string-append dir "/" prog)))
            (if (file-executable? fname)
                fname
                (loop (cdr path-list)))))))
   ((file-executable? prog)
    prog)
   (else #f)))

(define (exec/env prog env . arglist)
  (flush-all-ports)
  (with-resources-aligned
   (list environ-resource cwd-resource umask-resource euid-resource egid-resource)
   (lambda ()
     (%exec prog (cons prog arglist) env))))

(define exec-path-list)

(define (init-exec-path-list)
  (set! exec-path-list
        (make-preserved-thread-fluid
         (cond ((getenv "PATH") => split-colon-list)
               (else (warn "Starting up with no path ($PATH).") '())))))

;;; We keep SPLIT-COLON-LIST defined
;;; internally so the top-level startup code (INIT-SCSH) can use it
;;; to split up $PATH without requiring the field-splitter or regexp code.

(define (split-colon-list clist)
  (let ((len (string-length clist)))
    (if (= 0 len) '()                   ; Special case "" -> ().

        ;; Main loop.
        (let split ((i 0))
          (cond ((string-index clist #\: i) =>
                 (lambda (colon)
                   (cons (substring clist i colon)
                         (split (+ colon 1)))))
                (else (list (substring clist i len))))))))

;(define (exec-path/env prog env . arglist)
;  (cond ((exec-path-search (stringify prog) (fluid exec-path-list)) =>
;        (lambda (binary)
;          (apply exec/env binary env arglist)))
;       (else (error "No executable found." prog arglist))))

(define (exec-path/env prog env . arglist)
  (flush-all-ports)
  (with-resources-aligned
   (list environ-resource cwd-resource umask-resource euid-resource egid-resource)
   (lambda ()
     (let ((prog (stringify prog)))
       (if (string-index prog #\/)

           ;; Contains a slash -- no path search.
           (%exec prog (cons prog arglist) env)

           ;; Try each directory in PATH-LIST.
           (for-each
            (lambda (dir)
              (let ((binary (string-append dir "/" prog)))
                (with-handler (lambda (condition more) #f)
                              (lambda ()
                                (exec-with-alias binary #f env (cons prog arglist))))))
            (thread-fluid exec-path-list))))

     (error "No executable found." prog arglist))))

(define (exec-path prog . arglist)
  (apply exec-path/env prog #f arglist))

(define (exec prog . arglist)
  (apply exec/env prog #f arglist))


;;; Assumes niladic primitive %%FORK.

(define (fork . stuff)
  (apply fork-1 #t stuff))

(define (%fork . stuff)
  (apply fork-1 #f stuff))

(define (fork-1 clear-interactive? . rest)
  (let-optionals rest ((maybe-thunk #f)
                       (dont-narrow? #f))
    (really-fork clear-interactive?
                 maybe-thunk)))

(define (preserve-ports thunk)
  (let ((current-input (current-input-port))
        (current-output (current-output-port))
        (current-error (current-error-port)))
    (lambda ()
      (with-current-input-port*
       current-input
       (lambda ()
         (with-current-output-port*
          current-output
          (lambda ()
            (with-current-error-port*
             current-error
             thunk))))))))

(define (really-fork clear-interactive? maybe-thunk)
  (let ((proc #f))
    (if clear-interactive?
        (flush-all-ports))

    ;; There was an atomicity problem/race condition -- if a child
    ;; process died after it was forked, but before the scsh fork
    ;; procedure could register the child's procobj in the
    ;; pid/procobj table, then when the SIGCHLD signal-handler reaped
    ;; the process, there would be no procobj for it.  We now lock
    ;; out interrupts across the %%FORK and NEW-CHILD-PROC
    ;; operations.

    ((with-interrupts-inhibited
      (lambda ()
        ;; with-env-aligned is not neccessary here but it will
        ;; create the environ object in the parent process which
        ;; could reuse it on further forks
        (let ((pid (with-resources-aligned
                    (list environ-resource)
                    %%fork)))
          (if (zero? pid)
              ;; Child
              (lambda ()    ; Do all this outside the WITH-INTERRUPTS.
                (if maybe-thunk
                    (call-terminally maybe-thunk)))
              ;; Parent
              (begin
                (set! proc (new-child-proc pid))
                (lambda ()
                  (values))))))))
    proc))

(define (exit . maybe-status)
  (let ((status (:optional  maybe-status 0)))
    (if (not (integer? status))
        (error "non-integer argument to exit"))
    (call-exit-hooks-and-run
     (lambda ()
       (exit/errno status)
       (display "The evil undead walk the earth." 2)
       (if #t (error "(exit) returned."))))))

;;; Like FORK, but the parent and child communicate via a pipe connecting
;;; the parent's stdin to the child's stdout. This function side-effects
;;; the parent by changing his stdin.

(define (fork/pipe . stuff)
  (really-fork/pipe fork stuff))

(define (%fork/pipe . stuff)
  (really-fork/pipe %fork stuff))

;;; Common code for FORK/PIPE and %FORK/PIPE.
(define (really-fork/pipe forker rest)
  (let-optionals rest ((maybe-thunk #f)
                       (no-new-command-level? #f))
    (receive (r w) (pipe)
      (let ((proc (forker #f no-new-command-level?)))
        (cond (proc             ; Parent
               (close w)
               (move->fdes r 0))
               ;; (set-current-input-port! r)
              (else             ; Child
               (close r)
               (move->fdes w 1)
               (if maybe-thunk
                   (with-current-output-port
                    w
                    (call-terminally maybe-thunk)))))
                   ;; (set-current-output-port! w)
        proc))))


;;; FORK/PIPE with a connection list.
;;; (FORK/PIPE . m-t) = (apply fork/pipe+ '((1 0)) m-t)

(define (%fork/pipe+ conns . stuff)
  (really-fork/pipe+ %fork conns stuff))

(define (fork/pipe+ conns . stuff)
  (really-fork/pipe+ fork conns stuff))

;;; Common code.
(define (really-fork/pipe+ forker conns rest)
  (let-optionals rest ((maybe-thunk #f)
                       (no-new-command-level? #f))
    (let* ((pipes (map (lambda (conn) (call-with-values pipe cons))
                       conns))
           (rev-conns (map reverse conns))
           (froms (map (lambda (conn) (reverse (cdr conn)))
                       rev-conns))
           (tos (map car rev-conns)))

      (let ((proc (forker #f no-new-command-level?)))
        (cond (proc                     ; Parent
               (for-each (lambda (to r/w)
                           (let ((w (cdr r/w))
                                 (r (car r/w)))
                             (close w)
                             (move->fdes r to)))
                         tos pipes))

              (else                     ; Child
               (for-each (lambda (from r/w)
                           (let ((r (car r/w))
                                 (w (cdr r/w)))
                             (close r)
                             (for-each (lambda (fd) (dup w fd)) from)
                             (close w))) ; Unrevealed ports win.
                         froms pipes)
               (if maybe-thunk
                   (call-terminally maybe-thunk))))
        proc))))

(define (tail-pipe a b)
  (fork/pipe a)
  (call-terminally b))

(define (tail-pipe+ conns a b)
  (fork/pipe+ conns a)
  (call-terminally b))

;;; Lay a pipeline, one process for each thunk. Last thunk is called
;;; in this process. PIPE* never returns.

(define (pipe* . thunks)
  (letrec ((lay-pipe (lambda (thunks)
                       (let ((thunk (car thunks))
                             (thunks (cdr thunks)))
                         (if (pair? thunks)
                             (begin (fork/pipe thunk)
                                    (lay-pipe thunks))
                             (call-terminally thunk)))))) ; Last one.
    (if (pair? thunks)
        (lay-pipe thunks)
        (error "No thunks passed to PIPE*"))))

;;; Splice the processes into the i/o flow upstream from us.
;;; First thunk's process reads from our stdin; last thunk's process'
;;; output becomes our new stdin. Essentially, n-ary fork/pipe.
;;;
;;; This procedure is so trivial it isn't included.
;;; (define (pipe-splice . thunks) (for-each fork/pipe thunks))

;;; The classic T 2.0 primitive.
;;; This definition works for procedures running on top of Unix systems.
(define (halts? proc) #t)

; SIGTSTP blows s48 away. ???
(define (suspend) (signal-process 0 (signal stop)))

;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; usleep(3): Try to sleep for USECS microseconds.
;;; sleep(3):  Try to sleep for SECS seconds.

; De-released -- not POSIX and not on SGI systems.
; (define-foreign usleep (usleep (integer usecs)) integer)

(define (process-sleep secs) (process-sleep-until (+ secs (time))))

(define (process-sleep-until when)
  (let* ((when (floor when))    ; Painful to do real->int in Scheme.
         (when (if (exact? when) when (inexact->exact when))))
    (let lp ()
      (or (%sleep-until when) (lp)))))

(import-lambda-definition-2 %sleep-until (secs) "sleep_until")
