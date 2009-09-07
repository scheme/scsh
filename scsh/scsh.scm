;;; A Scheme shell.
;;; Copyright (c) 1992 by Olin Shivers.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

;;; Call THUNK, then die.
;;; A clever definition in a clever implementation allows the caller's stack
;;; and dynamic env to be gc'd away, since this procedure never returns.

(define (call-terminally thunk)
  (with-continuation
   null-continuation
   (lambda ()
     (with-handler
      (lambda (c more)
        (display-condition c (current-error-port))
        (exit 1))
     (lambda ()
       (dynamic-wind
        (lambda () (values))
        thunk
        (lambda () (exit 0))))))))

;; from shift-reset.scm:
(define null-continuation #f)

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
               (move->fdes r 0)
               (set-current-input-port! r))
              (else             ; Child
               (close r)
               (move->fdes w 1)
               (if maybe-thunk
                   (with-current-output-port
                    w
                    (call-terminally maybe-thunk))
                   (set-current-output-port! w))))
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




;;; Should be moved to somewhere else
(define (with-lock lock thunk)
  (dynamic-wind
   (lambda ()
     (obtain-lock lock))
   thunk
   (lambda ()
     (release-lock lock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; working directory per thread

(define *cwd-cache* 'uninitialized)
(define cwd-lock (make-lock))

(define (initialize-cwd)
  (set! *cwd-cache* (process-cwd))
  (set! $cwd ;;; TODO The old thread-fluid will remain
        (make-preserved-thread-fluid
         (cwd-cache))))
;  (set! cwd-lock (make-lock)))

(define (cwd-cache)
  *cwd-cache*)

;; Actually do the syscall and update the cache
;; assumes the cwd lock obtained
(define (change-and-cache-cwd new-cwd)
  (if (not (file-name-absolute? new-cwd))
      (process-chdir (string-append (cwd) "/" new-cwd))
      (process-chdir new-cwd))
  (set! *cwd-cache* (process-cwd)))

;; The thread-specific cwd: A thread fluid

(define $cwd 'empty-cwd-value)

(define (cwd) (thread-fluid $cwd))
(define (thread-set-cwd! cwd) (set-thread-fluid! $cwd cwd))
(define (let-cwd cwd thunk)
  (let-thread-fluid $cwd cwd thunk))

(define (with-cwd* new-cwd thunk)
  (let ((changed-cwd
         (with-lock cwd-lock
           (lambda ()
             (change-and-cache-cwd new-cwd)
             (cwd-cache)))))
    (let-cwd changed-cwd thunk)))

;; Align the value of the Unix cwd with scsh's value.
;; Since another thread could disalign, this call and
;; any ensuring syscall that relies upon it should
;; be "glued together" with the cwd lock.

(define (align-cwd!)
  (let ((thread-cwd (cwd)))
    (if (not (string=? thread-cwd (cwd-cache)))
        (change-and-cache-cwd thread-cwd))))

(define (chdir . maybe-dir)
  (let ((dir (:optional maybe-dir (home-dir))))
    (with-lock cwd-lock
      (lambda ()
        (change-and-cache-cwd dir)
        (thread-set-cwd! (cwd-cache))))))

(define-record-type resource :resource
  (make-resource align! lock)
  resource?
  (align! resource-align!)
  (lock resource-lock))

(define (with-resources-aligned resources thunk)
   (let ((locks (map resource-lock resources)))
     (apply obtain-all-or-none locks)
     (for-each
      (lambda (align!) (align!))
      (map resource-align! resources))
     (let ((val (with-handler
                 (lambda (cond more)
                   (for-each release-lock locks)
                   (more))
                 thunk)))
       (for-each release-lock locks)
       val)))

(define cwd-resource (make-resource align-cwd! cwd-lock))

;; example syscall
;; (define (exported-delete-file fname)
;;  (with-cwd-aligned (really-delete-file fname)))

(define cwd-reinitializer
  (make-reinitializer (lambda () (initialize-cwd))))


(initialize-cwd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; umask per thread
(define *umask-cache* 'uninitialized)
(define umask-lock (make-lock))

(define (initialize-umask)
  (set! *umask-cache* (process-umask))
  (set! $umask ;;; TODO The old thread-fluid will remain
        (make-preserved-thread-fluid
         (umask-cache))))
;  (set! umask-lock (make-lock)))

(define (umask-cache)
  *umask-cache*)

;; Actually do the syscall and update the cache
;; assumes the resource lock obtained
(define (change-and-cache-umask new-umask)
  (set-process-umask new-umask)
  (set! *umask-cache* (process-umask)))

;; The thread-specific umask: A thread fluid

(define $umask 'empty-umask-value)

(define (umask) (thread-fluid $umask))
(define (thread-set-umask! new-umask) (set-thread-fluid! $umask new-umask))
(define (let-umask new-umask thunk)
  (let-thread-fluid $umask new-umask thunk))

(define (with-umask* new-umask thunk)
  (let ((changed-umask
         (with-lock umask-lock
           (lambda ()
             (change-and-cache-umask new-umask)
             (umask-cache)))))
    (let-umask changed-umask thunk)))

(define (align-umask!)
  (let ((thread-umask (umask)))
    (if (not (= thread-umask (umask-cache)))
        (change-and-cache-umask thread-umask))))

(define (set-umask new-umask)
  (with-lock umask-lock
    (lambda ()
      (change-and-cache-umask new-umask)
      (thread-set-umask! (umask-cache)))))

(define umask-resource (make-resource align-umask! umask-lock))

(define umask-reinitializer
  (make-reinitializer (lambda () (initialize-umask))))


(initialize-umask)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; effective uid and gid per thread

(define-syntax make-Xid-resource
  (syntax-rules ()
    ((make-Xid-resource
      process-user-effective-Xid set-process-user-effective-Xid
      process-set-Xid set-Xid
      align-eXid! eXid-resource
      user-effective-Xid set-user-effective-Xid with-user-effective-Xid*)
     (begin
       (define *eXid-cache* 'uninitialized)
       (define eXid-lock (make-lock))

       (define (initialize-eXid)
         (set! *eXid-cache* (process-user-effective-Xid))
         (set! $eXid
               (make-preserved-thread-fluid
                (eXid-cache))))

       (define (eXid-cache)
         *eXid-cache*)

;; Actually do the syscall and update the cache
;; assumes the resource lock obtained
       (define (change-and-cache-eXid new-eXid)
         (set-process-user-effective-Xid new-eXid)
         (set! *eXid-cache* (process-user-effective-Xid)))

;; The thread-specific eXid: A thread fluid

       (define $eXid 'empty-eXid-value)

       (define (user-effective-Xid) (thread-fluid $eXid))
       (define (thread-set-eXid! new-eXid) (set-thread-fluid! $eXid new-eXid))
       (define (let-eXid new-eXid thunk)
         (let-thread-fluid $eXid new-eXid thunk))

;; set-Xid will affect the effective X id
       (define (set-Xid Xid)
         (with-lock eXid-lock
           (lambda ()
             (process-set-Xid Xid)
             (set! *eXid-cache* (process-user-effective-Xid))
             (thread-set-eXid! *eXid-cache*))))

       (define (with-user-effective-Xid* new-eXid thunk)
         (let ((changed-eXid
                (with-lock eXid-lock
                  (lambda ()
                    (change-and-cache-eXid new-eXid)
                    (eXid-cache)))))
           (let-eXid changed-eXid thunk)))

       (define (align-eXid!)
         (let ((thread-eXid (user-effective-Xid)))
           (if (not (= thread-eXid (eXid-cache)))
               (change-and-cache-eXid thread-eXid))))

       (define (set-user-effective-Xid new-eXid)
         (with-lock eXid-lock
           (lambda ()
             (change-and-cache-eXid new-eXid)
             (thread-set-eXid! (eXid-cache)))))

       (define eXid-resource (make-resource align-eXid! eXid-lock))

       (define eXid-reinitializer
         (make-reinitializer (lambda () (initialize-eXid))))

       (initialize-eXid)
       ))))

(make-Xid-resource
 process-user-effective-uid set-process-user-effective-uid
 process-set-uid set-uid
 align-euid! euid-resource
 user-effective-uid set-user-effective-uid with-user-effective-uid*)

(make-Xid-resource
 process-user-effective-gid set-process-user-effective-gid
 process-set-gid set-gid
 align-egid! egid-resource
 user-effective-gid set-user-effective-gid with-user-effective-gid*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment per thread
(define *env-cache* 'uninitialized)
(define env-lock (make-lock))

(define (install-env)
  (set! *env-cache* (environ**-read))
  (set! $env ;;; TODO The old thread-fluid will remain
        (make-preserved-thread-fluid
         (env-cache))))
;  (set! env-lock (make-lock)))

(define (env-cache)
  *env-cache*)

;; Actually do the syscall and update the cache
;; assumes the env lock obtained
(define (change-and-cache-env env)
  (environ**-set env)
  (set! *env-cache* env))

;; The thread-specific env: A thread fluid

(define $env 'empty-env-value)

(define (thread-read-env) (thread-fluid $env))
(define (thread-set-env! res) (set-thread-fluid! $env res))
(define (let-env res thunk)
  (let-thread-fluid $env res thunk))

(define (really-with-env* env thunk)
  (with-lock env-lock
    (lambda ()
      (change-and-cache-env env)))
  (let-env env thunk))

(define (align-env!)
  (let ((res (thread-read-env)))
    (if (not (env=? res (env-cache)))
        (change-and-cache-env res))))

(define (thread-change-env res)
  (with-lock env-lock
    (lambda ()
      (change-and-cache-env res)
      (thread-set-env! (env-cache)))))

(define environ-resource (make-resource align-env! env-lock))

(define env-reinitializer
  (make-reinitializer install-env))


(define-record env
      envvec
      alist)    ; Corresponding alist

(define-record-resumer type/env
  (lambda (env)
    (set-env:envvec env #f)))

(define (env=? e1 e2)
  (and (env:envvec e1)
       (eq? (env:envvec e1)
            (env:envvec e2))))

(define-record envvec
  environ ;; char**
  )

(define (add-envvec-finalizer! envvec)
  (add-finalizer! envvec envvec-finalizer))

(define-exported-binding "envvec-record-type" type/envvec)
(define-exported-binding "add-envvec-finalizer!" add-envvec-finalizer!)

(define (envvec-finalizer envvec)
  (%free-env envvec))

(define (environ**-read)
  (let ((alist.envvec (environ-env->alist)))
    (make-env (cdr alist.envvec) (car alist.envvec))))

(define (environ**-set env)
   (if (env:envvec env)
       (%align-env (env:envvec env))
       (set-env:envvec env (envvec-alist->env (env:alist env)))))

(define (getenv var)
  (let* ((env (thread-read-env))
         (res (assoc var (env:alist env))))
    (if res (cdr res) res)))

(define (env->alist)
  (env:alist (thread-read-env)))

(define (setenv var val)
  (let* ((env (thread-read-env))
         (alist (if val
                    (alist-update
                     var
                     val
                    (env:alist env))
                    (alist-delete
                     var
                     (env:alist env)))))
    (thread-set-env!
     (make-env
      #f
      alist))))

(define (alist->env alist)
  (thread-set-env!
   (make-env
    #f
    alist)))

(define (with-env* alist-delta thunk)
  (let ((new-env (fold (lambda (key/val alist)
                          (alist-update (car key/val) (cdr key/val) alist))
                        (env->alist)
                        alist-delta)))
    (with-total-env* new-env thunk)))

(define (with-total-env* alist thunk)
  (really-with-env* (make-env #f alist) thunk))


;;; These two functions are obsoleted by the more general INFIX-SPLITTER and
;;; JOIN-STRINGS functions. However, we keep SPLIT-COLON-LIST defined
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

;;; Unix colon lists typically use colons as separators, which
;;; is not as clean to deal with as terminators, but that's Unix.
;;; Note ambiguity: (s-l->c-l '()) = (s-l->c-l '("")) = "".

; (define (string-list->colon-list slist)
;   (if (pair? slist)
;       (apply string-append
;            (let colonise ((lis slist))        ; LIS is always
;              (let ((tail (cdr lis)))          ; a pair.
;                (cons (car lis)
;                      (if (pair? tail)
;                          (cons ":" (colonise tail))
;                          '())))))
;       ""))    ; () case.


(define (alist-delete key alist)
  (filter (lambda (key/val) (not (equal? key (car key/val)))) alist))

(define (alist-update key val alist)
  (cons (cons key val)
        (alist-delete key alist)))

;;; Remove shadowed entries from ALIST. Preserves element order.
;;; (This version shares no structure.)

(define (alist-compress alist)
  (reverse (let compress ((alist alist) (ans '()))
             (if (pair? alist)
                 (let ((key/val (car alist))
                       (alist (cdr alist)))
                   (compress alist (if (assoc (car key/val) ans) ans
                                       (cons key/val ans))))
                 ans))))

(define (add-before elt before list)
  (let rec ((list list))
    (if (pair? list)
        (let ((x (car list)))
          (if (equal? x before)
              (cons elt list)
              (cons x (rec (cdr list)))))
        (cons elt list))))

;;; In ADD-AFTER, the labelled LET adds ELT after the last occurrence of AFTER
;;; in LIST, and returns the list. However, if the LET finds no occurrence
;;; of AFTER in LIST, it returns #F instead.

(define (add-after elt after list)
  (or (let rec ((list list))
        (if (pair? list)
            (let* ((x (car list))
                   (tail (cdr list))
                   (ans (rec tail))) ; #f if AFTER wasn't encountered.
              (cond (ans (cons x ans))
                    ((equal? x after)
                     (cons x (cons elt tail)))
                    (else #f)))         ; AFTER doesn't appear in LIST.
            #f))                        ; AFTER doesn't appear in LIST.
      (cons elt list)))

;;; Sugar:

(define-simple-syntax (with-cwd dir . body)
  (with-cwd* dir (lambda () . body)))

(define-simple-syntax (with-umask mask . body)
  (with-umask* mask (lambda () . body)))

(define-simple-syntax (with-env delta . body)
  (with-env* `delta (lambda () . body)))

(define-simple-syntax (with-total-env env . body)
  (with-total-env* `env (lambda () . body)))

(define-simple-syntax (with-user-effective-uid uid . body)
  (with-user-effective-uid* uid (lambda () . body)))

(define-simple-syntax (with-user-effective-gid gid . body)
  (with-user-effective-gid* gid (lambda () . body)))

(define (call/temp-file writer user)
  (let ((fname #f))
    (dynamic-wind
      (lambda () (if fname (error "Can't wind back into a CALL/TEMP-FILE")
                     (set! fname (create-temp-file))))
      (lambda ()
        (with-output-to-file fname writer)
        (user fname))
      (lambda () (if fname (delete-file fname))))))

;;; Create a new temporary file and return its name.
;;; The optional argument specifies the filename prefix to use, and defaults
;;; to "/tmp/<pid>.", where <pid> is the current process' id. The procedure
;;; scans through the files named <prefix>0, <prefix>1, ... until it finds a
;;; filename that doesn't exist in the filesystem. It creates the file with
;;; permission #o600, and returns the filename.
;;;

(define (create-temp-file . maybe-prefix)
  (let ((oflags (bitwise-ior open/write
                             (bitwise-ior open/create open/exclusive))))
    (apply temp-file-iterate
           (lambda (fname)
             (close-fdes (open-fdes fname oflags #o600))
             fname)
           (if (null? maybe-prefix) '()
               (list (string-append (constant-format-string (car maybe-prefix))
                                    ".~a"))))))

(define (initial-temp-file)
  (let ((tmpdir (getenv "TMPDIR")))
    (string-append
     (if tmpdir
         tmpdir
         "/var/tmp")
     "/"
     (number->string (pid))
     "~a")))

(define *temp-file-template* (make-fluid 'not-initialized-temp-file-template))

(define temp-file-reinitializer
  (make-reinitializer
   (lambda ()
     (set-fluid! *temp-file-template* (initial-temp-file)))))

(define (temp-file-iterate maker . maybe-template)
  (let ((template (:optional maybe-template (fluid *temp-file-template*))))
    (let loop ((i 0))
      (if (> i 1000) (error "Can't create temp-file")
          (let ((fname (format #f template (number->string i))))
            (receive retvals (with-errno-handler
                               ((errno data)
                                ((errno/exist errno/acces) #f))
                               (maker fname))
              (if (car retvals) (apply values retvals)
                  (loop (+ i 1)))))))))


;; Double tildes in S.
;; Using the return value as a format string will output exactly S.
(define (constant-format-string s)      ; Ugly code. Would be much clearer
  (let* ((len (string-length s))        ; if written with string SRFI.
         (tilde? (lambda (s i) (char=? #\~ (string-ref s i))))
         (newlen (do ((i (- len 1) (- i 1))
                      (ans 0 (+ ans (if (tilde? s i) 2 1))))
                     ((< i 0) ans)))
         (fs (make-string newlen)))
    (let lp ((i 0) (j 0))
      (cond ((< i len)
             (let ((j (cond ((tilde? s i) (string-set! fs j #\~) (+ j 1))
                            (else j))))
               (string-set! fs j (string-ref s i))
               (lp (+ i 1) (+ j 1))))))
    fs))


;;; Roughly equivalent to (pipe).
;;; Returns two file ports [iport oport] open on a temp file.
;;; Use this when you may have to buffer large quantities between
;;; writing and reading. Note that if the consumer gets ahead of the
;;; producer, it won't hang waiting for input, it will just return
;;; EOF. To play it safe, make sure that the producer runs to completion
;;; before starting the consumer.
;;;
;;; The temp file is deleted before TEMP-FILE-CHANNEL returns, so as soon
;;; as the ports are closed, the file's disk storage is reclaimed.

(define (temp-file-channel)
  (let* ((fname (create-temp-file))
         (iport (open-input-file fname))
         (oport (open-output-file fname)))
    (delete-file fname)
    (values iport oport)))


;; Return a Unix port such that reads on it get the chars produced by
;; DISPLAYing OBJ. For example, if OBJ is a string, then reading from
;; the port produces the characters of OBJ.
;;
;; This implementation works by writing the string out to a temp file,
;; but that isn't necessary. It could work, for example, by forking off a
;; writer process that outputs to a pipe, i.e.,
;;     (run/port (begin (display obj (fdes->outport 1))))

(define (open-string-source obj)
  (receive (inp outp) (temp-file-channel)
    (display obj outp)
    (close-output-port outp)
    inp))


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
  (receive (r w) (pipe)
    (let ((proc (fork (lambda ()
                        (close r)
                        (move->fdes w 1)
                        (with-current-output-port* w thunk)))))
      (close w)
      (values r proc))))

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


;;; Read characters from PORT until EOF, collect into a string.

(define (port->string port)
  (let ((sc (make-string-collector)))
    (letrec ((lp (lambda ()
                   (cond ((read-string 1024 port) =>
                          (lambda (s)
                            (collect-string! sc s)
                            (lp)))
                         (else (string-collector->string sc))))))
      (lp))))

;;; (loop (initial (sc (make-string-collector)))
;;;       (bind (s (read-string 1024 port)))
;;;       (while s)
;;;       (do (collect-string! sc s))
;;;       (result (string-collector->string sc)))

;;; Read items from PORT with READER until EOF. Collect items into a list.

(define (port->list reader port)
  (let lp ((ans '()))
    (let ((x (reader port)))
      (if (eof-object? x) (reverse! ans)
          (lp (cons x ans))))))

(define (port->sexp-list port)
  (port->list read port))

(define (port->string-list port)
  (port->list read-line port))

(define (port-fold port reader op . seeds)
  (letrec ((fold (lambda seeds
                     (let ((x (reader port)))
                       (if (eof-object? x) (apply values seeds)
                           (call-with-values (lambda () (apply op x seeds))
                                             fold))))))
    (apply fold seeds)))

(define reduce-port
  (deprecated-proc port-fold 'reduce-port "Use port-fold instead."))

;;; Not defined:
;;; (field-reader field-delims record-delims)
;;; Returns a reader that reads strings delimited by 1 or more chars from
;;; the string FIELD-DELIMS. These strings are collected in a list until
;;; eof or until 1 or more chars from RECORD-DELIMS are read. Then the
;;; accumulated list of strings is returned. For example, if we want
;;; a procedure that reads one line of input, splitting it into
;;; whitespace-delimited strings, we can use
;;;     (field-reader " \t" "\n")
;;; for a reader.



;; Loop until EOF reading characters or strings and writing (FILTER char)
;; or (FILTER string). Useful as an arg to FORK or FORK/PIPE.

(define (make-char-port-filter filter)
  (lambda ()
    (let lp ()
      (let ((c (read-char)))
        (if (not (eof-object? c))
            (begin (write-char (filter c))
                   (lp)))))))

(define (make-string-port-filter filter . maybe-buflen)
  (let* ((buflen (:optional maybe-buflen 1024))
         (buf (make-string buflen)))
    (lambda ()
      (let lp ()
        (cond ((read-string! buf (current-input-port) 0 buflen) =>
               (lambda (nread)
                 (display (filter (if (= nread buflen) buf
                                      (substring buf 0 nread)))) ; last one.
                 (lp))))))))

(define (y-or-n? question . maybe-eof-value)
  (let loop ((count *y-or-n-eof-count*))
    (display question)
    (display " (y/n)? ")
    (let ((line (read-line)))
      (cond ((eof-object? line)
             (newline)
             (if (= count 0)
                 (:optional maybe-eof-value (error "EOF in y-or-n?"))
                 (begin (display "I'll only ask another ")
                        (write count)
                        (display " times.")
                        (newline)
                        (loop (- count 1)))))
            ((< (string-length line) 1) (loop count))
            ((char=? (string-ref line 0) #\y) #t)
            ((char=? (string-ref line 0) #\n) #f)
            (else (loop count))))))

(define *y-or-n-eof-count* 100)


;;; Stdio/stdport sync procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stdio->stdports)
  (set-current-input-port!  (fdes->inport 0))
  (set-current-output-port! (fdes->outport 1))
  (set-current-error-port!   (fdes->outport 2)))

(define (with-stdio-ports* thunk)
  (with-current-input-port (fdes->inport 0)
    (with-current-output-port (fdes->outport 1)
      (with-current-error-port (fdes->outport 2)
        (thunk)))))

(define-simple-syntax (with-stdio-ports body ...)
  (with-stdio-ports* (lambda () body ...)))


(define (stdports->stdio)
  (dup (current-input-port)  0)
  (dup (current-output-port) 1)
  (dup (current-error-port)   2))


;;; Command-line argument access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some globals.
(define %command-line '())              ; Includes program.
(define command-line-arguments #f)      ; Doesn't include program.

(define (set-command-line-args! args)
  (set! %command-line args)
  (set! command-line-arguments (append (cdr args) '())))

(define (arg* arglist n . maybe-default-thunk)
  (let ((oops (lambda () (error "argument out of bounds" arglist n))))
    (if (< n 1) (oops)
        (let lp ((al arglist) (n n))
          (if (pair? al)
              (if (= n 1) (car al)
                  (lp (cdr al) (- n 1)))
              (if (and (pair? maybe-default-thunk)
                       (null? (cdr maybe-default-thunk)))
                  ((car maybe-default-thunk))
                  (oops)))))))

(define (arg arglist n . maybe-default)
  (if maybe-default (arg* arglist n (lambda () (car maybe-default)))
      (arg* arglist n)))

(define (argv n . maybe-default)
  (apply arg %command-line (+ n 1) maybe-default))

(define (command-line) (append %command-line '()))

;;; EXEC support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assumes a low-level %exec procedure:
;;; (%exec prog arglist env)
;;;   ENV is either #t, meaning the current environment, or a string->string
;;;       alist.
;;;   %EXEC stringifies PROG and the elements of ARGLIST.

(define (stringify thing)
  (cond ((string? thing) thing)
        ((symbol? thing)
         (symbol->string thing))
;       ((symbol? thing)
;        (list->string (map char-downcase
;                           (string->list (symbol->string thing)))))
        ((integer? thing)
         (number->string thing))
        (else (error "Can only stringify strings, symbols, and integers."
                     thing))))

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
           (for-each (lambda (dir)
                       (let ((binary (string-append dir "/" prog)))
                         (exec-with-alias binary #f env (cons prog arglist))))
                     (thread-fluid exec-path-list))))

     (error "No executable found." prog arglist))))

(define (exec-path prog . arglist)
  (apply exec-path/env prog #t arglist))

(define (exec prog . arglist)
  (apply exec/env prog #t arglist))


;;; Assumes niladic primitive %%FORK.

(define (fork . stuff)
  (apply fork-1 #t stuff))

(define (%fork . stuff)
  (apply fork-1 #f stuff))

(define (fork-1 clear-interactive? . rest)
  (let-optionals rest ((maybe-thunk #f)
                       (dont-narrow? #f))
    (really-fork clear-interactive?
                 (not dont-narrow?)
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

(define (really-fork clear-interactive? narrow? maybe-thunk)
  (let ((proc #f)
        (maybe-narrow
         (if narrow?
             (lambda (thunk)
               ;; narrow loses the thread fluids and the dynamic environment
               (narrow (preserve-ports (preserve-thread-fluids thunk))
                       'forking))
             (lambda (thunk) (thunk)))))
    (maybe-narrow
     (lambda ()

       (if clear-interactive?
           (flush-all-ports))

       ;; There was an atomicity problem/race condition -- if a child
       ;; process died after it was forked, but before the scsh fork
       ;; procedure could register the child's procobj in the
       ;; pid/procobj table, then when the SIGCHLD signal-handler reaped
       ;; the process, there would be no procobj for it.  We now lock
       ;; out interrupts across the %%FORK and NEW-CHILD-PROC
       ;; operations.

       (((structure-ref interrupts with-interrupts-inhibited)
         (lambda ()
           ;; with-env-aligned is not neccessary here but it will
           ;; create the environ object in the parent process which
           ;; could reuse it on further forks
           (let ((pid (with-resources-aligned
                       (list environ-resource)
                       %%fork)))
             (if (zero? pid)
                 ;; Child
                 (lambda ()             ; Do all this outside the WITH-INTERRUPTS.
                   (if narrow?
                       (begin
                         ;; ugly kludge:
                         ;; the REPL thread is not running any more,
                         ;; hence unlock its command ports
                         (release-port-lock (command-input))
                         (release-port-lock (command-output))
                         (release-port-lock (command-error-output))))
                   ;; There is no session if parent was started in batch-mode
                   (if (and (session-started?) clear-interactive?)
                       (set-batch-mode?! #t)) ; Children are non-interactive.
                   (if maybe-thunk
                       (call-terminally maybe-thunk)))
                 ;; Parent
                 (begin
                   (set! proc (new-child-proc pid))
                   (lambda ()
                     (values))))))))))
    proc))

(define (exit . maybe-status)
  (let ((status (:optional  maybe-status 0)))
    (if (not (integer? status))
        (error "non-integer argument to exit"))
    (call-exit-hooks-and-narrow
     (lambda ()
       (exit/errno status)
       (display "The evil undead walk the earth." 2)
       (if #t (error "(exit) returned."))))))


;;; The classic T 2.0 primitive.
;;; This definition works for procedures running on top of Unix systems.
(define (halts? proc) #t)


;;; Low-level init absolutely required for any scsh program.

(define (init-scsh-hindbrain relink-ff?)
  (if #t (error "call to init-scsh-hindbrain which is dead"))
;  (if relink-ff? (lookup-all-externals)) ; Re-link C calls.
;  (init-fdports!)
;  (%install-unix-scsh-handlers)
)


;;; Some globals:
(define home-directory "")
(define exec-path-list)

(define (init-scsh-vars quietly?)
  (set! home-directory
        (cond ((getenv "HOME") => ensure-file-name-is-nondirectory)
              ;; loosing at this point would be really bad, so some
              ;; paranoia comes in order
              (else (call-with-current-continuation
                     (lambda (k)
                       (with-handler
                        (lambda (condition more)
                          (cond ((not quietly?)
                                 (display "Starting up with no home directory ($HOME).")
                                 (newline)))
                          (k "/"))
                        (lambda ()
                          (user-info:home-dir (user-info (user-uid))))))))))

  (set! exec-path-list
        (make-preserved-thread-fluid
         (cond ((getenv "PATH") => split-colon-list)
               (else (if (not quietly?)
                         (warn "Starting up with no path ($PATH)."))
                     '())))))


; SIGTSTP blows s48 away. ???
(define (suspend) (signal-process 0 signal/stop))

