;;; UMASK
(define (process-umask)
  (let ((mask (set-file-creation-mask! (file-mode- (file-mode all) (file-mode all)))))
    (set-file-creation-mask! mask)
    mask))

;;; These calls change/reveal the process working directory
;;;

(define (process-chdir . maybe-dir)
  (let ((dir (:optional maybe-dir (home-dir))))
    (set-working-directory! (ensure-file-name-is-nondirectory dir))))

(define (process-cwd)
  (os-string->string (working-directory)))

;;; GID

(define (user-gid)
  (group-id->integer (get-group-id)))

(define (process-user-effective-gid)
  (group-id->integer (get-effective-group-id)))

(define (process-set-gid gid)
  (set-group-id! (integer->group-id gid)))

(define (set-process-user-effective-gid gid)
  (set-effective-user-id! (integer->group-id gid)))

(define (user-supplementary-gids)
  (map group-id->integer (get-groups)))

;;; UID
(define (user-uid)
  (user-id->integer (get-user-id)))

(define (process-user-effective-uid)
  (user-id->integer (get-effective-user-id)))

(define (process-set-uid uid)
  (set-user-id! (integer->user-id uid)))

(define (set-process-user-effective-uid uid)
  (set-effective-user-id! (integer->user-id uid)))

(define (user-login-name)
  (let ((name (get-login-name)))
    (if name
        (os-string->string name)
        (error "user-login-name"
               "Cannot get the current user's name"))))

;;; PID

(define (pid)
  (process-id->integer (get-process-id)))

(define (parent-pid)
  (process-id->integer (get-parent-process-id)))

;;; Process groups and session ids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-lambda-definition-2 process-group () "scsh_getpgrp")

(import-lambda-definition-2 %set-process-group (pid groupid) "scsh_setpgid")

(define (set-process-group arg1 . maybe-arg2)
  (receive (pid pgrp) (if (null? maybe-arg2)
                          (values (pid) arg1)
                          (values arg1 (car maybe-arg2)))
           (%set-process-group pid pgrp)))


(import-lambda-definition-2 become-session-leader () "scsh_setsid")

;;; PROCESS TIMES

;;; OOPS: The POSIX times() has a mildly useful ret value we are throwing away.


(import-lambda-definition-2 process-times/list () "process_times")

(define (process-times)
  (apply values (process-times/list)))

(import-lambda-definition-2 cpu-ticks/sec () "cpu_clock_ticks_per_sec")


;;; Resources


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
  (set-file-creation-mask! new-umask)
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
    (if (not (file-mode=? thread-umask (umask-cache)))
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

       (initialize-eXid)))))

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

;;; Sugar:

(define-simple-syntax (with-cwd dir . body)
  (with-cwd* dir (lambda () . body)))

(define-simple-syntax (with-umask mask . body)
  (with-umask* mask (lambda () . body)))

(define-simple-syntax (with-user-effective-uid uid . body)
  (with-user-effective-uid* uid (lambda () . body)))

(define-simple-syntax (with-user-effective-gid gid . body)
  (with-user-effective-gid* gid (lambda () . body)))
