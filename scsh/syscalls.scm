;;; POSIX system-call Scheme binding.
;;; Copyright (c) 1993 by Olin Shivers.

;;; Scheme48 implementation.

;;; Need to rationalise names here. getgid. get-gid. "effective" as morpheme?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import a C function and convert the exception os-error to a syscall-error
;;;
;;; 1.) Import a C function
;;; 2.) Turn os-error into syscall-error
;;; 3.) Retry on EINTR
;;; The call/cc and the record is due to S48's broken exception system:
;;; You can't throw an error within a handler
;;;

(import-dynamic-externals "=scshexternal/scsh")

(define (byte-vector->string bytev)
  (os-string->string (byte-vector->os-string bytev)))

(define-syntax define/vector-args
  (syntax-rules ()
    ((define/vector-args new raw (string-arg ...) arg ...)
     (define (new string-arg ... arg ...)
       (raw (string->os-byte-vector string-arg) ...
            arg ...)))))

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

;;; Posix waitpid(2) call.
(import-lambda-definition-2 %wait-pid/list (pid options) "wait_pid")

(define (%wait-pid pid options)
  (apply values (%wait-pid/list pid options)))

;;; Miscellaneous process state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; UMASK

(define (set-process-umask mask)
  (let ((old-mask (set-file-creation-mask! (integer->file-mode mask))))
    (file-mode->integer old-mask)))

(define (process-umask)
  (let ((mask (set-process-umask 0)))
    (set-process-umask mask)
    mask))

;;; PROCESS TIMES

;;; OOPS: The POSIX times() has a mildly useful ret value we are throwing away.


(import-lambda-definition-2 process-times/list () "process_times")

(define (process-times)
  (apply values (process-times/list)))

(import-lambda-definition-2 cpu-ticks/sec () "cpu_clock_ticks_per_sec")

;;; File system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Useful little utility for generic ops that work on filenames, fd's or
;;; ports.

(define (generic-file-op thing fd-op fname-op)
  (if (string? thing)
      (with-resources-aligned (list cwd-resource euid-resource egid-resource)
                              (lambda () (fname-op thing)))
      (call/fdes thing fd-op)))


(import-lambda-definition-2 %set-file-mode-raw (path mode) "scsh_chmod")

(define/vector-args %set-file-mode %set-file-mode-raw (path) mode)

(import-lambda-definition-2 %set-fdes-mode (path mode) "scsh_fchmod")

(define (set-file-mode thing mode)
  (generic-file-op thing
                   (lambda (fd)    (%set-fdes-mode fd    mode))
                   (lambda (fname) (%set-file-mode fname mode))))


(import-lambda-definition-2 set-file-uid&gid-raw (path uid gid) "scsh_chown")

(define/vector-args set-file-uid&gid set-file-uid&gid-raw (path) uid gid)

(import-lambda-definition-2 set-fdes-uid&gid (fd uid gid) "scsh_fchown")

(define (set-file-owner thing uid)
  (generic-file-op thing
                   (lambda (fd)    (set-fdes-uid&gid fd    uid -1))
                   (lambda (fname) (set-file-uid&gid fname uid -1))))

(define (set-file-group thing gid)
  (generic-file-op thing
                   (lambda (fd)    (set-fdes-uid&gid fd    -1 gid))
                   (lambda (fname) (set-file-uid&gid fname -1 gid))))


;;; Uses real uid and gid, not effective. I don't use this anywhere.

(import-lambda-definition-2 %file-ruid-access-not-raw? (path perms) "scsh_access")

(define/vector-args %file-ruid-access-not? %file-ruid-access-not-raw? (path) perms)

;(define (file-access? path perms)
;  (not (%file-access-not? path perms)))
;
;(define (file-executable? fname)
;  (file-access? fname 1))
;
;(define (file-writable? fname)
;  (file-access? fname 2))
;
;(define (file-readable? fname)
;  (file-access? fname 4))

(define %create-hard-link link)

(define (%create-fifo path mode)
  (make-fifo path (integer->file-mode mode)))

(define (%%create-directory path mode)
  (make-directory path (integer->file-mode mode)))

(define (%create-directory path . maybe-mode)
  (let ((mode (:optional maybe-mode #o777))
        (fname (ensure-file-name-is-nondirectory path)))
    (%%create-directory fname mode)))

(define %rename-file rename)

(define %delete-directory remove-directory)

(import-lambda-definition-2 %read-symlink-raw (path) "scsh_readlink")

(define (%read-symlink path)
  (byte-vector->string
   (%read-symlink-raw (string->os-byte-vector path))))

(import-lambda-definition-2 %utime-raw (path ac m) "scm_utime")

(define/vector-args %utime %utime-raw (path) ac m)

(import-lambda-definition-2 %utime-now-raw (path) "scm_utime_now")

(define/vector-args %utime-now %utime-now-raw (path))

;;; (SET-FILE-TIMES path [access-time mod-time])

(define (set-file-times path . maybe-times)
  (with-resources-aligned
   (list cwd-resource euid-resource egid-resource)
   (lambda ()
     (if (pair? maybe-times)
         (let* ((access-time (real->exact-integer (car maybe-times)))
                (mod-time (if (pair? (cddr maybe-times))
                              (error "Too many arguments to set-file-times/errno"
                                     (cons path maybe-times))
                              (real->exact-integer (cadr maybe-times)))))
           (%utime path access-time
                   mod-time ))
         (%utime-now path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STAT

(import-lambda-definition-2 stat-file-raw (path data chase?) "scheme_stat")

(define/vector-args stat-file stat-file-raw (path) data chase?)

(import-lambda-definition-2 stat-fdes (fd data) "scheme_fstat")

(define-record-type :file-info
  (make-file-info type device inode mode nlinks uid gid size atime mtime ctime)
  file-info?
  (type file-info:type)
  (device file-info:device)
  (inode file-info:inode)
  (mode file-info:mode)
  (nlinks file-info:nlinks)
  (uid file-info:uid)
  (gid file-info:gid)
  (size file-info:size)
  (atime file-info:atime)
  (mtime file-info:mtime)
  (ctime file-info:ctime))


;;; Should be redone to return multiple-values.
(define (%file-info fd/port/fname chase?)
  (let ((ans-vec (make-vector 11))
        (file-type (lambda (type-code)
                     (vector-ref '#(block-special char-special directory fifo
                                                  regular socket symlink)
                                 type-code))))
    (generic-file-op fd/port/fname
                     (lambda (fd)
                       (stat-fdes fd ans-vec))
                     (lambda (fname)
                       (stat-file fname ans-vec chase?)))
    (make-file-info (file-type (vector-ref ans-vec 0))
                    (vector-ref ans-vec 1)
                    (vector-ref ans-vec 2)
                    (vector-ref ans-vec 3)
                    (vector-ref ans-vec 4)
                    (vector-ref ans-vec 5)
                    (vector-ref ans-vec 6)
                    (vector-ref ans-vec 7)
                    (vector-ref ans-vec 8)
                    (vector-ref ans-vec 9)
                    (vector-ref ans-vec 10))))


(define (file-info fd/port/fname . maybe-chase?)
  (let ((chase? (:optional maybe-chase? #t)))
    (%file-info fd/port/fname chase?)))


(define file-attributes
  (deprecated-proc file-info "file-attributes" "Use file-info instead."))

(define %create-symlink create-symbolic-link)

;;; "no-declare" as there is no agreement among the OS's as to whether or not
;;; the PATH arg is "const". It *should* be const.

(import-lambda-definition-2 %truncate-file-raw (path length) "scsh_truncate")

(define/vector-args %truncate-file %truncate-file-raw (path) length)

(import-lambda-definition-2 %truncate-fdes (path length) "scsh_ftruncate")

(define (truncate-file thing length)
  (generic-file-op thing
                   (lambda (fd)    (%truncate-fdes fd    length))
                   (lambda (fname) (%truncate-file fname length))))

(define (delete-file path)
  (with-resources-aligned (list cwd-resource euid-resource egid-resource)
                          (lambda () (unlink path))))

(import-lambda-definition-2 %sync-file (fd) "scsh_fsync")

(define (sync-file fd/port)
  (if (output-port? fd/port) (force-output fd/port))
  (sleazy-call/fdes fd/port %sync-file))


;;; Amazingly bogus syscall -- doesn't *actually* sync the filesys.
(import-lambda-definition-2 sync-file-system () "scsh_sync")

;;; I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-lambda-definition-2 %close-fdes (fd) "scsh_close")

(import-lambda-definition-2 %dup (fd) "scsh_dup")

(import-lambda-definition-2 %dup2 (fd-from fd-to) "scsh_dup2")

(import-lambda-definition-2 %fd-seek (fd offset whence) "scsh_lseek")


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

(import-lambda-definition-2 %char-ready-fdes? (fd) "char_ready_fdes")

(import-lambda-definition-2 %open-raw (path flags mode) "scsh_open")

(define/vector-args %open %open-raw (path) flags mode)

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

;;; Signals (rather incomplete)
;;; ---------------------------

(import-lambda-definition-2 signal-pid (pid signal) "scsh_kill")

(define (signal-process proc signal)
  (signal-pid (cond ((proc? proc)    (proc:pid proc))
                    ((integer? proc) proc)
                    (else (error "Illegal proc passed to signal-process" proc)))
              (signal-os-number signal)))

(define (signal-process-group proc-group signal)
  (signal-pid (- (cond ((proc? proc-group)    (proc:pid proc-group))
                       ((integer? proc-group) proc-group)
                       (else (error "Illegal proc passed to signal-process-group"
                                    proc-group))))
              (signal-os-number signal)))

;;; SunOS, not POSIX:
;;; (define-foreign signal-process-group/errno
;;;   (killpg (integer proc-group) (integer signal))
;;;   (to-scheme integer errno_or_false))
;;;
;;; (define-errno-syscall (signal-process-group proc-group signal)
;;;   signal-process-group/errno)


;;; User info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type :user-info
  (make-user-info name uid gid home-dir shell)
  (name user-info:name)
  (uid user-info:uid)
  (gid user-info:gid)
  (home-dir user-info:home-dir)
  (shell user-info:shell))

(define-record-discloser :user-info
  (lambda (self)
    (list 'user-info (user-info:name ui))))

(import-lambda-definition-2
 %uid->user-info
 (uid user-info-record)
 "user_info_uid")

(import-lambda-definition-2
 %name->user-info
 (name user-info-record)
 "user_info_name")

(define (uid->user-info uid)
  (let ((empty-user-info (make-user-info #f uid #f #f #f)))
    (if (%uid->user-info uid empty-user-info)
        empty-user-info
        (error "Cannot get user's information" uid->user-info uid))))


(define (name->user-info name)
  (let ((empty-user-info (make-user-info name #f #f #f #f)))
    (if (%name->user-info name empty-user-info)
        empty-user-info
        (error "Cannot get user's information" name->user-info name))))

(define (user-info uid/name)
  ((cond ((string?  uid/name) name->user-info)
         ((integer? uid/name) uid->user-info)
         (else (error "user-info arg must be string or integer" uid/name)))
   uid/name))

;;; Derived functions

(define (->uid uid/name)
  (user-info:uid (user-info uid/name)))

(define (->username uid/name)
  (user-info:name (user-info uid/name)))

(define (%homedir uid/name)
  (user-info:home-dir (user-info uid/name)))


;;; Group info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type :group-info
  (make-group-info name gid members)
  group-info?
  (name group-info:name)
  (gid group-info:gid)
  (members group-info:members))

;; Make group-info records print like #{group-info wheel}.
(define-record-discloser :group-info
  (lambda (self)
    (list 'group-info (group-info:name self))))

(import-lambda-definition-2
 %gid->group-info
 (gid group-info-record)
 "group_info_gid")

(import-lambda-definition-2
 %name->group-info
 (name group-info-record)
 "group_info_name")

(define (gid->group-info  gid)
  (let ((empty-group-info (make-group-info #f gid #f)))
    (if (%gid->group-info gid empty-group-info)
        empty-group-info
        (error "Cannot get group's information for gid" gid))))

(define (name->group-info name)
  (let ((empty-group-info (make-group-info name #f #f)))
    (if (%name->group-info name empty-group-info)
        empty-group-info
        (error "Cannot get group's information for name" name))))

(define (group-info gid/name)
  ((cond ((string?  gid/name) name->group-info)
         ((integer? gid/name) gid->group-info)
         (else (error "group-info arg must be string or integer" gid/name)))
   gid/name))

;;; Derived functions

(define (->gid name)
  (group-info:gid (group-info name)))

(define (->groupname gid)
  (group-info:name (group-info gid)))

;;; Directory stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (directory-files . args)
  (with-resources-aligned
   (list cwd-resource euid-resource egid-resource)
   (lambda ()
     (let-optionals args ((dir       ".")
                          (dotfiles? #f))
       (check-arg string? dir directory-files)
       (let* ((files (map (lambda (file) (os-string->string file))
                          (list-directory (ensure-file-name-is-nondirectory dir))))
              (files-sorted (sort-list! files filename<=)))
         (if dotfiles? files-sorted
             (filter (lambda (f) (not (dotfile? f)))
                     files-sorted)))))))

(define (dotfile? f)
  (char=? (string-ref f 0) #\.))

(define (filename<= f1 f2)
  (if (dotfile? f1)
      (if (dotfile? f2)
          (string<= f1 f2)
          #t)
      (if (dotfile? f2)
          #f
          (string<= f1 f2))))

; A record for directory streams.  It just has the name and a byte vector
; containing the C directory object.  The name is used only for printing.

(define-record-type :directory-stream
  (make-directory-stream name c-dir)
  directory-stream?
  (name directory-stream:name)
  (c-dir directory-stream:c-dir set-directory-stream:c-dir))

(define-record-discloser :directory-stream
  (lambda (self)
    (list 'directory-stream (directory-stream:name self))))

; Directory streams are meaningless in a resumed image.
(define-record-resumer :directory-stream #f)

(define (open-directory-stream name)
  (let ((dir (make-directory-stream
              name
              (with-resources-aligned
               (list cwd-resource euid-resource egid-resource)
               (lambda ()
                 (open-dir name))))))
    (add-finalizer! dir close-directory-stream)
    dir))

(define (read-directory-stream dir-stream)
  (read-dir (directory-stream:c-dir dir-stream)))

(define (close-directory-stream dir-stream)
  (let ((c-dir (directory-stream:c-dir dir-stream)))
    (if c-dir
        (begin
          (close-dir c-dir)
          (set-directory-stream:c-dir dir-stream #f)))))

(import-lambda-definition-2 open-dir (name) "scm_opendir")
(import-lambda-definition-2 close-dir (dir-stream) "scm_closedir")
(import-lambda-definition-2 read-dir (dir-stream) "scm_readdir")


;;; I do this one in C, I'm not sure why:
;;; It is used by MATCH-FILES.
;;; 99/7: No one is using this function, so I'm commenting it out.
;;; Later, we could tune up the globber or regexp file-matcher to use
;;; it (but should shift it into the rx directory). But I should also go
;;; to a file-at-a-time cursor model for directory fetching. -Olin

;(define-foreign %filter-C-strings!
;  (filter_stringvec (string pattern) ((C "char const ** ~a") cvec))
;  static-string        ; error message -- #f if no error.
;  integer)     ; number of files that pass the filter.


;(define (match-files regexp . maybe-dir)
;  (let ((dir (:optional maybe-dir ".")))
;    (check-arg string? dir match-files)
;    (receive (err cvec numfiles)
;            (%open-dir (ensure-file-name-is-nondirectory dir))
;      (if err (errno-error err match-files regexp dir))
;      (receive (err numfiles) (%filter-C-strings! regexp cvec)
;       (if err (error err match-files))
;       (%sort-file-vector cvec numfiles)
;       (let ((files (C-string-vec->Scheme&free cvec numfiles)))
;         (vector->list files))))))


;;; Environment manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (var . val) / "var=val" rep conversion:



(define (split-env-string var=val)
  (let ((i (string-index var=val #\=)))
    (if i (values (substring var=val 0 i)
                  (substring var=val (+ i 1) (string-length var=val)))
        (error "No \"=\" in environment string" var=val))))

(define (env-list->alist env-list)
  (map (lambda (var=val)
         (call-with-values (lambda () (split-env-string var=val))
                           cons))
       env-list))

(define (alist->env-list alist)
  (map (lambda (var.val)
         (string->os-byte-vector
          (string-append (car var.val) "="
                         (let ((val (cdr var.val)))
                           (if (string? val) val
                               (string-join val ":"))))))
       alist))

(define (alist->env-vec alist)
  (list->vector (alist->env-list alist)))


;;; ENV->ALIST

(import-lambda-definition-2 %load-env () "scm_envvec")

(define (environ-env->alist)
  (let ((env-list.envvec (%load-env)))
    (cons (env-list->alist (car env-list.envvec))
          (cdr env-list.envvec))))


;;; ALIST->ENV

;;; (%create-env ((vector 'X) -> address))
(import-lambda-definition-2 %create-env (envvec) "create_env")

;;; assumes aligned env
(define (envvec-alist->env alist)
  (%create-env (alist->env-vec alist)))

(import-lambda-definition-2 %align-env (envvec) "align_env")

(import-lambda-definition-2 %free-env (envvec) "free_envvec")


;;; Fd-ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-lambda-definition-2 %set-cloexec (fd val) "set_cloexec")


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

(import-lambda-definition-2 %gethostname () "scm_gethostname")

(define (system-name)
  (byte-vector->string (%gethostname)))

(import-lambda-definition-2 errno-msg (i) "errno_msg")

(define-record-type :uname
  (make-uname os-name node-name release version machine)
  uname?
  (os-name uname:os-name)
  (node-name uname:node-name)
  (release uname:release)
  (version uname:version)
  (machine uname:machine))

(define (uname)
  (make-uname (os-name)
              (os-node-name)
              (os-release-name)
              (os-version-name)
              (machine-name)))
