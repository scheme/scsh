;;; File system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Useful little utility for generic ops that work on filenames, fd's or
;;; ports.

(define (generic-file-op thing fd-op fname-op)
  (if (string? thing)
      (with-resources-aligned (list cwd-resource euid-resource egid-resource)
                              (lambda () (fname-op thing)))
      (call/fdes thing fd-op)))

(define (set-file-mode thing mode)
  (let ((mode (file-mode->integer mode)))
    (generic-file-op thing
                     (lambda (fd)    (%set-fdes-mode fd    mode))
                     (lambda (fname) (%set-file-mode fname mode)))))

(define (set-file-owner thing uid)
  (generic-file-op thing
                   (lambda (fd)    (%set-fdes-uid&gid fd    uid -1))
                   (lambda (fname) (%set-file-uid&gid fname uid -1))))

(define (set-file-group thing gid)
  (generic-file-op thing
                   (lambda (fd)    (%set-fdes-uid&gid fd    -1 gid))
                   (lambda (fname) (%set-file-uid&gid fname -1 gid))))

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

(define (truncate-file thing length)
  (generic-file-op thing
                   (lambda (fd)    (%truncate-fdes fd    length))
                   (lambda (fname) (%truncate-file fname length))))

(define (delete-file path)
  (with-resources-aligned (list cwd-resource euid-resource egid-resource)
                          (lambda () (unlink path))))

(define (sync-file fd/port)
  (if (output-port? fd/port) (force-output fd/port))
  (sleazy-call/fdes fd/port %sync-file))

(define sync-file-system %sync-file-system)
