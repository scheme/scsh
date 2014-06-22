;;; Copyright (c) 1993, 1994 by Olin Shivers.

;;; chase? true (the default) means if the file is a symlink, chase the link
;;; and report on the file it references. chase? = #f means check the actual
;;; file itself, even if it's a symlink.

;;; (file-not-accessible? perms fd/port/fname)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PERMS is 3 bits, not 9.
;;; writeable means (1) file exists & is writeable OR (2) file doesn't exist
;;;     and directory is writeable. That is, writeable means writeable or
;;;     creatable.
;;;
;;; There's a Posix call, access(), that checks using the *real* uid, not
;;; the effective uid, so that setuid programs can figure out if the luser
;;; has perms. file-not-accessible? is defined in terms of the effective uid,
;;; so we can't use access().
;;;
;;; This is a kind of bogus function. The only way to do a real check is to
;;; try an open() and see if it flies. Otherwise, there's an obvious atomicity
;;; problem. Also, we special case root, saying root always has all perms. But
;;; not even root can write on a read-only filesystem, such as a CD ROM. In
;;; this case, we'd blithely say the file was writeable -- there's no way to
;;; check for a ROFS without doing an open(). We need a euid analog to
;;; access(). Ah, well.
;;;
;;; I also should define a family of real uid perm-checking calls.
;;;
;;; Return values:
;;; #f			Accessible in at least one of the requested ways.
;;; search-denied	Can't stat
;;; permission		File exists but is protected
;;; 			(also for errno/rofs)
;;; no-directory	Some directory doesn't exist
;;; nonexistent		File itself doesn't exist
;;;
;;; Otherwise, signals an error.

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

(define (%file-info fd/port/fname chase?)
  (let ((ans-vec (make-vector 11))
        (file-type (lambda (type-code)
                     (vector-ref '#(block-special char-special directory fifo
                                                  regular socket symlink)
                                 type-code))))
    (generic-file-op fd/port/fname
                     (lambda (fd)
                       (%stat-fdes fd ans-vec))
                     (lambda (fname)
                       (%stat-file fname ans-vec chase?)))
    (make-file-info (file-type (vector-ref ans-vec 0))
                    (vector-ref ans-vec 1)
                    (vector-ref ans-vec 2)
                    (integer->file-mode (bitwise-and #o777 (vector-ref ans-vec 3)))
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

(define (fd/port/fname-not-accessible? perms fd/port/fname)
  (with-errno-handler ((err data)
		       ((acces) 'search-denied)
		       ((notdir) 'no-directory)

		       ;; If the file doesn't exist, we usually return
		       ;; 'nonexistent, but we special-case writability
		       ;; for the directory check.
		       ((noent)
			(and (or (zero? (bitwise-and perms 2))
				 ;; This string? test *has* to return #t.
				 ;; If fd/port/fname is an fd or a port,
				 ;; we wouldn't get an errno/noent error!
				 ;; Just being paranoid...
				 (not (string? fd/port/fname))
				 ;; OK, check to see if we can create
				 ;; files in the directory.
				 (fd/port/fname-not-accessible?
				  2
				  (directory-as-file-name
				   (file-name-directory fd/port/fname))))
			     'nonexistent)))
		      (file-info-not-accessible? perms
						 (file-info fd/port/fname))))

(define (file-info-not-accessible? perms info)
  (let ((uid (user-effective-uid)))
    (and (let ((acc (file-mode->integer (file-info:mode info))))
           (cond ((zero? uid) #f) ; Root can do as he wishes.
                 ((= (file-info:uid info) (user-effective-uid)) ; User
                  (zero? (bitwise-and acc (arithmetic-shift perms 6))))

                 ((or (= (file-info:gid info) (user-effective-gid)) ; Group
                      (memv (file-info:gid info) (user-supplementary-gids)))
                  (zero? (bitwise-and acc (arithmetic-shift perms 3))))
                 (else      ; Other
                  (zero? (bitwise-and acc perms)))))
         'permission)))

;;;;;;

(define (file-not-readable?   fd/port/fname)
  (fd/port/fname-not-accessible? 4 fd/port/fname))
(define (file-not-writable?   fd/port/fname)
  (fd/port/fname-not-accessible? 2 fd/port/fname))
(define (file-not-executable? fd/port/fname)
  (fd/port/fname-not-accessible? 1 fd/port/fname))

(define (file-readable?   fd/port/fname)
  (not (file-not-readable?   fd/port/fname)))
(define (file-writable?   fd/port/fname)
  (not (file-not-writable?   fd/port/fname)))
(define (file-executable? fd/port/fname)
  (not (file-not-executable? fd/port/fname)))

(define (file-info-not-readable?   info)  (file-info-not-accessible? 4 info))
(define (file-info-not-writable?   info)  (file-info-not-accessible? 2 info))
(define (file-info-not-executable? info)  (file-info-not-accessible? 1 info))

(define (file-info-readable?   info)  (not (file-info-not-readable?   info)))
(define (file-info-writable?   info)  (not (file-info-not-writable?   info)))
(define (file-info-executable? info)  (not (file-info-not-executable? info)))

;;; Spelling corrected.
(define file-not-writeable?
  (deprecated-proc file-not-writable? "file-not-writeable?"
		   "Use file-not-writable? instead"))

(define file-writeable?
  (deprecated-proc file-writable? "file-writeable?"
		   "Use file-writable? instead"))

;;;;;;

;;; Returns
;;; #f		   exists
;;; #t		   doesn't exist
;;; 'search-denied can't stat
;;; ...or signals an error

(define (file-not-exists? fd/port/fname . maybe-chase?)
  (let ((chase? (:optional maybe-chase? #t)))
    (with-resources-aligned
     (list cwd-resource euid-resource egid-resource)
     (lambda ()
       (with-errno-handler
        ((err data)
         ((acces) 'search-denied)
         ((noent notdir) #t))
        (if chase?
            (get-file-info fd/port/fname)
            (get-file/link-info fd/port/fname))
        #f)))))

(define (file-exists? fd/port/fname . maybe-chase?)
  (not (apply file-not-exists? fd/port/fname maybe-chase?)))

(define-simple-syntax (define-stat-proc proc info-slot)
  (define (proc fname/fd/port . maybe-chase?)
    (info-slot (apply file-info fname/fd/port maybe-chase?))))

(define-stat-proc file-info-type               file-info:type)
(define-stat-proc file-info-group              file-info:gid)
(define-stat-proc file-info-inode              file-info:inode)
(define-stat-proc file-info-last-access        file-info:atime)
(define-stat-proc file-info-last-mod           file-info:mtime)
(define-stat-proc file-info-last-status-change file-info:ctime)
(define-stat-proc file-info-mode               file-info:mode)
(define-stat-proc file-info-nlinks             file-info:nlinks)
(define-stat-proc file-info-owner              file-info:uid)
(define-stat-proc file-info-size               file-info:size)

(define (file-info-to-fname/fd/port predicate)
  (lambda (fname/fd/port . maybe-chase?)
    (predicate (apply file-info fname/fd/port maybe-chase?))))

(define (file-info-directory? file-info)
  (eq? 'directory (file-info:type file-info)))

(define file-directory?
  (file-info-to-fname/fd/port file-info-directory?))

(define (file-info-fifo? file-info)
  (eq? 'fifo (file-info:type file-info)))

(define file-fifo? (file-info-to-fname/fd/port file-info-fifo?))

(define (file-info-regular? file-info)
  (eq? 'regular (file-info:type file-info)))

(define file-regular? (file-info-to-fname/fd/port file-info-regular?))

(define (file-info-socket? file-info)
  (eq? 'socket (file-info:type file-info)))

(define file-socket? (file-info-to-fname/fd/port file-info-socket?))

(define (file-info-special? file-info)
  (let ((type (file-info:type file-info)))
    (or (eq? 'block-special type) (eq? 'char-special type))))

(define file-special? (file-info-to-fname/fd/port file-info-special?))

(define (file-info-symlink? file-info)
  (eq? 'symlink (file-info:type file-info)))

(define (file-symlink? fd/port/fname) ; No MAYBE-CHASE?, of course.
  (file-info-symlink? (file-info fd/port/fname #f)))
