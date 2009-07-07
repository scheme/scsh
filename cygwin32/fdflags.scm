;;; Flags for open(2) and fcntl(2).
;;; Copyright (c) 1993 by Olin Shivers.
;;; Copyright (c) 1999 by Brian D. Carlstrom

(define-enum-constants open
  ;; POSIX
  (read			#x0000)
  (write		#x0001)
  (read+write		#x0002)

  ;; Cygwin32
  (ndelay		#x0004)         ; non blocking I/O (4.2 style)

  ;; POSIX
  (append		#x0008)		; set append mode
  
  ;; Cygwin32
  (mark                 #x0010)         ; internal; mark during gc()
  (defer		#x0020)         ; internal; defer for next gc pass
  (async		#x0040)		; signal pgrep when data ready
  (shared-lock		#x0080)		; open with shared file lock
  (exclusive-lock	#x0100)		; open with exclusive file lock

  ;; POSIX
  (create               #x0200)		; create if nonexistant
  (truncate             #x0400)		; truncate to zero length
  (exclusive            #x0800)		; error if already exists

  ;; Cygwin32
  (non-blocking		#x1000)		; non blocking I/O (sys5 style)
  (fsync 	        #x2000)		; synchronus writes

  ;; POSIX
  (non-blocking		#x4000)		; non blocking I/O (POSIX style)
  (no-control-tty	#x8000)         ; don't assign controlling terminal

  ;; Cygwin32
  (binary              #x10000)
  (text                #x20000)
  (noinherit           #x40000))

(define open/access-mask
  (bitwise-ior open/read
	       (bitwise-ior open/write open/read+write)))

;;; fcntl() commands
(define-enum-constants fcntl
  (dup-fdes			0)      ; F_DUPFD
  (get-fdes-flags		1)      ; F_GETFD
  (set-fdes-flags		2)      ; F_SETFD
  (get-status-flags		3)      ; F_GETFL
  (set-status-flags		4)      ; F_SETFL
  (get-owner			5)	; F_GETOWN  (Not POSIX)
  (set-owner			6)	; F_SETOWN  (Not POSIX)
  (get-record-lock		7)	; F_GETLK
  (set-record-lock-no-block	8)	; F_SETLK
  (set-record-lock		9)	; F_SETLKW
  (remote-get-lock             10)      ; F_RGETLK  (Not POSIX)
  (remote-set-lock-no-block    11)      ; F_RSETLK  (Not POSIX)
  (convert                     12)      ; F_CNVT    (Not POSIX)
  (remote-get-lock             13))     ; F_RSETLKW (Not POSIX)

;;; fcntl fdes-flags (F_GETFD)

(define fdflags/close-on-exec 		1)

;;; fcntl status-flags (F_GETFL)
;;; Mostly, these are OPEN/... flags, like OPEN/APPEND.
;;; (define fdstatus/... ...)

;;; fcntl lock values.

(define-enum-constants lock
  (read		  1)	; F_RDLCK
  (release	  2)	; F_UNLCK
  (write	  3)	; F_WRLCK
  (release-remote 4))   ; F_UNLKSYS (Not POSIX)
