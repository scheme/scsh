;;; Flags for open(2) and fcntl(2).
;;; Copyright (c) 1993 by Olin Shivers.
;;; Copyright (c) 1994 by Brian D. Carlstrom

(define-enum-constants open
  ;; POSIX
  (read			#x0000)
  (write		#x0001)
  (read+write		#x0002)
  (non-blocking		#x0004)		; no delay
  (append		#x0008)		; set append mode
  
  ;; BSD4.4-Lite
  (shared-lock		#x0010)		; open with shared file lock
  (exclusive-lock	#x0020)		; open with exclusive file lock
  (async		#x0040)		; signal pgrep when data ready
  (fsync 	        #x0080)		; synchronus writes

  ;; POSIX
  (create               #x0200)		; create if nonexistant
  (truncate             #x0400)		; truncate to zero length
  (exclusive            #x0800)		; error if already exists
  (no-control-tty	#x0000))	; don't assign controlling terminal

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
  (get-owner			5)	; F_GETOWN (Not POSIX)
  (set-owner			6)	; F_SETOWN (Not POSIX)
  (get-record-lock		7)	; F_GETLK
  (set-record-lock-no-block	8)	; F_SETLK
  (set-record-lock		9))	; F_SETLKW

;;; fcntl fdes-flags (F_GETFD)

(define fdflags/close-on-exec 		1)

;;; fcntl status-flags (F_GETFL)
;;; Mostly, these are OPEN/... flags, like OPEN/APPEND.
;;; (define fdstatus/... ...)

;;; fcntl lock values.

(define-enum-constants lock
  (read		1)	; F_RDLCK
  (release	2)	; F_UNLCK
  (write	3))	; F_WRLCK
