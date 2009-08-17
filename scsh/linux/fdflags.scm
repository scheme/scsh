;;; Flags for open(2) and fcntl(2).
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom

(define-enum-constants open
  ;; POSIX
  (read			#x0000)
  (write		#x0001)
  (read+write		#x0002)
  (non-blocking		#x0800)		; no delay
  (append		#x0400)		; set append mode
  
  ;; Linux
  (shared-lock		#x0004)		; open with shared file lock
  (exclusive-lock		#x0008)		; open with exclusive file lock
  (async		#x2000)		; signal pgrep when data ready
  (fsync 	        #x1000)		; synchronus writes

  ;; POSIX
  (create               #x0040)		; create if nonexistant
  (truncate             #x0200)		; truncate to zero length
  (exclusive            #x0080)		; error if already exists
  (no-control-tty	#x0100))	; don't assign controlling terminal



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
  (get-owner			9)	; F_GETOWN (Not POSIX)
  (set-owner			8)	; F_SETOWN (Not POSIX)
  (get-record-lock		5)	; F_GETLK
  (set-record-lock-no-block	6)	; F_SETLK
  (set-record-lock		7))	; F_SETLKW

;;; fcntl fdes-flags (F_GETFD)

(define fdflags/close-on-exec 		1)

;;; fcntl status-flags (F_GETFL)
;;; Mostly, these are OPEN/... flags, like OPEN/APPEND.
;;; (define fdstatus/... ...)

;;; fcntl lock values.

(define-enum-constants lock
  (read		0)	; F_RDLCK
  (release	2)	; F_UNLCK
  (write	1))	; F_WRLCK
