;;; Flags for open(2) and fcntl(2).
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

(define-enum-constants open
  ;; POSIX
  (read			#x0001)		; Open for reading
  (write		#x0002)		; Open for writing
  (read+write		#x0003)		; Open for reading and writing
  (non-blocking		#x0008)		; Non-blocking open or non-blocking I/O
  (append		#x0100)		; Writes always append to the file
  (no-control-tty	0)		; Don't assign a controlling terminal
  (create               #x0010)		; Create file if it doesn't exist
  (truncate             #x00010000)	; Truncate file to zero length
  (exclusive            #x0020)		; Fail if file already exists

  ;; BSD
  (shared-lock		#x00020000)	; Open with shared file lock
  (exclusive-lock	#x00040000)	; Open with exclusive file lock
  (async		#x0200)		; Send SIGIO to owner when data is ready
  (fsync 	        #x0400)		; Synchronous writes

  ;; GNU
  (execute		#x0004)		; Open for execution
  (no-link		#x0040)		; No name mappings on final component
  (no-translator	#x0080)		; No translator on final component
  (no-access-time	#x0800)		; Don't set access time on read
  (ignore-control-tty	#x00080000))	; Don't do any ctty magic at all

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
  (write	2)	; F_WRLCK
  (release	3))	; F_UNLCK
