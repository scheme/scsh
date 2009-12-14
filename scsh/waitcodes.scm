;;; Scsh routines for analysing exit codes returned by WAIT.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;;
;;; To port these to a new OS, consult /usr/include/sys/wait.h,
;;; and check the WIFEXITED, WEXITSTATUS, WIFSTOPPED, WSTOPSIG,
;;; WIFSIGNALED, and WTERMSIG macros for the magic fields they use.
;;; These definitions are for Linux.
;;;
;;; I could have done a portable version by making C calls for this,
;;; but it's such overkill.


;;; If process terminated normally, return the exit code, otw #f.

(define (status:exit-val status)
  (and (not (= (bitwise-and #xFF status) #x7F))
       (zero? (bitwise-and #x7F status))
       (bitwise-and #xFF (arithmetic-shift status -8))))


;;; If the process was suspended, return the suspending signal, otw #f.

(define (status:stop-sig status)
  (and (= #x7F (bitwise-and status #xFF))
       (bitwise-and #xFF (arithmetic-shift status -8))))


;;; If the process terminated abnormally,
;;; return the terminating signal, otw #f.

(define (status:term-sig status)
  (let ((termsig (bitwise-and status #x7F)))
    (and (not (zero? termsig))				; Didn't exit.
	 (not (= #x7F (bitwise-and status #xFF)))	; Not suspended.
	 termsig)))


;;; Flags.
(define wait/poll 		1)	; Don't hang if nothing to wait for.
(define wait/stopped-children	2)	; Report on suspended subprocs, too.
