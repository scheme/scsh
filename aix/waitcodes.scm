;;; Scsh routines for analysing exit codes returned by WAIT.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;;
;;; To port these to a new OS, consult /usr/include/sys/wait.h, 
;;; and check the WIFEXITED, WEXITSTATUS, WIFSTOPPED, WSTOPSIG, 
;;; WIFSIGNALED, and WTERMSIG macros for the magic fields they use.
;;; These definitions are for NeXTSTEP.
;;;
;;; I could have done a portable version by making C calls for this,
;;; but it's such overkill.


;;; If process terminated normally, return the exit code, otw #f.

(define (status:exit-val status)
  (and (zero? (bitwise-and #xFF status))
       (bitwise-and #xFF (arithmetic-shift status -8))))



;;; If the process was suspended, return the suspending signal, otw #f.

(define (status:stop-sig status)
  (and (not (zero? (bitwise-and status #x40)))
       (bitwise-and #x7F (arithmetic-shift status -8))))


;;; If the process terminated abnormally, 
;;; return the terminating signal, otw #f.

(define (status:term-sig status)
  (and (not (zero? (bitwise-and status #xFF)))		; Didn't exit.
       (zero? (bitwise-and status #x40))		; Not suspended.
       (bitwise-and status #x7F)))



;;; Flags.
(define wait/poll 		1)	; Don't hang if nothing to wait for.
(define wait/stopped-children	2)	; Report on suspended subprocs, too.
