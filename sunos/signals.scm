;;; Signal constant definitions for Sun4
;;; Copyright (c) 1994, 1996 by Olin Shivers and Brian D. Carlstrom
;;; See file COPYING.

(define-enum-constants signal
  ;; POSIX
  (hup	1)	; hangup 
  (int	2)	; interrupt 
  (quit	3)	; quit 
  (ill	4)	; illegal instruction (not reset when caught) 
  ;; SunOS
  (trap 5)      ; trace trap (not reset when caught) */
  ;; POSIX 
  (iot	6)	; IOT instruction 
  (abrt 6)	; used by abort, replace SIGIOT in the future 
  ;; SunOS
  (emt  7)      ; EMT instruction
  ;; POSIX
  (fpe	8)	; floating point exception 
  (kill	9)	; kill (cannot be caught or ignored) 
  ;; SunOS
  (bus  10)     ; bus error
  ;; POSIX
  (segv	11)	; segmentation violation
  ;; SunOS
  (sys  12)     ; bad argument to system call
  ;; POSIX
  (pipe	13)	; write on a pipe with no one to read it 
  (alrm	14)	; alarm clock 
  (term	15)	; software termination signal from kill 
  ;; SunOS
  (urg  16)     ; urgent condition on IO channel 
  ;; POSIX
  (stop	17)	; sendable stop signal not from tty 
  (tstp	18)	; stop signal from tty 
  (cont	19)	; continue a stopped process 
  (chld	20)	; to parent on child stop or exit 
  (cld	20)	; System V name for SIGCHLD
  (ttin	21)	; to readers pgrp upon background tty read 
  (ttou	22)	; like TTIN for output if (tp->t_local&LTOSTOP) 
  ;; SunOS
  (io   23)     ; input/output possible signal 
  (poll 23)     ; System V name for SIGIO
  (xcpu 24)     ; exceeded CPU time limit 
  (xfsz 25)     ; exceeded file size limit
  (vtalrm 26)   ; virtual time alarm 
  (prof 27)     ; profiling time alarm
  (winch 28)    ; window changed 
  (lost 29)     ; resource lost (eg, record-lock lost) 
  ;; POSIX
  ;; User defined
  (usr1 30)	; user defined signal 1 
  (usr2 31)	; user defined signal 2 
  )

(define signals-ignored-by-default
  (list signal/chld signal/cont                         ; These are Posix.
	signal/urg signal/io signal/winch))             ; These are SunOS.
