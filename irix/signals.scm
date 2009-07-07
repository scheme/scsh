;;; Signal constant definitions for "irix"
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

;;POSIX only defined here.

(define-enum-constants signal
  ;; POSIX
  (hup	1)	; hangup 
  (int	2)	; interrupt 
  (quit	3)	; quit 
  (ill	4)	; illegal instruction (not reset when caught) 
  (iot	6)	; IOT instruction 
  (abrt 6)	; used by abort, replace SIGIOT in the future 
  (fpe	8)	; floating point exception 
  (kill	9)	; kill (cannot be caught or ignored) 
  (bus  10)	; bus error
  (segv	11)	; segmentation violation
  (sys  12)	; bad argument to system call
  (pipe	13)	; write on a pipe with no one to read it 
  (alrm	14)	; alarm clock 
  (term	15)	; software termination signal from kill 
  (stop 23)     ; sendable stop signal not from tty
  (tstp 24)     ; stop signal from tty
  (cont 25)     ; continue a stopped process
  (chld	18)	; to parent on child stop or exit 
  (cld	18)	; compat
  (ttin 26)     ; to readers pgrp upon background tty read
  (ttou 27)     ; like TTIN for output if (tp->t_local&LTOSTOP)
  ;; User defined
  (usr1 16)	; user defined signal 1 
  (usr2 17)	; user defined signal 2 

  (pwr  19)	; power-fail restart
  (poll 22)	; pollable event occurred
  (io   22)     ; input/output possible signal
  (urg  21)     ; urgent condition on io channel
  (winch 20)    ; window size changes
  (vtalrm 28)   ; virtual time alarm
  (prof 29)     ; profiling alarm
  (xcpu 30)     ; Cpu time limit exceeded
  (xfsz 31)     ; Filesize limit exceeded
  )

(define signals-ignored-by-default
  (list signal/chld signal/cont				 ; These are Posix.
	signal/pwr signal/urg signal/winch)) ; These are Irix.
