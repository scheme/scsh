;;; Signal constant definitions for the GNU Hurd
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

(define-enum-constants signal
  ;; POSIX
  (hup	1)	; hangup 
  (int	2)	; interrupt 
  (quit	3)	; quit 
  (ill	4)	; illegal instruction (not reset when caught) 

  ;; BSD4.4-Lite
  (trap	5)	; trace trap (not reset when caught) 

  ;; POSIX
  (iot	6)	; IOT instruction 
  (abrt 6)	; used by abort, replace SIGIOT in the future 

  ;; BSD4.4-Lite
  (emt	7)	; EMT instruction 

  ;; POSIX
  (fpe	8)	; floating point exception 
  (kill	9)	; kill (cannot be caught or ignored) 

  ;; BSD4.4-Lite
  (bus	10)	; bus error 

  ;; POSIX
  (segv	11)	; segmentation violation

  ;; BSD4.4-Lite
  (sys	12)	; bad argument to system call

  ;; POSIX
  (pipe	13)	; write on a pipe with no one to read it 
  (alrm	14)	; alarm clock 
  (term	15)	; software termination signal from kill 

  ;; BSD4.4-Lite
  (urg	16)	; urgent condition on IO channel 

  ;; POSIX
  (stop	17)	; sendable stop signal not from tty 
  (tstp	18)	; stop signal from tty 
  (cont	19)	; continue a stopped process 
  (chld	20)	; to parent on child stop or exit 

  ;; BSD4.4-Lite
  (cld	20)	; System V name for SIGCHLD 

  ;; POSIX
  (ttin	21)	; to readers pgrp upon background tty read 
  (ttou	22)	; like TTIN for output if (tp->t_local&LTOSTOP) 

  ;; BSD4.4-Lite
  (io	23)	; input/output possible signal 
  (poll	23)	; System V name for SIGIO 
  (xcpu	24)	; exceeded CPU time limit 
  (xfsz	25)	; exceeded file size limit 
  (vtalrm 26)	; virtual time alarm 
  (prof	27)	; profiling time alarm 
  (winch 28)	; window changed 
  (info 29)	; information request

  ;; User defined
  (usr1 30)	; user defined signal 1 
  (usr2 31)	; user defined signal 2 

  ;; GNU
  (lost 32)	; server died
  )

(define signals-ignored-by-default
  (list signal/chld signal/cont				; These are Posix.
	signal/info signal/io signal/urg signal/winch)) ; These are BSD.
