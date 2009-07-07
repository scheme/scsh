;;; Signal constant definitions for AIX
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom.
;;; AIX version by Chipsy Sperber

;;POSIX only defined here.

(define-enum-constants signal
  ;; POSIX
  (hup	1)	; hangup 
  (int	2)	; interrupt 
  (quit	3)	; quit 
  (ill	4)	; illegal instruction (not reset when caught) 
  (iot	5)	; IOT instruction 
  (abrt 6)	; used by abort, replace SIGIOT in the future 
  (fpe	8)	; floating point exception 
  (emt 7)	; EMT intruction
  (fpe 8)	; floating point exception
  (kill	9)	; kill (cannot be caught or ignored) 
  (bus 10)	; bus error (specification exception)
  (segv	11)	; segmentation violation
  (sys 12)	; bad argument to system call
  (pipe	13)	; write on a pipe with no one to read it 
  (alrm	14)	; alarm clock 
  (term	15)	; software termination signal from kill 
  (urg 16)	; urgent contition on I/O channel
  (stop	17)	; sendable stop signal not from tty 
  (tstp	18)	; stop signal from tty 
  (cont	19)	; continue a stopped process 
  (chld	20)	; to parent on child stop or exit 
  (ttin	21)	; to readers pgrp upon background tty read 
  (ttou	22)	; like TTIN for output if (tp->t_local&LTOSTOP) 

  (io 23)	; I/O possible, or completed
  (xcpu 24)	; cpu time limit exceeded (see setrlimit)
  (xfsz 25)	; file size limit exceeded (see setrlimit)
  (msg 27)	; input data is in the HFT ring buffer
  (winch 28)	; window size changed
  (pwr 29)	; power-fail restart
  (usr1 30)	; user defined signal 1
  (usr2 31)	; user defined signal 2
  (prof 32)	; profiling time alarm (see setitimer)
  (danger 33)	; system crash imminent; free up some page space
  (vtalrm 34)	; virtual time alarm (see setitimer)
  (migrate 35)	; migrate process (see TCF)
  (pre 36)	; programming exception
  (virt 37)	; AIX virtual time alarm
  )

(define signals-ignored-by-default
  (list signal/chld signal/cont		             ; These are Posix.
	signal/urg signal/io signal/winch signal/pwr ; These are AIX
	signal/danger))
