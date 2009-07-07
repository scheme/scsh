;;; Signal constant definitions for "generic"
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
  (segv	11)	; segmentation violation
  (pipe	13)	; write on a pipe with no one to read it 
  (alrm	14)	; alarm clock 
  (term	15)	; software termination signal from kill 
  (stop	17)	; sendable stop signal not from tty 
  (tstp	18)	; stop signal from tty 
  (cont	19)	; continue a stopped process 
  (chld	20)	; to parent on child stop or exit 
  (ttin	21)	; to readers pgrp upon background tty read 
  (ttou	22)	; like TTIN for output if (tp->t_local&LTOSTOP) 
  ;; User defined
  (usr1 30)	; user defined signal 1 
  (usr2 31)	; user defined signal 2 
  )

(define signals-ignored-by-default
  (list signal/chld signal/cont))			; These are Posix.
