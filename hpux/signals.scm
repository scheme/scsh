;;; Signal constant definitions for HP-UX
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

(define-enum-constants signal
  (hup	1)		; floating point exception 
  (int	2)		; Interrupt 
  (quit	3)		; quit 
  (ill	4)		; Illegal instruction (not reset when caught) 
  (trap	5)		; trace trap (not reset when caught) 
  (abrt	6)		; Process abort signal 
  (iot	signal/abrt)	; IOT instruction 
  (emt	7)		; EMT instruction 
  (fpe	8)		; Floating point exception 
  (kill	9)		; kill (cannot be caught of ignored) 
  (bus	10)		; bus error 
  (segv	11)		; Segmentation violation 
  (sys	12)		; bad argument to system call 
  (pipe	13)		; write on a pipe with no one to read it 
  (alrm	14)		; alarm clock 
  (term	15)		; Software termination signal from kill 
  (usr1	16)		; user defined signal 1 
  (usr2	17)		; user defined signal 2 
  (chld	18)		; Child process terminated or stopped 
  (cld	signal/chld)	; death of a child 
  (pwr	19)		; power state indication 
  (vtalrm 20)		; virtual timer alarm 
  (prof	21)		; profiling timer alarm 
  (io	22)		; asynchronous I/O 
  (poll	signal/io)	; for HP-UX hpstreams signal 
  (winch 23)		; window size change signal 
  (window signal/winch)	; added for compatibility reasons 
  (stop	24)		; Stop signal (cannot be caught or ignored) 
  (tstp	25)		; Interactive stop signal 
  (cont	26)		; Continue if stopped 
  (ttin	27)		; Read from control terminal attempted by a
			;   member of a background process group 
  (ttou	28)		; Write to control terminal attempted by a 
			;   member of a background process group 
  (urg	29)		; urgent condition on IO channel 
  (lost	30)		; remote lock lost  (NFS)        
			; Signal 31 is reserved for future use.
  (dil	32))		; DIL signal 


(define signals-ignored-by-default
  (list signal/chld signal/cont				; These are Posix.
	signal/io signal/pwr signal/urg signal/winch))	; These are HP-UX.
