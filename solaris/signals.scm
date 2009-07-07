;;; Signal constant definitions for Sun4
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Modified for Solaris by tvb@math.ufl.edu
 
;; Adapted from signal.h - tvb
 
(define-enum-constants signal
  ;; POSIX
  (hup	1)	; Hangup 
  (int	2)	; Interrupt (Rubout)
  (quit	3)	; Quit (Ascii Fs)
  (ill	4)	; Illegal Instruction (Not Reset When Caught)
;;
  (trap	5)	; Trace Trap (Not Reset When Caught)
  (iot	6)	; Iot Instruction 
  (abrt 6)	; Used By Abort, Replace SIGIOT In The Future (POSIX)
  (emt	7)	; Emt Instruction 
  (fpe	8)	; Floating Point Exception (POSIX)
  (kill	9)	; Kill (Cannot Be Caught Or Ignored)  (POSIX)
  (bus	10)	; Bus Error 
  (segv	11)	; Segmentation Violation (POSIX)
  (sys	12)	; Bad Argument To System Call 
  (pipe	13)	; Write On A Pipe With No One To Read It (POSIX)
  (alrm	14)	; Alarm Clock 
  (term	15)	; Software Termination Signal From Kill  (POSIX)
  (usr1	16)	; User Defined Signal 1  (POSIX)
  (usr2	17)	; User Defined Signal 2  (POSIX)
  (cld	18)	; Child Status Change 
  (chld	18)	; Child Status Change Alias (Posix) 
  (pwr	19)	; Power-Fail Restart 
  (winch 20)	; Window Size Change 
  (urg	21)	; Urgent Socket Condition 
  (poll 22)	; Pollable Event Occured 
  (io	22)	; Socket I/O Possible (poll Alias) 
  (stop 23)	; Stop (Cannot Be Caught Or Ignored)  (POSIX)
  (tstp 24)	; User Stop Requested From Tty  (POSIX)
  (cont 25)	; Stopped Process Has Been Continued  (POSIX)
  (ttin 26)	; Background Tty Read Attempted  (POSIX)
  (ttou 27)	; Background Tty Write Attempted  (POSIX)
  (vtalrm 28)	; Virtual Timer Expired 
  (prof 29)	; Profiling Timer Expired 
  (xcpu 30)	; Exceeded Cpu Limit 
  (xfsz 31)	; Exceeded File Size Limit 
  (waiting 32)	; Process's Lwps Are Blocked 
  (lwp	33)	; Special Signal Used By Thread Library 
  (freeze 34)	; Special Signal Used By Cpr 
  (thaw 35)	; Special Signal Used By Cpr 
  (cancel 36)   ; Thread cancellation signal used by libthread
  )

(define signals-ignored-by-default
  (list signal/chld signal/cont                         ; These are Posix.
        signal/pwr signal/urg signal/winch              ; These are Solaris.
        signal/waiting signal/lwp signal/freeze signal/thaw signal/cancel))
