;;; Errno constant definitions.
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.
;;; AIX version by Chipsy Sperber

(define errno/2big 7) ; 2big is not a legit Scheme symbol. Lose, lose.

(define-enum-constants errno
  ;; POSIX:
  (perm		1)		; Operation not permitted
  (noent	2)		; No such file or directory
  (srch		3)		; No such process
  (intr		4)		; Interrupted function call
  (io		5)		; Input/output error
  (nxio		6)		; No such device or address
; (2big		7)		; Arg list too long
  (noexec	8)		; Exec format error
  (badf		9)		; Bad file descriptor
  (child	10)		; No child processes
  (again	11)		; Resource temporarily unavailable
  (nomem	12)		; Not enough space
  (acces	13)		; Permission denied
  (fault	14)		; Bad address
  (notblk	15)		; Block device required
  (busy		16)		; Resource busy
  (exist	17)		; File exists
  (xdev		18)		; Improper link
  (nodev	19)		; No such device
  (notdir	20)		; Not a directory
  (isdir	21)		; Is a directory
  (inval	22)		; Invalid argument
  (nfile	23)		; Too many open files in system
  (mfile	24)		; Too many open files
  (notty	25)		; Inappropriate I/O control operation
  (xtbsy	26)		; Text file busy
  (fbig		27)		; File too large
  (nospc	28)		; No space left on device
  (spipe	29)		; Invalid seek
  (rofs		30)		; Read-only file system
  (mlink	31)		; Too many links
  (pipe		32)		; Broken pipe
 
  ;; POSIX:
  ;; math software
  (dom		33)		; Domain error
  (range	34)		; Result too large
 
  (nomsg        35)             ; No message of desired type
  (idrm         36)             ; Identifier removed
  (chrng        37)             ; Channel number out of range
  (l2nsync      38)             ; Level 2 not synchronized
  (l3hlt	39)		; Level 3 halted
  (l3rst	40)		; Level 3 reset
  (lnrng	41)		; Link number out of range
  (unatch 	42)		; Protocol driver not attached
  (nocsi	43)		; No CSI structure available
  (l2hlt	44)		; Level 2 halted
  (deadlk 	45)		; Resource deadlock avoided

  (notready	46)		; Device not ready
  (wrprotect	47)		; Write-protected media
  (format	48)		; Unformatted media

  (nolck		49)		; No locks available

  ;; non-blocking and interrupt i/o
  (wouldblock	54)		; Operation would block

  (inprogress     55)		; Operation now in progress
  (already        56)		; Operation already in progress

  ;; ipc/network software
  (notsock        57)		; Socket operation on non-socket
  (destaddrreq    58)		; Destination address required
  (msgsize        59)		; Message too long
  (prototype      60)		; Protocol wrong type for socket
  (noprotoopt     61)		; Protocol not available
  (protonosupport 62)		; Protocol not supported
  (socktnosupport 63)		; Socket type not supported
  (opnotsupp      64)		; Operation not supported on socket
  (pfnosupport    65)		; Protocol family not supported
  (afnosupport    66)		; Address family not supported by protocol family
  (addrinuse      67)		; Address already in use
  (addrnotavail   68)		; Can't assign requested address
  (netdown        69)		; Network is down
  (netunreach     70)		; Network is unreachable
  (netreset       71)		; Network dropped connection on reset
  (connaborted    72)		; Software caused connection abort
  (connreset      73)		; Connection reset by peer
  (nobufs         74)		; No buffer space available
  (isconn         75)		; Socket is already connected
  (notconn        76)		; Socket is not connected
  (shutdown       77)		; Can't send after socket shutdown

  (timedout       78)		; Connection timed out
  (connrefused    79)		; Connection refused

  (hostdown       80)		; Host is down
  (hostunreach    81)		; No route to host

  (restart	82)		; restart the system call

  ;; quotas and limits
  (proclim	83)		; Too many processes
  (users		84)		; Too many users
  (loop		85)		; Too many levels of symbolic links
  (nametoolong	86)		; File name too long

  (notempty	87)		; Directory not empty
  (dquot	88)		; Disc quota exceeded

  ;; network file system
  (remote	93)		; Item is not local to host

  (nosys	109)		; Function not implemented  POSIX

  ;; disk device driver
  (media	110)		; media surface error
  (soft           111)		; I/O completed, but needs relocation

  ;; security
  (noattr	112)		; no attribute found
  (sad		113)		; security authentication denied
  (notrust	114)		; not a trusted program

  ;; BSD 4.3 RENO
  (toomanyrefs    115)		; Too many references: can't splice

  (ilseq	116)		; Invalid wide character
  (canceled 	117)		; asynchronous i/o cancelled

  ;; SVR4 STREAMS
  (nosr		118)		; temp out of streams resources
  (time		119)		; I_STR ioctl timed out
  (badmsg	120)		; wrong message type at stream head
  (proto	121)		; STREAMS protocol error
  (nodata	122)		; no message ready at stream head
  (nostr	123)		; fd is not a stream

  (cloneme	82)		; this is the way we clone a stream
)
