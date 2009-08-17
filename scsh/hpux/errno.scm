;;; HP-UX errno definitions. This file adapted from errno.h on an HP machine.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.

;;; NOTE: When the hp9000s500 symbol is set, errno.h defines ENOMSG to be 250
;;; instead of 35. What to do? We go with 35 in this file.

(define errno/2big 7) ; 2big is not a legit Scheme symbol. Lose, lose.

(define-enum-constants errno
  (perm		1)			; Not super-user
  (noent	2)			; No such file or directory
  (srch		3)			; No such process
  (intr		4)			; interrupted system call
  (io		5)			; I/O error
  (nxio		6)			; No such device or address
; (2big		7)			; Arg list too long
  (noexec	8)			; Exec format error
  (badf		9)			; Bad file number
  (child	10)			; No children
  (again	11)			; No more processes
  (nomem	12)			; Not enough core
  (acces	13)			; Permission denied
  (fault	14)			; Bad address
  (busy		16)			; Mount device busy
  (exist	17)			; File exists
  (xdev		18)			; Cross-device link
  (nodev	19)			; No such device
  (notdir	20)			; Not a directory
  (isdir	21)			; Is a directory
  (inval	22)			; Invalid argument
  (nfile	23)			; File table overflow
  (mfile	24)			; Too many open files
  (notty	25)			; Not a typewriter
  (fbig		27)			; File too large
  (nospc	28)			; No space left on device
  (spipe	29)			; Illegal seek
  (rofs		30)			; Read only file system
  (mlink	31)			; Too many links
  (pipe		32)			; Broken pipe
  (dom		33)			; Math arg out of domain of func
  (range	34)			; Math result not representable
  (deadlk	45)			; A deadlock would occur
  (nolck	46)			; System record lock table was full
  (ilseq	47)			; Illegal byte sequence
  (notempty	247)			; Directory not empty
  (nametoolong 	248)			; File name too long
  (nosys	251)			; Function not implemented


  ;; Things in XPG3 not in POSIX or ANSI C.
  (notblk	15)			; Block device required
  (txtbsy	26)			; Text file busy
  (nomsg	35)			; No message of desired type
  (idrm		36)			; Identifier removed

  ;; Things in AES not in  XPG3, POSIX or ANSI C.
  (loop		249)			; Too many levels of symbolic links

  ;; Things in HP-UX not in XPG3, POSIX or ANSI C.

  ;; The error numbers between 37 and 44 are not produced by HP-UX. 
  ;; They will track whatever the UNIX(tm) system does in the future.
  (chrng	37)			; Channel number out of range
  (l2nsync	38)			; Level 2 not synchronized
  (l3hlt	39)			; Level 3 halted
  (l3rst	40)			; Level 3 reset
  (lnrng	41)			; Link number out of range
  (unatch	42)			; Protocol driver not attached
  (nocsi	43)			; No CSI structure available
  (l2hlt	44)			; Level 2 halted

  (nonet	50)			; Machine is not on the network
  (nodata	51)			; no data (for no delay io)
  (time		52)			; timer expired
  (nosr		53)			; out of streams resources
  (nostr	54)			; Device not a stream
  (nopkg	55)			; Package not installed
  (nolink	57)			; the link has been severed
  (adv		58)			; advertise error
  (srmnt	59)			; srmount error
  (comm		60)			; Communication error on send
  (proto	61)			; Protocol error
  (multihop	64)			; multihop attempted
  (dotdot	66)			; Cross mount point (not really error)
  (badmsg	67)			; trying to read unreadable message

  (nosym	215)			; symbol does not exist in executable

  (users	68)			; For Sun compatibilty, will not occur.
  (dquot	69)			; Disc quota exceeded

  (stale	70)			; Stale NFS file handle
  (remote	71)			; Too many levels of remote in path

  ;; hp9000s500 only
  (unexpect	99)			; Unexpected Error

  ;; hp9000s300,  hp9000s800
  ;; ipc/network software

  ;; Argument errors
  (notsock		216)		; Socket operation on non-socket
  (destaddrreq		217)		; Destination address required
  (msgsize		218)		; Message too long
  (prototype		219)		; Protocol wrong type for socket
  (noprotoopt		220)		; Protocol not available
  (protonosupport	221)		; Protocol not supported
  (socktnosupport	222)		; Socket type not supported
  (opnotsupp		223)		; Operation not supported
  (pfnosupport		224)		; Protocol family not supported
  (afnosupport		225)		; Address family not supported by
			  		;   protocol family
  (addrinuse		226)		; Address already in use
  (addrnotavail		227)		; Can't assign requested address

  ;; operational errors
  (netdown		228)		; Network is down
  (netunreach		229)		; Network is unreachable
  (netreset		230)		; Network dropped connection on reset
  (connaborted		231)		; Software caused connection abort
  (connreset		232)		; Connection reset by peer
  (nobufs		233)		; No buffer space available
  (isconn		234)		; Socket is already connected
  (notconn		235)		; Socket is not connected
  (shutdown		236)		; Can't send after socket shutdown
  (toomanyrefs		237)		; Too many references: can't splice
  (timedout		238)		; Connection timed out
  (connrefused		239)		; Connection refused

  ;; hp9000s800 only
  (refused	errno/connrefused)	; Double define for NFS

  (remoterelease	240)		; Remote peer released connection
  (hostdown		241)		; Host is down
  (hostunreach		242)		; No route to host
  ;; endif hp9000s300, hp9000s800

  (already		244)		; Operation already in progress
  (inprogress		245)		; Operation now in progress
  (wouldblock		246)		; Operation would block

  ;; hp9000s500 only
; (nomsg		250)		; No message of desired type
  )
