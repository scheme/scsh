;;; Errno constant definitions.
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

;;; These are the correct values for my SparcStation.

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
 
  ;; non-blocking and interrupt i/o
  (wouldblock	35)		; Operation would block
  (inprogress	36)		; Operation now in progress
  (already	37)		; Operation already in progress

  ;; ipc/network software
  
  ;; argument errors
  (notsock	38)		; Socket operation on non-socket
  (destaddrreq	39)		; Destination address required
  (msgsize	40)		; Message too long
  (prototype	41)		; Protocol wrong type for socket
  (noprotoopt	42)		; Protocol not available
  (protonosupport 43)		; Protocol not supported
  (socktnosupport 44)		; Socket type not supported
  (opnotsupp	45)		; Operation not supported on socket
  (pfnosupport	46)		; Protocol family not supported
  (afnosupport	47)		; Address family not supported by protocol family
  (addrinuse	48)		; Address already in use
  (addrnotavail	49)		; Can't assign requested address
  
  ;; operational errors
  (netdown	50)		; Network is down
  (netunreach	51)		; Network is unreachable
  (netreset	52)		; Network dropped connection on reset
  (connaborted	53)		; Software caused connection abort
  (connreset	54)		; Connection reset by peer
  (nobufs	55)		; No buffer space available
  (isconn	56)		; Socket is already connected
  (notconn	57)		; Socket is not connected
  (shutdown	58)		; Can't send after socket shutdown
  (toomanyrefs	59)		; Too many references: can't splice
  (timedout	60)		; Connection timed out
  (connrefused	61)		; Connection refused
  
  (loop		62)		; Too many levels of symbolic links
  
  ;; POSIX:
  (nametoolong	63)		; File name too long
  
  ;; should be rearranged
  (hostdown	64)		; Host is down
  (hostunreach	65)		; No route to host
  
  ;; POSIX:
  (notempty	66)		; Directory not empty
  
  ;; quotas & mush
  (proclim	67)		; Too many processes
  (users	68)		; Too many users
  (dquot	69)		; Disc quota exceeded
  
  ;; Network File System
  (stale	70)		; Stale NFS file handle
  (remote	71)		; Too many levels of remote in path
  
  ;; streams
  (nostr	72)		; Device is not a stream
  (time		73)		; Timer expired
  (nosr		74)		; Out of streams resources
  (nomsg	75)		; No message of desired type
  (badmsg	76)		; Trying to read unreadable message
  
  ;; SystemV IPC
  (idrm		77)		; Identifier removed
  
  ;; POSIX
  ;; SystemV Record Locking
  (deadlk	78)		; Resource deadlock avoided
  (nolck	79)		; No locks available
  
  ;; RFS
  (nonet	80)		; Machine is not on the network
  (rremote	81)		; Object is remote
  (nolink	82)		; the link has been severed
  (adv		83)		; advertise error
  (srmnt	84)		; srmount error
  (comm		85)		; Communication error on send
  (proto	86)		; Protocol error
  (multihop	87)		; multihop attempted
  (dotdot	88)		; Cross mount point (not an error)
  (remchg	89)		; Remote address changed
  
  ;; POSIX
  (nosys	90))		; function not implemented

