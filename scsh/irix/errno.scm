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
 
  ;; SystemV IPC
  (idrm		36)		; Identifier removed
  (chrng	37)		; channel number out of range		
  (l2nsync 	38)		; level 2 not synchronized		
  (l3hlt	39)		; level 3 halted			
  (l3rst	40)		; level 3 reset			
  (lnrng	41)		; link number out of range		
  (unatch 	42)		; protocol driver not attached		
  (nocsi	43)		; no csi structure available		
  (l2hlt	44)		; level 2 halted			
  
  ;; POSIX
  ;; SystemV Record Locking
  (deadlk	45)		; Resource deadlock avoided
  (nolck	46)		; No locks available
  
  (bade		50) 		; bad exchange descriptor		
  (badr		51) 		; bad request descriptor		
  (xfull	52) 		; message tables full			
  (noano	53) 		; anode table overflow			
  (badrqc	54) 		; bad request code			
  (badslt	55) 		; invalid slot				
  (deadlock 	56) 		; file locking deadlock 		

  (bfont	57) 		; bad font file format			

  ;; streams
  (nostr	60)		; Device is not a stream
  (time		62)		; Timer expired
  (nosr		63)		; Out of streams resources
  (nomsg	35)		; No message of desired type
  (badmsg	77)		; Trying to read unreadable message
  
  ;; RFS
  (nonet	64)		; Machine is not on the network
  (rremote	66)		; Object is remote
  (nolink	67)		; the link has been severed
  (adv		68)		; advertise error
  (srmnt	69)		; srmount error
  (comm		70)		; Communication error on send
  (proto	71)		; Protocol error
  (multihop	74)		; multihop attempted
; (dotdot	)		; Cross mount point (not an error)
  (notuniq 	80)		; name not unique on network	       
  (badfd	81)		; file descriptor in bad state		
  (remchg	82)		; Remote address changed
  
  (libacc	83)		; can not access a needed shared lib.
  (libbad	84)		; accessing a corrupted shared lib.
  (libscn	85)		; .lib section in a.out corrupted.
  (libmax	86)		; attempting to link in more shared libraries than system limit
  (libexec 	87)		; can not exec a shared library directly
  (nosys	88)		; irix uses einval; posix wants enosys

  ;; POSIX
  (nosys	88)		; function not implemented

  ;; non-blocking and interrupt i/o
  (wouldblock	101)		; Operation would block
  (inprogress	102)		; Operation now in progress
  (already	103)		; Operation already in progress

  ;; ipc/network software
  
  ;; argument errors
  (notsock	104)		; Socket operation on non-socket
  (destaddrreq	105)		; Destination address required
  (msgsize	106)		; Message too long
  (prototype	107)		; Protocol wrong type for socket
  (noprotoopt	108)		; Protocol not available
  (protonosupport 109)		; Protocol not supported
  (socktnosupport 110)		; Socket type not supported
  (opnotsupp	111)		; Operation not supported on socket
  (pfnosupport	112)		; Protocol family not supported
  (afnosupport	113)		; Address family not supported by protocol family
  (addrinuse	114)		; Address already in use
  (addrnotavail	115)		; Can't assign requested address
  
  ;; operational errors
  (netdown	116)		; Network is down
  (netunreach	117)		; Network is unreachable
  (netreset	118)		; Network dropped connection on reset
  (connaborted	119)		; Software caused connection abort
  (connreset	120)		; Connection reset by peer
  (nobufs	121)		; No buffer space available
  (isconn	122)		; Socket is already connected
  (notconn	123)		; Socket is not connected
  (shutdown     124)		; Can't send after socket shutdown
  (toomanyrefs	125)		; Too many references: can't splice
  (timedout	126)		; Connection timed out
  (connrefused	127)		; Connection refused
  (hostdown	128)		; Host is down
  (hostunreach	129)		; No route to host
  
  (loop		130)		; Too many levels of symbolic links
  
  ;; POSIX:
  (nametoolong	131)		; File name too long
  
  ;; POSIX:
  (notempty	132)		; Directory not empty
  
  ;; quotas & mush
;  (proclim	)		; Too many processes
  (users	133)		; Too many users
  (dquot	134)		; Disc quota exceeded
  
  ;; Network File System
  (stale	135)		; Stale NFS file handle
  (remote	136)		; Too many levels of remote in path
  )
