;;; Errno constant definitions.
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.
;;; Revised for Solaris 1994 by tvb@math.ufl.edu

;;; These are the correct values for my SparcStation.

(define errno/2big 7) ; 2big is not a legit Scheme symbol. Lose, lose.

(define-enum-constants errno
  ;; POSIX:
  (perm			1)	; Not Super-User			
  (noent		2)	; No Such File Or Directory		
  (srch			3)	; No Such Process			
  (intr			4)	; Interrupted System Call		
  (io			5)	; I/O Error				
  (nxio			6)	; No Such Device Or Address		
;  (2big		7)	; Arg List Too Long			
  (noexec		8)	; Exec Format Error			
  (badf			9)	; Bad File Number			
  (child		10)	; No Children				
  (again		11)	; Resource Temporarily Unavailable	
  (nomem		12)	; Not Enough Core			
  (acces		13)	; Permission Denied			
  (fault		14)	; Bad Address				

  (notblk		15)	; Block Device Required		

  ;; POSIX:
  (busy			16)	; Mount Device Busy			
  (exist		17)	; File Exists				
  (xdev			18)	; Cross-Device Link			
  (nodev		19)	; No Such Device			
  (notdir		20)	; Not A Directory			
  (isdir		21)	; Is A Directory			
  (inval		22)	; Invalid Argument			
  (nfile		23)	; File Table Overflow			
  (mfile		24)	; Too Many Open Files			
  (notty		25)	; Inappropriate Ioctl For Device	
;;
  (txtbsy		26)	; Text File Busy			

  ;; POSIX:
  (fbig			27)	; File Too Large			
  (nospc		28)	; No Space Left On Device		
  (spipe		29)	; Illegal Seek				
  (rofs			30)	; Read Only File System		
  (mlink		31)	; Too Many Links			
  (pipe			32)	; Broken Pipe				
  (dom			33)	; Math Arg Out Of Domain Of Func	
  (range		34)	; Math Result Not Representable	
;;
  (nomsg		35)	; No Message Of Desired Type		
  (idrm			36)	; Identifier Removed			
  (chrng		37)	; Channel Number Out Of Range		
  (l2nsync		38)	; Level 2 Not Synchronized		
  (l3hlt		39)	; Level 3 Halted			
  (l3rst		40)	; Level 3 Reset			
  (lnrng		41)	; Link Number Out Of Range		
  (unatch		42)	; Protocol Driver Not Attached		
  (nocsi		43)	; No Csi Structure Available		
  (l2hlt		44)	; Level 2 Halted			

  ;; POSIX:
  (deadlk		45)	; Deadlock Condition.			
  (nolck		46)	; No Record Locks Available.		

;;
  (canceled		47)	; Operation Canceled			
  (notsup		48)	; Operation Not Supported		
  
  ; Convergent Error Returns 
  (bade			50)	; Invalid Exchange			
  (badr			51)	; Invalid Request Descriptor		
  (xfull		52)	; Exchange Full			
  (noano		53)	; No Anode				
  (badrqc		54)	; Invalid Request Code			
  (badslt		55)	; Invalid Slot				
  (deadlock		56)	; File Locking Deadlock Error		
  
  (bfont		57)	; Bad Font File Fmt			
  
  ; Stream Problems 
  (nostr		60)	; Device Not A Stream			
  (nodata		61)	; No Data (for No Delay Io)		
  (time			62)	; Timer Expired			
  (nosr			63)	; Out Of Streams Resources		
  
  (nonet		64)	; Machine Is Not On The Network	
  (nopkg		65)	; Package Not Installed		
  (remote		66)	; The Object Is Remote			
  (nolink		67)	; The Link Has Been Severed		
  (adv			68)	; Advertise Error			
  (srmnt		69)	; Srmount Error			
  
  (comm			70)	; Communication Error On Send		
  (proto		71)	; Protocol Error			
  (multihop		74)	; Multihop Attempted			
  (badmsg		77)	; Trying To Read Unreadable Message	
  (nametoolong		78)	; Path Name Is Too Long	 (POSIX)
  (overflow		79)	; Value Too Large To Be Stored In Data Type 
  (notuniq		80)	; Given Log. Name Not Unique		
  (badfd		81)	; F.D. Invalid For This Operation	
  (remchg		82)	; Remote Address Changed		
  
  ; Shared Library Problems 
  (libacc		83)	; Can'T Access A Needed Shared Lib.	
  (libbad		84)	; Accessing A Corrupted Shared Lib.	
  (libscn		85)	; .Lib Section In A.Out Corrupted.	
  (libmax		86)	; Attempting To Link In Too Many Libs.	
  (libexec		87)	; Attempting To Exec A Shared Library.	
  (ilseq		88)	; Illegal Byte Sequence.		
  (nosys		89)	; Unsupported File System Operation  (POSIX)
  (loop			90)	; Symbolic Link Loop			
  (restart		91)	; Restartable System Call		
  (strpipe		92)	; If Pipe/Fifo, Don'T Sleep In Stream Head 
  (notempty		93)	; Directory Not Empty	(POSIX)
  (users		94)	; Too Many Users (for Ufs)		
  
  ; Bsd Networking Software 
; Argument Errors 
  (notsock		95)	; Socket Operation On Non-Socket 
  (destaddrreq		96)	; Destination Address Required 
  (msgsize		97)	; Message Too Long 
  (prototype		98)	; Protocol Wrong Type For Socket 
  (noprotoopt		99)	; Protocol Not Available 
  (protonosupport	120)	; Protocol Not Supported 
  (socktnosupport	121)	; Socket Type Not Supported 
  (opnotsupp		122)	; Operation Not Supported On Socket 
  (pfnosupport		123)	; Protocol Family Not Supported 
  (afnosupport		124)	; Address Family Not Supported By 
; Protocol Family 
  (addrinuse		125)	; Address Already In Use 
  (addrnotavail		126)	; Can'T Assign Requested Address 
; Operational Errors 
  (netdown		127)	; Network Is Down 
  (netunreach		128)	; Network Is Unreachable 
  (netreset		129)	; Network Dropped Connection Because 
				; Of Reset 
  (connaborted		130)	; Software Caused Connection Abort 
  (connreset		131)	; Connection Reset By Peer 
  (nobufs		132)	; No Buffer Space Available 
  (isconn		133)	; Socket Is Already Connected 
  (notconn		134)	; Socket Is Not Connected 
; Xenix Has 135 - 142 
  (shutdown		143)	; Can'T Send After Socket Shutdown 
  (toomanyrefs		144)	; Too Many References: Can'T Splice 
  (timedout		145)	; Connection Timed Out 
  (connrefused		146)	; Connection Refused 
  (hostdown		147)	; Host Is Down 
  (hostunreach		148)	; No Route To Host 
  (wouldblock		11)	; (again)
  (already		149)	; Operation Already In Progress 
  (inprogress		150)	; Operation Now In Progress 
  
  ; Sun Network File System 
  (stale		151)	; Stale Nfs File Handle 
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Non-POSIX messages
; Some of these Solaris messages are better. Some are stupid.
;
; Error	 Solaris			    POSIX
; perm	 Not super-user			    Operation not permitted
; intr	 Interrupted system call	    Interrupted function call
; io	 I/O error			    Input/output error
; badf	 Bad file number		    Bad file descriptor
; child	 No children			    No child processes
; nomem	 Not enough core 		    Not enough space
; busy	 Mount device busy		    Resource busy
; xdev	 Cross-device link		    Improper link
; nfile	 File table overflow		    Too many open files in system
; notty	 Inappropriate ioctl for device	    Inappropriate I/O control operation
; spipe	 Illegal seek			    Invalid seek
; dom	 Math arg out of domain of func	    Domain error
; deadlk Deadlock condition		    Resource deadlock avoided
; nolck	 No record locks available	    No locks available
