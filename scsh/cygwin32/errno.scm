;;; Errno constant definitions.
;;; Copyright (c) 1993 by Olin Shivers.
;;; Copyright (c) 1999 by Brian D. Carlstrom.

;;; These are the correct values for Cygwin32

(define errno/2big 7) ; 2big is not a legit Scheme symbol. Lose, lose.

(define-enum-constants errno
  (perm 1)              ; Not super-user 
  (noent 2)             ; No such file or directory 
  (srch 3)              ; No such process 
  (intr 4)              ; Interrupted system call 
  (io 5)                ; I/O error 
  (nxio 6)              ; No such device or address 
; (2big 7)              ; Arg list too long 
  (noexec 8)            ; Exec format error 
  (badf 9)              ; Bad file number 
  (child 10)            ; No children 
  (again 11)            ; No more processes 
  (wouldblock 11)       ; EAGAIN again
  (nomem 12)            ; Not enough core 
  (acces 13)            ; Permission denied 
  (fault 14)            ; Bad address 
  (notblk 15)           ; Block device required 
  (busy 16)             ; Mount device busy 
  (exist 17)            ; File exists 
  (xdev 18)             ; Cross-device link 
  (nodev 19)            ; No such device 
  (notdir 20)           ; Not a directory 
  (isdir 21)            ; Is a directory 
  (inval 22)            ; Invalid argument 
  (nfile 23)            ; Too many open files in system 
  (mfile 24)            ; Too many open files 
  (notty 25)            ; Not a typewriter 
  (txtbsy 26)           ; Text file busy 
  (fbig 27)             ; File too large 
  (nospc 28)            ; No space left on device 
  (spipe 29)            ; Illegal seek 
  (rofs 30)             ; Read only file system 
  (mlink 31)            ; Too many links 
  (pipe 32)             ; Broken pipe 
  (dom 33)              ; Math arg out of domain of func 
  (range 34)            ; Math result not representable 
  (nomsg 35)            ; No message of desired type 
  (idrm 36)             ; Identifier removed 
  (chrng 37)            ; Channel number out of range 
  (l2nsync 38)          ; Level 2 not synchronized 
  (l3hlt 39)            ; Level 3 halted 
  (l3rst 40)            ; Level 3 reset 
  (lnrng 41)            ; Link number out of range 
  (unatch 42)           ; Protocol driver not attached 
  (nocsi 43)            ; No CSI structure available 
  (l2hlt 44)            ; Level 2 halted 
  (deadlk 45)           ; Deadlock condition 
  (nolck 46)            ; No record locks available 
  (bade 50)             ; Invalid exchange 
  (badr 51)             ; Invalid request descriptor 
  (xfull 52)            ; Exchange full 
  (noano 53)            ; No anode 
  (badrqc 54)           ; Invalid request code 
  (badslt 55)           ; Invalid slot 
  (deadlock 56)         ; File locking deadlock error 
  (bfont 57)            ; Bad font file fmt 
  (nostr 60)            ; Device not a stream 
  (nodata 61)           ; No data (for no delay io) 
  (time 62)             ; Timer expired 
  (nosr 63)             ; Out of streams resources 
  (nonet 64)            ; Machine is not on the network 
  (nopkg 65)            ; Package not installed 
  (remote 66)           ; The object is remote 
  (nolink 67)           ; The link has been severed 
  (adv 68)              ; Advertise error 
  (srmnt 69)            ; Srmount error 
  (comm 70)             ; Communication error on send 
  (proto 71)            ; Protocol error 
  (multihop 74)         ; Multihop attempted 
  (lbin 75)             ; Inode is remote (not really error) 
  (dotdot 76)           ; Cross mount point (not really error) 
  (badmsg 77)           ; Trying to read unreadable message 
  (notuniq 80)          ; Given log. name not unique 
  (badfd 81)            ; f.d. invalid for this operation 
  (remchg 82)           ; Remote address changed 
  (libacc 83)           ; Can't access a needed shared lib 
  (libbad 84)           ; Accessing a corrupted shared lib 
  (libscn 85)           ; .lib section in a.out corrupted 
  (libmax 86)           ; Attempting to link in too many libs 
  (libexec 87)          ; Attempting to exec a shared library 
  (nosys 88)            ; Function not implemented 
  (nmfile 89)           ; No more files 
  (notempty 90)         ; Directory not empty 
  (nametoolong 91)      ; File or path name too long 
  (loop 92)             ; Too many symbolic links 
  (opnotsupp 95)        ; Operation not supported on transport endpoint 
  (pfnosupport 96)      ; Protocol family not supported 
  (connreset 104)       ; Connection reset by peer 
  (nobufs 105)          ; No buffer space available 
  (afnosupport 106)     ;
  (prototype 107)       ;
  (notsock 108)         ;
  (noprotoopt 109)      ;
  (shutdown 110)        ;
  (connrefused 111)     ; Connection refused 
  (addrinuse 112)       ; Address already in use 
  (connaborted 113)     ; Connection aborted 
  (netunreach 114)      ;
  (netdown 115)         ;
  (timedout 116)        ;
  (hostdown 117)        ;
  (hostunreach 118)     ;
  (inprogress 119)      ;
  (already 120)         ;
  (destaddrreq 121)     ;
  (msgsize 122)         ;
  (protonosupport 123)  ;
  (socktnosupport 124)  ;
  (addrnotavail 125)    ;
  (netreset 126)        ;
  (isconn 127)          ;
  (notconn 128)         ;
  (toomanyrefs 129)     ;
  (proclim 130)         ;
  (users 131)           ;
  (dquot 132)           ;
  (stale 133)           ;
  (notsup 134)          ;
  (last   134))         ; Must be equal largest errno
