;;; Errno constant definitions.
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

;;; These are the correct values for the GNU Hurd.

(define (hurd-errno n)
  (bitwise-ior (arithmetic-shift #x10 26) (bitwise-and n #x3fff)))

(define errno/perm (hurd-errno 1))		; Operation not permitted
(define errno/noent (hurd-errno 2))		; No such file or directory
(define errno/srch (hurd-errno 3))		; No such process
(define errno/intr (hurd-errno 4))		; Interrupted system call
(define errno/io (hurd-errno 5))		; Input/output error
(define errno/nxio (hurd-errno 6))		; No such device or address
(define errno/2big (hurd-errno 7))		; Argument list too long
(define errno/noexec (hurd-errno 8))		; Exec format error
(define errno/badf (hurd-errno 9))		; Bad file descriptor
(define errno/child (hurd-errno 10))		; No child processes
(define errno/deadlk (hurd-errno 11))		; Resource deadlock avoided
(define errno/nomem (hurd-errno 12))		; Cannot allocate memory
(define errno/acces (hurd-errno 13))		; Permission denied
(define errno/fault (hurd-errno 14))		; Bad address
(define errno/notblk (hurd-errno 15))		; Block device required
(define errno/busy (hurd-errno 16))		; Device or resource busy
(define errno/exist (hurd-errno 17))		; File exists
(define errno/xdev (hurd-errno 18))		; Invalid cross-device link
(define errno/nodev (hurd-errno 19))		; No such device
(define errno/notdir (hurd-errno 20))		; Not a directory
(define errno/isdir (hurd-errno 21))		; Is a directory
(define errno/inval (hurd-errno 22))		; Invalid argument
(define errno/nfile (hurd-errno 23))		; Too many open files in system
(define errno/mfile (hurd-errno 24))		; Too many open files
(define errno/notty (hurd-errno 25))		; Inappropriate ioctl for device
(define errno/txtbsy (hurd-errno 26))		; Text file busy
(define errno/fbig (hurd-errno 27))		; File too large
(define errno/nospc (hurd-errno 28))		; No space left on device
(define errno/spipe (hurd-errno 29))		; Illegal seek
(define errno/rofs (hurd-errno 30))		; Read-only file system
(define errno/mlink (hurd-errno 31))		; Too many links
(define errno/pipe (hurd-errno 32))		; Broken pipe
(define errno/dom (hurd-errno 33))		; Numerical argument out of domain
(define errno/range (hurd-errno 34))		; Numerical result out of range
(define errno/again (hurd-errno 35))		; Resource temporarily unavailable
(define errno/wouldblock (hurd-errno 35))	; Operation would block
(define errno/inprogress (hurd-errno 36))	; Operation now in progress
(define errno/already (hurd-errno 37))		; Operation already in progress
(define errno/notsock (hurd-errno 38))		; Socket operation on non-socket
(define errno/destaddrreq (hurd-errno 39))	; Destination address required
(define errno/msgsize (hurd-errno 40))		; Message too long
(define errno/prototype (hurd-errno 41))	; Protocol wrong type for socket
(define errno/noprotoopt (hurd-errno 42))	; Protocol not available
(define errno/protonosupport (hurd-errno 43))	; Protocol not supported
(define errno/socktnosupport (hurd-errno 44))	; Socket type not supported
(define errno/opnotsupp (hurd-errno 45))	; Operation not supported
(define errno/pfnosupport (hurd-errno 46))	; Protocol family not supported
(define errno/afnosupport (hurd-errno 47))	; Address family not supported by protocol
(define errno/addrinuse (hurd-errno 48))	; Address already in use
(define errno/addrnotavail (hurd-errno 49))	; Cannot assign requested address
(define errno/netdown (hurd-errno 50))		; Network is down
(define errno/netunreach (hurd-errno 51))	; Network is unreachable
(define errno/netreset (hurd-errno 52))		; Network dropped connection on reset
(define errno/connaborted (hurd-errno 53))	; Software caused connection abort
(define errno/connreset (hurd-errno 54))	; Connection reset by peer
(define errno/nobufs (hurd-errno 55))		; No buffer space available
(define errno/isconn (hurd-errno 56))		; Transport endpoint is already connected
(define errno/notconn (hurd-errno 57))		; Transport endpoint is not connected
(define errno/shutdown (hurd-errno 58))		; Cannot send after transport endpoint shutdown
(define errno/toomanyrefs (hurd-errno 59))	; Too many references: cannot splice
(define errno/timedout (hurd-errno 60))		; Connection timed out
(define errno/connrefused (hurd-errno 61))	; Connection refused
(define errno/loop (hurd-errno 62))		; Too many levels of symbolic links
(define errno/nametoolong (hurd-errno 63))	; File name too long
(define errno/hostdown (hurd-errno 64))		; Host is down
(define errno/hostunreach (hurd-errno 65))	; No route to host
(define errno/notempty (hurd-errno 66))		; Directory not empty
(define errno/proclim (hurd-errno 67))		; Too many processes
(define errno/users (hurd-errno 68))		; Too many users
(define errno/dquot (hurd-errno 69))		; Disk quota exceeded
(define errno/stale (hurd-errno 70))		; Stale NFS file handle
(define errno/remote (hurd-errno 71))		; Object is remote
(define errno/badrpc (hurd-errno 72))		; RPC struct is bad
(define errno/rpcmismatch (hurd-errno 73))	; RPC version wrong
(define errno/progunavail (hurd-errno 74))	; RPC program not available
(define errno/progmismatch (hurd-errno 75))	; RPC program version wrong
(define errno/procunavail (hurd-errno 76))	; RPC bad procedure for program
(define errno/nolck (hurd-errno 77))		; No locks available
(define errno/nosys (hurd-errno 78))		; Function not implemented
(define errno/ftype (hurd-errno 79))		; Inappropriate file type or format
(define errno/auth (hurd-errno 80))		; Authentication error
(define errno/needauth (hurd-errno 81))		; Need authenticator
(define errno/background (hurd-errno 100))	; Inappropriate operation for background process
(define errno/died (hurd-errno 101))		; Translator died
(define errno/d (hurd-errno 102))		; ?
(define errno/gregious (hurd-errno 103))	; You really blew it this time
(define errno/ieio (hurd-errno 104))		; Computer bought the farm
(define errno/gratuitous (hurd-errno 105))	; Gratuitous error
(define errno/ilseq (hurd-errno 106))		; Invalid or incomplete multibyte or wide character
(define errno/badmsg (hurd-errno 107))		; Bad message
(define errno/idrm (hurd-errno 108))		; Identifier removed
(define errno/multihop (hurd-errno 109))	; Multihop attempted
(define errno/nodata (hurd-errno 110))		; No data available
(define errno/nolink (hurd-errno 111))		; Link has been severed
(define errno/nomsg (hurd-errno 112))		; No message of desired type
(define errno/nosr (hurd-errno 113))		; Out of streams resources
(define errno/nostr (hurd-errno 114))		; Device not a stream
(define errno/overflow (hurd-errno 115))	; Value too large for defined data type
(define errno/proto (hurd-errno 116))		; Protocol error
(define errno/time (hurd-errno 117))		; Timer expired
(define errno/canceled (hurd-errno 118))	; Operation cancelled
(define errno/notsup (hurd-errno 118))		; Not supported
