;;; Interfaces and packages for the Cygwin32 specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers.
;;; Copyright (c) 1999 by Brian D. Carlstrom.

(define-interface cygwin32-fdflags-extras-interface
  (export open/shared-lock
	  open/exclusive-lock
	  open/async
	  open/fsync
	  fcntl/get-owner
	  fcntl/set-owner))

(define-interface cygwin32-errno-extras-interface
  (export errno/notblk	  
	  errno/txtbsy
	  errno/wouldblock
	  errno/inprogress
	  errno/already
	  errno/notsock
	  errno/destaddrreq
	  errno/msgsize
	  errno/prototype
	  errno/noprotoopt
	  errno/protonosupport
	  errno/socktnosupport
	  errno/opnotsupp
	  errno/pfnosupport
	  errno/afnosupport
	  errno/addrinuse
	  errno/addrnotavail
	  errno/netdown
	  errno/netunreach
	  errno/netreset
	  errno/connaborted
	  errno/connreset
	  errno/nobufs
	  errno/isconn
	  errno/notconn
	  errno/shutdown
	  errno/toomanyrefs
	  errno/timedout
	  errno/connrefused
	  errno/loop
	  errno/hostdown
	  errno/hostunreach
	  errno/proclim
	  errno/users
	  errno/dquot
	  errno/stale
	  errno/remote
	  errno/last))

(define-interface cygwin32-signals-extras-interface
  (export signal/trap
	  signal/emt
	  signal/bus
	  signal/sys
	  signal/urg
	  signal/cld
	  signal/io
	  signal/xcpu
	  signal/xfsz
	  signal/vtalrm
	  signal/prof
	  signal/winch
	  signal/info))

(define-interface cygwin32-network-extras-interface
  (export socket/debug
	  socket/accept-connect
	  socket/reuse-address
	  socket/keep-alive
	  socket/dont-route
	  socket/broadcast
	  socket/use-loop-back
	  socket/linger
	  socket/oob-inline
	  socket/send-buffer
	  socket/receive-buffer
	  socket/send-low-water
	  socket/receive-low-water
	  socket/send-timeout
	  socket/receive-timeout
	  socket/error
	  socket/type
;;; all ip/* but ip/options and ip/time-to-live cygwin32 only
	  ip/options
	  ip/type-of-service
	  ip/time-to-live		
	  ip/multicast-if		
	  ip/multicast-ttl	
	  ip/multicast-loop	
	  ip/add-membership	
	  ip/drop-membership	
	  tcp/no-delay
	  tcp/max-segment))

(define-interface cygwin32-extras-interface
  (compound-interface cygwin32-errno-extras-interface
		      cygwin32-fdflags-extras-interface
		      cygwin32-network-extras-interface
		      cygwin32-signals-extras-interface))

(define-interface cygwin32-defs-interface
  (compound-interface cygwin32-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface
		      posix-signals-interface
		      signals-internals-interface))

(define-structure cygwin32-defs cygwin32-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno signals netconst))

(define-interface os-extras-interface cygwin32-extras-interface)
(define os-dependent cygwin32-defs)
