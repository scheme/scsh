;;; Interfaces and packages for the BSD4.4-Lite specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

(define-interface bsd44lite-fdflags-extras-interface
  (export open/shared-lock
	  open/exclusive-lock
	  open/async
	  open/fsync
	  fcntl/get-owner
	  fcntl/set-owner))

(define-interface bsd44lite-errno-extras-interface
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
	  errno/badrpc
	  errno/rpcmismatch
	  errno/progunavail
	  errno/progmismatch
	  errno/ftype 
	  errno/auth
	  errno/needauth
	  errno/last))

(define-interface bsd44lite-signals-extras-interface
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

(define-interface bsd44lite-network-extras-interface
  (export socket/debug
	  socket/accept-connect
	  socket/reuse-address
	  socket/keep-alive
	  socket/dont-route
	  socket/broadcast
	  socket/use-loop-back
	  socket/linger
	  socket/oob-inline
	  socket/reuse-port		;bsd44lite
;	  socket/use-privileged
;	  socket/cant-signal
	  socket/send-buffer
	  socket/receive-buffer
	  socket/send-low-water
	  socket/receive-low-water
	  socket/send-timeout
	  socket/receive-timeout
	  socket/error
	  socket/type
;;; all ip/* but ip/options and ip/time-to-live bsd44lite only
	  ip/options
	  ip/header-included
	  ip/type-of-service
	  ip/time-to-live		
	  ip/receive-options	
	  ip/response-options	
	  ip/destination-address	
	  ip/ret-options		
	  ip/multicast-if		
	  ip/multicast-ttl	
	  ip/multicast-loop	
	  ip/add-membership	
	  ip/drop-membership	
	  tcp/no-delay
	  tcp/max-segment
	  message/eor
	  message/trunc
	  message/ctrunc
	  message/wait-all
	  message/dont-wait))

(define-interface bsd44lite-extras-interface
  (compound-interface bsd44lite-errno-extras-interface
		      bsd44lite-fdflags-extras-interface
		      bsd44lite-network-extras-interface
		      bsd44lite-signals-extras-interface))

(define-interface bsd44lite-defs-interface
  (compound-interface bsd44lite-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface
		      posix-signals-interface
		      signals-internals-interface))

(define-structure bsd44lite-defs bsd44lite-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno signals netconst))

(define-interface os-extras-interface bsd44lite-extras-interface)
(define os-dependent bsd44lite-defs)
