;;; Interfaces and packages for the GNU Hurd specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.

(define-interface gnu-fdflags-extras-interface
  (export open/shared-lock
	  open/exclusive-lock
	  open/async
	  open/fsync
	  open/execute
	  open/no-link
	  open/no-translator
	  open/no-access-time
	  open/ignore-control-tty
	  fcntl/get-owner
	  fcntl/set-owner))

(define-interface gnu-errno-extras-interface
  (export errno/notblk
	  errno/txtbsy
	  errno/nospc
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
	  errno/procunavail
	  errno/ftype
	  errno/auth
	  errno/needauth
	  errno/background
	  errno/died
	  errno/d
	  errno/gregious
	  errno/ieio
	  errno/gratuitous
	  errno/ilseq
	  errno/badmsg
	  errno/idrm
	  errno/multihop
	  errno/nodata
	  errno/nolink
	  errno/nomsg
	  errno/nosr
	  errno/nostr
	  errno/overflow
	  errno/proto
	  errno/time
	  errno/canceled
	  errno/notsup))

(define-interface gnu-signals-extras-interface
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
	  signal/info
	  signal/lost))

(define-interface gnu-network-extras-interface
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

(define-interface gnu-extras-interface
  (compound-interface gnu-errno-extras-interface
		      gnu-fdflags-extras-interface
		      gnu-network-extras-interface
		      gnu-signals-extras-interface))

(define-interface gnu-defs-interface
  (compound-interface gnu-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface
		      posix-signals-interface
		      signals-internals-interface))

(define-structure gnu-defs gnu-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno signals netconst))

(define-interface os-extras-interface gnu-extras-interface)
(define os-dependent gnu-defs)
