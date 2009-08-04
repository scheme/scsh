;;; Interfaces and packages for the Sun specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.

(define-interface generic-fdflags-extras-interface
  (export open/no-delay
	  open/sync
	  ))

(define-interface generic-errno-extras-interface
  (export errno/addrinuse
	  errno/addrnotavail
	  errno/adv
	  errno/afnosupport
	  errno/already
	  errno/badmsg
	  errno/comm
	  errno/connaborted
	  errno/connrefused
	  errno/connreset
	  errno/destaddrreq
	  errno/dotdot
	  errno/dquot
	  errno/hostdown
	  errno/hostunreach
	  errno/idrm
	  errno/inprogress
	  errno/isconn
	  errno/loop
	  errno/msgsize
	  errno/multihop
	  errno/netdown
	  errno/netreset
	  errno/netunreach
	  errno/nobufs
	  errno/nolink
	  errno/nomsg
	  errno/nonet
	  errno/noprotoopt
	  errno/nosr
	  errno/nostr
	  errno/notblk
	  errno/notconn
	  errno/notsock
	  errno/opnotsupp
	  errno/pfnosupport
	  errno/proclim
	  errno/proto
	  errno/protonosupport
	  errno/prototype
	  errno/remchg
	  errno/remote
	  errno/rremote
	  errno/shutdown
	  errno/socktnosupport
	  errno/srmnt
	  errno/stale
	  errno/time
	  errno/timedout
	  errno/toomanyrefs
	  errno/users
	  errno/wouldblock
	  errno/xtbsy))

(define-interface generic-network-extras-interface
  (export socket/debug
	  socket/accept-connect
	  socket/reuse-address
	  socket/keep-alive
	  socket/dont-route
	  socket/broadcast
	  socket/use-loop-back
	  socket/linger
	  socket/oob-inline
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
	  ip/options
;	  ip/time-to-live
	  tcp/no-delay
	  tcp/max-segment))

(define-interface generic-extras-interface
  (compound-interface generic-errno-extras-interface
		      generic-fdflags-extras-interface
		      generic-network-extras-interface))

(define-interface generic-defs-interface
  (compound-interface generic-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface))

(define-structure generic-defs generic-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno netconst))

(define-interface os-extras-interface generic-extras-interface)
(define os-dependent generic-defs)
