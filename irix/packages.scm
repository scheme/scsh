;;; Interfaces and packages for the machine specific parts of scsh.
;;; This is the IRIX version.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

(define-interface irix-fdflags-extras-interface
  (export open/no-delay
	  open/sync))

(define-interface irix-errno-extras-interface
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
; 	   errno/dotdot
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
;	   errno/proclim
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
	   errno/xtbsy

	   errno/chrng
	   errno/l2nsync
	   errno/l3hlt
	   errno/l3rst
	   errno/lnrng
	   errno/unatch
	   errno/nocsi
	   errno/l2hlt

	   errno/bade
	   errno/badr
	   errno/xfull
	   errno/noano
	   errno/badrqc
	   errno/badslt
	   errno/deadlock
	   errno/bfont

	   errno/libacc
	   errno/libbad
	   errno/libscn
	   errno/libmax
	   errno/libexec
	   ))

(define-interface irix-network-extras-interface
  (export socket/debug
	  socket/accept-connect
	  socket/reuse-address
	  socket/keep-alive
	  socket/dont-route
	  socket/broadcast
	  socket/use-loop-back
	  socket/linger
	  socket/oob-inline
	  socket/reuse-port		;irix
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
	  ip/time-to-live
	  ip/include-header		;irix
	  ip/type-of-service		;irix
	  tcp/no-delay
	  tcp/max-segment))

(define-interface irix-extras-interface
  (compound-interface irix-errno-extras-interface
		      irix-fdflags-extras-interface
		      irix-network-extras-interface))

(define-interface irix-defs-interface
  (compound-interface irix-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface))

(define-structure irix-defs irix-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno netconst))

(define-interface os-extras-interface irix-extras-interface)
(define os-dependent irix-defs)
