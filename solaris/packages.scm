;;; Interfaces and packages for the Sun specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.

(define-interface solaris-fdflags-extras-interface
  (export open/no-delay
	  open/sync
	  fcntl/get-owner
	  fcntl/set-owner
	  ))

(define-interface solaris-errno-extras-interface
  (export errno/addrinuse
	  errno/addrnotavail
	  errno/adv
	  errno/afnosupport
	  errno/already
	  errno/bade
	  errno/badfd
	  errno/badmsg
	  errno/badr
	  errno/badrqc
	  errno/badslt
	  errno/bfont
	  errno/canceled
	  errno/chrng
	  errno/comm
	  errno/connaborted
	  errno/connrefused
	  errno/connreset
	  errno/deadlock
	  errno/destaddrreq
	  errno/hostdown
	  errno/hostunreach
	  errno/idrm
	  errno/ilseq
	  errno/inprogress
	  errno/isconn
	  errno/l2hlt
	  errno/l2nsync
	  errno/l3hlt
	  errno/l3rst
	  errno/libacc
	  errno/libbad
	  errno/libexec
	  errno/libmax
	  errno/libscn
	  errno/lnrng
	  errno/loop
	  errno/msgsize
	  errno/multihop
	  errno/netdown
	  errno/netreset
	  errno/netunreach
	  errno/noano
	  errno/nobufs
	  errno/nocsi
	  errno/nodata
	  errno/nolink
	  errno/nomsg
	  errno/nonet
	  errno/nopkg
	  errno/noprotoopt
	  errno/nosr
	  errno/nostr
	  errno/notblk
	  errno/notconn
	  errno/notsock
	  errno/notsup
	  errno/notuniq
	  errno/opnotsupp
	  errno/overflow
	  errno/pfnosupport
	  errno/proto
	  errno/protonosupport
	  errno/prototype
	  errno/remchg
	  errno/remote
	  errno/restart
	  errno/shutdown
	  errno/socktnosupport
	  errno/srmnt
	  errno/stale
	  errno/strpipe
	  errno/time
	  errno/timedout
	  errno/toomanyrefs
	  errno/txtbsy
	  errno/unatch
	  errno/users
	  errno/wouldblock
	  errno/xfull
	  ))

(define-interface solaris-signals-extras-interface
  (export signal/bus	
	  signal/cld	
	  signal/emt	
	  signal/freeze
	  signal/io	
	  signal/iot	
	  signal/lwp	
	  signal/poll
	  signal/prof 
	  signal/pwr	
	  signal/sys	
	  signal/thaw 
	  signal/trap
	  signal/urg	
	  signal/vtalrm
	  signal/waiting
	  signal/winch 
	  signal/xcpu 
	  signal/xfsz 
	  ))

(define-interface solaris-network-extras-interface
  (export socket/debug
	  socket/accept-connect
	  socket/reuse-address
	  socket/keep-alive
	  socket/dont-route
	  socket/broadcast
	  socket/use-loop-back
	  socket/linger
	  socket/oob-inline
	  socket/use-privileged
	  socket/send-buffer
	  socket/receive-buffer
	  socket/send-low-water
	  socket/receive-low-water
	  socket/send-timeout
	  socket/receive-timeout
	  socket/error
	  socket/type
	  tcp/no-delay
	  tcp/max-segment))

(define-interface solaris-extras-interface
  (compound-interface solaris-errno-extras-interface
		      solaris-fdflags-extras-interface
		      solaris-network-extras-interface
		      solaris-signals-extras-interface))

(define-interface solaris-defs-interface
  (compound-interface solaris-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface
		      posix-signals-interface
		      signals-internals-interface))

(define-structure solaris-defs solaris-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno signals netconst))

(define-interface os-extras-interface solaris-extras-interface)
(define os-dependent solaris-defs)
