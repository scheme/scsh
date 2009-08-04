;;; Interfaces and packages for the NeXTSTEP specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.

(define-interface next-fdflags-extras-interface
  (export open/async
	  open/sync
	  fcntl/get-owner
	  fcntl/set-owner))

(define-interface next-errno-extras-interface
  (export errno/addrinuse
	  errno/addrnotavail
	  errno/afnosupport
	  errno/already
	  errno/badarch
	  errno/badexec
	  errno/badmacho
	  errno/connaborted
	  errno/connrefused
	  errno/connreset
	  errno/destaddrreq
	  errno/deverr
	  errno/dquot
	  errno/hostdown
	  errno/hostunreach
	  errno/inprogress
	  errno/isconn
	  errno/loop
	  errno/msgsize
	  errno/netdown
	  errno/netreset
	  errno/nobufs
	  errno/noinit
	  errno/noprotoopt
	  errno/notblk
	  errno/notconn
	  errno/notsock
	  errno/netunreach
	  errno/opnotsupp
	  errno/pfnosupport
	  errno/proclim
	  errno/protonosupport
	  errno/prototype
	  errno/pwroff
	  errno/remote
	  errno/shlibvers
	  errno/shutdown
	  errno/socktnosupport
	  errno/stale
	  errno/timedout
	  errno/toomanyrefs
	  errno/users
	  errno/wouldblock
	  errno/txtbsy))

(define-interface next-network-extras-interface
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
	  socket/cant-signal
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
	  tcp/no-delay
	  tcp/max-segment))

(define-interface next-extras-interface
  (compound-interface next-errno-extras-interface
		      next-fdflags-extras-interface
		      next-network-extras-interface))

(define-interface next-defs-interface
  (compound-interface next-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface))

(define-structure next-defs next-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno netconst))

(define-interface os-extras-interface next-extras-interface)
(define os-dependent next-defs)
