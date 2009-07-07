;;; Interfaces and packages for the machine specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom.
;;; AIX version by Chipsy Sperber 

(define-interface aix-fdflags-extras-interface
  (export open/no-delay
	  open/sync))

(define-interface aix-errno-extras-interface
  (export errno/nomsg
	  errno/idrm
	  errno/chrng
	  errno/l2nsync
	  errno/l3hlt
	  errno/l3rst
	  errno/lnrng
	  errno/unatch
	  errno/nocsi
	  errno/l2hlt
	  errno/notready
	  errno/wrprotect
	  errno/format
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
	  errno/timedout
	  errno/connrefused
	  errno/hostdown
	  errno/hostunreach
	  errno/restart
	  errno/proclim
	  errno/users
	  errno/loop
	  errno/dquot
	  errno/remote
	  errno/media
	  errno/soft
	  errno/noattr
	  errno/sad
	  errno/notrust
	  errno/toomanyrefs
	  errno/ilseq
	  errno/canceled
	  errno/nosr
	  errno/time
	  errno/badmsg
	  errno/proto
	  errno/nodata
	  errno/nostr
	  errno/cloneme))

(define-interface aix-signals-extras-interface
  (export signal/io
	  signal/xcpu
	  signal/xfsz
	  signal/msg
	  signal/winch
	  signal/pwr
	  signal/prof
	  signal/danger
	  signal/vtalrm
	  signal/migrate
	  signal/pre
	  signal/virt))

(define-interface aix-network-extras-interface
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
	  ip/include-header
	  ip/type-of-service
	  ip/time-to-live 
	  ip/recvopt      
	  ip/recvret      
	  ip/recvdst      
	  ip/retopts      
	  tcp/no-delay
	  tcp/max-segment
	  message/eor   	
	  message/trunc  
	  message/ctrunc 
	  message/waitall
	  ))

(define-interface aix-extras-interface
  (compound-interface aix-errno-extras-interface
		      aix-fdflags-extras-interface
		      aix-network-extras-interface
		      aix-signals-extras-interface))

(define-interface aix-defs-interface
  (compound-interface aix-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface
		      posix-signals-interface
		      signals-internals-interface))

(define-structure aix-defs aix-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno signals netconst))

(define-interface os-extras-interface aix-extras-interface)
(define os-dependent aix-defs)
