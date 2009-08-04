;;; Interfaces and packages for the HP-UX specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.

(define-interface hpux-fdflags-extras-interface
  (export open/sync))

(define-interface hpux-errno-extras-interface
  (export errno/wouldblock
	  errno/connaborted))

(define-interface hpux-network-extras-interface
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
	  socket/send-avoid-copy	;hpux
	  socket/receive-avoid-copy	;hpux
	  ;; all options except ip/options & ip/time-to-live hpux specific
	  ip/options
	  ip/multicast-if
	  ip/multicast-ttl
	  ip/multicast-loop
	  ip/add-membership
	  ip/drop-membership
	  ip/time-to-live
	  tcp/no-delay
	  tcp/max-segment))

(define-interface hpux-extras-interface
  (compound-interface hpux-errno-extras-interface
		      hpux-fdflags-extras-interface
		      hpux-network-extras-interface))

(define-interface hpux-defs-interface
  (compound-interface hpux-extras-interface
		      sockets-network-interface
		      posix-errno-interface
		      posix-fdflags-interface))

(define-structure hpux-defs hpux-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags errno netconst))

(define-interface os-extras-interface hpux-extras-interface)
(define os-dependent hpux-defs)
