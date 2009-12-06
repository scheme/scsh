;;; Interfaces and packages for the Linux specific parts of scsh.
;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Copyright (c) 1994 by Brian D. Carlstrom.

(define-interface linux-fdflags-extras-interface
  (export open/shared-lock
	  open/exclusive-lock
	  open/async
	  open/fsync
	  fcntl/get-owner
	  fcntl/set-owner))

(define-interface linux-network-extras-interface
  (export socket/debug
;	  socket/accept-connect
	  socket/reuse-address
	  socket/keep-alive
	  socket/dont-route
	  socket/broadcast
;	  socket/use-loop-back
	  socket/linger
	  socket/oob-inline
;	  socket/use-privileged
;	  socket/cant-signal
	  socket/send-buffer
	  socket/receive-buffer
;	  socket/send-low-water
;	  socket/receive-low-water
;	  socket/send-timeout
;	  socket/receive-timeout
	  socket/error
	  socket/type
	  socket/no-check
	  socket/priority
	  ip/options
	  ip/time-to-live
	  ip/type-of-service		;linux
	  ip/include-header		;linux
	  tcp/no-delay
	  tcp/max-segment))

(define-interface linux-extras-interface
  (compound-interface linux-fdflags-extras-interface
		      linux-network-extras-interface))

(define-interface linux-defs-interface
  (compound-interface linux-extras-interface
		      sockets-network-interface
		      posix-fdflags-interface))

(define-structure linux-defs linux-defs-interface
  (open scheme bitwise defenum-package)
  (files fdflags netconst))

(define-interface os-extras-interface linux-extras-interface)
(define os-dependent linux-defs)
