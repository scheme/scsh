;;; Magic Numbers for Networking
;;; Copyright (c) 1994 by Brian D. Carlstrom. See file COPYING.

;;; magic numbers not from header file
;;; but from man page
;;; why can't unix make up its mind
(define shutdown/receives 0)
(define shutdown/sends 1)
(define shutdown/sends+receives 2)

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; BELOW THIS POINT ARE BITS FROM:
;;; <sys/socket.h> 
;;; <sys/un.h> 
;;; <netinet/in.h>
;;; <netinet/tcp.h>
;;; <netdb.h>
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

;;; ADDRESS FAMILIES -- <sys/socket.h>
(define address-family/unspecified 0)	; unspecified
(define address-family/unix 1)		; local to host (pipes, portals)
(define address-family/internet 2)	; internetwork: UDP, TCP, etc.

;;; SOCKET TYPES -- <sys/socket.h>
(define socket-type/stream 1)		; stream socket 
(define socket-type/datagram 2)		; datagram socket
(define socket-type/raw 3)		; raw-protocol interface
;;(define socket-type/rdm 4)		; reliably-delivered message
;;(define socket-type/seqpacket 5)      ; sequenced packet stream

;;; PROTOCOL FAMILIES -- <sys/socket.h>
(define protocol-family/unspecified 0)	; unspecified
(define protocol-family/unix 1)		; local to host (pipes, portals)
(define protocol-family/internet 2)	; internetwork: UDP, TCP, etc.

;;; Well know addresses -- <netinet/in.h>
(define internet-address/any #x00000000)
(define internet-address/loopback #x7f000001)
(define internet-address/broadcast #xffffffff)	; must be masked

;;; errors from host lookup -- <netdb.h>
(define	herror/host-not-found 1) ;Authoritative Answer Host not found
(define	herror/try-again   2) ;Non-Authoritive Host not found, or SERVERFAIL
(define	herror/no-recovery 3) ;Non recoverable errors, FORMERR, REFUSED, NOTIMP
(define	herror/no-data     4) ;Valid name, no data record of requested type
(define	herror/no-address herror/no-data) ;no address, look for MX record

;;; flags for send/recv -- <sys/socket.h>
(define message/out-of-band 1) ; process out-of-band data
(define message/peek        2) ; peek at incoming message
(define message/dont-route  4) ; send without using routing tables

;;; protocol level for socket options -- <sys/socket.h>
(define level/socket #xffff)		; SOL_SOCKET: options for socket level

;;; socket options -- <sys/socket.h>
(define socket/debug #x0001)		; turn on debugging info recording 
(define socket/accept-connect #x0002)	; socket has had listen() 
(define socket/reuse-address #x0004)	; allow local address reuse 
(define socket/keep-alive #x0008)	; keep connections alive 
(define socket/dont-route #x0010)	; just use interface addresses 
(define socket/broadcast #x0020)	; permit sending of broadcast msgs 
(define socket/use-loop-back #x0040)	; bypass hardware when possible 
(define socket/linger	#x0080)		; linger on close if data present 
(define socket/oob-inline #x0100)	; leave received OOB data in line 
(define socket/use-privileged #x4000)	; allocate from privileged port area 
(define socket/cant-signal #x8000)	; prevent SIGPIPE on SS_CANTSENDMORE 
(define socket/send-buffer #x1001)	; send buffer size 
(define socket/receive-buffer #x1002)	; receive buffer size 
(define socket/send-low-water #x1003)	; send low-water mark 
(define socket/receive-low-water #x1004) ; receive low-water mark 
(define socket/send-timeout #x1005)	; send timeout 
(define socket/receive-timeout #x1006)	; receive timeout 
(define socket/error #x1007)		; get error status and clear 
(define socket/type #x1008)		; get socket type 

;;; ip options -- <netinet/in.h>
(define	ip/options 1)			; set/get IP per-packet options
(define	ip/time-to-live 2)		; set/get IP time-to-live value

;;; tcp options -- <netinet/tcp.h>
(define tcp/no-delay #x01)		; don't delay send to coalesce packets
(define tcp/max-segment #x02)		; set maximum segment size

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; OPTION SETS FOR SOCKET-OPTION AND SET-SOCKET-OPTION

;;; Boolean Options
(define options/boolean
  (list socket/debug
	socket/accept-connect
	socket/reuse-address
	socket/keep-alive
	socket/dont-route
	socket/broadcast
	socket/use-loop-back
	socket/oob-inline
	socket/use-privileged
	socket/cant-signal
	tcp/no-delay))

;;; Integer Options
(define options/value
  (list socket/send-buffer
	socket/receive-buffer
	socket/send-low-water
	socket/receive-low-water
	socket/error
	socket/type
	ip/time-to-live
	tcp/max-segment))

;;; #f or Positive Integer
(define options/linger
  (list socket/linger))

;;; Real Number
(define options/timeout
  (list socket/send-timeout
	socket/receive-timeout))
