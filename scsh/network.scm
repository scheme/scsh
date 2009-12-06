;;; Networking for the Scheme Shell
;;; Copyright (c) 1994-1995 by Brian D. Carlstrom.
;;; Copyright (c) 1994 by Olin Shivers.
;;; See file COPYING.

;;; Scheme48 implementation.

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; High Level Prototypes
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (socket-connect protocol-family socket-type . args)
  (let* ((sock (create-socket protocol-family
			      socket-type))
	 (addr (cond ((= protocol-family
			 protocol-family/internet)
		      (let* ((host (car  args))
			     (port (cadr args))
			     (host (car (host-info:addresses
					  (name->host-info host))))
			     (port (cond ((integer? port) port)
					 ((string? port)
					  (service-info:port
					   (service-info (cadr args) "tcp")))
					 (else
					  (error
					   "socket-connect: bad port arg ~s"
					   args)))))
			(internet-address->socket-address host port)))
		     ((= protocol-family
			 protocol-family/unix)
		      (unix-address->socket-address (car args)))
		     (else
		      (error "socket-connect: unsupported protocol-family ~s"
			     protocol-family)))))
    ;; Close the socket and free the file-descriptors
    ;; if the connect fails:
    (let ((connected #f))
      (dynamic-wind
       (lambda () #f)
       (lambda () (connect-socket sock addr) (set! connected #t))
       (lambda ()
         (if (not connected)
             (close-socket sock))
	 ))
      (if connected
          sock
          #f))))

(define (bind-listen-accept-loop protocol-family proc arg)
  (bind-prepare-listen-accept-loop protocol-family (lambda () #t) proc arg))

(define (bind-prepare-listen-accept-loop protocol-family prepare proc arg)
  (let* ((sock (create-socket protocol-family socket-type/stream))
	 (addr (cond ((= protocol-family
			 protocol-family/internet)
		      (let ((port (cond ((integer? arg) arg)
					((string? arg)
					 (service-info:port
					  (service-info arg "tcp")))
					(else
					 (error "socket-connect: bad arg ~s"
						arg)))))
			(internet-address->socket-address internet-address/any
							  arg)))
		     ((= protocol-family
			 protocol-family/unix)
		      (unix-address->socket-address arg))
		     (else
		      (error "bind-listen-accept-loop: unsupported protocol-family ~s"
			     protocol-family)))))
    (set-socket-option sock level/socket socket/reuse-address #t)
    (bind-socket sock addr)
    (with-handler
     (lambda (condition more)
       (with-handler
	(lambda (condition ignore) (more))
	(lambda () (close-socket sock)))
       (more))
     prepare)
    (listen-socket sock 5)
    (with-handler
     (lambda (condition more)
       (with-handler
	(lambda (condition ignore) (more))
	(lambda () (close-socket sock)))
       (more))
     (lambda ()
       (let loop ()
	 (with-errno-handler
	  ;; ECONNABORTED we just ignore
	  ((errno packet)
	   ((connaborted) 'fick-dich-ins-knie))
	  (call-with-values
	   (lambda () (accept-connection sock))
	   proc))
	 (loop))))))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Socket Record Structure
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record-type :socket
  (make-socket family inport outport)
  socket?
  (family socket:family)
  (inport socket:inport)
  (outport socket:outport))

(define-record-type :socket-address
  (make-socket-address family address)
  socket-address?
  (family socket-address:family)
  (address socket-address:address))

;;; returns the fdes of a socket
;;; not exported
(define (socket->fdes sock)
  (fdport-data:fd (port-data (socket:inport sock))))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Socket Address Routines
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (internet-address->socket-address address32 port16)
  (cond ((not (<= 0 address32 #xffffffff))
	 (error "internet-address->socket-address: address out of range ~s"
		address32))
	((not (<= 0 port16 #xffff))
	 (error "internet-address->socket-address: port out of range ~s"
		port16))
	(else
	 (make-socket-address address-family/internet
			      (cons address32 port16)))))

(define (socket-address->internet-address sockaddr)
  (if (or (not (socket-address? sockaddr))
	  (not (= (socket-address:family sockaddr)
		  address-family/internet)))
      (error "socket-address->internet-address: internet socket expected ~s"
	     sockaddr)
      (values (car (socket-address:address sockaddr))
	      (cdr (socket-address:address sockaddr)))))

(define (unix-address->socket-address path)
  (if (> (string-length path) 108)
      (error "unix-address->socket-address: path too long ~s" path)
      (make-socket-address address-family/unix path)))

(define (socket-address->unix-address sockaddr)
  (if (or (not (socket-address? sockaddr))
	  (not (= (socket-address:family sockaddr)
		  address-family/unix)))
      (error "socket-address->unix-address expects an unix socket ~s" sockaddr)
      (socket-address:address sockaddr)))

(define (make-addr af)
  (make-string (cond ((= af address-family/unix) 108)
		     ((= af address-family/internet) 8)
		     (else
		      (error "make-addr: unknown address-family ~s" af)))))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; socket syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (create-socket pf type . maybe-protocol)
  (let ((protocol (:optional maybe-protocol 0)))
    (if (not (and (integer? pf)
		  (integer? type)
		  (integer? protocol)))
	(error "create-socket: integer arguments expected ~s ~s ~s"
	       pf type protocol)
	(let* ((fd  (%socket pf type protocol))
	       (in  (make-input-fdport fd 0))
	       (out (dup->outport in)))
	  (set-fdes-status in open/non-blocking)
	  (set-fdes-status out open/non-blocking)
	  (make-socket pf in out)))))


;;; Turn a file descriptor into a socket.

(define (port->socket port pf)
  ;;; ensure underlying fd is a socket by a random getsockopt call
  (if (not (port? port))
      (error "first argument to port->socket is not a port" port))
  (sleazy-call/fdes
   port
   (lambda (fd)
     (%getsockopt fd level/socket socket/debug)))
  (let ((in (dup->inport port))
	(out (dup->outport port)))
    (make-socket pf in out)))

(import-os-error-syscall %socket (pf type protocol) "scsh_socket")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; close syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (close-socket sock)
  (close (socket:inport  sock))
  (close (socket:outport sock)))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; bind syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (bind-socket sock name)
  (cond ((not (socket? sock))
	 (error "bind-socket: socket expected ~s" sock))
	((not (socket-address? name))
	 (error "bind-socket: socket-address expected ~s" name))
	(else
	 (let ((family (socket:family sock)))
	   (cond ((not (= family (socket-address:family name)))
		  (error
		   "bind-socket: trying to bind incompatible address to socket ~s"
		   name))
		 ((and (= family address-family/unix)
		       (> (string-length (socket-address->unix-address name)) 107))
		  (error "bind-socket: path too long" name))
		 (else
		  (%bind (socket->fdes sock)
			 family
			 (socket-address:address name))))))))

(import-os-error-syscall %bind (sockfd family name) "scheme_bind")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; connect syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (connect-socket-no-wait sock name)
  (cond ((not (socket? sock))
	 (error "connect-socket: socket expected ~s" sock))
	((not (socket-address? name))
	 (error "connect-socket: socket-address expected ~s" name))
	(else
	 (let ((family (socket:family sock)))
	   (cond ((not (= family (socket-address:family name)))
		  (error
		   "connect: trying to connect socket to incompatible address ~s"
		   name))
		 ((and (= family address-family/unix)
					; save space for \0
		       (> (string-length (socket-address:address name)) 107))
		  (error "connect: filename too long" name))
		 (else
		  (let loop ()
		    (let* ((fdes (socket->fdes sock))
			   (error?.einprogress?
			    (%connect fdes
				      (socket:family sock)
				      (socket-address:address name))))
		      (if (car error?.einprogress?)
			  (if (cdr error?.einprogress?)
			      #f
			      (loop))
			  #t)))))))))

(define (connect-socket-successful? sock)
  ;; If connect returned EINPROGRESS, we can check
  ;; it's success after  the next success with getsockopt
  (zero? (socket-option sock level/socket socket/error)))

(define (connect-socket sock name)
  (let ((success? (connect-socket-no-wait sock name)))
    (cond ((not success?)
	   (select '#()
		   (vector (fdport-data:fd
			    (fdport-data
			     (socket:outport sock))))
		   '#())
           (let ((errno (socket-option sock level/socket socket/error)))
             (if (not (zero? errno))
                 (errno-error errno
                              (errno-msg errno)
                              %connect
                              sock
                              name)))))))

(import-os-error-syscall %connect (sockfd family name) "scheme_connect")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; listen syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (listen-socket sock backlog)
  (cond ((not (socket? sock))
	 (error "listen-socket: socket expected ~s" sock))
	((not (integer? backlog))
	 (error "listen-socket: integer expected ~s" backlog))
	(else
	 (%listen (socket->fdes sock) backlog))))

(import-os-error-syscall %listen (sockfd backlog) "scsh_listen")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; accept syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (accept-connection sock)
  (if (not (socket? sock))
      (error "accept-connection: socket expected ~s" sock)
      (let ((family (socket:family sock)))
	(let loop ()
	  ((structure-ref interrupts disable-interrupts!))
	  (let ((fd-addr (%accept (socket->fdes sock) family)))
	    (cond ((pair? fd-addr)
		   (let ((fd (car fd-addr))
			 (addr (cdr fd-addr)))
		     ((structure-ref interrupts
				     enable-interrupts!))
		     (let* ((in     (make-input-fdport fd 0))
			    (out    (dup->outport in)))
		       (values (make-socket family in out)
			       (make-socket-address family addr)))))
		  (else (wait-for-channel
			 (fdport-data:channel
			  (fdport-data (socket:inport sock))))
			(loop))))))))

(import-os-error-syscall %accept (sockfd family) "scheme_accept")


;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; getpeername syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (socket-remote-address sock)
  (if (or (not (socket? sock))
	  (not (= (socket:family sock) address-family/internet)))
      (error "socket-remote-address: internet socket expected ~s" sock)
      (let* ((family (socket:family sock))
	     (addr (%peer-name (socket->fdes sock)
			       family)))
	(make-socket-address family addr))))

(import-os-error-syscall %peer-name  (sockfd family) "scheme_peer_name")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; getsockname syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (socket-local-address sock)
  (if (or (not (socket? sock))
	  (not (= (socket:family sock) address-family/internet)))
      (error "socket-local-address: internet socket expected ~s" sock)
      (let* ((family (socket:family sock))
	     (addr (%socket-name (socket->fdes sock) family)))
	(make-socket-address family addr))))

(import-os-error-syscall %socket-name (sockfd family) "scheme_socket_name")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; shutdown syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (shutdown-socket sock how)
  (cond ((not (socket? sock))
	 (error "shutdown-socket: socket expected ~s" sock))
	((not (integer? how))
	 (error "shutdown-socket: integer expected ~s" how))
	(else
	 (%shutdown (socket->fdes sock) how))))

(import-os-error-syscall %shutdown (sockfd how) "scsh_shutdown")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; socketpair syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (create-socket-pair type)
  (if (not (integer? type))
      (error "create-socket-pair: integer argument expected ~s" type)
      (apply
       (lambda (s1 s2)
        (let* ((in1  (make-input-fdport s1 0))
	       (out1 (dup->outport in1))
	       (in2  (make-input-fdport s2 0))
	       (out2 (dup->outport in2)))
	  (values (make-socket protocol-family/unix in1 out1)
		  (make-socket protocol-family/unix in2 out2))))
       (%socket-pair type))))

(import-os-error-syscall %socket-pair (type) "scheme_socket_pair")


;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; recv syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (receive-message socket len . maybe-flags)
  (let ((flags (:optional maybe-flags 0)))
    (cond ((not (socket? socket))
	   (error "receive-message: socket expected ~s" socket))
	  ((or (not (integer? flags))
	       (not (integer? len)))
	   (error "receive-message: integer expected ~s ~s" flags len))
	  (else
	   (let ((s (make-string len)))
	     (receive (nread from)
		      (receive-message! socket s 0 len flags)
               (values
		(cond ((not nread) #f)	; EOF
		      ((= nread len) s)
		      (else (substring s 0 nread)))
		from)))))))

(define (receive-message! socket s . args)
  (if (not (string? s))
      (error "receive-message!: string expected ~s" s)
      (let-optionals args ((start 0) (end (string-length s)) (flags 0))
        (cond ((not (socket? socket))
	       (error "receive-message!: socket expected ~s" socket))
	      ((not (or (integer? flags)
			(integer? start)
			(integer? end)))
	       (error "receive-message!: integer expected ~s ~s ~s"
		      flags start end))
	      (else
	       (generic-receive-message! socket flags
					 s start end
					 recv-substring!
					 (socket:family socket)))))))

(define (generic-receive-message! socket flags s start end reader family)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices"
	     reader socket flags
	     s start end family))
    (let loop ((i start) (remote #f))
      (if (>= i end)
	  (values (- i start) (make-socket-address family remote))
	  (let* ((res  (reader socket flags s i end)))
	  (apply (lambda (nread from)
		   (cond  ((zero? nread)	; EOF
			   (values
			    (let ((result (- i start)))
			      (and (not (zero? result)) result))
			    (make-socket-address family from)))
			  (else (loop (+ i nread) from))))
		 res)))))

(define (receive-message/partial socket len . maybe-flags)
  (let ((flags (:optional maybe-flags 0)))
    (cond ((not (socket? socket))
	   (error "receive-message/partial: socket expected ~s" socket))
	  ((or (not (integer? flags))
	       (not (integer? len)))
	   (error "receive-message/partial: integer expected ~s ~s" flags len))
	  (else
	   (let ((s (make-string len)))
	     (receive (nread addr)
		      (receive-message!/partial socket s 0 len flags)
		      (values
		       (cond ((not nread) #f)	; EOF
			     ((= nread len) s)
			     (else (substring s 0 nread)))
		       addr)))))))

(define (receive-message!/partial socket s . args)
  (if (not (string? s))
      (error "receive-message!/partial: string expected ~s" s)
      (let-optionals args ((start 0) (end (string-length s)) (flags 0))
        (cond ((not (socket? socket))
	       (error "receive-message!/partial: socket expected ~s"
		      socket))
	      ((not (integer? flags))
	       (error "receive-message!/partial: integer expected ~s"
		      flags))
	      (else
	       (generic-receive-message!/partial socket
						 flags
						 s start end
						 recv-substring!
						 (socket:family socket)))))))

(define (generic-receive-message!/partial socket flags s start end reader from)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" reader s start end))

  (if (= start end) 0 ; Vacuous request.
	(let loop ()
	  (apply (lambda (nread addr)
		    (values (and (not (zero? nread)) nread)
		    (make-socket-address from addr)))
	   (reader socket flags s start end)))))


(define (recv-substring! socket flags buf start end)
	(let loop ()
	  ((structure-ref interrupts disable-interrupts!))
	  (let ((maybe-size-addr
		 (%recv-substring! (socket->fdes socket)
					 flags buf start end)))
	    (cond (maybe-size-addr
		   ((structure-ref interrupts
				   enable-interrupts!))
		   maybe-size-addr)
		  (else (wait-for-channel
			 (fdport-data:channel
			  (fdport-data (socket:inport socket))))
			(loop))))))

(import-os-error-syscall %recv-substring! (sockfd flags buf start end)
  "recv_substring")
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; send syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (send-message socket s . args)
  (let-optionals args ((start 0) (end (string-length s)) (flags 0) (addr #f))
    (cond ((not (socket? socket))
	   (error "send-message: socket expected ~s" socket))
	  ((not (integer? flags))
	   (error "send-message: integer expected ~s" flags))
	  ((not (string? s))
	   (error "send-message: string expected ~s" s))
	  (else
	   (generic-send-message socket flags
				 s start end
				 send-substring
				 (if addr (socket-address:family addr) 0)
				 (if addr (socket-address:address addr) #f))))))

(define (generic-send-message socket flags s start end writer family addr)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices"
	     socket flags family addr
	     s start end writer))
  (if (= start end)
      ;; there is such a thing as an empty message
      (writer socket flags s start end family addr)
      (let loop ((i start))
	(if (< i end)
	    (loop (+ i (writer socket flags s i end family addr)))))))


(define (send-message/partial socket s . args)
  (let-optionals args ((start 0) (end (string-length s)) (flags 0) (addr #f))
    (cond ((not (socket? socket))
	   (error "send-message/partial: socket expected ~s" socket))
	  ((not (integer? flags))
	   (error "send-message/partial: integer expected ~s" flags))
	  ((not (string? s))
	   (error "send-message/partial: string expected ~s" s))
	  (else
           (generic-send-message/partial socket flags
					 s start end
					 send-substring
					 (if addr (socket-address:family addr) 0)
					 (if addr
					     (socket-address:address addr)
					     #f))))))

(define (generic-send-message/partial socket flags s start end writer family
				      addr)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices"
	     socket flags family addr
	     s start end writer))
  (if (= start end)
      0			; Vacuous request.
      (writer socket flags s start end family addr)))

(define (send-substring socket flags buf start end family name)
  (let loop ()
    ((structure-ref interrupts disable-interrupts!))
    (cond ((%send-substring (socket->fdes socket) flags buf start end
			    family name)
	   => (lambda (nwritten)
		((structure-ref interrupts
				enable-interrupts!))
		nwritten))
	  (else (wait-for-channel
		 (fdport-data:channel
		  (fdport-data (socket:inport socket))))
		(loop)))))

(import-os-error-syscall
  %send-substring (sockfd flags buf start end family name)
  "send_substring")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; getsockopt syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (socket-option sock level option)
  (cond ((not (socket? sock))
	 (error "socket-option: socket expected ~s" sock))
	((or (not (integer? level))(not (integer? option)))
	 (error "socket-option: integer expected ~s ~s" level option))
	((boolean-option? option)
	 (let ((result (%getsockopt (socket->fdes sock) level option)))
	   (not (= result 0))))
	((value-option? option)
	 (%getsockopt (socket->fdes sock) level option))
	((linger-option? option)
	 (apply (lambda  (result/on-off time)
		  (if (= result/on-off 0) #f time))
		(%getsockopt-linger (socket->fdes sock) level option)))
	((timeout-option? option)
	 (apply (lambda (result/secs usecs)
		  (cond ((= result/secs -1)
			 (error "socket-option ~s ~s ~s" sock level option))
			(else (+ result/secs (/ usecs 1000)))))
		(%getsockopt-timeout (socket->fdes sock) level option)))
	(else
	 "socket-option: unknown option type ~s" option)))

(import-os-error-syscall %getsockopt (sock level option) "scheme_getsockopt")

;;; returns (list on-off linger)
(import-os-error-syscall %getsockopt-linger (sockfd level optname)
  "scheme_getsockopt_linger")

;;; returns (list secs usecs)
(import-os-error-syscall %getsockopt-timeout (sockfd level optname)
  "scheme_getsockopt_timeout")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; setsockopt syscall
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (set-socket-option sock level option value)
  (cond ((not (socket? sock))
	 (error "set-socket-option: socket expected ~s" sock))
	((or (not (integer? level)) (not (integer? option)))
	 (error "set-socket-option: integer expected ~s ~s" level option))
	((boolean-option? option)
	 (%setsockopt (socket->fdes sock) level option (if value 1 0)))
	((value-option? option)
	 (%setsockopt (socket->fdes sock) level option value))
	((linger-option? option)
	 (%setsockopt-linger (socket->fdes sock)
			     level option
			     (if value 1 0)
			     (if value value 0)))
	((timeout-option? option)
	 (let ((secs (truncate value)))
	   (%setsockopt-timeout (socket->fdes sock) level option
				secs
				(truncate (* (- value secs) 1000)))))
	(else
	 "set-socket-option: unknown option type")))

(import-os-error-syscall %setsockopt (sockfd level optname optval)
  "scheme_setsockopt")

(import-os-error-syscall %setsockopt-linger
  (sockfd level optname on-off time) "scheme_setsockopt_linger")

(import-os-error-syscall %setsockopt-timeout
  (sockfd level optname secs usecs) "scheme_setsockopt_timeout")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; socket-option routines
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (boolean-option? opt)
  (member opt options/boolean))

(define (value-option? opt)
  (member opt options/value))

(define (linger-option? opt)
  (member opt options/linger))

(define (timeout-option? opt)
  (member opt options/timeout))

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; host lookup
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record-type :host-info
  (make-host-info name aliases addresses)
  host-info?
  (name host-info:name)
  (aliases host-info:aliases)
  (addresses host-info:addresses))

(define-record-discloser :host-info
  (lambda (self)
    (list 'host (host-info:name self))))

(define-exported-binding "host-info-type" :host-info)

(define (host-info arg)
  (cond ((string? arg) (name->host-info arg))
	((socket-address? arg) (address->host-info arg))
	(else (error "host-info: string or socket-address expected ~s" arg))))

(define (address->host-info name)
  (if (or (not (socket-address? name))
	  (not (= (socket-address:family name) address-family/internet)))
      (error "address->host-info: internet address expected ~s" name)
      (let ((res (%host-address->host-info/h-errno
		  (socket-address:address name))))
	(if (number? res)
	    (error "address->host-info: non-zero herrno" res name)
	    res))))

(import-lambda-definition %host-address->host-info/h-errno (name)
  "scheme_host_address2host_info")

(define (name->host-info name)
  (if (not (string? name))
      (error "name->host-info: string expected ~s" name)
      (let ((res (%host-name->host-info/h-errno name)))
	 (if (number? res)
	     (error "name->host-info: non-zero herrno" res name)
	     res))))

(import-lambda-definition %host-name->host-info/h-errno (name)
  "scheme_host_name2host_info")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; network lookup
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record-type :network-info
  (make-network-info name aliases net)
  network-info?
  (name network-info:name)
  (aliases network-info:aliases)
  (net network-info:net))

(define-exported-binding "network-info-type" :network-info)

(define (network-info arg)
  (cond ((string? arg) (name->network-info arg))
	((socket-address? arg) (car (socket-address:address arg)))
	(else
	 (error "network-info: string or socket-address expected ~s" arg))))

(define (address->network-info addr)
  (if (not (integer? addr))
      (error "address->network-info: integer expected ~s" addr)
      (%net-address->network-info addr)))

(import-lambda-definition %net-address->network-info (addr)
  "scheme_net_address2net_info")

(define (name->network-info name)
  (if (not (string? name))
      (error "name->network-info: string expected ~s" name)
      (%net-name->network-info name)))

(import-lambda-definition %net-name->network-info (name)
  "scheme_net_name2net_info")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; service lookup
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record-type :service-info
  (make-service-info name aliases port protocol)
  service-info?
  (name service-info:name)
  (aliases service-info:aliases)
  (port service-info:port)
  (protocol service-info:protocol))

(define-exported-binding "service-info-type" :service-info)

(define (service-info . args)
  (apply (cond ((string?  (car args)) name->service-info)
	       ((integer? (car args)) port->service-info)
	       (else (error "service-info: string or integer expected ~s" args)))
	 args))

(define (port->service-info name . maybe-proto)
  (let ((proto (:optional maybe-proto "")))
    (cond ((not (integer? name))
	   (error "port->service-info: integer expected ~s" name))
	  ((not (string? proto))
	   (error "port->service-info: string expected ~s" proto))
	  (else
	   (%service-port->service-info name (if (equal? "" proto)
						 #f
						 proto))))))

(import-lambda-definition %service-port->service-info (port proto)
  "scheme_serv_port2serv_info")

(define (name->service-info name . maybe-proto)
  (let ((proto (:optional maybe-proto "")))
    (cond ((not (string? name))
	   (error "name->service-info: integer expected ~s" name))
	  ((not (string? proto))
	   (error "name->service-info: string expected ~s" proto))
	  (else
	   (%service-name->service-info name (if (equal? "" proto)
						 #f
						 proto))))))

(import-lambda-definition %service-name->service-info (name proto)
  "scheme_serv_name2serv_info")

;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; protocol lookup
;;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define-record-type :protocol-info
  (make-protocol-info name aliases number)
  protocol-info?
  (name protocol-info:name)
  (aliases protocol-info:aliases)
  (number protocol-info:number))

(define-exported-binding "protocol-info-type" :protocol-info)

(define (protocol-info arg)
  (cond ((string? arg)  (name->protocol-info arg))
	((integer? arg) (number->protocol-info arg))
	(else (error "protocol-info: string or integer expected ~s" arg))))

(define (number->protocol-info name)
  (if (not (integer? name))
      (error "number->protocol-info: integer expected ~s" name)
      (%protocol-port->protocol-info name)))

(import-lambda-definition %protocol-port->protocol-info (name)
  "scheme_proto_num2proto_info")

(define (name->protocol-info name)
  (if (not (string? name))
      (error "name->protocol-info: string expected ~s" name)
      (%protocol-name->protocol-info name)))

(import-lambda-definition %protocol-name->protocol-info (name)
  "scheme_proto_name2proto_info")

