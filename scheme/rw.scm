;;; Basic read and write
;;; Copyright (c) 1993 by Olin Shivers.

;;; Note: read ops should check to see if their string args are mutable.

;;; Best-effort/forward-progress reading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-string!/partial s . args)
  (let-optionals args ((fd/port (current-input-port))
		       (start   0)
		       (end     (string-length s)))
    (if (bogus-substring-spec? s start end)
	(error "Bad substring indices" s start end))

    (cond ((integer? fd/port)
	   (let ((port (fdes->inport fd/port)))
	     (set-port-buffering port bufpol/none)
	     (read-string!/partial s port start end)))

          ((open-input-port? fd/port)
	   (if (= start end)
	       0
	       (let* ((needed (if (file-options-on? (i/o-flags fd/port)
                                                    (file-options nonblocking))
				  'any
				  'immediate))
		      (nread (if (= end (string-length s))
				 (read-block s start needed fd/port)
     		      ;;; READ-BLOCK doesn't allow us to specify a
     		      ;;; maximum number of characters to read/partial
     		      ;;; but fills the buffer at most to the end.
		      ;;; Therefore we allocate a new buffer here:
				 (let* ((buf (make-string (- end start)))
					(nread-any
					 (read-block buf 0 needed fd/port)))
				   (if (not (eof-object? nread-any))
				       (copy-bytes! buf 0 s start nread-any))
				   nread-any))))

		 (if (eof-object? nread)
		     #f
		     nread))))

	  (else
	   (apply error "Not a fd/port in read-string!/partial" s args)))))


(define (read-string/partial len . maybe-fd/port)
  (let* ((fd/port (:optional maybe-fd/port (current-input-port))))
    (cond ((integer? fd/port)
	   (let ((port (fdes->inport fd/port)))
	     (set-port-buffering port bufpol/none)
	     (read-string/partial len port)))

	  ((open-input-port? fd/port)
	   (if (= len 0)
               ""
	       (let* ((buffer (make-string len))
		      (nread (read-string!/partial buffer fd/port)))
		 (cond ((not nread) #f)
		       ((= nread len) buffer)
		       (else (substring buffer 0 nread))))))
	  (else
	   (error "Not a fd/port in read-string/partial" len fd/port)))))


;;; Persistent reading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operation on ports is easy, since we can use read-block

(define (read-string! s . args)
  (let-optionals args ((fd/port (current-input-port))
		       (start   0)
		       (end     (string-length s)))
    (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" s start end))

    (cond ((integer? fd/port)
	   (let ((port (fdes->inport fd/port)))
	     (set-port-buffering port bufpol/block (- end start))
	     (read-string! port start end)))

	  (else ; no differnce between fd/port and s48 ports
	   (let ((nbytes/eof (read-block s start (- end start) fd/port)))
	     (if (eof-object? nbytes/eof)
		 #f
		 nbytes/eof))))))

(define (read-string len . maybe-fd/port)
  (let* ((s (make-string len))
	 (fd/port (:optional maybe-fd/port (current-input-port)))
	 (nread (read-string! s fd/port 0 len)))
    (cond ((not nread) #f) ; EOF
	  ((= nread len) s)
	  (else (substring s 0 nread)))))


;;; Best-effort/forward-progress writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-blocking output to a buffered port is not defined.

(define (write-string/partial s . args)
  (let-optionals args ((fd/port (current-output-port))
		       (start 0)
		       (end (string-length s)))
    (if (bogus-substring-spec? s start end)
	(error "Bad substring indices" s start end))

    (cond ((integer? fd/port)
	   (let ((port (fdes->outport fd/port)))
	     (set-port-buffering port bufpol/block (- end start))
	     (write-string/partial s port start end)))
	  (else
	   ;; the only way to implement this, would be to use
	   ;; channel-maybe-write. But this is an VM-instruction which is not
	   ;; exported. Since we now have threads this shouldn;t matter.
	   (error "write-string/parital is currently dereleased.
See the RELEASE file for details")))))


;;; Persistent writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-string s . args)
  (let-optionals args ((fd/port (current-output-port))
		       (start   0)
		       (end     (string-length s)))
    (if (bogus-substring-spec? s start end)
	(error "Bad substring indices" s start end))

    (cond ((integer? fd/port)
	   (let ((port (fdes->outport fd/port)))
	     (set-port-buffering port bufpol/block (- end start))
	     (write-string s port start end)))
	  (else (write-block (string->os-byte-vector s) start (- end start) fd/port)))))
