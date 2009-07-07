;;; Delimited readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These procedures ran their inner I/O loop in a C primitive in
;;; earlier versions of scsh. In a multi-threaded environment this
;;; causes lots of trouble in case the operation would
;;; block. Therefore the current implementation runs in Scheme but
;;; operates directly on the buffer of the port for speed. This also
;;; allows us to implement the push-back behaviour without a peek/read
;;; pair.
;;;

;;; (read-delimited delims [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns a string or the EOF object. DELIM-ACTION determines what to do
;;; with the terminating delimiter:
;;; - PEEK
;;;   Leave it in the input stream for later reading.
;;; - TRIM (the default)
;;;   Drop it on the floor.
;;; - CONCAT
;;;   Append it to the returned string.
;;; - SPLIT
;;;   Return it as a second return value.
;;;
;;; We repeatedly allocate a buffer and fill it with READ-DELIMITED!
;;; until we hit a delimiter or EOF. Each time through the loop, we
;;; double the total buffer space, so the loop terminates with a log
;;; number of reads, but uses at most double the optimal buffer space.

(define (read-delimited delims . args)
  (let-optionals args ((port         (current-input-port))
		       (delim-action 'trim))
	(let ((substr (lambda (s end)		; Smart substring.
			(if (= end (string-length s)) s
			    (substring s 0 end))))
	      (delims (x->char-set delims))
	      (gobble? (not (eq? delim-action 'peek))))
      ;; BUFLEN is total amount of buffer space allocated to date.
      (let lp ((strs '()) (buflen 80) (buf (make-string 80)))
	(receive (terminator num-read)
	         (%read-delimited! delims buf gobble? port)
	  (if terminator

	      ;; We are done. NUM-READ is either a read count or EOF.
	      (let ((retval (if (and (zero? num-read)
				     (eof-object? terminator)
				     (null? strs))
				terminator		; EOF -- got nothing.

				;; Got something. Stick all the strings
				;; together, plus the terminator if the
				;; client said 'CONCAT.
				(let ((s (substr buf num-read)))
				  (cond ((and (eq? delim-action 'concat)
					      (char? terminator))
					 (apply string-append
						(reverse `(,(string terminator)
							   ,s . ,strs))))

					((null? strs) s)    ; Gratuitous opt.
					(else (apply string-append
						     (reverse (cons s strs)))))))))
		(if (eq? delim-action 'split)
		    (values retval terminator)
		    retval))

	      ;; We are not done. Loop and read in some more.
	      (lp (cons buf strs)
		  (+ buflen buflen)
		  (make-string buflen))))))))


;;; (read-delimited! delims buf [port delim-action start end])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns:
;;; - EOF if at end of file, and a non-zero read was requested.
;;; - Integer j if that many chars read into BUF.
;;; - #f if the buffer was filled w/o finding a delimiter.
;;;
;;; DELIM-ACTION determines what to do with the terminating delimiter;
;;; it is as in READ-DELIMITED.
;;;
;;; In determining the return value, there is an ambiguous case: when the 
;;; buffer is full, *and* the following char is a delimiter char or EOF.
;;; Ties are broken favoring termination over #f -- after filling the buffer,
;;; READ-DELIMITED! won't return #f until it has peeked one past the end
;;; of the buffer to ensure the next char doesn't terminate input (or is EOF).
;;; However, this rule is relaxed with delim-action = CONCAT -- if the buffer
;;; is full, READ-DELIMITED! won't wait around trying to peek at the following
;;; char to determine whether or not it is a delimiter char, since it doesn't
;;; have space to store the character anyway. It simply immediately returns #f;
;;; a following read can pick up the delimiter char.

(define (read-delimited! delims buf . args) ; [port delim-action start end]
  (let-optionals args ((port         (current-input-port))
		       (delim-action 'trim)
		       (start        0)
		       (end          (string-length buf)))
    (receive (terminator num-read)
	     (%read-delimited! delims buf
			       (not (eq? delim-action 'peek)) ;Gobble delim?
			       port
			       start
			       (if (eq? delim-action 'concat)
				   (- end 1) ; Room for terminator.
				   end))

      (if terminator	; Check for buffer overflow.
	  (let ((retval (if (and (zero? num-read)
				 (eof-object? terminator))
			    terminator	; EOF -- got nothing.
			    num-read))) ; Got something.

	    (case delim-action
	      ((peek trim)	retval)
	      ((split)	(values retval terminator))
	      ((concat)	(cond ((char? terminator)
			       (string-set! buf (+ start num-read) terminator)
			       (+ num-read 1))
			      (else retval)))))

	  ;; Buffer overflow.
	  (case delim-action
	    ((peek trim) #f)
	    ((split)     (values #f #f))
	    ((concat)    (let ((last (read-char port)))
			   (if (char? last)
			       (string-set! buf (+ start num-read) last))
			   (and (or (eof-object? last)
				    (char-set-contains? (x->char-set delims)
							last))
				(+ num-read 1)))))))))
		  

;;; (%read-delimited! delims buf gobble? [port start end])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This low-level routine uses a different interface. It returns two values:
;;; - TERMINATOR: A value describing why the read was terminated:
;;;   + character or eof-object => read terminated by this value; 
;;;   + #f                      => filled buffer w/o terminating read.
;;; - NUM-READ: Number of chars read into buf.
;;; 
;;; Note:
;;; - Invariant: TERMINATOR = #f  =>  NUM-READ = END - START.
;;; - Invariant: TERMINATOR = eof-object and NUM-READ = 0 => at EOF.
;;; - When determining the TERMINATOR return value, ties are broken
;;;   favoring character or the eof-object over #f. That is, if the buffer
;;;   fills up, %READ-DELIMITED! will peek at one more character from the
;;;   input stream to determine if it terminates the input. If so, that
;;;   is returned, not #f.
;;;
;;; If GOBBLE? is true, then a terminator character is removed from
;;; the input stream. Otherwise, it is left in place for a following input
;;; operation.


(define (port-buffer-read-delimited delims buf gobble? port start end)
  (obtain-port-lock port)
  (let ((the-port-limit (port-limit port)))
    (let lp ((i start) (lp-port-index (port-index port))) 
      (cond ((port-pending-eof? port)
	     (set-port-index! port lp-port-index)
	     (release-port-lock port)
	     (values (eof-object) (- i start)))
	    ((>= i end)
	     (set-port-index! port lp-port-index)
	     (release-port-lock port)
	     (values #f (- i start)))
	    ((< lp-port-index the-port-limit)
	     (let ((the-read-char 
		    (ascii->char (byte-vector-ref 
				  (port-buffer port) lp-port-index))))
	       (if (char-set-contains? delims the-read-char)
		   (begin 
		     (if gobble? 
			 (set-port-index! port (+ lp-port-index 1))
			 (set-port-index! port lp-port-index))
		     (release-port-lock port)
		     (values the-read-char (- i start)))
		   (begin
		     (string-set! buf i the-read-char)
		     (lp (+ i 1) (+ lp-port-index 1))))))
	    (else  (set-port-index! port 0)
		   (set-port-limit! port 0)
		   (release-port-lock port)
		   (values 'port-buffer-exhausted (- i start)))))))
		 


(define (%read-delimited! delims buf gobble? . args)
  (let-optionals args ((port  (current-input-port))
		       (start 0)
		       (end   (string-length buf)))
    (if (immutable? buf)
        (error "Immutable buffer argument to %read-delimited!" buf))
    (let ((delims (x->char-set delims)))
      (let lp ((start start) (total 0))
	(receive (terminator num-read) 
	    (port-buffer-read-delimited delims buf gobble? port start end)
	  (if (not (eq? terminator 'port-buffer-exhausted))
	      (values terminator (+ num-read total))
	      (begin (peek-char port)	; kludge to fill the buffer
		     (lp (+ start num-read) (+ total num-read)))))))))
			  


; overwrites port-index :-(
(define (push-back port char)
  (byte-vector-set! (port-buffer port) (port-index port) (char->ascii char))
  (set-port-limit! port (+ (port-limit port) 1)))


(define (skip-char-set skip-chars . maybe-port)
  (let ((port (:optional maybe-port (current-input-port)))
	(cset (x->char-set skip-chars)))
    
    (let lp ((total 0))
      (receive (succ num-read) (buffer-skip-char-set cset port)
	(if (not succ)
	    (+ total num-read)		; eof
	    (begin (peek-char port)	; kludge to fill the buffer
		   (lp  (+ total num-read))))))))

(define (buffer-skip-char-set cset port)
  (let ((the-port-limit (port-limit port)))
    (let lp ((lp-port-index (port-index port)) (i 0))
      (cond ((port-pending-eof? port)
	     (set-port-index! port lp-port-index)
	     (values #f i))
	    ((< lp-port-index the-port-limit)
	     (let ((the-read-char 
		    (ascii->char (byte-vector-ref 
				  (port-buffer port) lp-port-index))))
	       (cond ((char-set-contains? cset the-read-char)
		      (lp (+ lp-port-index 1) (+ i 1)))
		     (else 
		      (set-port-index! port lp-port-index)
		      (values #f i)))))
	    (else (set-port-index! port 0)
		  (set-port-limit! port 0)
		  (values 'port-buffer-exhausted i))))))

;;; (read-line [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read in a line of data. Input is terminated by either a newline or EOF.
;;; The newline is trimmed from the string by default.

(define charset:newline (char-set #\newline))

(define (read-line . rest) (apply read-delimited charset:newline rest))


;;; (read-paragraph [port handle-delim])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define blank-line-regexp (rx bos (* white) #\newline eos))

(define (read-paragraph . args)
  (let-optionals args ((port         (current-input-port))
		       (handle-delim 'trim))
    ;; First, skip all blank lines.
    (let lp ()
      (let ((line (read-line port 'concat)))
	(cond ((eof-object? line)
	       (if (eq? handle-delim 'split) (values line line) line))

	      ((regexp-search? blank-line-regexp line) (lp))

	      ;; Then, read in non-blank lines.
	      (else
	       (let lp ((lines (list line)))
		 (let ((line (read-line port 'concat)))
		   (if (and (string? line)
			    (not (regexp-search? blank-line-regexp line)))

		       (lp (cons line lines))

		       ;; Return the paragraph
		       (let ((->str (lambda (lns) (apply string-append (reverse lns)))))
			 (case handle-delim
			   ((trim) (->str lines))

			   ((concat)
			    (->str (if (eof-object? line) lines (cons line lines))))

			   ((split)
			    (values (->str lines) line))

			   (else (error "Illegal HANDLE-DELIM parameter to READ-PARAGRAPH")))))))))))))
