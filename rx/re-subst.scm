;;; Substitution ops with regexps
;;; Copyright (c) 1998 by Olin Shivers.

;;; These function have to be in a separate package because they use
;;; the scsh I/O function WRITE-STRING. The rest of the regexp system
;;; has no dependencies on scsh system code, and is defined independently
;;; of scsh -- which scsh, in turn, relies upon: pieces of scsh-level-0
;;; use the regexp basics. So we have to split this code out to avoid
;;; a circular dependency in the modules: scsh-level-0 needs the regexp
;;; package which needs WRITE-STRING, which comes from the regexp package.

(define (regexp-substitute port match . items)
  (let* ((str (regexp-match:string match))
	 (submatches (regexp-match:submatches match))
	 (range (lambda (item)			; Return start & end of
		  (cond ((integer? item)	; ITEM's range in STR.
			 (let ((submatch (vector-ref submatches item)))
			   (values (match-start submatch)
				   (match-end submatch))))
			((eq? 'pre item) (values 0
						 (match-start
						  (vector-ref submatches 0))))
			((eq? 'post item) (values (match-end
						   (vector-ref submatches 0))
						  (string-length str)))
			(else (error "Illegal substitution item."
				     item
				     regexp-substitute))))))
    (if port

	;; Output port case.
	(for-each (lambda (item)
		    (if (string? item) (write-string item port)
			(receive (si ei) (range item)
			  (write-string str port si ei))))
		  items)

	;; Here's the string case. Make two passes -- one to
	;; compute the length of the target string, one to fill it in.
	(let* ((len (fold (lambda (item i)
			    (+ i (if (string? item) (string-length item)
				     (receive (si ei) (range item) (- ei si)))))
			  0 items))
	       (ans (make-string len)))

	  (fold (lambda (item index)
		  (cond ((string? item)
			 (string-copy! ans index item)
			 (+ index (string-length item)))
			(else (receive (si ei) (range item)
				(string-copy! ans index str si ei)
				(+ index (- ei si))))))
		0 items)
	  ans))))



(define (regexp-substitute/global port re str . items)
  (let ((str-len (string-length str))
	(range (lambda (start submatches item)	; Return start & end of
		 (cond ((integer? item)		; ITEM's range in STR.
			 (let ((submatch (vector-ref submatches item)))
			   (values (match-start submatch)
				   (match-end submatch))))
		       ((eq? 'pre item)
			(values start
				(match-start
				 (vector-ref submatches 0))))
		       (else (error "Illegal substitution item."
				    item
				    regexp-substitute/global)))))
	(num-posts (fold (lambda (item count)
			   (+ count (if (eq? item 'post) 1 0)))
			 0 items)))

    (if (and port (< num-posts 2))

	;; Output port case, with zero or one POST items.
	(let recur ((start 0))
	  (if (<= start str-len)
	      (let ((match (regexp-search re str start)))
		(if match
		    (let* ((submatches (regexp-match:submatches match))
			   (s (match-start (vector-ref submatches 0)))
			   (e (match-end (vector-ref submatches 0)))
			   (empty? (= s e)))
		      (for-each (lambda (item)
				  (cond ((string? item) (write-string item port))

					((procedure? item) (write-string (item match) port))

					((eq? 'post0 item)
					 (if (and empty? (< s str-len))
					     (write-char (string-ref str s) port)))

					((eq? 'post item)
					 (recur (if empty? (+ 1 e) e)))

					(else (receive (si ei)
						  (range start submatches item)
						(write-string str port si ei)))))
				items))

		    (write-string str port start))))) ; No match.

	;; Either we're making a string, or >1 POST.
	(let* ((pieces (let recur ((start 0))
			 (if (> start str-len)
			     '()
			     (let ((match (regexp-search re str start))
				   (cached-post #f))
			       (if match
				   (let* ((submatches
					   (regexp-match:submatches match))
					  (s (match-start (vector-ref submatches 0)))
					  (e (match-end (vector-ref submatches 0)))
					  (empty? (= s e)))
				     (fold (lambda (item pieces)
					     (cond ((string? item)
						    (cons item pieces))

						   ((procedure? item)
						    (cons (item match) pieces))

						   ((eq? 'post0 item)
						    (if (and empty? (< s str-len))
							(cons (string (string-ref str s))
							      pieces)
							pieces))

						   ((eq? 'post item)
						    (if (not cached-post)
							(set! cached-post
							      (recur (if empty? (+ e 1) e))))
						    (append cached-post pieces))

						   (else (receive (si ei)
							     (range start submatches item)
							   (cons (substring str si ei)
								 pieces)))))
					   '() items))

				   ;; No match. Return str[start,end].
				   (list (if (zero? start) str 
					     (substring str start (string-length str)))))))))
			     
	       (pieces (reverse pieces)))
	  (if port (for-each (lambda (p) (write-string p port)) pieces)
	      (apply string-append pieces))))))
