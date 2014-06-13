;;; Copyright (c) 1994 by Olin Shivers

;;; String collectors
;;; ===========================================================================
;;; string-collector
;;; (make-string-collector)
;;; (collect-string! SC S)
;;; (clear-string-collector! SC)
;;; (string-collector->string SC)
;;;
;;; A string collector is a data structure that reduces the overhead of
;;; accumulating a large string in bits and pieces. It is basically a
;;; "chunk list," where a chunk is a string of at least 128 chars. In this
;;; way, the list overhead is kept under 2% of the whole data structure.
;;; When a new string is added to the collection, it is added to the current
;;; chunk. When the chunk reaches 128 chars, it is added to the chunk list,
;;; and a new chunk is started. If a large string is added to the collection,
;;; it is added as a chunk itself, so large strings are not split into small
;;; pieces. (Actually, a *copy* of the original large string is saved as a
;;; single chunk, so the collector's chunks are not shared with client data.)
;;;
;;; MAKE-STRING-COLLECTOR allocates a new string collector data structure.
;;; COLLECT-STRING! appends a string to the current collection.
;;; CLEAR-STRING-COLLECTOR! clears out accumulated strings from a collector.
;;; STRING-COLLECTOR->STRING converts a collector into a contiguous string.
;;;
;;; This facility makes it reasonably efficient to accumulate strings
;;; of any size in increments of any size.

(define-record-type :string-collector
  (really-make-string-collector len chunks chunk chunk-left)
  string-collector?
  ;; How many chars have we accumulated?
  (len string-collector:len set-string-collector:len)
  ;; The chunk list.
  (chunks string-collector:chunks set-string-collector:chunks)
  ;; The current chunk being filled, if any.
  (chunk string-collector:chunk set-string-collector:chunk)
  ;; How many chars left to fill in the current chunk.
  (chunk-left  string-collector:chunk-left set-string-collector:chunk-left))

(define (make-string-collector)
  (really-make-string-collector 0 '() #f 0))

(define (clear-string-collector! sc)
  (set-string-collector:len    sc 0)
  (set-string-collector:chunks sc '())
  (set-string-collector:chunk  sc #f)
  sc)

;;; (COLLECT-STRING! sc s)
;;; ----------------------
;;; S is a string. Append it to the string being collected in the
;;; string-collector SC.
;;;
;;; The algorithm:
;;; First, do nothing if S is the empty string. Otherwise:
;;; If there is a current chunk:
;;;    Copy characters from S into it.
;;;    If we filled up the chunk
;;;        Put the chunk on the chunk list.
;;;        Look at the remaining chars from S we haven't copied yet.
;;;        If there a lot of characters left (>= 128)
;;;            Save them as a single chunk on the chunk list.
;;;            No current chunk.
;;;	   Else if there a just a few characters left (> 0, < 128)
;;;            Start a new current chunk, copy the chars left into it.
;;;        Else if there aren't any characters left
;;;            No current chunk.
;;;
;;; If there is no current chunk:
;;;     If there are a lot of characters in S (>= 128)
;;;         Save a copy of S as a single chunk on the chunk list.
;;;         Still no current chunk.
;;;     Else if there are a few characters in S (> 0, < 128)
;;;         Start a new current chunk, copy the S into it.

(define (collect-string! sc s)
  (let ((slen (string-length s))
	(chunk (string-collector:chunk sc))
	(chunk-left (string-collector:chunk-left sc))

	;; Add the chunk C to the collector's chunk list.
	(push-chunk! (lambda (c)
		       (set-string-collector:chunks sc
		           (cons c (string-collector:chunks sc)))))

	;; Copy nchars characters from src[j] to dest[i]
	;; *Way* too much bounds checking going on in this loop.
	(copy-substring! (lambda (dest i src j nchars)
	  (do ((i i (+ i 1))
	       (j j (+ j 1))
	       (nchars nchars (- nchars 1)))
	      ((zero? nchars))
	    (string-set! dest i (string-ref src j))))))

    (cond ((zero? slen)) ; Empty string, do nothing.
	  (chunk
	   (let ((ncopy (min slen chunk-left)))
	     (copy-substring! chunk (- 128 chunk-left) s 0 ncopy)
	     (if (> chunk-left slen)
		 (set-string-collector:chunk-left sc (- chunk-left slen))
		 ;; Current chunk is full.
		 (let ((s-left (- slen chunk-left)))
		   (push-chunk! chunk) ; Push the current chunk.
		   ;; Handle remaining chars from S that weren't copied into
		   ;; the current chunk we just pushed:
		   (cond ((>= s-left 128)
			  ;; A lot more chars left. Push them as one chunk.
			  (push-chunk! (substring s chunk-left slen))
			  (set-string-collector:chunk sc #f))
			 ((> s-left 0)
			  ;; A few more chars left. Start a new chunk.
			  (let ((new-chunk (make-string 128)))
			    (copy-substring! new-chunk 0 s chunk-left s-left)
			    (set-string-collector:chunk sc new-chunk)
			    (set-string-collector:chunk-left sc
							     (- 128 s-left))))
			 ;; No more chars left. No current chunk.
			 (else (set-string-collector:chunk sc #f)))))))

	  (else ; No current chunk.
	   (if (>= slen 128)  ; How many chars is S?
	       (push-chunk! (string-copy s))    ; A lot. Push as one chunk.
	       (let ((chunk (make-string 128))) ; Not many. Start a new chunk.
		    (set-string-collector:chunk sc chunk)
		    (copy-substring! chunk 0 s 0 slen)
		    (set-string-collector:chunk-left sc (- 128 slen))))))

  ;; We don't actually do anything with this, but we keep it updated anyway.
  (set-string-collector:len sc (+ (string-collector:len sc) slen))
  sc))

;;; A bummed version for collecting a single character.

(define (collect-char! sc c)
  (let ((chunk (string-collector:chunk sc))
        (chunk-left (string-collector:chunk-left sc)))
    (cond (chunk
           (string-set! chunk (- 128 chunk-left) c)
           (cond ((> chunk-left 1)
                  (set-string-collector:chunk-left sc (- chunk-left 1)))
                 (else
                  (set-string-collector:chunks sc (cons chunk (string-collector:chunks sc)))
                  (set-string-collector:chunk sc #f))))
          (else
           (let ((new-chunk (make-string 128 c)))
             (set-string-collector:chunk-left sc 127)
             (set-string-collector:chunk sc new-chunk)))))

  ;; We don't actually do anything with this, but we keep it updated anyway.
  (set-string-collector:len sc (+ (string-collector:len sc) 1))
  sc)

;;; Convert the data in the string-collector SC to a single contiguous
;;; string and return it.

(define (string-collector->string sc)
  (let ((chunk  (string-collector:chunk sc))
        (chunks (string-collector:chunks sc)))
    (apply string-append
           (reverse (if chunk
                        (cons (substring chunk 0
                                         (- 128
                                            (string-collector:chunk-left sc)))
                              chunks)
                        chunks)))))

;;; It's too bad we can't side-effect the string-collector's chunk list
;;; to be a single chunk after this coalescing operation, but we don't
;;; want to share the string we return -- the user might side-effect it.
