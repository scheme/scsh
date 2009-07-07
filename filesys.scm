;;; Ops that create objects in the file system:
;;; create-{directory,fifo,hard-link,symlink}
;;; Copyright (c) 1993 by Olin Shivers.

;;; This procedure nukes FNAME, whatever it may be: directory, file, fifo,
;;; symlink.
;;;
;;; We can't probe FNAME to find out what it is and then do the right
;;; delete operation because there's a window in-between the probe and the
;;; delete where the file system can be altered -- the probe and delete
;;; aren't atomic. In order to deliver on our contract, we have to spin
;;; in a funny loop until we win. In practice, the loop will probably never
;;; execute more than once.

(define (delete-filesys-object fname)
  (let loop ()
    (or (with-errno-handler ; Assume it's a file and try.
	    ((err data)
	     ((errno/perm) #f) ; Return #f if directory
	     ((errno/isdir) #f)
	     ((errno/noent) #t))
	    (delete-file fname)
	    #t)

	(with-errno-handler ; Assume it's a directory and try.
	    ((err data)
	     ((errno/notdir) #f) ; Return #f if fname is not a directory.
	     ((errno/noent) #t))
	    (delete-directory fname)
	    #t)

	(loop)))) ; Strange things are happening. Try again.


;;; For similar reasons, all of these ops must loop.

;;; Abstract out common code for create-{directory,fifo,hard-link,symlink}:

(define (create-file-thing fname makeit override? op-name syscall)
    (let ((query (lambda ()
		   (y-or-n? (string-append op-name ": " fname
					   " already exists. Delete")))))
      (let ((result
	     (let loop ((override? override?))
	       (with-errno-handler
		((err data)
		 ((errno/exist)
		  (cond ((if (eq? override? 'query)
			     (query)
			     override?)
			 (delete-filesys-object fname)
			 (loop #t))
		      ;;; raising an error here won't work due to S48's
		      ;;; broken exception system
			(else (list err syscall fname)))))
		(with-resources-aligned 
		 (list cwd-resource umask-resource euid-resource egid-resource)
		 (lambda ()
		   (makeit fname)))
		#f))))
	(if (list? result)
	    (apply errno-error result)
	    (if #f #f)))))

;;;;;;;

(define (create-directory dir . rest)
  (let ((perms (if (null? rest) #o777 (car rest)))
	(override? (if (or (null? rest) (null? (cdr rest))) #f
		       (cadr rest))))
    (create-file-thing dir
		       (lambda (dir) (%create-directory dir perms))
		       override?
		       "create-directory"
		       create-directory)))

(define (create-fifo fifo . rest)
  (let ((perms (if (null? rest) #o777 (car rest)))
	(override? (if (or (null? rest) (null? (cdr rest))) #f
		       (cadr rest))))
    (create-file-thing fifo
		       (lambda (fifo) (%create-fifo fifo perms))
		       override?
		       "create-fifo"
		       create-fifo)))

(define (create-hard-link old-fname new-fname . maybe-override?)
  (create-file-thing new-fname
		     (lambda (new-fname)
		       (%create-hard-link old-fname new-fname))
		     (:optional maybe-override? #f)
		     "create-hard-link"
		     create-hard-link))

(define (create-symlink old-fname new-fname . maybe-override?)
  (create-file-thing new-fname
		     (lambda (symlink)
		       (%create-symlink old-fname symlink))
		     (:optional maybe-override? #f)
		     "create-symlink"
		     create-symlink))

;;; Unix rename() works backwards from mkdir(), mkfifo(), link(), and 
;;; symlink() -- it overrides by default, (though insisting on a type
;;; match between the old and new object). So we can't use create-file-thing.
;;; Note that this loop has a tiny atomicity problem -- if someone
;;; creates a file *after* we do our existence check, but *before* we 
;;; do the rename, we could end up overriding it, when the user asked
;;; us not to. That's life in the food chain.

(define (rename-file old-fname new-fname . maybe-override?)
  (let ((override? (:optional maybe-override? #f)))
    (if (or (and override? (not (eq? override? 'query)))
	    (file-not-exists? new-fname)
	    (and override?
		 (y-or-n? (string-append "rename-file:" new-fname
					 " already exists. Delete"))))
	(with-resources-aligned 
	 (list cwd-resource euid-resource egid-resource)
	 (lambda ()
	   (%rename-file old-fname new-fname))))))

(define (read-symlink path)
  (with-resources-aligned 
	 (list cwd-resource euid-resource egid-resource)
	 (lambda ()
	   (%read-symlink path))))


(define (delete-directory path)
  (with-resources-aligned 
	 (list cwd-resource euid-resource egid-resource)
	 (lambda ()
	   (%delete-directory path))))