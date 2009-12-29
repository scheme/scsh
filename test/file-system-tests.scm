;;; Tests for the function in section 3.3 of the scsh-manual "File system"
;;; Author: David Frese

; file-type: don't know how to test block-special, char-special
;	   socket should be tested in section "Networking"!!
; file-device: ??
; file-inode: only tested for overflow
;
; sync-file: Test is not very stable, I guess??
;
; glob: hard work ??
; temp-file-iterate: could it be ignored?? create-temp-file uses it anyway??
; temp-file-channel: ??


(define create-temp-dir
  (let ((temp-dir "/tmp/scsh-test/"))
    (lambda ()
      (if (file-not-exists? temp-dir)
	  (create-directory temp-dir))
      temp-dir)))

(define (file-perms fname/fd/port)
  (bitwise-and (file-mode fname/fd/port)
	       #o777))

(define (mask fixnum)
  (bitwise-and fixnum
	       (bitwise-not (umask))))

(define (create-file fname)
  (close-output-port (open-output-file fname)))

(define (open/create-file fname flags)
  (if (file-not-exists? fname)
      (create-file fname))
  (open-file fname flags))

(define (symbol-append symbol string)
  (string->symbol (string-append
		   (symbol->string symbol)
		   string)))

;; --- Create-Directory ---

(add-test! 'create-directory-1 'file-system
	   (lambda (name)
	     (with-cwd (create-temp-dir)
		       (create-directory name)
		       (let ((result (file-directory? name)))
			 (delete-filesys-object name)
			 result)))
	   "dir")

(add-test! 'create-directory-2 'file-system
	   (lambda (name perms)
	     (with-cwd (create-temp-dir)
		       (create-directory name perms)
		       (let ((result (and (file-directory? name)
					  (= (file-perms name)
					     (mask perms)))))
			 (delete-filesys-object name)
			 result)))
	   "dir" #o700)

;; --- Create FIFO ---

(add-test! 'create-fifo-1 'file-system
	   (lambda (name)
            (with-cwd (create-temp-dir)
			  (create-fifo name)
			  (let ((result (eq? (file-type name)
					     'fifo)))
			    (delete-filesys-object name)
			    result)))
	   "fifo")

(add-test! 'create-fifo-2 'file-system
	   (lambda (name perms)
	     (with-cwd (create-temp-dir)
		       (create-fifo name perms)
		       (let ((result (and (eq? (file-type name)
					       'fifo)
					  (= (file-perms name)
					     (mask perms)))))
			 (delete-filesys-object name)
			 result)))
	   "fifo" #o700)

;; --- Create-hard-link ---

(add-test! 'create-hard-link 'file-system
	   (lambda (fname linkname)
	     (with-cwd (create-temp-dir)
		       (close-output-port (open-output-file fname))
		       (create-hard-link fname linkname)
		       (let ((result (file-exists? linkname)))
			 (delete-filesys-object fname)
			 (delete-filesys-object linkname)
			 result)))
	   "file" "hard-link")

;; --- Create-symlink ---

(add-test! 'create-symlink 'file-system
	   (lambda (fname linkname)
	     (with-cwd (create-temp-dir)
		       (create-file fname)
		       (create-symlink fname linkname)
		       (let ((result (and (file-exists? linkname)
					  (eq? (file-type linkname #f)
					       'symlink)
					  (eq? (file-type linkname #t)
					       'regular))))
			 (delete-filesys-object fname)
			 (delete-filesys-object linkname)
			 result)))
	   "file" "symlink")

;; --- Delete-Directory ---

(add-test! 'delete-directory 'file-system
	   (lambda (name)
	     (with-cwd (create-temp-dir)
		       (create-directory name)
		       (delete-directory name)
		       (file-not-exists? name)))
	   "dir")

;; --- Delete-File ---

(add-test! 'delete-file 'file-system
	   (lambda (name)
	     (with-cwd (create-temp-dir)
		       (create-file name)
		       (delete-file name)
		       (file-not-exists? name)))
	   "file")


(add-test! 'delete-filesys-object 'file-system
	   (lambda (name)
	     (with-cwd (create-temp-dir)
		       (create-file name)
		       (delete-filesys-object name)
		       (and (file-not-exists? name)
			    ;; even now, it shouldn't signal an error
			    (delete-filesys-object name))))
	   "file")

;; --- Read-Symlink ---

(add-test! 'read-symlink 'file-system
	   (lambda (fname linkname)
	     (with-cwd (create-temp-dir)
		       (create-file fname)
		       (create-symlink fname linkname)
		       (let ((result (equal? fname
					     (read-symlink linkname))))
			 (delete-filesys-object fname)
			 (delete-filesys-object linkname)
			 result)))
	   "file" "symlink")

;; --- Rename-File ---

(add-test! 'rename-file 'file-system
	   (lambda (name1 name2)
	     (with-cwd (create-temp-dir)
		       (create-file name1)
		       (rename-file name1 name2)
		       (let ((result (and (file-exists? name2)
					  (file-not-exists? name1))))
			 (delete-filesys-object name2)
			 result)))
	   "file-1" "file-2")

;; --- Little Abstraction for funcs with fname/fd/port ---
;; uses add-test-multiple!

(define (add-test/fname/fd/port! name before-func func result-func . input-lists)
  (let ((name-1 (string->symbol (string-append (symbol->string name)
					       "/fname")))
	(name-2 (string->symbol (string-append (symbol->string name)
					       "/fd")))
	(name-3 (string->symbol (string-append (symbol->string name)
					       "/port"))))
    ;; Test as a filename
    (apply add-test-multiple!
	   name-1 'file-system
	   (lambda (fname . params)
	     (with-cwd (create-temp-dir)
		       (let ((port (open/create-file fname (file-options write-only))))
			 (if before-func (before-func port))
			 (let ((result (apply func (cons fname params))))
			   (close port)
			   (delete-filesys-object fname)
			   (if result-func
			       (apply result-func result params)
			       result)))))
	   input-lists)

    ;; Test as a fdes
    (apply add-test-multiple!
	   name-2 'file-system
	   (lambda (fname . params)
	     (with-cwd (create-temp-dir)
		       (let ((port (open/create-file fname (file-options write-only))))
			 (if before-func (before-func port))
			 (let ((result (apply func (cons (port->fd port)
							 params))))
			   (close port)
			   (delete-filesys-object fname)
			   (if result-func
			       (apply result-func result params)
			       result)))))
	   input-lists)

    ;; Test as a port
    (apply add-test-multiple!
	   name-3 'file-system
	   (lambda (fname . params)
	     (with-cwd (create-temp-dir)
		       (let ((port (open/create-file fname (file-options write-only))))
			 (if before-func (before-func port))
			 (let ((result (apply func (cons port params))))
			   (close port)
			   (delete-filesys-object fname)
			   (if result-func
			       (apply result-func result params)
			       result)))))
	   input-lists)
    ))



;; --- Set-file-mode ---

(add-test/fname/fd/port! 'set-file-mode
			 #f
			 (lambda (fname/fd/port mode)
			   (set-file-mode fname/fd/port mode)
			   (file-perms fname/fd/port))
			 =
			 '("file") '(#o754))

;; --- Set-file-owner ---

(add-test/fname/fd/port! 'set-file-owner
			 #f
			 (lambda (fname/fd/port uid)
			   (set-file-owner fname/fd/port uid)
			   (file-owner fname/fd/port))
			 equal?
			 '("file") (list (user-uid)))


;; --- Set-file-group ---

(add-test/fname/fd/port! 'set-file-group
			 #f
			 (lambda (fname/fd/port gid)
			   (set-file-group fname/fd/port gid)
			   (file-group fname/fd/port))
			 equal?
			 '("file") (list (user-gid)))

;; --- set-file-times ---

(add-test! 'set-file-times-1 'file-system
	   (lambda (fname time-1)
	     (with-cwd (create-temp-dir)
		       (create-file fname)
		       (set-file-times fname time-1 0)
		       (let ((result (file-last-access fname)))
			 (delete-filesys-object fname)
			 (= result time-1))))
	   "file" 10000)

(add-test! 'set-file-times-2 'file-system
	   (lambda (fname time-2)
	     (with-cwd (create-temp-dir)
		       (create-file fname)
		       (set-file-times fname 0 time-2)
		       (let ((result (file-last-mod fname)))
			 (delete-filesys-object fname)
			 (= result time-2))))
	   "file" 10000)

;; --- sync-file ---

(add-test! 'sync-file 'file-system
	   (lambda (fname)
	     (with-cwd (create-temp-dir)
		       (create-file fname)
		       (let ((port (open-file fname (file-options write-only))))
			 (write-string "1" port)
			 (let ((res-1 (file-size fname)))
			   (sync-file port)
			   (let ((res-2 (file-size fname)))
			     (close port)
			     (delete-filesys-object fname)
			     (and (= res-1 0) (> res-2 0)))))))
	   "file")

;; --- truncate-file ---

(add-test/fname/fd/port! 'truncate-file
			(lambda (port)
			  (write (make-string 100 #\*) port))
			(lambda (fname/fd/port len)
			  (truncate-file fname/fd/port len)
			  (file-size fname/fd/port))
			=
			'("file") '(10))

;; --- file-info stuff ---

;; --- file-type ---

(add-test! 'file-type-dir 'file-system
	   (lambda (fname)
	     (with-cwd (create-temp-dir)
		       (create-directory fname)
		       (let ((result (file-type fname)))
			 (delete-filesys-object fname)
			 (equal? result 'directory))))
	   "dir")

(add-test! 'file-type-fifo 'file-system
	   (lambda (fname)
	     (with-cwd (create-temp-dir)
		       (create-fifo fname)
		       (let ((result (file-type fname)))
			 (delete-filesys-object fname)
			 (equal? result 'fifo))))
	   "fifo")

(add-test! 'file-type-regular 'file-system
	   (lambda (fname)
	     (with-cwd (create-temp-dir)
		       (create-file fname)
		       (let ((result (file-type fname)))
			 (delete-filesys-object fname)
			 (equal? result 'regular))))
	   "file")

;(add-test! 'file-type-socket 'file-system
;	   (lambda (fname)
;	     (let* ((pathname (string-append (create-temp-dir)
;					     fname))
;		    (socket (create-socket protocol-family/unix
;					   socket-type/raw))
;		    (addr (unix-address->socket-address
;			   pathname)))
;	       (bind-socket socket addr)
;	       ;; now fname should be a socket
;	       (let ((result (file-info pathname)))
;		 (delete-filesys-object pathname)
;		 (equal? result 'socket))))
;	   "socket")

(add-test! 'file-type-symlink 'file-system
	   (lambda (fname linkname)
	     (create-file fname)
	     (create-symlink fname linkname)
	     (let ((result (file-type linkname #f))
		   (result-2 (file-type linkname #t)))
	       (delete-filesys-object linkname)
	       (delete-filesys-object fname)
	       (and (equal? result 'symlink)
		    (equal? result-2 'regular))))
	   "file" "symlink")

;; --- file-inode ---
;; only check for overrun (problem on AFS according to Martin)
(add-test/fname/fd/port! 'file-inode
			 #f
			 (lambda (fname/fd/port)
			   (> 0 (file-inode fname/fd/port)))
			 '("file"))


;; --- file-mode ---

(add-test/fname/fd/port! 'file-mode
			 #f
			 (lambda (fname/fd/port mode)
			   (set-file-mode fname/fd/port mode)
			   (bitwise-and (file-mode fname/fd/port)
					#o777))
			 =
			 '("file") (list #o754))

;; --- file-nlinks ---

(add-test/fname/fd/port! 'file-nlinks
			 #f
			 (lambda (fname/fd/port fname1 fname2)
			   (create-hard-link fname1 fname2)
			   (let ((result (file-nlinks fname/fd/port)))
			     (delete-filesys-object fname2)
			     (= result 2)))
			 #f
			 '("file-1") '("file-1") '("file-2"))

;; --- file-owner ---

(add-test/fname/fd/port! 'file-owner
			 #f
			 (lambda (fname/fd/port uid)
			   (set-file-owner fname/fd/port uid)
			   (file-owner fname/fd/port))
			 equal?
			 '("file") (list (user-uid)))


;; --- file-group ---

(add-test/fname/fd/port! 'file-group
			 #f
			 (lambda (fname/fd/port gid)
			   (set-file-group fname/fd/port gid)
			   (file-group fname/fd/port))
			 equal?
			 '("file") (list (user-gid)))

;; --- file-size ---

(add-test/fname/fd/port! 'file-size
			 (lambda (port)
			   (write-string "0123456789" port)
			   (sync-file port))
			 file-size
			 (lambda (res) (= res 10))
			 '("file"))

;; --- file-last-access ---

(add-test/fname/fd/port! 'file-last-access
			 #f
			 (lambda (fname/fd/port fname atime)
			   (set-file-times fname atime 0)
			   (file-last-access fname/fd/port))
			 (lambda (restime fname mtime)
			   (= restime mtime))
			 '("file") '("file") '(10000))

;; --- file-last-mod ---

(add-test/fname/fd/port! 'file-last-mod
			 #f
			 (lambda (fname/fd/port fname mtime)
			   (set-file-times fname 0 mtime)
			   (file-last-mod fname/fd/port))
			 (lambda (restime fname mtime)
			   (= restime mtime))
			 '("file") '("file") '(10000))

;; -- file-last-status-change ---
(add-test/fname/fd/port! 'file-last-status-change
			 #f
			 (lambda (fname/fd/port)
			   (let ((before (file-last-status-change
					  fname/fd/port)))
			     ;; do anything
			     (set-file-mode fname/fd/port #o777)
			     (let ((after (file-last-status-change
					   fname/fd/port)))
			       (> after before) ;; how much??
			       )))
			 '("file"))

;; --- file-not-read/write/exec-able ---

(define (add-file-not-?-able func name perms)
  ;; normal function
  (add-test! (symbol-append name "-normal") 'file-system
	     (lambda (fname)
	       (with-cwd (create-temp-dir)
			 (create-file fname)
			 (set-file-mode fname perms)
			 (let ((result (not (func fname))))
			   (delete-filesys-object fname)
			   result)))
	     "file")
  ;; search-denied
  (add-test! (symbol-append name "-search-denied") 'file-system
	     (lambda (fname dirname)
	       (with-cwd (create-temp-dir)
			 (create-directory dirname)
			 (create-file (string-append dirname fname))
			 (set-file-mode dirname 0) ;; or 666 ??
			 (let ((result (func (string-append dirname fname))))
			   (set-file-mode dirname #o777)
			   (delete-filesys-object (string-append dirname fname))
			   (delete-filesys-object dirname)
			   (equal? result 'search-denied))))
	     "file" "dir/")
  ;; permission denied
  (add-test! (symbol-append name "-permission") 'file-system
	     (lambda (fname)
	       (with-cwd (create-temp-dir)
			 (create-file fname)
			 (set-file-mode fname
					(bitwise-xor perms #o777))
			 (let ((result (func fname)))
			   (delete-filesys-object fname)
			   (equal? result 'permission))))
	     "file")
  ;; not-directory
  (add-test! (symbol-append name "-no-directory") 'file-system
	     (lambda (fname fname2)
	       (with-cwd (create-temp-dir)
			 (create-file fname2)
			 (let ((result (func (string-append
					      fname2 "/" fname))))
			   (delete-filesys-object fname2)
			   (equal? result 'no-directory))))
	     "file" "file2")
  ;; nonexistent
  (add-test! (symbol-append name "-nonexistent") 'file-system
	     (lambda (fname)
	       (with-cwd (create-temp-dir)
			 (delete-filesys-object fname)
			 (let ((result (func fname)))
			   (or (equal? result 'nonexistent)
			       (and (not result)
				    (eq? func file-not-writable?))))))
	     "file"))

(add-file-not-?-able file-not-readable? 'file-not-readable? #o444)
(add-file-not-?-able file-not-writable? 'file-not-writable? #o222)
(add-file-not-?-able file-not-executable? 'file-not-executable? #o111)


;; --- file-(not)-exists? --

(add-test! 'file-not-exists-1? 'file-system
	   (lambda (fname)
	     (with-cwd (create-temp-dir)
		       (delete-filesys-object fname)
		       (let ((res-1 (file-not-exists? fname)))
			 (create-file fname)
			 (let ((res-2 (file-exists? fname)))
			   (delete-filesys-object fname)
			   (and res-1 res-2)))))
	   "file")

(add-test! 'file-not-exists-2? 'file-system
	   (lambda (fname dirname)
	     (with-cwd (create-temp-dir)
		       (create-directory dirname)
		       (create-file (string-append dirname fname))
		       (set-file-mode dirname 0)
		       (let ((result (file-not-exists? (string-append
							dirname fname))))
			 (set-file-mode dirname #o777)
			 (delete-filesys-object (string-append dirname fname))
			 (delete-filesys-object dirname)
			 (equal? result 'search-denied))))
	   "file" "dir/")

;; --- directory-files ---

(add-test-multiple! 'directory-files 'file-system
		    (lambda (fname dotfiles?)
		      (with-cwd (create-temp-dir)
				(create-file fname)
				(or (and (string-ref fname 0) (not dotfiles?))
				    (member fname (directory-files (cwd) dotfiles?)))))
		    '("file" ".file") '(#t #f))

;; --- create-temp-file ---

(add-test! 'create-temp-file 'file-system
	   (lambda ()
	     (let ((temp-dir (create-temp-dir)))
	       (let ((file-1 (create-temp-file temp-dir))
		     (file-2 (create-temp-file temp-dir)))
		 (let ((result (and (not (equal? file-1 file-2))
				    (file-exists? file-1)
				    (file-exists? file-2))))
		   (delete-filesys-object file-1)
		   (delete-filesys-object file-2)
		   result)))))

