;;; DBM processing code

;;; Copyright (c) 1995 by David Albertz (dalbertz@clark.lcs.mit.edu).
;;; See file COPYING

;;; Usage:	(dbm-open name flags mode . access_method access_info)
;;;                 name          := name of database file (no extension)
;;;                 flags         := file access flags (open/create etc.)
;;;                 mode          := file access modes (privileges)
;;;                 access_method := *if* you have Berkeley dbm, then
;;;        			     you can specify btree, hash, or
;;;        			     recno access methods (0, 1, or 2)
;;;                 access_info   := *if* you have Berkeley dbm, then
;;;        			     you can specify an access information
;;;        			     record, which must correspond to the
;;;        			     correct access method.
;;;             *Note*: If you do *not* have Berkeley dbm, then specifying
;;;                     access_method and/or access_info will generate an
;;;                     error.  If access_method is omitted and you *do*
;;;                     have Berkeley dbm, the default is btree.

;;; Return:	dbm-record which contains the Alien value pointer
;;;                        to the open DBM structure and an open
;;;                        status flag set to #t.


;;; Usage:      (dbm-close db)
;;;                 db := The dbm-record returned by dbm-open

;;; Return:     Return value is undefined


;;; Usage:      (dbm-fetch db key)
;;;                 db  := The dbm-record returnd by dbm-open
;;;                 key := The key value of data to be retrieved

;;; Return:     String containing data associated with key


;;; Usage:      (dbm-insert db key data)
;;;                 db   := The dbm-record returned by dbm-open
;;;                 key  := The key value to be associated with data
;;;                 data := The data to be stored with the key
;;;             Note: Insert will return an error if you try to
;;;                   insert a duplicate key into the database

;;; Return:     Return value is undefined

;;; Usage:      (dbm-replace db key data)
;;;                 db   := The dbm-record returned by dbm-open
;;;                 key  := The key value whose data is to be changed
;;;                 data := The data to be stored with the key
;;;             Note: If you try to replace the data for a non-existent
;;;                   key, dbm-replace will act like dbm-insert

;;; Return:     Return value is undefined


;;; Usage:      (dbm-delete db key)
;;;                 db  := The dbm-record returned by dbm-open
;;;                 key := The key value of data to be deleted

;;; Return:     Integer returned by UNIX dbm_delete routine


;;; Usage:      (dbm-firstkey db)
;;;                 db   := The dbm-record returned by dbm-open

;;; Return:     First key value stored in database hash table.


;;; Usage:      (dbm-nextkey db)
;;;                 db   := The dbm-record returned by dbm-open

;;; Return:     Next key value stored in database hash table.
;;;             Returns the null string when there are no more keys.


;;; If a database error is detected during any read or write operation,
;;; the error number returned by the UNIX dbm_error routine is passed
;;; back as an error condition.

;;; ***NOTE:  All key and data elements must be strings

;;; Scheme48 implementation.

(foreign-source
  "#include <sys/types.h>"
  "#include <limits.h>"
  "#include <ndbm.h>"
  "#include <db.h>"
  ""
  "extern int errno;"
  ""
  "#define errno_or_false(x) (((x) == -1) ? s48_enter_integer(errno) : S48_FALSE)"
  "" "")

;;; This record will hold the pointer the the dbm structure plus
;;; a boolean flag with open status information
(define-record-type :dbm-record
  (make-dbm-record open? dbm)
  dbm-record?
  (open? dbm-record:open? set-dbm-record:open?)
  (dbm dbm-record:dbm))

;;; Use this record to pass btree access method specific data to dbm-open
(define-record-type :btree-info
  (make-btree-info flags cachesize maxkeypage minkeypage psize lorder)
  btree-info?
  (flags btree-info:flags)
  (cachesize btree-info:cachesize)
  (maxkeypage btree-info:maxkeypage)
  (minkeypage btree-info:minkeypage)
  (psize btree-info:psize)
  (lorder btree-info:lorder))

;;; Use this record to pass hash access method specific data to dbm-open
(define-record-type :hash-info
  (make-hash-info bsize ffactor nelem cachesize lorder)
  hash-info?
  (bsize hash-info:bsize)
  (ffactor hash-info:ffactor)
  (nelem hash-info:nelem)
  (cachesize hash-info:cachesize)
  (lorder hash-info:lorder))

;;; Use this record to pass recno access method specific data to dbm-open
(define-record-type :recno-info
  (make-recno-info flags cachesize psize lorder reclen bval bfname)
  recno-info?
  (flags recno-info:flags)
  (cachesize recno-info:cachesize)
  (psize recno-info:psize)
  (lorder recno-info:lorder)
  (reclen recno-info:reclen)
  (bval recno-info:bval)
  (bfname recno-info:bfname))

;;; Internal routine returns true if Berkeley dbm code is available
(define-foreign %db-check (db_check)
  bool)

;;; If you don't specifiy an access method, this is the default
;;; internal routine that will be called.  The only one you can
;;; use if you don't have Berkely dbm.
(define-foreign %dbm-open (db_open_default (string file)
					   (integer flags)
					   (integer mode))
  (to-scheme integer errno_or_false) ; error flag
  (C DB**)) ; DB structure

;;; Internal routine to open btree database
(define-foreign %dbm-open-btree (db_open_btree (string file)
					       (integer flags)
					       (integer mode)
					       (integer pass-info?)
					       (integer access-flags)
					       (integer cachesize)
					       (integer maxkeypage)
					       (integer minkeypage)
					       (integer psize)
					       (integer lorder))
  (to-scheme integer errno_or_false) ; error flag
  (C DB**)) ; DB structure

;;; Internal routine to open hash database
(define-foreign %dbm-open-hash (db_open_hash (string file)
					     (integer flags)
					     (integer mode)
					     (integer pass-info?)
					     (integer bsize)
					     (integer ffactor)
					     (integer nelem)
					     (integer cachesize)
					     (integer lorder))
  (to-scheme integer errno_or_false) ; error flag
  (C DB**)) ; DB structure

;;; Internal routine to open recno database
(define-foreign %dbm-open-recno (db_open_recno (string file)
					       (integer flags)
					       (integer mode)
					       (integer pass-info?)
					       (integer access-flags)
					       (integer cachesize)
					       (integer psize)
					       (integer lorder)
					       (integer reclen)
					       (char bval)
					       (string bfname))
  (to-scheme integer errno_or_false) ; error flag
  (C DB**)) ; DB structure

;;; Convenient names for the access methods - these are exported
(define btree/method 0)
(define hash/method 1)
(define recno/method 2)


;;; Several utility routines to help parse optional parameters
(define (maybe-car lst)
  (if (pair? lst)
      (car lst)
      #f))

(define (maybe-cdr lst)
  (if (pair? lst)
      (cdr lst)
      #f))

(define (maybe-cadr lst)
  (maybe-car (maybe-cdr lst)))

;;; This routine returns to correct internal %dbm-open-foo routine
;;; based on the specified access method.  If Berkeley dbm is not
;;; present on the system it will return an error condition if
;;; any access method is specified.
(define (get-access-method access-parms)
  (let ((Berkeley? (%db-check))
	(access-method (maybe-car access-parms)))
    (if (and (not Berkeley?) access-method)
	(error "You need the Berkeley dbm library - it's free!")
	(cond ((equal? access-method btree/method) %dbm-open-btree)
	      ((equal? access-method hash/method)  %dbm-open-hash)
	      ((equal? access-method recno/method) %dbm-open-recno)
	      ((not access-method)                 %dbm-open)
	      (else (error "Invalid access method specified"))))))

;;; This routine checks for an optional access method specific information
;;; record (btree-info, hash-info, or recno-info).  It returns an error
;;; condition of the record type does not match the access method.
;;; Case 1: no access method or access info record provided
;;;         Return the empty list
;;; Case 2: Access method provided but not the info record
;;;         Return a list with 0 as the first element
;;;                  and the correct number of remaining
;;;                  elements for the specified access method.
;;;                  The values in these elements are arbitrary.
;;; Case 3: Both access method and access info record provided
;;;         Return a list with 1 as the first element and
;;;         the individual fields within the info record as
;;;         the remaining elements in the list.
;;;
;;; The resulting list will be used for application of the %dbm-open-foo
(define (get-access-data access-parms)
  (let ((access-method (maybe-car  access-parms))
	(access-info   (maybe-cadr access-parms)))
    (cond ((btree-info? access-info)
	   (if (eqv? access-method btree/method)
	       (list 1
		     (btree-info:flags      access-info)
		     (btree-info:cachesize  access-info)
		     (btree-info:maxkeypage access-info)
		     (btree-info:minkeypage access-info)
		     (btree-info:psize      access-info)
		     (btree-info:lorder     access-info))
	       (error "Invalid access method for btree information")))
	  ((hash-info? access-info)
	   (if (eqv? access-method hash/method)
	       (list 1
		     (hash-info:bsize     access-info)
		     (hash-info:ffactor   access-info)
		     (hash-info:nelem     access-info)
		     (hash-info:cachesize access-info)
		     (hash-info:lorder    access-info))
	       (error "Invalid access method for hash information")))
	  ((recno-info? access-info)
	   (if (eqv? access-method recno/method)
	       (list 1
		     (recno-info:flags access-info)
		     (recno-info:cachesize access-info)
		     (recno-info:psize     access-info)
		     (recno-info:lorder    access-info)
		     (recno-info:reclen    access-info)
		     (recno-info:bval      access-info)
		     (recno-info:bfname    access-info))
	       (error "Invalid access method for recno information")))
	  ((not access-info)
	   (cond ((eqv? access-method btree/method)
		  (list 0 0 0 0 0 0 0))
		 ((equal? access-method hash/method)
		  (list 0 0 0 0 0 0))
		 ((eqv? access-method recno/method)
		  (list 0 0 0 0 0 0 #\0 ""))
		 ((not access-method)
		  '())
		 (else (error "Invalid access method specified"))))
	  (else (error "Invalid access information specified")))))

;;; The visible version of the dbm-open routine
;;; Returns error or a cons cell with the tag "dbm" in car
;;; and the alien value from %dbm-open-foo in cdr
(define (dbm-open file flags mode . maybe-access)
  (let ((access-method (get-access-method maybe-access))
	(access-data   (append (list file flags mode)
			       (get-access-data   maybe-access))))
    (receive (err dbm) (apply access-method access-data)
	     (if err
		 (errno-error err dbm-open)
		 (make-dbm-record #t dbm)))))

;;; Common utility routine that makes sure dbm is an open database
(define (check-dbm dbm)
  (check-arg dbm-record? dbm "Not a database")
  (check-arg dbm-record:open? dbm "Database not open"))

;;; Common utility routine to check for database errors
;;; result should be the result of applying the routine that might cause
;;; the error, e.g. (dbm-error dbm (%dbm-delete dbm key)) would
;;; give back the result of the delete, or an error if it occurred
(define (dbm-error dbm result)
  (let ((err (%dbm-error (dbm-record:dbm dbm))))
    (if (= err 0)
	result
	(begin
	  (%dbm-clearerr (dbm-record:dbm dbm))
	  (error "Database error" err)))))

;;; Close routines.  Note that the cdr of a dbm cons cell is set to #f
;;; to prevent someone from issuing subsequent calls to that database
;;; without re-opening it.
(define-foreign %dbm-close (dbm_close ((C DBM*) dbm))
  integer);

(define (dbm-close dbm)
  (%dbm-close (dbm-record:dbm (check-dbm dbm)))
  (set-dbm-record:open? dbm #f))

;;; Database error return.  Straight forward implementation of UNIX call
;;; If this returns zero, you can be confident that the previous call
;;; to the database worked correctly.
(define-foreign %dbm-error (dbm_error ((C DBM*) dbm))
  integer)

;;;  Clear database errors.  Straight forward implementation of UNIX call
;;;  Resets database so dbm-error returns zero again.
(define-foreign %dbm-clearerr (dbm_clearerr ((C DBM*) dbm))
  integer)

;;;  Delete key from database if it exists
(define-foreign %dbm-delete (database_delete ((C DBM*) dbm)
					    (string-desc key))
  integer)

(define (dbm-delete dbm key)
  (dbm-error dbm (%dbm-delete (dbm-record:dbm (check-dbm dbm)) key)))

;;; Return the data associated with key if it exists, otherwise
;;; it returns a null string
(define-foreign %dbm-fetch (database_fetch ((C DBM*) dbm)
					  (string-desc key))
  string)

(define (dbm-fetch dbm key)
  (dbm-error dbm (%dbm-fetch (dbm-record:dbm (check-dbm dbm)) key)))

;;; Store a new occurance of the associated <key,data> pair in the database
;;; if flags is zero, otherwise replace old data for key with new data
(define-foreign %dbm-store (database_store ((C DBM*) dbm)
					   (string-desc key)
					   (string-desc data)
					   (integer flags))
  integer)

;;; Insert a new occurance of <key,data> into database
(define (dbm-insert dbm key data)
  (let ((insret (dbm-error dbm
			   (%dbm-store (dbm-record:dbm (check-dbm dbm))
				       key
				       data
				       0))))
    (if (not (= insret 0))
	(error "Attempt to insert duplicate key")
	insret)))

;;; Replace old data for key with new data
(define (dbm-replace dbm key data)
  (dbm-error dbm (%dbm-store (dbm-record:dbm (check-dbm dbm)) key data 1)))


;;; Returns a string containing the key of first record in database
(define-foreign %dbm-firstkey (database_first ((C DBM*) dbm))
  string)

(define (dbm-firstkey dbm)
  (dbm-error dbm (%dbm-firstkey (dbm-record:dbm (check-dbm dbm)))))

;;; Returns a string containing the key of the next sequential
;;; record on the database since the last firstkey or nextkey
;;; operation.  Records are returned in some arbitrary sequence.
(define-foreign %dbm-nextkey (database_next ((C DBM*) dbm))
  string)

(define (dbm-nextkey dbm)
  (dbm-error dbm (%dbm-nextkey (dbm-record:dbm (check-dbm dbm)))))
