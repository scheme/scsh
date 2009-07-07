;;; Very vanilla DBM processing code

;;; Copyright (c) 1995 by David Albertz (dalbertz@clark.lcs.mit.edu).
;;; See file COPYING

;;; This is just a straight translation of the UNIX freebie NDBM code.

;;; Usage:	(db-open name flags mode type info)
;;;                 name  := name of database file (no extension)
;;;                 flags := file access flags (open/create etc.)
;;;                 mode  := file access modes (privileges)
;;;                 type  := DBTYPE as defined in db.h (btree, hash, recno)
;;;                 info  := access method specific structure
;;;                          (NULL gives defaults)

;;; Return:	Alien value pointer to the open DBM structure

;;; Usage:      (dbm-close db)
;;;                 db := The pointer returned by dbm-open

;;; Return:     Always returns 0

;;; Usage:      (dbm-fetch db key)

;;;                 db  := The pointer returned by dbm-open
;;;                 key := The key value of data to be retrieved

;;; Return:     String containing data associated with key

;;; Usage:      (dbm-store db key data flag)
;;;                 db   := The pointer returned by dbm-open
;;;                 key  := The key value to be associated with data
;;;                 data := The data to be stored with the key
;;;                 flag := 0 for insert, 1 for replace

;;; Return:     Integer returned by UNIX dbm_store routine

;;; Usage:      (dbm-delete db key)
;;;                 db  := The pointer returned by dbm-open
;;;                 key := The key value of data to be deleted

;;; Return:     Integer returned by UNIX dbm_delete routine

;;; Usage:      (dbm-firstkey db)
;;;                 db   := The pointer returned by dbm-open

;;; Return:     First key value stored in database hash table.

;;; Usage:      (dbm-nextkey db)
;;;                 db   := The pointer returned by dbm-open

;;; Return:     Next key value stored in database hash table.

;;; Usage:      (dbm-error db)
;;;                 db   := The pointer returned by dbm-open

;;; Return:     Error number returned by UNIX dbm_error routine

;;; Usage:      (dbm-clearerr db)
;;;                 db   := The pointer returned by dbm-open

;;; Return:     Clears any database error condition and returns the 
;;;             integer from the UNIX dbm_clearerr routine

;;; ***NOTE:  All key and data elements must be strings

;;; Scheme48 implementation.

(foreign-source
  "#include <sys/types.h>"
  "#include <limits.h>"
  "#include \"/usr/local/include/db.h\""
  ""
  "extern int errno;"
  ""
  "#define errno_or_false(x) (((x) == -1) ? s48_enter_integer(errno) : S48_FALSE)"
  "" "")

(define-foreign %db-open (db_open (string file)
				  (integer flags)
				  (integer mode)
				  (integer type))
  (to-scheme integer errno_or_false) ; error flag
  (C DB**)) ; DBM structure

(define (db-open file flags mode type)
  (receive (err dbm) (%db-open file flags mode type)
	   (if err
	       (errno-error err db-open)
	       dbm)))
