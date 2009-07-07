;;; ODBC handle types
(define-record-type environment-handle :environment-handle
  (really-make-environment-handle handle)
  environment-handle?
  (handle environment-handle-handle))

(define-exported-binding "environment-handle" :environment-handle)

(define-record-type connection-handle :connection-handle
  (really-make-connection-handle handle environment connected?)
  connection-handle?
  (handle connection-handle-handle)
  (environment connection-handle-environment)
  (connected? connection-handle-connected? set-connection-handle-connected?!))

(define-exported-binding "connection-handle" :connection-handle)

(define-record-type statement-handle :statement-handle
  (really-make-statement-handle handle connection)
  statement-handle?
  (handle statement-handle-handle)
  (connection statement-handle-connection))

(define-exported-binding "statement-handle" :statement-handle)

(define-record-type descriptor-handle :descriptor-handle
  (really-make-descriptor-handle handle)
  descriptor-handle?
  (handle descriptor-handle-handle))

(define-exported-binding "descriptor-handle" :descriptor-handle)

; record type to store infos from SQLGetDiagRec()
(define-record-type odbc-diag :odbc-diag
  (really-make-odbc-diag-rec sql-state native-error message)
  odbc-diag?
  (sql-state odbc-diag-sql-state)
  (native-error odbc-diag-native-error)
  (message odbc-diag-message))

(define-exported-binding "odbc-diag" :odbc-diag)

(define (odbc-handle? thing)
  (or (environment-handle? thing)
      (connection-handle? thing)
      (statement-handle? thing)
      (descriptor-handle? thing)))

(define (odbc-handle handle)
  (cond 
   ((environment-handle? handle) (environment-handle-handle handle))
   ((connection-handle? handle) (connection-handle-handle handle))
   ((statement-handle? handle) (statement-handle-handle handle))
   ((descriptor-handle? handle) (descriptor-handle-handle handle))
   (else
    (error "Expected odbc-handle, got " handle odbc-handle))))

;;; map a record to a handle type identifier (see sql.h)
(define (handle-record-type->c-handle-identifier record)
  (cond ((environment-handle? record) 1) ; SQL_HANDLE_ENV
	((connection-handle? record) 2) ; SQL_HANDLE_DBC 
	((statement-handle? record) 3) ; SQL_HANDLE_STMT
	(else
	 (error "Unknown handle type: " record))))

;;; conditions
(define-condition-type 'odbc-error '(error))

(define odbc-error? 
  (condition-predicate 'odbc-error))

(define-condition-type 'odbc-api-version-mismatch '(odbc-error))

(define odbc-api-version-mismatch?
  (condition-predicate 'odbc-api-version-mismatch))

(define (raise-odbc-api-version-mismatch-error function-name 
					       api-version
					       api-version-needed)
  (apply signal 'odbc-api-version-mismatch
	 'function-name function-name 
	 'odbc-driver-manager-api-version api-version 
	 'min-api-version-required api-version-needed))

(define-exported-binding 
  "raise-odbc-api-version-mismatch-error"
  raise-odbc-api-version-mismatch-error)

;;;
(define-condition-type 'odbc-unknown-integer-type '(odbc-error))

(define odbc-unknown-integer-type?
  (condition-predicate 'odbc-unknown-integer-type))

(define (raise-odbc-unknown-integer-type-error function-name type-id)
  (apply signal 'odbc-unknown-integer-type 
	 'function-name function-name 'type-id type-id))

(define-exported-binding
  "raise-odbc-unknown-integer-type-error" raise-odbc-unknown-integer-type-error)

;;;
(define-condition-type 'odbc-buffer-alloc-error '(odbc-error))

(define odbc-buffer-alloc-error?
  (condition-predicate 'odbc-buffer-alloc-error))

(define (raise-odbc-buffer-alloc-error buffer-length)
  (apply signal 'odbc-buffer-alloc-error 'buffer-length buffer-length))

(define-exported-binding
  "raise-odbc-buffer-alloc-error" raise-odbc-buffer-alloc-error)

;;;
(define-condition-type 'odbc-unknown-c-type-identifier-error '(odbc-error))

(define odbc-unknown-c-type-identifier-error?
  (condition-predicate 'odbc-unknown-c-type-identifier-error))

(define (raise-odbc-unknown-c-type-identifier-error buffer ctypeid)
  (apply signal 'odbc-unknown-c-type-identifier-error 
	 'buffer buffer 'ctypeid ctypeid))

(define-exported-binding
  "raise-odbc-unknown-c-type-identifier-error" 
  raise-odbc-unknown-c-type-identifier-error)

;;;
(define-condition-type 'odbc-bindcol-unbound-column '(odbc-error))

(define odbc-bindcol-unbound-column-error?
  (condition-predicate 'odbc-bindcol-unbound-column))

(define (raise-odbc-bindcol-unbound-column-error stmt-handle column-no)
  (apply signal 'odbc-bindcol-unbound-column
	 'statement-handle stmt-handle 'column-no column-no))

(define-exported-binding "raise-odbc-bindcol-unbound-column-error" 
  raise-odbc-bindcol-unbound-column-error)

;;;
(define-condition-type 'odbc-bindcol-rebinding-error '(odbc-error))

(define odbc-bindcol-rebinding-error?
  (condition-predicate 'odbc-bindcol-rebinding-error))

(define (raise-odbc-bindcol-rebinding-error text-msg)
  (apply signal 'odbc-bindcol-rebinding-error text-msg))

(define-exported-binding "raise-odbc-bindcol-rebinding-error"
  raise-odbc-bindcol-rebinding-error)

;;;
(define-record-type odbc-column :odbc-column
  (really-make-odbc-column name type size digits nullable?)
  odbc-column?
  (name odbc-column-name)
  (type odbc-column-type)
  (size odbc-column-size)
  (digits odbc-column-digits)
  (nullable? odbc-column-nullable?))

(define-exported-binding "odbc-column" :odbc-column)

(define-record-type odbc-parameter :odbc-parameter
  (really-make-odbc-parameter type size digits nullable)
  odbc-parameter?
  (type odbc-parameter-type)
  (size odbc-parameter-size)
  (digits odbc-parameter-digits)
  (nullable odbc-parameter-nullable))

(define-exported-binding "odbc-parameter" :odbc-parameter)

;;; options for SQLFreeStmt from sql.h
(define sql-disconnect-opt-close 0)
(define sql-disconnect-opt-drop 1)
(define sql-disconnect-opt-unbind 2)
(define sql-disconnect-opt-reset-params 3)

;;; options for SQLDataSource from sql.h
(define sql-datasources-fetch-next  1)
(define sql-datasources-fetch-first 2)

;;; C type identifier
(define sql-type-c-char 1)         ; SQL_C_CHAR
(define sql-type-c-long 4)         ; SQL_C_LONG
(define sql-type-c-short 5)        ; SQL_C_SHORT
(define sql-type-c-float 7)        ; SQL_C_FLOAT
(define sql-type-c-double 8)       ; SQL_C_DOUBLE
(define sql-type-c-numeric 2)      ; SQL_C_NUMERIC
(define sql-type-c-default 99)     ; SQL_C_DEFAULT  (not mentioned in Appendix D?!?!?)
(define sql-type-c-date 9)         ; SQL_C_DATE
(define sql-type-c-time 10)        ; SQL_C_TIME
(define sql-type-c-timestamp 11)   ; SQL_C_TIMESTAMP
(define sql-type-c-binary -2)      ; SQL_C_BINARY
(define sql-type-c-bit -7)         ; SQL_C_BIT
; missing: all the SQL_C_INTERVAL* stuff

;;; ODBC type identifier
(define sql-type-unknown 0)     ; SQL_UNKNOWN_TYPE
(define sql-type-char 1)        ; SQL_CHAR
(define sql-type-numeric 2)     ; SQL_NUMERIC
(define sql-type-decimal 3)     ; SQL_DECIMAL
(define sql-type-integer 4)     ; SQL_INTEGER
(define sql-type-smallint 5)    ; SQL_SMALLINT
(define sql-type-float 6)       ; SQL_FLOAT
(define sql-type-real 7)        ; SQL_REAL
(define sql-type-double 8)      ; SQL_DOUBLE
(define sql-type-datetime 9)    ; SQL_DATETIME
(define sql-type-varchar 12)    ; SQL_VARCHAR
(define sql-type-date 91)       ; SQL_TYPE_DATE
(define sql-type-time 92)       ; SQL_TYPE_TIME
(define sql-type-timestamp 93)  ; SQL_TYPE_TIMESTAMP

(define (c-type-identifier->odbc-type-identifier c-type)
  (error "Not yet implemented"))

(define (odbc-type-identifier->c-type-identifier odbc-type)
  (cond ((member odbc-type '(sql-type-char sql-type-varchar))
	 sql-type-c-char)
	((member odbc-type '(sql-type-real sql-type-float sql-type-double))
	 sql-type-c-double)
	((equal? odbc-type sql-type-numeric)
	 sql-type-c-numeric)
	((equal? odbc-type sql-type-decimal)
	 (error "Can't handle type SQL_DECIMAL yet"))
	((equal? odbc-type sql-type-integer)
	 sql-type-c-long)
; 	((equal? odbc-type sql-type-smallint)
; 	 sql-type-c-smallint)
	((equal? odbc-type sql-type-datetime)
	 (error "Can't handle type SQL_DATETIME yet"))
	((equal? odbc-type sql-type-date)
	 (error "Can't handle type SQL_TYPE_DATE yet"))
	((equal? odbc-type sql-type-time)
	 (error "Can't handle type SQL_TYPE_TIME yet"))
	((equal? odbc-type sql-type-timestamp)
	 (error "Can't handle type SQL_TYPE_TIMESTAMP yet"))
	((equal? odbc-type sql-type-unknown)
	 (error "Can't handle type SQL_UNKNOWN_TYPE"))
	(else
	 (error "unknown SQL type"))))
	
;;; ODBC function ids for SQLGetFunctions
(define sql-api-sqlallocconnect 1)
(define sql-api-sqlallocenv 2)
(define sql-api-sqlallochandle 1001)
(define sql-api-sqlallocstmt 3)
(define sql-api-sqlbindcol 4)
(define sql-api-sqlbinparam 1002)
(define sql-api-sqlcancel 5)
(define sql-api-sqlclosecursor 1003)
(define sql-api-sqlcolattribute 6)
(define sql-api-sqlcolumns 40)
(define sql-api-sqlconnect 7)
(define sql-api-sqlcopydesc 1004)
(define sql-api-sqldatasources 57)
(define sql-api-sqldescribecol 8)
(define sql-api-sqldisconnect 9)
(define sql-api-sqlendtran 1005)
(define sql-api-sqlerror 10)
(define sql-api-sqlexecdirect 11)
(define sql-api-sqlexecute 12)
(define sql-api-sqlfetch 13)
(define sql-api-sqlfetchscroll 1021)
(define sql-api-sqlfreeconnect 14)
(define sql-api-sqlfreeenv 15)
(define sql-api-sqlfreehandle 1006)
(define sql-api-sqlfreestmt 16)
(define sql-api-sqlgetconnectattr 1007)
(define sql-api-sqlgetconenctoption 42)
(define sql-api-sqlgetcursorname 17)
(define sql-api-sqlgetdata 43)
(define sql-api-sqlgetdescfield 1008)
(define sql-api-sqlgetdescrec 1009)
(define sql-api-sqlgetdiagfield 1010)
(define sql-api-sqlgetdiagrec 1011)
(define sql-api-sqlgetenvattr 1012)
(define sql-api-sqlgetfunctions 44)
(define sql-api-sqlgetinfo 45)
(define sql-api-sqlgetstmtattr 1014)
(define sql-api-sqlgetstmtoption 46)
(define sql-api-sqlgettypeinfo 47)
(define sql-api-sqlnumresultcols 18)
(define sql-api-sqlparamdata 48)
(define sql-api-sqlprepare 19)
(define sql-api-sqlputdata 49)
(define sql-api-sqlrowcount 20)
(define sql-api-sqlsetconnectattr 1016)
(define sql-api-sqlsetconnectoption 50)
(define sql-api-sqlsetcursorname 21)
(define sql-api-sqlsetdescfield 1017)
(define sql-api-sqlsetdescrec 1018)
(define sql-api-sqlsetenvattr 1019)
(define sql-api-sqlsetparam 22)
(define sql-api-sqlsetstmtattr 1020)
(define sql-api-sqlsetstmtoption 51)
(define sql-api-sqlspecialcolumns 52)
(define sql-api-sqlstatistics 53)
(define sql-api-sqltables 54)
(define sql-api-transact 23)

;;; additional function identifiers
(define sql-api-sqlallochandlestd 73)
(define sql-api-sqlbulkoperations 24)
(define sql-api-sqlbindparameter 72)
(define sql-api-sqlbrowseconnect 55)
(define sql-api-sqlcolattributes 6)
(define sql-api-sqlcolumnprivileges 56)
(define sql-api-sqldescribeparam 58)
(define sql-api-sqldriverconnect 41)
(define sql-api-sqldrivers 71)
(define sql-api-sqlextendedfetch 59)
(define sql-api-sqlforeignkeys 60)
(define sql-api-sqlmoreresults 61)
(define sql-api-sqlnativesql 62)
(define sql-api-sqlnumparams 63)
(define sql-api-sqlparamoptions 64)
(define sql-api-sqlprimarykeys 65)
(define sql-api-sqlprocedurecolumns 66)
(define sql-api-sqlprcoedures 67)
(define sql-api-sqlsetpos 68)
(define sql-api-sqlsetscrolloptions 69)
(define sql-api-sqltableprivileges 70)

;;; info keys for odbc-sql-get-info-arg-int/string
; ODBC 1.0, returns integer
(define sql-get-info-arg-maxdriverconnections 0)
; ODBC 1.0, returns integer 
(define sql-get-info-arg-maxconcurrentactivities 1)
; ODBC 1.0, returns string
(define sql-get-info-arg-datasourcename 2)
; deprecated in ODBC 3.x returns ?
(define sql-get-info-arg-fetchdirection 8) 
; ODBC 1.0, returns string
(define sql-get-info-arg-servername 13)
; ODBC 1.0, returns string
(define sql-get-info-arg-searchpatternescape 14)
; ODBC 1.0, returns string
(define sql-get-info-arg-dbmsname 17)
; ODBC 1.0, returns string
(define sql-get-info-arg-dbmsver 18)
; ODBC 1.0, returns string
(define sql-get-info-arg-accessibletables 19)
; ODBC 1.0, returns string
(define sql-get-info-arg-accessibaleprocedures 20)
; ODBC 1.0, returns integer
(define sql-get-info-arg-cursor-commit-behaviour 23)
; ODBC 1.0, returns string
(define sql-get-info-arg-datasourcereadonly 25)
; ODBC 1.0, returns integer
(define sql-get-info-arg-defaulttxnisolation 26)
; ODBC 1.0, returns integer
(define sql-get-info-arg-identifiercase 28)
; ODBC 1.0, returns string
(define sql-get-info-arg-identifierquotechar 29)
; ODBC 1.0 returns integer
(define sql-get-info-arg-maxcolumnnamelen 30)
(define sql-get-info-arg-maximumcolumnnamelen 30)
; ODBC 1.0 returns integer
(define sql-get-info-arg-maxcursornamelen 31)
(define sql-get-info-arg-maximumcursornamelen 31)
; ODBC 1.0 returns integer 
(define sql-get-info-arg-maxschemanamelen 32)
(define sql-get-info-arg-maximumschemenamelen 32)
; ODBC 1.0 returns integer
(define sql-get-info-arg-maxcatalognamelen 34)
(define sql-get-info-arg-maximumcatalognamelen 34)
; ODBC 1.0 returns integer
(define sql-get-info-arg-maxtablenamelen 35)
; defined in sql.h, but no reference in ODBC manual
; (define sql-get-info-arg--scrollconcurrency 43)
; ODBC 1.0 returns integer
(define sql-get-info-arg-txncapable 46)
(define sql-get-info-arg-transaction-capable 46)
; ODBC 1.0 returns string
(define sql-get-info-arg-username 47)
; ODBC 1.0 returns integer
(define sql-get-info-arg-txnisolationoption 72)
(define sql-get-info-arg-transcationisolationoption 72)
; ODBC 1.0 returns string
(define sql-get-info-arg-integrity 73)
; ODBC 2.0 returns integer
(define sql-get-info-arg-getdataextensions 81)
; ODBC 2.0 returns integer
(define sql-get-info-arg-nullcollation 85)
; ODBC 2.0 returns integer
(define sql-get-info-arg-altertable 86)
; ODBC 2.0 returns string
(define sql-get-info-arg-specialcharacters 94)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxcolumnsingroupby 97)
(define sql-get-info-arg-maximumcolumnsingroupby 97)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxcolumnsinindex 98)
(define sql-get-info-arg-maximumcolumnsinindex 98)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxcolumnsinorderby 99)
(define sql-get-info-arg-maximumcolumnsinorderby 99)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxcolumnsinselect 100)
(define sql-get-info-arg-maximumcolumnsinselect 100)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxcolumnsintable 101)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxindexsize 102)
(define sql-get-info-arg-maximumindexsize 102)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxrowsize 104)
(define sql-get-info-arg-maximumrowsize 104)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxstatementlen 105)
(define sql-get-info-arg-maximumstatemenlen 105)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxtablesinselect 106)
(define sql-get-info-arg-maximumtablesinselect 106)
; ODBC 2.0 returns integer
(define sql-get-info-arg-maxusernamelen 107)
(define sql-get-info-arg-maximumusernamelen 107)
; ODBC 2.01 returns integer
(define sql-get-info-arg-ojcapabilities 115)
(define sql-get-info-arg-outerjoincapabilities 115)

; ODBC return values for SQLGetInfo sql-get-info-arg-identifiercase (SQL_IDENTIFIER_CASE)
(define sql-ic-upper 1)
(define sql-ic-lower 2)
(define sql-ic-sensitive 3)
(define sql-ic-mixed 4)

; ODBC return values for SQLGetInfo sql-get-info-arg-defaulttxnisolation (SQL_DEFAULT_TXN_ISOLATION)
(define sql-txn-read-uncommited #x00000001)
(define sql-txn-read-committed  #x00000002)
(define sql-txn-repeatable-read #x00000004)
(define sql-txn-serializable #x00000008)

; ODBC return values for SQLGetInfo sql-get-info-arg-txncapable (SQL_TXN_CAPABLE)
(define sql-tc-none 0)
(define sql-tc-dml 1)
(define sql-tc-all 2)
(define sql-tc-ddl-commit 3)
(define sql-tc-ddl-ignore 4)

; ODBC return values for SQLGetInfo sql-get-info-arg-getdataextensions (SQL_GETDATA_EXTENSIONS)
(define sql-gd-any-column  #x00000001)
(define sql-gd-any-order #x00000002)
(define sql-gd-block #x00000004)
(define sql-gd-bound #x00000008)

; ODBC return values for SQLGetInfo sql-get-info-arg-nullcollation (SQL_NULL_COLLATION)
(define sql-nc-end #x0004)
(define sql-nc-start #x0002)
; missing sql-nc-high sql-nc-low

; ODBC return values for SQLGetInfo sql-get-info-arg-altertable (SQL_ALTER_TABLE)
(define sql-at-add-colum #x00000001)
(define sql-at-drop-column #x00000002)
(define sql-at-add-constraint #x00000008)
(define sql-at-add-column-single #x00000020)
(define sql-at-add-column-default #x00000040)
(define sql-at-add-column-collation #x00000080)
(define sql-at-set-column-default #x00000100)
(define sql-at-drop-column-default #x00000200)
(define sql-at-drop-column-cascade #x00000400)
(define sql-at-drop-column-restrict #x00000800)
(define sql-at-drop-table-constraint #x00001000)
(define sql-at-drop-table-constraint-cascade #x00002000)
(define sql-at-drop-table-constraint-restrict #x00004000)
(define sql-at-constraint-name-definition #x00008000)
(define sql-at-constraint-initially-deferred #x00010000)
(define sql-at-constraint-initially-immediate #x00020000)
(define sql-at-constraint-deferrable #x00040000)
(define sql-at-constraint-non-deferrable #x00080000)

; ODBC return values for SQLGetInfo sql-get-info-arg-ojcapabilities (SQL_OJ_CAPABILITIES)
(define sql-oj-left #x00000001)
(define sql-oj-right #x00000002)
(define sql-oj-full #x00000004)
(define sql-oj-nested #x00000008)
(define sql-oj-not-ordered #x00000010)
(define sql-oj-inner #x00000020)
(define sql-oj-all-comparison-ops #x00000040)

; ODBC data types for use with odbc_sql_get_type_info
(define sql-datatype-unknown 0)
(define sql-datatype-char 1)
(define sql-datatype-numeric 2)
(define sql-datatype-decimal 3)
(define sql-datatype-integer 4)
(define sql-datatype-smallint 5)
(define sql-datatype-float 6)
(define sql-datatype-real 7)
(define sql-datatype-double 8)
(define sql-datatype-datetime 9)
(define sql-datatype-interval 10)
(define sql-datatype-varchar 12)
(define sql-datatype-date 91)
(define sql-datatype-time 92)
(define sql-datatype-timestamp 93)

; possible identifier-type arguments for SQLSpecialColumns() (odbc-sql-special-columns)
(define sql-best-rowid 1)
(define sql-rowver 2)

; possible scope arguments for SQLSpecialColumns() (odbc-sql-special-columns)
(define sql-scope-currow 0)
(define sql-scope-transaction 1)
(define sql-scope-session 2)

; possible nullable arguments for SQLSpecialColumns() (odbc-sql-special-columns)
(define sql-no-nulls 0)
(define sql-nullable 1)

; possible unique arguments for SQLStatistics() (odbc-sql-statistics)
(define sql-index-unique 0)
(define sql-index-all 1)

; possible reserved arguments for SQLStatistics() (odbc-sql-statistics)
(define sql-ensure 1)
(define sql-quick 0)

; possible operation arguments for SQLSetPos() (odbc-sql-set-pos)
(define sql-position 0)
(define sql-refresh 1)
(define sql-update 2)
(define sql-delete 3)

; possible lock-type arguments for SQLSetPos() (odbc-sql-set-pos)
(define sql-lock-no-change 0)
(define sql-lock-exclusive 1)
(define sql-lock-unlock 2)

; possible operation arguments for SQLBulkOperations() (odbc-sql-bulk-operations)
(define sql-add 4)
(define sql-update-by-bookmark 5)
(define sql-delete-by-bookmark 6)
(define sql-fetch-by-bookmark 7)

;;; ODBC return values
(define sql-error -1)
(define sql-success 0)
(define sql-success-with-info 1)
(define sql-no-data 100)
(define sql-invalid-handle -2)
(define sql-need-data 99)

;;; PART 1

(define (odbc-alloc-environment-handle)
  (let* ((status.value (odbc-alloc-environment-handle-internal))
	 (status (car status.value))
	 (value (cadr status.value)))
    (if (odbc-call-successful? status)
	(let ((env-handle (really-make-environment-handle value)))
	  (add-finalizer! env-handle odbc-sql-free-handle)
	  (values status env-handle))
	(values status value))))

(import-lambda-definition odbc-alloc-environment-handle-internal
			  ()
			  "odbc_alloc_environment_handle")

(define (odbc-alloc-connection-handle env-handle)
  (let* ((status.value (odbc-alloc-connection-handle-internal 
			(environment-handle-handle env-handle)))
	 (status (car status.value))
	 (value (cadr status.value)))
    (if (odbc-call-successful? status)
	(let ((conn-handle (really-make-connection-handle value env-handle #f)))
	  (add-finalizer! conn-handle free-connection-handle)
	  (values status conn-handle))
	(values status value))))

(import-lambda-definition odbc-alloc-connection-handle-internal
			  (env-handle)
			  "odbc_alloc_connection_handle")

;;; maybe we should raise a warning like "implicit connect" here?
(define (free-connection-handle conn-handle)
  (if (connection-handle-connected? conn-handle)
      (odbc-sql-disconnect conn-handle))
  (odbc-sql-free-handle conn-handle))

(define (odbc-alloc-statement-handle conn-handle)
  (let* ((status.value (odbc-alloc-statement-handle-internal
			(connection-handle-handle conn-handle)))
	 (status (car status.value))
	 (value (cadr status.value)))
    (if (odbc-call-successful? status)
	(let ((stmt-handle (really-make-statement-handle value conn-handle)))
	  (add-finalizer! stmt-handle free-statement-handle)
	  (values status stmt-handle))
	(values status value))))

(define (free-statement-handle stmt-handle)
  (bindcol-finalize-bindcols (statement-handle-handle stmt-handle))
  (odbc-sql-free-handle stmt-handle))

(import-lambda-definition odbc-alloc-statement-handle-internal
			  (db-handle)
			  "odbc_alloc_statement_handle")

(import-lambda-definition bindcol-finalize-bindcols
			  (stmt-handle)
			  "bindcol_finalize_bindcols")

;;; returns odbc-return-value
(define (odbc-sql-connect conn-handle server-name user-name auth)
  (let ((return-value (odbc-sql-connect-internal 
		       (connection-handle-handle conn-handle)
		       server-name user-name auth)))
    (if (odbc-call-successful? return-value)
	  (set-connection-handle-connected?! conn-handle #t))
    return-value))

(import-lambda-definition odbc-sql-connect-internal
			  (conn-handle server-name user-name auth)
			  "odbc_sql_connect")

(define (odbc-sql-browse-connect conn-handle connection-string)
  (apply values
	 (odbc-sql-browse-connect-internal 
	  (connection-handle-handle conn-handle) connection-string)))
	 
(import-lambda-definition odbc-sql-browse-connect-internal
			  (conn-handle connection-string)
			  "odbc_sql_browse_connect")

;;; PART 2

(define (odbc-sql-data-sources env-handle)
  (apply values
	 (odbc-sql-data-sources-internal 
	  (environment-handle-handle env-handle))))
  
(import-lambda-definition odbc-sql-data-sources-internal
			  (env-handle)
			  "odbc_sql_data_sources")

(define (odbc-sql-drivers env-handle)
  (apply values
	 (odbc-sql-drivers-internal 
	  (environment-handle-handle env-handle))))

(import-lambda-definition odbc-sql-drivers-internal
			  (env-handle)
			  "odbc_sql_drivers")

(define (odbc-sql-get-info-int conn-handle info-key)
  (apply values
	 (odbc-sql-get-info-int-internal 
	  (connection-handle-handle conn-handle) info-key)))

(import-lambda-definition odbc-sql-get-info-int-internal 
			  (conn-handle info-key)
			  "odbc_sql_get_info_int")

(define (odbc-sql-get-info-string conn-handle info-key)
  (apply values
	 (odbc-sql-get-info-string-internal 
	  (connection-handle-handle conn-handle) info-key)))

(import-lambda-definition odbc-sql-get-info-string-internal
			  (conn-handle info-key)
			  "odbc_sql_get_info_string")

(define (odbc-sql-get-func conn-handle fun-id)
  (apply values
	 (odbc-sql-get-func-exists-internal 
	  (connection-handle-handle conn-handle) fun-id)))

(import-lambda-definition odbc-sql-get-func-exists-internal
			  (conn-handle fun-id)
			  "odbc_sql_get_func_exists")

(define (odbc-sql-get-type-info stmt-handle data-type)
  (odbc-sql-get-type-info-internal 
   (statement-handle-handle stmt-handle) data-type))

(import-lambda-definition odbc-sql-get-type-info-internal
			  (stmt-handle data-type)
			  "odbc_sql_get_type_info")

;;; PART 3

(define (odbc-sql-set-connect-attr-int conn-handle attribute value)
  (odbc-sql-set-connect-attr-int-internal 
   (connection-handle-handle conn-handle) attribute value))

(import-lambda-definition odbc-sql-set-connect-attr-int-internal
			  (conn-handle attribute value)
			  "odbc_sql_set_connect_attr_int")

(define (odbc-sql-set-connect-attr-string conn-handle attribute value)
  (odbc-sql-set-connect-attr-string-internal 
   (connection-handle-handle conn-handle) attribute value))

(import-lambda-definition odbc-sql-set-connect-attr-string-internal
			  (conn-handle attribute value)
			  "odbc_sql_set_connect_attr_string")

(define (odbc-sql-get-connect-attr-string conn-handle attribute)
  (apply values
	 (odbc-sql-get-connect-attr-string-internal 
	  (connection-handle-handle conn-handle) attribute)))
  
(import-lambda-definition odbc-sql-get-connect-attr-string-internal
			  (conn-handle attribute)
			  "odbc_sql_get_connect_attr_string")

(define (odbc-sql-get-connect-attr-int conn-handle attribute)
  (apply values
	 (odbc-sql-get-connect-attr-int-internal 
	  (connection-handle-handle conn-handle) attribute)))

(import-lambda-definition odbc-sql-get-connect-attr-int-internal
			  (conn-handle attribute)
			  "odbc_sql_get_connect_attr_int")

(define (odbc-sql-set-env-attr-int env-handle attribute value)
  (odbc-sql-set-env-attr-int-internal 
   (environment-handle-handle env-handle) attribute value))

(import-lambda-definition odbc-sql-set-env-attr-int-internal
			  (env-handle attribute value)
			  "odbc_sql_set_env_attr_int")

(define (odbc-sql-get-env-attr-int env-handle attribute value)
  (apply values
	 (odbc-sql-get-env-attr-int-internal 
	  (environment-handle-handle env-handle) attribute value)))

(import-lambda-definition odbc-sql-get-env-attr-int-internal
			  (env-handle attribute value)
			  "odbc_sql_get_env_attr_int")

(define (odbc-sql-set-stmt-attr-int stmt-handle attribute value)
  (odbc-sql-set-stmt-attr-int-internal 
   (statement-handle-handle stmt-handle) attribute value))

(import-lambda-definition odbc-sql-set-stmt-attr-int-internal
			  (stmt-handle attribute value)
			  "odbc_sql_set_stmt_attr_int")

(define (odbc-sql-set-stmt-attr-string stmt-handle attribute value)
  (odbc-sql-set-stmt-attr-string-internal 
   (statement-handle-handle stmt-handle) attribute value))

(import-lambda-definition odbc-sql-set-stmt-attr-string-internal
			  (stmt-handle attribute value)
			  "odbc_sql_set_stmt_attr_string")

(define (odbc-sql-get-stmt-attr-int stmt-handle attribute)
  (apply values
	 (odbc-sql-get-stmt-attr-int-internal 
	  (statement-handle-handle stmt-handle) attribute)))

(import-lambda-definition odbc-sql-get-stmt-attr-int-internal
			  (stmt-handle attribute)
			  "odbc_sql_get_stmt_attr_int")

(define (odbc-sql-get-stmt-attr-string stmt-handle attribute)
  (apply values
	 (odbc-sql-get-stmt-attr-string-internal 
	  (statement-handle-handle stmt-handle) attribute)))

(import-lambda-definition odbc-sql-get-stmt-attr-string-internal
			  (stmt-handle attribute)
			  "odbc_sql_get_stmt_attr_string")

;;; PART 4

(define (odbc-sql-get-desc-field-int desc-handle record-number field-id)
  (apply values
	 (odbc-sql-get-desc-field-int-internal 
	  (descriptor-handle-handle desc-handle) record-number field-id)))

(import-lambda-definition odbc-sql-get-desc-field-int-internal
			  (desc-handle record-number field-id)
			  "odbc_sql_get_desc_field_int")

(define (odbc-sql-get-desc-field-string desc-handle record-number field-id)
  (apply values
	 (odbc-sql-get-desc-field-string-internal 
	  (descriptor-handle-handle desc-handle) record-number field-id)))

(import-lambda-definition odbc-sql-get-desc-field-string-internal
			  (desc-handle record-number field-id)
			  "odbc_sql_get_desc_field_string")

;;; PART 5

(define (odbc-sql-prepare stmt-handle stmt-txt)
  (odbc-sql-prepare-internal 
   (statement-handle-handle stmt-handle) stmt-txt))

(import-lambda-definition odbc-sql-prepare-internal
			  (stmt-handle stmt-txt)
			  "odbc_sql_prepare")

(define (odbc-sql-get-cursor-name stmt-handle)
  (apply values 
	 (odbc-sql-get-cursor-name-internal 
	  (statement-handle-handle stmt-handle))))

(import-lambda-definition odbc-sql-get-cursor-name-internal
			  (stmt-handle)
			  "odbc_sql_get_cursor_name")

(define (odbc-sql-set-cursor-name stmt-handle cursor-name)
  (odbc-sql-set-cursor-name-internal 
   (statement-handle-handle stmt-handle) cursor-name))

(import-lambda-definition odbc-sql-set-cursor-name-internal
			  (stmt-handle cursor-name)
			  "odbc_sql_set_cursor_name")

;;; PART 6

(define (odbc-sql-execute stmt-handle)
  (odbc-sql-execute-internal (statement-handle-handle stmt-handle)))

(import-lambda-definition odbc-sql-execute-internal
			  (stmt-handle)
			  "odbc_sql_execute")

(define (odbc-sql-execute-direct stmt-handle stmt-txt)
  (odbc-sql-execute-direct-internal 
   (statement-handle-handle stmt-handle) stmt-txt))

(import-lambda-definition odbc-sql-execute-direct-internal
			  (stmt-handle stmt-txt)
			  "odbc_sql_execute_direct")

(define (odbc-sql-native-sql conn-handle stmt-txt)
  (apply values
	 (odbc-sql-native-sql-internal 
	  (connection-handle-handle conn-handle) stmt-txt)))

(import-lambda-definition odbc-sql-native-sql-internal
			  (conn-handle stmt-txt)
			  "odbc_sql_native_sql")

(define (odbc-sql-describe-param stmt-handle parameter-no)
  (apply values
	 (odbc-sql-describe-param-internal
	  (statement-handle-handle stmt-handle) parameter-no)))

(import-lambda-definition odbc-sql-describe-param-internal
			  (stmt-handle parameter-no)
			  "odbc_sql_describe_param")

(define (odbc-sql-num-params stmt-handle)
  (apply values
	 (odbc-sql-num-params-internal 
	  (statement-handle-handle stmt-handle))))

(import-lambda-definition odbc-sql-num-params-internal
			  (stmt-handle)
			  "odbc_sql_num_params")

;;; PART 7

(define (odbc-sql-row-count stmt-handle)
  (apply values
	 (odbc-sql-row-count-internal 
	  (statement-handle-handle stmt-handle))))

(import-lambda-definition odbc-sql-row-count-internal
			  (stmt-handle)
			  "odbc_sql_row_count")

(define (odbc-sql-get-data stmt-handle column-number target-type)
  (apply values
	 (odbc-sql-get-data-internal (statement-handle-handle stmt-handle)
				     column-number target-type)))
			      
(import-lambda-definition odbc-sql-get-data-internal
			  (stmt-handle column-number target-type)
			  "odbc_sql_get_data")

(define (odbc-sql-set-pos stmt-handle row-number operation lock-type)
  (odbc-sql-set-pos-internal 
   (statement-handle-handle stmt-handle) row-number operation lock-type))

(import-lambda-definition odbc-sql-set-pos-internal
			  (stmt-handle row-number operation lock-type)
			  "odbc_sql_set_pos")

(define (odbc-sql-bulk-operations stmt-handle operation)
  (odbc-sql-bulk-operations-internal 
   (statement-handle-handle stmt-handle) operation))

(import-lambda-definition odbc-sql-bulk-operations-internal
			  (stmt-handle operation)
			  "odbc_sql_bulk_operations")

(define (odbc-sql-more-results stmt-handle)
  (odbc-sql-more-results-internal 
   (statement-handle-handle stmt-handle)))

(import-lambda-definition odbc-sql-more-results-internal
			  (stmt-handle)
			  "odbc_sql_more_results")

(define (odbc-sql-fetch stmt-handle)
  (odbc-sql-fetch-internal 
   (statement-handle-handle stmt-handle)))

(import-lambda-definition odbc-sql-fetch-internal
			  (stmt-handle)
			  "odbc_sql_fetch")

(define (odbc-sql-bindcol stmt-handle column-no target-type buffer-len)
  (let ((handle (statement-handle-handle stmt-handle)))
    (odbc-sql-bindcol-internal handle column-no target-type buffer-len)
    (lambda ()
      (bindcol-lookup-binding-scheme handle column-no))))

(import-lambda-definition odbc-sql-bindcol-internal
			  (stmt-handle column-no target-type buffer-len)
			  "odbc_sql_bindcol")

(import-lambda-definition bindcol-lookup-binding-scheme
			  (stmt-handle column-no)
			  "bindcol_lookup_binding_scheme")

;;; PART 8

(define (odbc-sql-column-privileges stmt-handle catalog-name schema-name
				    table-name column-name)
  (check-arg statement-handle? stmt-handle odbc-sql-column-privileges)
  (odbc-sql-column-privileges-internal (statement-handle-handle stmt-handle)
				       catalog-name schema-name
				       table-name column-name))

(import-lambda-definition odbc-sql-column-privileges-internal
			  (stmt-handle catalog-name schema-name table-name column-name)
			  "odbc_sql_column_privileges")

(define (odbc-sql-columns stmt-handle catalog-name schema-name
			  table-name column-name)
  (check-arg statement-handle? stmt-handle odbc-sql-columns)
  (odbc-sql-columns-internal (statement-handle-handle stmt-handle)
			     catalog-name schema-name table-name column-name))

(import-lambda-definition odbc-sql-columns-internal 
			  (stmt-handle catalog-name schema-name table-name column-name)
			  "odbc_sql_columns")

(define (odbc-sql-foreign-keys stmt-handle 
			       pk-catalog-name pk-schema-name pk-table-name
			       fk-catalog-name fk-schema-name fk-table-name)
  (check-arg statement-handle? stmt-handle odbc-sql-foreign-keys)
  (odbc-sql-foreign-keys-internal (statement-handle-handle stmt-handle)
				  pk-catalog-name pk-schema-name pk-table-name
				  fk-catalog-name fk-schema-name fk-table-name))

(import-lambda-definition odbc-sql-foreign-keys-internal
			  (stmt-handle 
			   pk-catalog-name pk-schema-name pk-table-name
			   fk-catalog-name fk-schema-name fk-table-name)
			  "odbc_sql_foreign_keys")

(define (odbc-sql-primary-keys stmt-handle catalog-name schema-name table-name)
  (check-arg statement-handle? stmt-handle odbc-sql-primary-keys)
  (odbc-sql-primary-keys-internal (statement-handle-handle stmt-handle)
				  catalog-name schema-name table-name))

(import-lambda-definition odbc-sql-primary-keys-internal
			  (stmt-handle catalog-name schema-name table-name)
			  "odbc_sql_primary_keys")

(define (odbc-sql-procedure-columns stmt-handle catalog-name schema-name
				    proc-name column-name)
  (check-arg statement-handle? stmt-handle odbc-sql-procedure-columns)
  (odbc-sql-procedure-columns-internal (statement-handle-handle stmt-handle)
				       catalog-name schema-name proc-name column-name))

(import-lambda-definition odbc-sql-procedure-columns-internal
			  (stmt-handle catalog-name schema-name proc-name column-name)
			  "odbc_sql_procedure_columns")

(define (odbc-sql-procedures stmt-handle catalog-name schema-name proc-name)
  (check-arg statement-handle? stmt-handle odbc-sql-procedures)
  (odbc-sql-procedures-internal (statement-handle-handle stmt-handle)
				catalog-name schema-name proc-name))

(import-lambda-definition odbc-sql-procedures-internal
			  (stmt-handle catalog-name schema-name proc-name)
			  "odbc_sql_procedures")

(define (odbc-sql-special-columns stmt-handle identifier-type catalog-name
				  schema-name table-name scope nullable)
  (check-arg statement-handle? stmt-handle odbc-sql-special-columns)
  (odbc-sql-special-columns-internal (statement-handle-handle stmt-handle)
				     identifier-type catalog-name
				     schema-name table-name schema-name nullable))

(import-lambda-definition odbc-sql-special-columns-internal
			  (stmt-handle identifier-type catalog-name 
				       schema-name table-name scope nullable?)
			  "odbc_sql_special_columns")

(define (odbc-sql-statistics stmt-handle catalog-name schema-name table-name
			     unique reserved)
  (check-arg statement-handle? stmt-handle odbc-sql-statistics)
  (odbc-sql-statistics-internal (statement-handle-handle stmt-handle)
				catalog-name schema-name table-name unique reserved))

(import-lambda-definition odbc-sql-statistics-internal
			  (stmt-handle catalog-name schema-name table-name
				       unique reserved)
			  "odbc_sql_statistics")

(define (odbc-sql-table-privileges stmt-handle catalog-name schema-name table-name)
  (check-arg statement-handle? stmt-handle odbc-sql-table-privileges)
  (odbc-sql-table-privileges-internal (statement-handle-handle stmt-handle)
				      catalog-name schema-name table-name))

(import-lambda-definition odbc-sql-table-privileges-internal
			  (stmt-handle catalog-name schema-name table-name)
			  "odbc_sql_table_privileges")

(define (odbc-sql-tables stmt-handle catalog-name schema-name table-name table-type)
  (check-arg statement-handle? stmt-handle odbc-sql-tables)
  (odbc-sql-tables-internal (statement-handle-handle stmt-handle)
			    catalog-name schema-name table-name table-type))

(import-lambda-definition odbc-sql-tables-internal
			  (stmt-handle catalog-name schema-name table-type table-type)
			  "odbc_sql_tables")

;;; PART 9

(define (odbc-sql-free-statement stmt-handle option)
  (check-arg statement-handle? stmt-handle odbc-sql-free-statement)
  (odbc-sql-free-statement-internal (statement-handle-handle stmt-handle) option))

(import-lambda-definition odbc-sql-free-statement-internal
			  (stmt-handle option)
			  "odbc_sql_free_statement")

(define (odbc-sql-close-cursor stmt-handle)
  (check-arg statement-handle? stmt-handle odbc-sql-close-cursor)
  (odbc-sql-close-cursor-internal (statement-handle-handle stmt-handle)))

(import-lambda-definition odbc-sql-close-cursor-internal
			  (stmt-handle)
			  "odbc_sql_close_cursor")

(define (odbc-sql-cancel stmt-handle)
  (check-arg statement-handle? stmt-handle odbc-sql-cancel)
  (odbc-sql-cancel-internal (statement-handle-handle stmt-handle)))

(import-lambda-definition odbc-sql-cancel-internal
			  (stmt-handle)
			  "odbc_sql_cancel")

(define (odbc-sql-num-result-cols stmt-handle)
  (check-arg statement-handle? stmt-handle odbc-sql-num-result-cols)
  (odbc-sql-num-result-cols-internal (statement-handle-handle stmt-handle)))

(import-lambda-definition odbc-sql-num-result-cols-internal
			  (stmt-handle)
			  "odbc_sql_num_result_cols")

(define (odbc-sql-describe-col stmt-handle column-number)
  (check-arg statement-handle? stmt-handle odbc-sql-describe-col)
  (odbc-sql-describe-col-internal (statement-handle-handle stmt-handle) column-number))
  
(import-lambda-definition odbc-sql-describe-col-internal
			  (stmt-handle column-number)
			  "odbc_sql_describe_col")

(define (odbc-sql-col-attribute stmt-handle column-number field-id)
  (check-arg statement-handle? stmt-handle odbc-sql-col-attribute)
  (let ((pair (odbc-sql-col-attribute-internal 
	       (statement-handle-handle stmt-handle)
	       column-number field-id)))
    (if (zero? (string-length (car pair)))
	(cdr pair)
	(car pair))))

(import-lambda-definition odbc-sql-col-attribute-internal 
			  (stmt-handle column-number field-id)
			  "odbc_sql_col_attribute")

;;; PART 10

(define (odbc-sql-disconnect conn-handle)
  (check-arg connection-handle? conn-handle odbc-sql-disconnect)
  (let ((return-value (odbc-sql-disconnect-internal
		       (connection-handle-handle conn-handle))))
    (if (odbc-call-successful? return-value)
	(set-connection-handle-connected?! conn-handle #f))
    return-value))

(import-lambda-definition odbc-sql-disconnect-internal
			  (conn-handle)
			  "odbc_sql_disconnect")

(define (odbc-sql-free-handle handle)
  (check-arg odbc-handle? handle odbc-sql-free-handle)
  (odbc-sql-free-handle-internal (handle-record-type->c-handle-identifier handle) 
				 (odbc-handle handle)))

(import-lambda-definition odbc-sql-free-handle-internal
			  (handle-type handle)
			  "odbc_sql_free_handle")

(define (odbc-sql-get-diag-recs handle)
  (apply values
	 (odbc-sql-get-diag-recs-internal 
	  (handle-record-type->c-handle-identifier handle) (odbc-handle handle))))

(import-lambda-definition odbc-sql-get-diag-recs-internal
			  (handle-type handle)
			  "odbc_sql_get_diag_recs")

(import-lambda-definition odbc-set-initial-retval-buffer-size
			  (no-bytes)
			  "odbc_set_initial_retval_buffer_size")

(import-lambda-definition odbc-get-initial-retval-buffer-size
			  ()
			  "odbc_get_intial_retval_buffer_size")

;;; misc stuff
(define (odbc-call-successful? odbc-return-value)
  (or (equal? odbc-return-value sql-success)
      (equal? odbc-return-value sql-success-with-info)))
