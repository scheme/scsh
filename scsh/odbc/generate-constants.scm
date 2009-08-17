; ,open srfi-13

(define (pretty-print-constant str)
  (string-map 
   (lambda (c) (if (char=? c #\_) #\- c))
   (string-downcase str)))

(define (generate-print-func-dec c-name scheme-name type)
  (format 
   (current-output-port)
   "printf(\"(define ~A %d)\\n\", ~A);~%" scheme-name c-name))

(define (generate-print-func-string c-name scheme-name type)
  (format 
   (current-output-port)
   "printf(\"(define ~A %s)\\n\", ~A);~%" scheme-name c-name))

(define (generate-print-func c-name scheme-name type)
  (let ((real-scheme-name (or scheme-name 
			      (pretty-print-constant c-name))))
    (cond ((equal? type 'dec)
	   (generate-print-func-dec c-name real-scheme-name type))
	  ((equal? type 'string)
	   (generate-print-func-string c-name real-scheme-name type))
	  (else
	   (error "don't know this type " type)))))

(define (generate-constant-list c-name scheme-name type)
  (let ((real-scheme-name (or scheme-name (pretty-print-constant c-name))))
    (format (current-output-port) "\t ~A\\n" real-scheme-name)))

(define (generate-comment str)
  (format (current-output-port)
	  "printf(\"\\n\\n~A\\n\");~%" str))

(define (do-with-constants const-list print-func)
  (map
   (lambda (thing)   
     (cond ((string? thing) (generate-comment thing))
	   ((list? thing) 
	    (print-func (car thing) (cadr thing) (caddr thing)))
	   (else
	    (error "Don't know what to do with this " thing))))
   const-list))

(define (generate-constants const-list)
  (do-with-constants const-list generate-constants))

(define (generate-list-of-constants const-list)
  (do-with-constants const-list generate-constant-list))

(define odbc-constants-from-sql-h
  '(";;; some return values"
    ("SQL_NULL_DATA" #f dec)
    ("SQL_DATA_AT_EXEC" #f dec)
    ("SQL_SUCCESS" #f dec)
    ("SQL_SUCCESS_WITH_INFO" #f dec)
    ("SQL_NO_DATA" #f dec)
    ("SQL_ERROR" #f dec)
    ("SQL_INVALID_HANDLE" #f dec)
    ("SQL_STILL_EXECUTING" #f dec)
    ("SQL_NEED_DATA" #f dec)

    ";;; use these to indicate string termination to some function"
    ("SQL_NTS" #f dec)
    ("SQL_NTSL" #f dec)

    ";;; maximum message length"
    ("SQL_MAX_MESSAGE_LENGTH" #f dec)

    ";;; date/time length constants"
    ("SQL_DATE_LEN" #f dec)
    ("SQL_TIME_LEN" #f dec)
    ("SQL_TIMESTAMP_LEN" #f dec)

    ";;; handle type identifiers"
    ("SQL_HANDLE_ENV" #f dec)
    ("SQL_HANDLE_DBC" #f dec)
    ("SQL_HANDLE_STMT" #f dec)
    ("SQL_HANDLE_DESC" #f dec)

    ";;; environment attribute"
    ("SQL_ATTR_OUTPUT_NTS" #f dec)

    ";;; connection attributes"
    ("SQL_ATTR_AUTO_IPD" #f dec)
    ("SQL_ATTR_METADATA_ID" #f dec)

    ";;; statement attributes"
    ("SQL_ATTR_APP_ROW_DESC" #f dec)
    ("SQL_ATTR_APP_PARAM_DESC" #f dec)
    ("SQL_ATTR_IMP_ROW_DESC" #f dec)
    ("SQL_ATTR_IMP_PARAM_DESC" #f dec)
    ("SQL_ATTR_CURSOR_SCROLLABLE" #f dec)
    ("SQL_ATTR_CURSOR_SENSITIVITY" #f dec)
    ("SQL_NONSCROLLABLE" #f dec)
    ("SQL_SCROLLABLE" #f dec)

    ";;; identifiers of fields in the SQL descriptor"
    ("SQL_DESC_COUNT" #f dec)
    ("SQL_DESC_TYPE" #f dec)
    ("SQL_DESC_LENGTH" #f dec)
    ("SQL_DESC_OCTET_LENGTH_PTR" #f dec)
    ("SQL_DESC_PRECISION" #f dec)
    ("SQL_DESC_SCALE" #f dec)
    ("SQL_DESC_DATETIME_INTERVAL_CODE" #f dec)
    ("SQL_DESC_NULLABLE" #f dec)
    ("SQL_DESC_INDICATOR_PTR" #f dec)
    ("SQL_DESC_DATA_PTR" #f dec)
    ("SQL_DESC_NAME" #f dec)
    ("SQL_DESC_UNNAMED" #f dec)
    ("SQL_DESC_OCTET_LENGTH" #f dec)
    ("SQL_DESC_ALLOC_TYPE" #f dec)

    ";;; identifiers of fields in the diagnostics area"
    ("SQL_DIAG_RETURNCODE" #f dec)
    ("SQL_DIAG_NUMBER" #f dec)
    ("SQL_DIAG_ROW_COUNT" #f dec)
    ("SQL_DIAG_SQLSTATE" #f dec)
    ("SQL_DIAG_NATIVE" #f dec)
    ("SQL_DIAG_MESSAGE_TEXT" #f dec)
    ("SQL_DIAG_DYNAMIC_FUNCTION" #f dec)
    ("SQL_DIAG_CLASS_ORIGIN" #f dec)
    ("SQL_DIAG_SUBCLASS_ORIGIN" #f dec)
    ("SQL_DIAG_CONNECTION_NAME" #f dec)
    ("SQL_DIAG_SERVER_NAME" #f dec)
    ("SQL_DIAG_DYNAMIC_FUNCTION_CODE" #f dec)
    ("SQL_DIAG_ALTER_DOMAIN" #f dec)
    ("SQL_DIAG_ALTER_TABLE" #f dec)
    ("SQL_DIAG_CALL" #f dec)
    ("SQL_DIAG_CREATE_ASSERTION" #f dec)
    ("SQL_DIAG_CREATE_CHARACTER_SET" #f dec)
    ("SQL_DIAG_CREATE_COLLATION" #f dec)
    ("SQL_DIAG_CREATE_DOMAIN" #f dec)
    ("SQL_DIAG_CREATE_INDEX" #f dec)
    ("SQL_DIAG_CREATE_SCHEMA" #f dec)
    ("SQL_DIAG_CREATE_TABLE" #f dec)
    ("SQL_DIAG_CREATE_TRANSLATION" #f dec)
    ("SQL_DIAG_CREATE_VIEW" #f dec)
    ("SQL_DIAG_DELETE_WHERE" #f dec)
    ("SQL_DIAG_DROP_ASSERTION" #f dec)
    ("SQL_DIAG_DROP_CHARACTER_SET" #f dec)
    ("SQL_DIAG_DROP_COLLATION" #f dec)
    ("SQL_DIAG_DROP_DOMAIN" #f dec)
    ("SQL_DIAG_DROP_INDEX" #f dec)
    ("SQL_DIAG_DROP_SCHEMA" #f dec)
    ("SQL_DIAG_DROP_TABLE" #f dec)
    ("SQL_DIAG_DROP_TRANSLATION" #f dec)
    ("SQL_DIAG_DROP_VIEW" #f dec)
    ("SQL_DIAG_DYNAMIC_DELETE_CURSOR" #f dec)
    ("SQL_DIAG_DYNAMIC_UPDATE_CURSOR" #f dec)
    ("SQL_DIAG_GRANT" #f dec)
    ("SQL_DIAG_INSERT" #f dec)
    ("SQL_DIAG_REVOKE" #f dec)
    ("SQL_DIAG_SELECT_CURSOR" #f dec)
    ("SQL_DIAG_UNKNOWN_STATEMENT" #f dec)
    ("SQL_DIAG_UPDATE_WHERE" #f dec)

    ";;; SQL data type codes"
    ("SQL_UNKNOWN_TYPE" #f dec)
    ("SQL_CHAR" #f dec)
    ("SQL_NUMERIC" #f dec)
    ("SQL_DECIMAL" #f dec)
    ("SQL_INTEGER" #f dec)
    ("SQL_SMALLINT" #f dec)
    ("SQL_FLOAT" #f dec)
    ("SQL_REAL" #f dec)
    ("SQL_DOUBLE" #f dec)
    ("SQL_DATETIME" #f dec)
    ("SQL_VARCHAR" #f dec)
    ("SQL_TYPE_DATE" #f dec)
    ("SQL_TYPE_TIME" #f dec)
    ("SQL_TYPE_TIMESTAMP" #f dec)
    ("SQL_UNSPECIFIED" #f dec)
    ("SQL_INSENSITIVE" #f dec)
    ("SQL_SENSITIVE" #f dec)

    ("SQL_ALL_TYPES" #f dec)
    ("SQL_DEFAULT" #f dec)
    ("SQL_ARD_TYPE" #f dec)
    ("SQL_CODE_DATE" #f dec)
    ("SQL_CODE_TIME" #f dec)
    ("SQL_CODE_TIMESTAMP" #f dec)

    ("SQL_FALSE" #f dec)
    ("SQL_TRUE" #f dec)

    ("SQL_NO_NULLS" #f dec)
    ("SQL_NULLABLE" #f dec)
    ("SQL_NULLABLE_UNKNOWN" #f dec)
    
    ("SQL_PRED_NONE" #f dec)
    ("SQL_PRED_CHAR" #f dec)
    ("SQL_PRED_BASIC" #f dec)

    ("SQL_NAMED" #f dec)
    ("SQL_UNNAMED" #f dec)

    ("SQL_DESC_ALLOC_AUTO" #f dec)
    ("SQL_DESC_ALLOC_USER" #f dec)

    ("SQL_CLOSE" #f dec)
    ("SQL_DROP" #f dec)
    ("SQL_UNBIND" #f dec)
    ("SQL_RESET_PARAMS" #f dec)

    ("SQL_FETCH_NEXT" #f dec)
    ("SQL_FETCH_FIRST" #f dec)

    ("SQL_FETCH_LAST" #f dec)
    ("SQL_FETCH_PRIOR" #f dec)
    ("SQL_FETCH_ABSOLUTE" #f dec)
    ("SQL_FETCH_RELATIVE" #f dec)

    ("SQL_COMMIT" #f dec)
    ("SQL_ROLLBACK" #f dec)

    ("SQL_NULL_HENV" #f dec)
    ("SQL_NULL_HDBC" #f dec)
    ("SQL_NULL_HSTMT" #f dec)
    ("SQL_NULL_HDESC" #f dec)

    ("SQL_NULL_HANDLE" #f dec)

    ("SQL_SCOPE_CURROW" #f dec)
    ("SQL_SCOPE_TRANSACTION" #f dec)
    ("SQL_SCOPE_SESSION" #f dec)
    
    ("SQL_PC_UNKNOWN" #f dec)
    ("SQL_PC_NON_PSEUDO" #f dec)
    ("SQL_PC_PSEUDO" #f dec)

    ("SQL_ROW_IDENTIFIER" #f dec)
    
    ("SQL_INDEX_UNIQUE" #f dec)
    ("SQL_INDEX_ALL" #f dec)
    ("SQL_INDEX_CLUSTERED" #f dec)
    ("SQL_INDEX_HASHED" #f dec)
    ("SQL_INDEX_OTHER" #f dec)

    ("SQL_API_SQLALLOCCONNECT" #f dec)
    ("SQL_API_SQLALLOCENV" #f dec)
    ("SQL_API_SQLALLOCHANDLE" #f dec)
    ("SQL_API_SQLALLOCSTMT" #f dec)
    ("SQL_API_SQLBINDCOL" #f dec)
    ("SQL_API_SQLBINDPARAM" #f dec)
    ("SQL_API_SQLCANCEL" #f dec)
    ("SQL_API_SQLCLOSECURSOR" #f dec)
    ("SQL_API_SQLCOLATTRIBUTE" #f dec)
    ("SQL_API_SQLCOLUMNS" #f dec)
    ("SQL_API_SQLCONNECT" #f dec)
    ("SQL_API_SQLCOPYDESC" #f dec)
    ("SQL_API_SQLDATASOURCES" #f dec)
    ("SQL_API_SQLDESCRIBECOL" #f dec)
    ("SQL_API_SQLDISCONNECT" #f dec)
    ("SQL_API_SQLENDTRAN" #f dec)
    ("SQL_API_SQLERROR" #f dec)
    ("SQL_API_SQLEXECDIRECT" #f dec)
    ("SQL_API_SQLEXECUTE" #f dec)
    ("SQL_API_SQLFETCH" #f dec)
    ("SQL_API_SQLFETCHSCROLL" #f dec)
    ("SQL_API_SQLFREECONNECT" #f dec)
    ("SQL_API_SQLFREEENV" #f dec)
    ("SQL_API_SQLFREEHANDLE" #f dec)
    ("SQL_API_SQLFREESTMT" #f dec)
    ("SQL_API_SQLGETCONNECTATTR" #f dec)
    ("SQL_API_SQLGETCURSORNAME" #f dec)
    ("SQL_API_SQLGETDATA" #f dec)
    ("SQL_API_SQLGETDESCFIELD" #f dec)
    ("SQL_API_SQLGETDESCREC" #f dec)
    ("SQL_API_SQLGETDIAGFIELD" #f dec)
    ("SQL_API_SQLGETDIAGREC" #f dec)
    ("SQL_API_SQLGETENVATTR" #f dec)
    ("SQL_API_SQLGETFUNCTIONS" #f dec)
    ("SQL_API_SQLGETINFO" #f dec)
    ("SQL_API_SQLGETSTMTATTR" #f dec)
    ("SQL_API_SQLGETSTMTOPTION" #f dec)
    ("SQL_API_SQLGETTYPEINFO" #f dec)
    ("SQL_API_SQLNUMRESULTCOLS" #f dec)
    ("SQL_API_SQLPARAMDATA" #f dec)
    ("SQL_API_SQLPREPARE" #f dec)
    ("SQL_API_SQLPUTDATA" #f dec)
    ("SQL_API_SQLROWCOUNT" #f dec)
    ("SQL_API_SQLSETCONNECTATTR" #f dec)
    ("SQL_API_SQLSETCONNECTOPTION" #f dec)
    ("SQL_API_SQLSETCURSORNAME" #f dec)
    ("SQL_API_SQLSETDESCFIELD" #f dec)
    ("SQL_API_SQLSETDESCREC" #f dec)
    ("SQL_API_SQLSETENVATTR" #f dec)
    ("SQL_API_SQLSETPARAM" #f dec)
    ("SQL_API_SQLSETSTMTATTR" #f dec)
    ("SQL_API_SQLSETSTMTOPTION" #f dec)
    ("SQL_API_SQLSPECIALCOLUMNS" #f dec)
    ("SQL_API_SQLSTATISTICS" #f dec)
    ("SQL_API_SQLTABLES" #f dec)
    ("SQL_API_SQLTRANSACT" #f dec)
    
    ";;; Information requested by SQLGetInfo()"
    ("SQL_MAX_DRIVER_CONNECTIONS" #f dec)
    ("SQL_MAXIMUM_DRIVER_CONNECTIONS" #f dec)
    ("SQL_MAX_CONCURRENT_ACTIVITIES" #f dec)
    ("SQL_MAXIMUM_DRIVER_CONNECTIONS" #f dec)
    ("SQL_DATA_SOURCE_NAME" #f dec)
    ("SQL_FETCH_DIRECTION" #f dec)
    ("SQL_SERVER_NAME" #f dec)
    ("SQL_SEARCH_PATTERN_ESCAPE" #f dec)
    ("SQL_DBMS_NAME" #f dec)
    ("SQL_DBMS_VER" #f dec)
    ("SQL_ACCESSIBLE_TABLES" #f dec)
    ("SQL_ACCESSIBLE_PROCEDURES" #f dec)
    ("SQL_CURSOR_COMMIT_BEHAVIOR" #f dec)
    ("SQL_DATA_SOURCE_READ_ONLY" #f dec)
    ("SQL_DEFAULT_TXN_ISOLATION" #f dec)
    ("SQL_IDENTIFIER_CASE" #f dec)
    ("SQL_IDENTIFIER_QUOTE_CHAR" #f dec)
    ("SQL_MAX_COLUMN_NAME_LEN" #f dec)
    ("SQL_MAXIMUM_COLUMN_NAME_LENGTH" #f dec)
    ("SQL_MAX_CURSOR_NAME_LEN" #f dec)
    ("SQL_MAXIMUM_CURSOR_NAME_LENGTH" #f dec)
    ("SQL_MAX_SCHEMA_NAME_LEN" #f dec)
    ("SQL_MAXIMUM_SCHEMA_NAME_LENGTH" #f dec)
    ("SQL_MAX_CATALOG_NAME_LEN" #f dec)
    ("SQL_MAXIMUM_CATALOG_NAME_LENGTH" #f dec)
    ("SQL_MAX_TABLE_NAME_LEN" #f dec)
    ("SQL_SCROLL_CONCURRENCY" #f dec)
    ("SQL_TXN_CAPABLE" #f dec)
    ("SQL_TRANSACTION_CAPABLE" #f dec)
    ("SQL_USER_NAME" #f dec)
    ("SQL_TXN_ISOLATION_OPTION" #f dec)
    ("SQL_TRANSACTION_ISOLATION_OPTION" #f dec)
    ("SQL_INTEGRITY" #f dec)
    ("SQL_GETDATA_EXTENSIONS" #f dec)
    ("SQL_NULL_COLLATION" #f dec)
    ("SQL_ALTER_TABLE" #f dec)
    ("SQL_ORDER_BY_COLUMNS_IN_SELECT" #f dec)
    ("SQL_SPECIAL_CHARACTERS" #f dec)
    ("SQL_MAX_COLUMNS_IN_GROUP_BY" #f dec)
    ("SQL_MAXIMUM_COLUMNS_IN_GROUP_BY" #f dec)
    ("SQL_MAX_COLUMNS_IN_INDEX" #f dec)
    ("SQL_MAXIMUM_COLUMNS_IN_INDEX" #f dec)
    ("SQL_MAX_COLUMNS_IN_ORDER_BY" #f dec)
    ("SQL_MAXIMUM_COLUMNS_IN_ORDER_BY" #f dec)
    ("SQL_MAX_COLUMNS_IN_SELECT" #f dec)
    ("SQL_MAXIMUM_COLUMNS_IN_SELECT" #f dec)
    ("SQL_MAX_COLUMNS_IN_TABLE" #f dec)
    ("SQL_MAX_INDEX_SIZE" #f dec)
    ("SQL_MAXIMUM_INDEX_SIZE" #f dec)
    ("SQL_MAX_ROW_SIZE" #f dec)
    ("SQL_MAXIMUM_ROW_SIZE" #f dec)
    ("SQL_MAX_STATEMENT_LEN" #f dec)
    ("SQL_MAXIMUM_STATEMENT_LENGTH" #f dec)
    ("SQL_MAX_TABLES_IN_SELECT" #f dec)
    ("SQL_MAXIMUM_TABLES_IN_SELECT" #f dec)
    ("SQL_MAX_USER_NAME_LEN" #f dec)
    ("SQL_MAXIMUM_USER_NAME_LENGTH" #f dec)

    ("SQL_OJ_CAPABILITIES" #f dec)
    ("SQL_OUTER_JOIN_CAPABILITIES" #f dec)
    ("SQL_XOPEN_CLI_YEAR" #f dec)
    ("SQL_CURSOR_SENSITIVITY" #f dec)
    ("SQL_DESCRIBE_PARAMETER" #f dec)
    ("SQL_CATALOG_NAME" #f dec)
    ("SQL_COLLATION_SEQ" #f dec)
    ("SQL_MAX_IDENTIFIER_LEN" #f dec)
    ("SQL_MAXIMUM_IDENTIFIER_LENGTH" #f dec)

    ("SQL_AT_ADD_COLUMN" #f dec)
    ("SQL_AT_DROP_COLUMN" #f dec)
    ("SQL_AT_ADD_CONSTRAINT" #f dec)

    ";;; SQL_ASYNC_MODE values"
    ("SQL_AM_NONE" #f dec)
    ("SQL_AM_CONNECTION" #f dec)
    ("SQL_AM_STATEMENT" #f dec)
    
    ";;; SQL_CURSOR_COMMIT_BEHAVIOR values"
    ("SQL_CB_DELETE" #f dec)
    ("SQL_CB_CLOSE" #f dec)
    ("SQL_CB_PRESERVE" #f dec)

    ";;; SQL_FETCH_DIRECTION bitmasks"
    ("SQL_FD_FETCH_NEXT" #f dec)
    ("SQL_FD_FETCH_FIRST" #f dec)
    ("SQL_FD_FETCH_LAST" #f dec)
    ("SQL_FD_FETCH_PRIOR" #f dec)
    ("SQL_FD_FETCH_ABSOLUTE" #f dec)
    ("SQL_FD_FETCH_RELATIVE" #f dec)

    ";;; SQL_GETDATA_EXTENSIONS bitmasks"
    ("SQL_GD_ANY_COLUMN" #f dec)
    ("SQL_GD_ANY_ORDER" #f dec)

    ";;; SQL_IDENTIFIER_CASE values"
    ("SQL_IC_UPPER" #f dec)
    ("SQL_IC_LOWER" #f dec)
    ("SQL_IC_SENSITIVE" #f dec)
    ("SQL_IC_MIXED" #f dec)

    ";;; SQL_OJ_CAPABILITIES bitmasks"
    ("SQL_OJ_LEFT" #f dec)
    ("SQL_OJ_RIGHT" #f dec)
    ("SQL_OJ_FULL" #f dec)
    ("SQL_OJ_NESTED" #f dec)
    ("SQL_OJ_NOT_ORDERED" #f dec)
    ("SQL_OJ_INNER" #f dec)
    ("SQL_OJ_ALL_COMPARISON_OPS" #f dec)

    ("SQL_SCCO_READ_ONLY" #f dec)
    ("SQL_SCCO_LOCK" #f dec)
    ("SQL_SCCO_OPT_ROWVER" #f dec)
    ("SQL_SCCO_OPT_VALUES" #f dec)

    ("SQL_TC_NONE" #f dec)
    ("SQL_TC_DML" #f dec)
    ("SQL_TC_ALL" #f dec)
    ("SQL_TC_DDL_COMMIT" #f dec)
    ("SQL_TC_DDL_IGNORE" #f dec)

    ("SQL_TXN_READ_UNCOMMITTED" #f dec)
    ("SQL_TRANSACTION_READ_UNCOMMITTED" #f dec)
    ("SQL_TXN_READ_UNCOMMITTED" #f dec)
    ("SQL_TRANSACTION_READ_COMMITTED" #f dec)
    ("SQL_TXN_REPEATABLE_READ" #f dec)
    ("SQL_TRANSACTION_REPEATABLE_READ" #f dec)
    ("SQL_TXN_SERIALIZABLE" #f dec)
    ("SQL_TRANSACTION_SERIALIZABLE" #f dec)

    ("SQL_NC_HIGH" #f dec)
    ("SQL_NC_LOW" #f dec)))

(define odbc-constants-from-sqlext-h
  '(";;; constants from sqlext.h"
   
   ";;; generally useful constants"
   ("SQL_SPEC_MAJOR" #f dec)
   ("SQL_SPEC_MINOR" #f dec)
   ("SQL_SPEC_STRING" #f string)
   ("SQL_SQLSTATE_SIZE" #f dec)
   ("SQL_MAX_DSN_LENGTH" #f dec)
   ("SQL_MAX_OPTION_STRING_LENGTH" #f dec)

   ("SQL_HANDLE_SENV" #f dec)
   ("SQL_ATTR_ODBC_VERSION" #f dec)
   ("SQL_ATTR_CONNECTION_POOLING" #f dec)
   ("SQL_ATTR_CP_MATCH" #f dec)

   ";;; values for SQL_ATTR_CONNECTION_POOLING"
;   ("SQL_CP_OF" #f dec)
   ("SQL_CP_ONE_PER_DRIVER" #f dec)
   ("SQL_CP_ONE_PER_HENV" #f dec)
   ("SQL_CP_DEFAULT" #f dec)

   ";;; values for SQL_ATTR_CP_MATCH"
   ("SQL_CP_STRICT_MATCH" #f dec)
   ("SQL_CP_RELAXED_MATCH" #f dec)
   ("SQL_CP_MATCH_DEFAULT" #f dec)

   ";;; values for SQL_ATTR_ODBC_VERSION"
   ("SQL_OV_ODBC2" #f dec)
   ("SQL_OV_ODBC3" #f dec)

   ";;; connection attributes"
   ("SQL_ACCESS_MODE" #f dec)
   ("SQL_AUTOCOMMIT" #f dec)
   ("SQL_LOGIN_TIMEOUT" #f dec)
   ("SQL_OPT_TRACE" #f dec)
   ("SQL_OPT_TRACEFILE" #f dec)
   ("SQL_TRANSLATE_DLL" #f dec)
   ("SQL_TRANSLATE_OPTION" #f dec)
   ("SQL_TXN_ISOLATION" #f dec)
   ("SQL_CURRENT_QUALIFIER" #f dec)
   ("SQL_ODBC_CURSORS" #f dec)
   ("SQL_QUIET_MODE" #f dec)
   ("SQL_PACKET_SIZE" #f dec)

   ";;; connection attributes with new names"
   ("SQL_ATTR_ACCESS_MODE" #f dec)
   ("SQL_ATTR_AUTOCOMMIT" #f dec)
   ("SQL_ATTR_CONNECTION_TIMEOUT" #f dec)
   ("SQL_ATTR_CURRENT_CATALOG" #f dec)
   ("SQL_ATTR_DISCONNECT_BEHAVIOR" #f dec)
   ("SQL_ATTR_ENLIST_IN_DTC" #f dec)
   ("SQL_ATTR_ENLIST_IN_XA" #f dec)
   ("SQL_ATTR_LOGIN_TIMEOUT" #f dec)
   ("SQL_ATTR_ODBC_CURSORS" #f dec)
   ("SQL_ATTR_PACKET_SIZE" #f dec)
   ("SQL_ATTR_QUIET_MODE" #f dec)
   ("SQL_ATTR_TRACE" #f dec)
   ("SQL_ATTR_TRACEFILE" #f dec)
   ("SQL_ATTR_TRANSLATE_LIB" #f dec) 
   ("SQL_ATTR_TRANSLATE_OPTION" #f dec)
   ("SQL_ATTR_TXN_ISOLATION" #f dec)
   ("SQL_ATTR_CONNECTION_DEAD" #f dec)

   ";;; values for SQL_ATTR_DISCONNECT_BEHAVIOR"
   ("SQL_DB_RETURN_TO_POOL" #f dec)
   ("SQL_DB_DISCONNECT" #f dec)
   ("SQL_DB_DEFAULT" #f dec)
   ("SQL_DTC_DONE" #f dec)

   ";;; values for SQL_ATTR_CONNECTION_DEAD"
   ("SQL_CD_TRUE" #f dec)
   ("SQL_CD_FALSE" #f dec)

   ";;; statement attributes"
   ("SQL_QUERY_TIMEOUT" #f dec)
   ("SQL_MAX_ROWS" #f dec)
   ("SQL_NOSCAN" #f dec)
   ("SQL_MAX_LENGTH" #f dec)
   ("SQL_ASYNC_ENABLE" #f dec)
   ("SQL_BIND_TYPE" #f dec)
   ("SQL_CURSOR_TYPE" #f dec)
   ("SQL_CONCURRENCY" #f dec)
   ("SQL_KEYSET_SIZE" #f dec)
   ("SQL_ROWSET_SIZE" #f dec)
   ("SQL_SIMULATE_CURSOR" #f dec)
   ("SQL_RETRIEVE_DATA" #f dec)
   ("SQL_USE_BOOKMARKS" #f dec)
   ("SQL_GET_BOOKMARK" #f dec)
   ("SQL_ROW_NUMBER" #f dec)

   ";;; statement attributes for ODBC 3.0"
   ("SQL_ATTR_ASYNC_ENABLE" #f dec)
   ("SQL_ATTR_CONCURRENCY" #f dec)
   ("SQL_ATTR_CURSOR_TYPE" #f dec)
   ("SQL_ATTR_ENABLE_AUTO_IPD" #f dec)
   ("SQL_ATTR_FETCH_BOOKMARK_PTR" #f dec)
   ("SQL_ATTR_KEYSET_SIZE" #f dec)
   ("SQL_ATTR_MAX_LENGTH" #f dec)
   ("SQL_ATTR_MAX_ROWS" #f dec)
   ("SQL_ATTR_NOSCAN" #f dec)
   ("SQL_ATTR_PARAM_BIND_OFFSET_PTR" #f dec)
   ("SQL_ATTR_PARAM_BIND_TYPE" #f dec)
   ("SQL_ATTR_PARAM_OPERATION_PTR" #f dec)
   ("SQL_ATTR_PARAM_STATUS_PTR" #f dec)
   ("SQL_ATTR_PARAMS_PROCESSED_PTR" #f dec)
   ("SQL_ATTR_PARAMSET_SIZE" #f dec)
   ("SQL_ATTR_QUERY_TIMEOUT" #f dec)
   ("SQL_ATTR_RETRIEVE_DATA" #f dec)
   ("SQL_ATTR_ROW_BIND_OFFSET_PTR" #f dec)
   ("SQL_ATTR_ROW_BIND_TYPE" #f dec)
   ("SQL_ATTR_ROW_NUMBER" #f dec)
   ("SQL_ATTR_ROW_OPERATION_PTR" #f dec)
   ("SQL_ATTR_ROW_STATUS_PTR" #f dec)
   ("SQL_ATTR_ROWS_FETCHED_PTR" #f dec)
   ("SQL_ATTR_ROW_ARRAY_SIZE" #f dec)
   ("SQL_ATTR_SIMULATE_CURSOR" #f dec)
   ("SQL_ATTR_USE_BOOKMARKS" #f dec)

   ";;; New defines for SEARCHABLE column in SQLGetTypeInfo"
   ("SQL_COL_PRED_CHAR" #f dec)
   ("SQL_COL_PRED_BASIC" #f dec)

   ";;; whether an attribute is a pointer or not"
   ("SQL_IS_POINTER" #f dec)
   ("SQL_IS_UINTEGER" #f dec)
   ("SQL_IS_INTEGER" #f dec)
   ("SQL_IS_USMALLINT" #f dec)
   ("SQL_IS_SMALLINT" #f dec)

   ";;; the value of SQL_ATTR_PARAM_BIND_TYPE"
   ("SQL_PARAM_BIND_BY_COLUMN" #f dec)
   ("SQL_PARAM_BIND_TYPE_DEFAULT" #f dec)
   ("SQL_QUERY_TIMEOUT_DEFAULT" #f dec)
   ("SQL_MAX_ROWS_DEFAULT" #f dec)
   ("SQL_NOSCAN_OFF" #f dec)
   ("SQL_NOSCAN_ON" #f dec)
   ("SQL_NOSCAN_DEFAULT" #f dec)
   ("SQL_MAX_LENGTH_DEFAULT" #f dec)
   ("SQL_ASYNC_ENABLE_OFF" #f dec)
   ("SQL_ASYNC_ENABLE_ON" #f dec)
   ("SQL_ASYNC_ENABLE_DEFAULT" #f dec)
   ("SQL_BIND_BY_COLUMN" #f dec)
   ("SQL_BIND_TYPE_DEFAULT" #f dec)
   ("SQL_CONCUR_READ_ONLY" #f dec)
   ("SQL_CONCUR_LOCK" #f dec)
   ("SQL_CONCUR_ROWVER" #f dec)
   ("SQL_CONCUR_VALUES" #f dec)
   ("SQL_CONCUR_DEFAULT" #f dec)
   ("SQL_CURSOR_FORWARD_ONLY" #f dec)
   ("SQL_CURSOR_KEYSET_DRIVEN" #f dec)
   ("SQL_CURSOR_DYNAMIC" #f dec)
   ("SQL_CURSOR_STATIC" #f dec)
   ("SQL_CURSOR_TYPE_DEFAULT" #f dec)
   ("SQL_ROWSET_SIZE_DEFAULT" #f dec)
   ("SQL_KEYSET_SIZE_DEFAULT" #f dec)
   ("SQL_SC_NON_UNIQUE" #f dec)
   ("SQL_SC_TRY_UNIQUE" #f dec)
   ("SQL_SC_UNIQUE" #f dec)
   ("SQL_RD_OFF" #f dec)
   ("SQL_RD_ON" #f dec)
   ("SQL_RD_DEFAULT" #f dec)
   ("SQL_UB_OFF" #f dec)
   ("SQL_UB_ON" #f dec)
   ("SQL_UB_DEFAULT" #f dec)
   ("SQL_UB_FIXED" #f dec)
   ("SQL_UB_VARIABLE" #f dec)

   ";;; extended descriptor field"
   ("SQL_DESC_ARRAY_SIZE" #f dec)
   ("SQL_DESC_ARRAY_STATUS_PTR" #f dec)
   ("SQL_DESC_AUTO_UNIQUE_VALUE" #f dec)
   ("SQL_DESC_BASE_COLUMN_NAME" #f dec)
   ("SQL_DESC_BASE_TABLE_NAME" #f dec)
   ("SQL_DESC_BIND_OFFSET_PTR" #f dec)
   ("SQL_DESC_BIND_TYPE" #f dec)
   ("SQL_DESC_CASE_SENSITIVE" #f dec)
   ("SQL_DESC_CATALOG_NAME" #f dec)
   ("SQL_DESC_CONCISE_TYPE" #f dec)
   ("SQL_DESC_DATETIME_INTERVAL_PRECISION" #f dec)
   ("SQL_DESC_DISPLAY_SIZE" #f dec)
   ("SQL_DESC_FIXED_PREC_SCALE" #f dec)
   ("SQL_DESC_LABEL" #f dec)
   ("SQL_DESC_LITERAL_PREFIX" #f dec)
   ("SQL_DESC_LITERAL_SUFFIX" #f dec)
   ("SQL_DESC_LOCAL_TYPE_NAME" #f dec)
   ("SQL_DESC_MAXIMUM_SCALE" #f dec)
   ("SQL_DESC_MINIMUM_SCALE" #f dec)
   ("SQL_DESC_NUM_PREC_RADIX" #f dec)
   ("SQL_DESC_PARAMETER_TYPE" #f dec)
   ("SQL_DESC_ROWS_PROCESSED_PTR" #f dec)
   ("SQL_DESC_SCHEMA_NAME" #f dec)
   ("SQL_DESC_SEARCHABLE" #f dec)
   ("SQL_DESC_TYPE_NAME" #f dec)
   ("SQL_DESC_TABLE_NAME" #f dec)
   ("SQL_DESC_UNSIGNED" #f dec)
   ("SQL_DESC_UPDATABLE" #f dec)
   ("SQL_DIAG_CURSOR_ROW_COUNT" #f dec)
   ("SQL_DIAG_ROW_NUMBER" #f dec)
   ("SQL_DIAG_COLUMN_NUMBER" #f dec)

   ";;; SQL extended datatypes"
   ("SQL_DATE" #f dec)
   ("SQL_INTERVAL" #f dec)
   ("SQL_TIME" #f dec)
   ("SQL_TIMESTAMP" #f dec)
   ("SQL_LONGVARCHAR" #f dec)
   ("SQL_BINARY" #f dec)
   ("SQL_VARBINARY" #f dec)
   ("SQL_LONGVARBINARY" #f dec)
   ("SQL_BIGINT" #f dec)
   ("SQL_TINYINT" #f dec)
   ("SQL_BIT" #f dec)

   ";;; interval code"
   ("SQL_CODE_YEAR" #f dec)
   ("SQL_CODE_MONTH" #f dec)
   ("SQL_CODE_DAY" #f dec)
   ("SQL_CODE_HOUR" #f dec)
   ("SQL_CODE_MINUTE" #f dec)
   ("SQL_CODE_SECOND" #f dec)
   ("SQL_CODE_YEAR_TO_MONTH" #f dec)
   ("SQL_CODE_DAY_TO_HOUR" #f dec)
   ("SQL_CODE_DAY_TO_MINUTE" #f dec)
   ("SQL_CODE_DAY_TO_SECOND" #f dec)
   ("SQL_CODE_HOUR_TO_MINUTE" #f dec)
   ("SQL_CODE_HOUR_TO_SECOND" #f dec)
   ("SQL_CODE_MINUTE_TO_SECOND" #f dec)
   ("SQL_INTERVAL_YEAR" #f dec)
   ("SQL_INTERVAL_MONTH" #f dec)
   ("SQL_INTERVAL_DAY" #f dec)
   ("SQL_INTERVAL_HOUR" #f dec)
   ("SQL_INTERVAL_MINUTE" #f dec)
   ("SQL_INTERVAL_SECOND" #f dec)
   ("SQL_INTERVAL_YEAR_TO_MONTH" #f dec)
   ("SQL_INTERVAL_DAY_TO_HOUR" #f dec)
   ("SQL_INTERVAL_DAY_TO_MINUTE" #f dec)
   ("SQL_INTERVAL_DAY_TO_SECOND" #f dec)
   ("SQL_INTERVAL_HOUR_TO_MINUTE" #f dec)
   ("SQL_INTERVAL_HOUR_TO_SECOND" #f dec)
   ("SQL_INTERVAL_MINUTE_TO_SECOND" #f dec)

   ";;; C datatype to SQL datatype mapping"
   ("SQL_C_CHAR" #f dec)
   ("SQL_C_LONG" #f dec)
   ("SQL_C_SHORT" #f dec)
   ("SQL_C_FLOAT" #f dec)
   ("SQL_C_DOUBLE" #f dec)
   ("SQL_C_NUMERIC" #f dec)
   ("SQL_SIGNED_OFFSET" #f dec)
   ("SQL_UNSIGNED_OFFSET" #f dec)

   ";;; C datatype to SQL datatype mapping"
   ("SQL_C_DATE" #f dec)
   ("SQL_C_TIME" #f dec)
   ("SQL_C_TIMESTAMP" #f dec)
   ("SQL_C_TYPE_DATE" #f dec)
   ("SQL_C_TYPE_TIME" #f dec)
   ("SQL_C_TYPE_TIMESTAMP" #f dec)
   ("SQL_C_INTERVAL_YEAR" #f dec)
   ("SQL_C_INTERVAL_MONTH" #f dec)
   ("SQL_C_INTERVAL_DAY" #f dec)
   ("SQL_C_INTERVAL_HOUR" #f dec)
   ("SQL_C_INTERVAL_MINUTE" #f dec)
   ("SQL_C_INTERVAL_SECOND" #f dec)
   ("SQL_C_INTERVAL_YEAR_TO_MONTH" #f dec)
   ("SQL_C_INTERVAL_DAY_TO_HOUR" #f dec)
   ("SQL_C_INTERVAL_DAY_TO_MINUTE" #f dec)
   ("SQL_C_INTERVAL_DAY_TO_SECOND" #f dec)
   ("SQL_C_INTERVAL_HOUR_TO_MINUTE" #f dec)
   ("SQL_C_INTERVAL_HOUR_TO_SECOND" #f dec)
   ("SQL_C_INTERVAL_MINUTE_TO_SECOND" #f dec)
   ("SQL_C_BINARY" #f dec)
   ("SQL_C_BIT" #f dec)
   ("SQL_C_SBIGINT" #f dec)
   ("SQL_C_UBIGINT" #f dec)
   ("SQL_C_TINYINT" #f dec)
   ("SQL_C_SLONG" #f dec)
   ("SQL_C_SSHORT" #f dec)
   ("SQL_C_STINYINT" #f dec)
   ("SQL_C_ULONG" #f dec)
   ("SQL_C_USHORT" #f dec)
   ("SQL_C_UTINYINT" #f dec)
   ("SQL_C_BOOKMARK" #f dec)
   ("SQL_C_VARBOOKMARK" #f dec)

   ";;; define for SQL_DIAG_ROW_NUMBER and SQL_DIAG_COLUMN_NUMBER"
   ("SQL_NO_ROW_NUMBER" #f dec)
   ("SQL_NO_COLUMN_NUMBER" #f dec)
   ("SQL_ROW_NUMBER_UNKNOWN" #f dec)
   ("SQL_COLUMN_NUMBER_UNKNOWN" #f dec)

   ";;; SQLBindParameter extensions"
   ("SQL_DEFAULT_PARAM" #f dec)
   ("SQL_IGNORE" #f dec)
   ("SQL_COLUMN_IGNORE" #f dec)
   ("SQL_LEN_DATA_AT_EXEC_OFFSET" #f dec)
   ("SQL_PARAM_TYPE_DEFAULT" #f dec)
   ("SQL_SETPARAM_VALUE_MAX" #f dec)

   ";;; SQLColAttributes defines"
   ("SQL_COLUMN_COUNT" #f dec)
   ("SQL_COLUMN_NAME" #f dec)
   ("SQL_COLUMN_TYPE" #f dec)
   ("SQL_COLUMN_LENGTH" #f dec)
   ("SQL_COLUMN_PRECISION" #f dec)
   ("SQL_COLUMN_SCALE" #f dec)
   ("SQL_COLUMN_DISPLAY_SIZE" #f dec)
   ("SQL_COLUMN_NULLABLE" #f dec)
   ("SQL_COLUMN_UNSIGNED" #f dec)
   ("SQL_COLUMN_MONEY" #f dec)
   ("SQL_COLUMN_UPDATABLE" #f dec)
   ("SQL_COLUMN_AUTO_INCREMENT" #f dec)
   ("SQL_COLUMN_CASE_SENSITIVE" #f dec)
   ("SQL_COLUMN_SEARCHABLE" #f dec)
   ("SQL_COLUMN_TYPE_NAME" #f dec)
   ("SQL_COLUMN_TABLE_NAME" #f dec)
   ("SQL_COLUMN_OWNER_NAME" #f dec)
   ("SQL_COLUMN_QUALIFIER_NAME" #f dec)
   ("SQL_COLUMN_LABEL" #f dec)
   ("SQL_COLATT_OPT_MAX" #f dec)
   ("SQL_COLATT_OPT_MIN" #f dec)

   ";;; SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE"
   ("SQL_ATTR_READONLY" #f dec)
   ("SQL_ATTR_WRITE" #f dec)
   ("SQL_ATTR_READWRITE_UNKNOWN" #f dec)

   ";;; SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE. These are also used by SQLGetInfo"
   ("SQL_UNSEARCHABLE" #f dec)
   ("SQL_LIKE_ONLY" #f dec)
   ("SQL_ALL_EXCEPT_LIKE" #f dec)
   ("SQL_SEARCHABLE" #f dec)
   ("SQL_PRED_SEARCHABLE" #f dec)

   ";;; Special return values for SQLGetData"
   ("SQL_NO_TOTAL" #f dec)

   ";;; SQLGetFunctions: additional values for Function to represent functions that are not in the X/Open spec."
   ("SQL_API_SQLALLOCHANDLESTD" #f dec)
   ("SQL_API_SQLBULKOPERATIONS" #f dec)
   ("SQL_API_SQLBINDPARAMETER" #f dec)
   ("SQL_API_SQLBROWSECONNECT" #f dec)
   ("SQL_API_SQLCOLATTRIBUTES" #f dec)
   ("SQL_API_SQLCOLUMNPRIVILEGES" #f dec)
   ("SQL_API_SQLDESCRIBEPARAM" #f dec)
   ("SQL_API_SQLDRIVERCONNECT" #f dec)
   ("SQL_API_SQLDRIVERS" #f dec)
   ("SQL_API_SQLEXTENDEDFETCH" #f dec)
   ("SQL_API_SQLFOREIGNKEYS" #f dec)
   ("SQL_API_SQLMORERESULTS" #f dec)
   ("SQL_API_SQLNATIVESQL" #f dec)
   ("SQL_API_SQLNUMPARAMS" #f dec)
   ("SQL_API_SQLPARAMOPTIONS" #f dec)
   ("SQL_API_SQLPRIMARYKEYS" #f dec)
   ("SQL_API_SQLPROCEDURECOLUMNS" #f dec)
   ("SQL_API_SQLPROCEDURES" #f dec)
   ("SQL_API_SQLSETPOS" #f dec)
   ("SQL_API_SQLSETSCROLLOPTIONS" #f dec)
   ("SQL_API_SQLTABLEPRIVILEGES" #f dec)

   ("SQL_API_ALL_FUNCTIONS" #f dec)
   ("SQL_API_LOADBYORDINAL" #f dec)
   ("SQL_API_ODBC3_ALL_FUNCTIONS" #f dec)
   ("SQL_API_ODBC3_ALL_FUNCTIONS_SIZE" #f dec)
   
   ";;; Extended definitions for SQLGetInfo"
   ("SQL_INFO_FIRST" #f dec)
   ("SQL_ACTIVE_CONNECTIONS" #f dec)
   ("SQL_ACTIVE_STATEMENTS" #f dec)
   ("SQL_DRIVER_HDBC" #f dec)
   ("SQL_DRIVER_HENV" #f dec)
   ("SQL_DRIVER_HSTMT" #f dec)
   ("SQL_DRIVER_NAME" #f dec)
   ("SQL_DRIVER_VER" #f dec)
   ("SQL_ODBC_API_CONFORMANCE" #f dec)
   ("SQL_ODBC_VER" #f dec)
   ("SQL_ROW_UPDATES" #f dec)
   ("SQL_ODBC_SAG_CLI_CONFORMANCE" #f dec)
   ("SQL_ODBC_SQL_CONFORMANCE" #f dec)
   ("SQL_PROCEDURES" #f dec)
   ("SQL_CONCAT_NULL_BEHAVIOR" #f dec)
   ("SQL_CURSOR_ROLLBACK_BEHAVIOR" #f dec)
   ("SQL_EXPRESSIONS_IN_ORDERBY" #f dec)
   ("SQL_MAX_OWNER_NAME_LEN" #f dec)
   ("SQL_MAX_PROCEDURE_NAME_LEN" #f dec)
   ("SQL_MAX_QUALIFIER_NAME_LEN" #f dec)
   ("SQL_MULT_RESULT_SETS" #f dec)
   ("SQL_MULTIPLE_ACTIVE_TXN" #f dec)
   ("SQL_OUTER_JOINS" #f dec)
   ("SQL_OWNER_TERM" #f dec)
   ("SQL_PROCEDURE_TERM" #f dec)
   ("SQL_QUALIFIER_NAME_SEPARATOR" #f dec)
   ("SQL_QUALIFIER_TERM" #f dec)
   ("SQL_SCROLL_OPTIONS" #f dec)
   ("SQL_TABLE_TERM" #f dec)
   ("SQL_CONVERT_FUNCTIONS" #f dec)
   ("SQL_NUMERIC_FUNCTIONS" #f dec)
   ("SQL_STRING_FUNCTIONS" #f dec)
   ("SQL_SYSTEM_FUNCTIONS" #f dec)
   ("SQL_TIMEDATE_FUNCTIONS" #f dec)
   ("SQL_CONVERT_BIGINT" #f dec)
   ("SQL_CONVERT_BINARY" #f dec)
   ("SQL_CONVERT_BIT" #f dec)
   ("SQL_CONVERT_CHAR" #f dec)
   ("SQL_CONVERT_DATE" #f dec)
   ("SQL_CONVERT_DECIMAL" #f dec)
   ("SQL_CONVERT_DOUBLE" #f dec)
   ("SQL_CONVERT_FLOAT" #f dec)
   ("SQL_CONVERT_INTEGER" #f dec)
   ("SQL_CONVERT_LONGVARCHAR" #f dec)
   ("SQL_CONVERT_NUMERIC" #f dec)
   ("SQL_CONVERT_REAL" #f dec)
   ("SQL_CONVERT_SMALLINT" #f dec)
   ("SQL_CONVERT_TIME" #f dec)
   ("SQL_CONVERT_TIMESTAMP" #f dec)
   ("SQL_CONVERT_TINYINT" #f dec)
   ("SQL_CONVERT_VARBINARY" #f dec)
   ("SQL_CONVERT_VARCHAR" #f dec)
   ("SQL_CONVERT_LONGVARBINARY" #f dec)
   ("SQL_ODBC_SQL_OPT_IEF" #f dec)
   ("SQL_CORRELATION_NAME" #f dec)
   ("SQL_NON_NULLABLE_COLUMNS" #f dec)
   ("SQL_DRIVER_HLIB" #f dec)
   ("SQL_DRIVER_ODBC_VER" #f dec)
   ("SQL_LOCK_TYPES" #f dec)
   ("SQL_POS_OPERATIONS" #f dec)
   ("SQL_POSITIONED_STATEMENTS" #f dec)
   ("SQL_BOOKMARK_PERSISTENCE" #f dec)
   ("SQL_STATIC_SENSITIVITY" #f dec)
   ("SQL_FILE_USAGE" #f dec)
   ("SQL_COLUMN_ALIAS" #f dec)
   ("SQL_GROUP_BY" #f dec)
   ("SQL_KEYWORDS" #f dec)
   ("SQL_OWNER_USAGE" #f dec)
   ("SQL_QUALIFIER_USAGE" #f dec)
   ("SQL_QUOTED_IDENTIFIER_CASE" #f dec)
   ("SQL_SUBQUERIES" #f dec)
   ("SQL_UNION" #f dec)
   ("SQL_MAX_ROW_SIZE_INCLUDES_LONG" #f dec)
   ("SQL_MAX_CHAR_LITERAL_LEN" #f dec)
   ("SQL_TIMEDATE_ADD_INTERVALS" #f dec)
   ("SQL_TIMEDATE_DIFF_INTERVALS" #f dec)
   ("SQL_NEED_LONG_DATA_LEN" #f dec)
   ("SQL_MAX_BINARY_LITERAL_LEN" #f dec)
   ("SQL_LIKE_ESCAPE_CLAUSE" #f dec)
   ("SQL_QUALIFIER_LOCATION" #f dec)

   ";;; ODBC 3.0 SQLGetInfo values that are not part of the X/Open standard at this time.   X/Open standard values are in sql.h."
   ("SQL_ACTIVE_ENVIRONMENTS" #f dec)
   ("SQL_ALTER_DOMAIN" #f dec)
   ("SQL_SQL_CONFORMANCE" #f dec)
   ("SQL_DATETIME_LITERALS" #f dec)
   ("SQL_ASYNC_MODE" #f dec)
   ("SQL_BATCH_ROW_COUNT" #f dec)
   ("SQL_BATCH_SUPPORT" #f dec)
   ("SQL_CATALOG_LOCATION" #f dec)
   ("SQL_CATALOG_NAME_SEPARATOR" #f dec)
   ("SQL_CATALOG_TERM" #f dec)
   ("SQL_CATALOG_USAGE" #f dec)
   ("SQL_CONVERT_WCHAR" #f dec)
   ("SQL_CONVERT_INTERVAL_DAY_TIME" #f dec)
   ("SQL_CONVERT_INTERVAL_YEAR_MONTH" #f dec)
   ("SQL_CONVERT_WLONGVARCHAR" #f dec)
   ("SQL_CONVERT_WVARCHAR" #f dec)
   ("SQL_CREATE_ASSERTION" #f dec)
   ("SQL_CREATE_CHARACTER_SET" #f dec)
   ("SQL_CREATE_COLLATION" #f dec)
   ("SQL_CREATE_DOMAIN" #f dec)
   ("SQL_CREATE_SCHEMA" #f dec)
   ("SQL_CREATE_TABLE" #f dec)
   ("SQL_CREATE_TRANSLATION" #f dec)
   ("SQL_CREATE_VIEW" #f dec)
   ("SQL_DRIVER_HDESC" #f dec)
   ("SQL_DROP_ASSERTION" #f dec)
   ("SQL_DROP_CHARACTER_SET" #f dec)
   ("SQL_DROP_COLLATION" #f dec)
   ("SQL_DROP_DOMAIN" #f dec)
   ("SQL_DROP_SCHEMA" #f dec)
   ("SQL_DROP_TABLE" #f dec)
   ("SQL_DROP_TRANSLATION" #f dec)
   ("SQL_DROP_VIEW" #f dec)
   ("SQL_DYNAMIC_CURSOR_ATTRIBUTES1" #f dec)
   ("SQL_DYNAMIC_CURSOR_ATTRIBUTES2" #f dec)
   ("SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1" #f dec)
   ("SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2" #f dec)
   ("SQL_INDEX_KEYWORDS" #f dec)
   ("SQL_INFO_SCHEMA_VIEWS" #f dec)
   ("SQL_KEYSET_CURSOR_ATTRIBUTES1" #f dec)
   ("SQL_KEYSET_CURSOR_ATTRIBUTES2" #f dec)
   ("SQL_MAX_ASYNC_CONCURRENT_STATEMENTS" #f dec)
   ("SQL_ODBC_INTERFACE_CONFORMANCE" #f dec)
   ("SQL_PARAM_ARRAY_ROW_COUNTS" #f dec)
   ("SQL_PARAM_ARRAY_SELECTS" #f dec)
   ("SQL_SCHEMA_TERM" #f dec)
   ("SQL_SCHEMA_USAGE" #f dec)
   ("SQL_SQL92_DATETIME_FUNCTIONS" #f dec)
   ("SQL_SQL92_FOREIGN_KEY_DELETE_RULE" #f dec)
   ("SQL_SQL92_FOREIGN_KEY_UPDATE_RULE" #f dec)
   ("SQL_SQL92_GRANT" #f dec)
   ("SQL_SQL92_NUMERIC_VALUE_FUNCTIONS" #f dec)
   ("SQL_SQL92_PREDICATES" #f dec)
   ("SQL_SQL92_RELATIONAL_JOIN_OPERATORS" #f dec)
   ("SQL_SQL92_REVOKE" #f dec)
   ("SQL_SQL92_ROW_VALUE_CONSTRUCTOR" #f dec)
   ("SQL_SQL92_STRING_FUNCTIONS" #f dec)
   ("SQL_SQL92_VALUE_EXPRESSIONS" #f dec)
   ("SQL_STANDARD_CLI_CONFORMANCE" #f dec)
   ("SQL_STATIC_CURSOR_ATTRIBUTES1" #f dec)
   ("SQL_STATIC_CURSOR_ATTRIBUTES2" #f dec)
   ("SQL_AGGREGATE_FUNCTIONS" #f dec)
   ("SQL_DDL_INDEX" #f dec)
   ("SQL_DM_VER" #f dec)
   ("SQL_INSERT_STATEMENT" #f dec)
   ("SQL_UNION_STATEMENT" #f dec)

   ";;; SQL_ALTER_TABLE bitmasks"
   ("SQL_AT_ADD_COLUMN_SINGLE" #f dec)
   ("SQL_AT_ADD_COLUMN_DEFAULT" #f dec)
   ("SQL_AT_ADD_COLUMN_COLLATION" #f dec)
   ("SQL_AT_SET_COLUMN_DEFAULT" #f dec)
   ("SQL_AT_DROP_COLUMN_DEFAULT" #f dec)
   ("SQL_AT_DROP_COLUMN_CASCADE" #f dec)
   ("SQL_AT_DROP_COLUMN_RESTRICT" #f dec)
   ("SQL_AT_ADD_TABLE_CONSTRAINT" #f dec)
   ("SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE" #f dec)
   ("SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT" #f dec)
   ("SQL_AT_CONSTRAINT_NAME_DEFINITION" #f dec)
   ("SQL_AT_CONSTRAINT_INITIALLY_DEFERRED" #f dec)
   ("SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE" #f dec)
   ("SQL_AT_CONSTRAINT_DEFERRABLE" #f dec)
   ("SQL_AT_CONSTRAINT_NON_DEFERRABLE" #f dec)

   ";;; SQL_CONVERT_*  return value bitmasks"
   ("SQL_CVT_CHAR" #f dec)
   ("SQL_CVT_NUMERIC" #f dec)
   ("SQL_CVT_DECIMAL" #f dec)
   ("SQL_CVT_INTEGER" #f dec)
   ("SQL_CVT_SMALLINT" #f dec)
   ("SQL_CVT_FLOAT" #f dec)
   ("SQL_CVT_REAL" #f dec)
   ("SQL_CVT_DOUBLE" #f dec)
   ("SQL_CVT_VARCHAR" #f dec)
   ("SQL_CVT_LONGVARCHAR" #f dec)
   ("SQL_CVT_BINARY" #f dec)
   ("SQL_CVT_VARBINARY" #f dec)
   ("SQL_CVT_BIT" #f dec)
   ("SQL_CVT_TINYINT" #f dec)
   ("SQL_CVT_BIGINT" #f dec)
   ("SQL_CVT_DATE" #f dec)
   ("SQL_CVT_TIME" #f dec)
   ("SQL_CVT_TIMESTAMP" #f dec)
   ("SQL_CVT_LONGVARBINARY" #f dec)
   ("SQL_CVT_INTERVAL_YEAR_MONTH" #f dec)
   ("SQL_CVT_INTERVAL_DAY_TIME" #f dec)
   ("SQL_CVT_WCHAR" #f dec)
   ("SQL_CVT_WLONGVARCHAR" #f dec)
   ("SQL_CVT_WVARCHAR" #f dec)

   ";;; SQL_STRING_FUNCTIONS functions"
   ("SQL_FN_STR_CONCAT" #f dec)
   ("SQL_FN_STR_INSERT" #f dec)
   ("SQL_FN_STR_LEFT" #f dec)
   ("SQL_FN_STR_LTRIM" #f dec)
   ("SQL_FN_STR_LENGTH" #f dec)
   ("SQL_FN_STR_LOCATE" #f dec)
   ("SQL_FN_STR_LCASE" #f dec)
   ("SQL_FN_STR_REPEAT" #f dec)
   ("SQL_FN_STR_REPLACE" #f dec)
   ("SQL_FN_STR_RIGHT" #f dec)
   ("SQL_FN_STR_RTRIM" #f dec)
   ("SQL_FN_STR_SUBSTRING" #f dec)
   ("SQL_FN_STR_UCASE" #f dec)
   ("SQL_FN_STR_ASCII" #f dec)
   ("SQL_FN_STR_CHAR" #f dec)
   ("SQL_FN_STR_DIFFERENCE" #f dec)
   ("SQL_FN_STR_LOCATE_2" #f dec)
   ("SQL_FN_STR_SOUNDEX" #f dec)
   ("SQL_FN_STR_SPACE" #f dec)
   ("SQL_FN_STR_BIT_LENGTH" #f dec)
   ("SQL_FN_STR_CHAR_LENGTH" #f dec)
   ("SQL_FN_STR_CHARACTER_LENGTH" #f dec)
   ("SQL_FN_STR_OCTET_LENGTH" #f dec)
   ("SQL_FN_STR_POSITION" #f dec)

   ";;; SQL_SQL92_STRING_FUNCTIONS"
   ("SQL_SSF_CONVERT" #f dec)
   ("SQL_SSF_LOWER" #f dec)
   ("SQL_SSF_UPPER" #f dec)
   ("SQL_SSF_SUBSTRING" #f dec)
   ("SQL_SSF_TRANSLATE" #f dec)
   ("SQL_SSF_TRIM_BOTH" #f dec)
   ("SQL_SSF_TRIM_LEADING" #f dec)
   ("SQL_SSF_TRIM_TRAILING" #f dec)

   ";;; SQL_NUMERIC_FUNCTIONS functions"
   ("SQL_FN_NUM_ABS" #f dec)
   ("SQL_FN_NUM_ACOS" #f dec)
   ("SQL_FN_NUM_ASIN" #f dec)
   ("SQL_FN_NUM_ATAN" #f dec)
   ("SQL_FN_NUM_ATAN2" #f dec)
   ("SQL_FN_NUM_CEILING" #f dec)
   ("SQL_FN_NUM_COS" #f dec)
   ("SQL_FN_NUM_COT" #f dec)
   ("SQL_FN_NUM_EXP" #f dec)
   ("SQL_FN_NUM_FLOOR" #f dec)
   ("SQL_FN_NUM_LOG" #f dec)
   ("SQL_FN_NUM_MOD" #f dec)
   ("SQL_FN_NUM_SIGN" #f dec)
   ("SQL_FN_NUM_SIN" #f dec)
   ("SQL_FN_NUM_SQRT" #f dec)
   ("SQL_FN_NUM_TAN" #f dec)
   ("SQL_FN_NUM_PI" #f dec)
   ("SQL_FN_NUM_RAND" #f dec)
   ("SQL_FN_NUM_DEGREES" #f dec)
   ("SQL_FN_NUM_LOG10" #f dec)
   ("SQL_FN_NUM_POWER" #f dec)
   ("SQL_FN_NUM_RADIANS" #f dec)
   ("SQL_FN_NUM_ROUND" #f dec)
   ("SQL_FN_NUM_TRUNCATE" #f dec)
   ("SQL_SNVF_BIT_LENGTH" #f dec)
   ("SQL_SNVF_CHAR_LENGTH" #f dec)
   ("SQL_SNVF_CHARACTER_LENGTH" #f dec)
   ("SQL_SNVF_EXTRACT" #f dec)
   ("SQL_SNVF_OCTET_LENGTH" #f dec)
   ("SQL_SNVF_POSITION" #f dec)

   ";;; SQL_TIMEDATE_FUNCTIONS functions"
   ("SQL_FN_TD_NOW" #f dec)
   ("SQL_FN_TD_CURDATE" #f dec)
   ("SQL_FN_TD_DAYOFMONTH" #f dec)
   ("SQL_FN_TD_DAYOFWEEK" #f dec)
   ("SQL_FN_TD_DAYOFYEAR" #f dec)
   ("SQL_FN_TD_MONTH" #f dec)
   ("SQL_FN_TD_QUARTER" #f dec)
   ("SQL_FN_TD_WEEK" #f dec)
   ("SQL_FN_TD_YEAR" #f dec)
   ("SQL_FN_TD_CURTIME" #f dec)
   ("SQL_FN_TD_HOUR" #f dec)
   ("SQL_FN_TD_MINUTE" #f dec)
   ("SQL_FN_TD_SECOND" #f dec)
   ("SQL_FN_TD_TIMESTAMPADD" #f dec)
   ("SQL_FN_TD_TIMESTAMPDIFF" #f dec)
   ("SQL_FN_TD_DAYNAME" #f dec)
   ("SQL_FN_TD_MONTHNAME" #f dec)
   ("SQL_FN_TD_CURRENT_DATE" #f dec)
   ("SQL_FN_TD_CURRENT_TIME" #f dec)
   ("SQL_FN_TD_CURRENT_TIMESTAMP" #f dec)
   ("SQL_FN_TD_EXTRACT" #f dec)

   ";;; SQL_SQL92_DATETIME_FUNCTIONS"
   ("SQL_SDF_CURRENT_DATE" #f dec)
   ("SQL_SDF_CURRENT_TIME" #f dec)
   ("SQL_SDF_CURRENT_TIMESTAMP" #f dec)

   ";;; SQL_SYSTEM_FUNCTIONS functions"
   ("SQL_FN_SYS_USERNAME" #f dec)
   ("SQL_FN_SYS_DBNAME" #f dec)
   ("SQL_FN_SYS_IFNULL" #f dec)

   ";;; SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions"
   ("SQL_FN_TSI_FRAC_SECOND" #f dec)
   ("SQL_FN_TSI_SECOND" #f dec)
   ("SQL_FN_TSI_MINUTE" #f dec)
   ("SQL_FN_TSI_HOUR" #f dec)
   ("SQL_FN_TSI_DAY" #f dec)
   ("SQL_FN_TSI_WEEK" #f dec)
   ("SQL_FN_TSI_MONTH" #f dec)
   ("SQL_FN_TSI_QUARTER" #f dec)
   ("SQL_FN_TSI_YEAR" #f dec)

   ";;; supported SQLFetchScroll FetchOrientation's"
   ("SQL_CA1_NEXT" #f dec)
   ("SQL_CA1_ABSOLUTE" #f dec)
   ("SQL_CA1_RELATIVE" #f dec)
   ("SQL_CA1_BOOKMARK" #f dec)

   ";;; supported SQLSetPos LockType's"
   ("SQL_CA1_LOCK_NO_CHANGE" #f dec)
   ("SQL_CA1_LOCK_EXCLUSIVE" #f dec)
   ("SQL_CA1_LOCK_UNLOCK" #f dec)

   ";;; supported SQLSetPos Operations"
   ("SQL_CA1_POS_POSITION" #f dec)
   ("SQL_CA1_POS_UPDATE" #f dec)
   ("SQL_CA1_POS_DELETE" #f dec)
   ("SQL_CA1_POS_REFRESH" #f dec)

   ";;; positioned updates and delete"
   ("SQL_CA1_POSITIONED_UPDATE" #f dec)
   ("SQL_CA1_POSITIONED_DELETE" #f dec)
   ("SQL_CA1_SELECT_FOR_UPDATE" #f dec)

   ";;; supported SQLBulkOperations operations"
   ("SQL_CA1_BULK_ADD" #f dec)
   ("SQL_CA1_BULK_UPDATE_BY_BOOKMARK" #f dec)
   ("SQL_CA1_BULK_DELETE_BY_BOOKMARK" #f dec)
   ("SQL_CA1_BULK_FETCH_BY_BOOKMARK" #f dec)

   ";;; supported values for SQL_ATTR_SCROLL_CONCURRENCY "
   ("SQL_CA2_READ_ONLY_CONCURRENCY" #f dec)
   ("SQL_CA2_LOCK_CONCURRENCY" #f dec)
   ("SQL_CA2_OPT_ROWVER_CONCURRENCY" #f dec)
   ("SQL_CA2_OPT_VALUES_CONCURRENCY" #f dec)

   ";;; sensitivity of the cursor to its own inserts, deletes, and updates"
   ("SQL_CA2_SENSITIVITY_ADDITIONS" #f dec)
   ("SQL_CA2_SENSITIVITY_DELETIONS" #f dec)
   ("SQL_CA2_SENSITIVITY_UPDATES" #f dec)

   ";;; semantics of SQL_ATTR_MAX_ROWS"
   ("SQL_CA2_MAX_ROWS_SELECT" #f dec)
   ("SQL_CA2_MAX_ROWS_INSERT" #f dec)
   ("SQL_CA2_MAX_ROWS_DELETE" #f dec)
   ("SQL_CA2_MAX_ROWS_UPDATE" #f dec)
   ("SQL_CA2_MAX_ROWS_CATALOG" #f dec)
   ("SQL_CA2_MAX_ROWS_AFFECTS_ALL" #f dec)

   ";;; semantics of SQL_DIAG_CURSOR_ROW_COUNT"
   ("SQL_CA2_CRC_EXACT" #f dec)
   ("SQL_CA2_CRC_APPROXIMATE" #f dec)

   ";;; the kinds of positioned statements that can be simulated"
   ("SQL_CA2_SIMULATE_NON_UNIQUE" #f dec)
   ("SQL_CA2_SIMULATE_TRY_UNIQUE" #f dec)
   ("SQL_CA2_SIMULATE_UNIQUE" #f dec)

   ";;; SQL_ODBC_API_CONFORMANCE values"
   ("SQL_OAC_NONE" #f dec)
   ("SQL_OAC_LEVEL1" #f dec)
   ("SQL_OAC_LEVEL2" #f dec)

   ";;; SQL_ODBC_SAG_CLI_CONFORMANCE values"
   ("SQL_OSCC_NOT_COMPLIANT" #f dec)
   ("SQL_OSCC_COMPLIANT" #f dec)

   ";;; SQL_ODBC_SQL_CONFORMANCE values"
   ("SQL_OSC_MINIMUM" #f dec)
   ("SQL_OSC_CORE" #f dec)
   ("SQL_OSC_EXTENDED" #f dec)

   ";;; SQL_CONCAT_NULL_BEHAVIOR values"
   ("SQL_CB_NULL" #f dec)
   ("SQL_CB_NON_NULL" #f dec)

   ";;; SQL_SCROLL_OPTIONS masks"
   ("SQL_SO_FORWARD_ONLY" #f dec)
   ("SQL_SO_KEYSET_DRIVEN" #f dec)
   ("SQL_SO_DYNAMIC" #f dec)
   ("SQL_SO_MIXED" #f dec)
   ("SQL_SO_STATIC" #f dec)

   ("SQL_FD_FETCH_BOOKMARK" #f dec)

   ";;; SQL_CORRELATION_NAME values"
   ("SQL_CN_NONE" #f dec)
   ("SQL_CN_DIFFERENT" #f dec)
   ("SQL_CN_ANY" #f dec)

   ";;; SQL_NON_NULLABLE_COLUMNS values"
   ("SQL_NNC_NULL" #f dec)
   ("SQL_NNC_NON_NULL" #f dec)

   ";;; SQL_NULL_COLLATION values"
   ("SQL_NC_START" #f dec)
   ("SQL_NC_END" #f dec)

   ";;; SQL_FILE_USAGE values"
   ("SQL_FILE_NOT_SUPPORTED" #f dec)
   ("SQL_FILE_TABLE" #f dec)
   ("SQL_FILE_QUALIFIER" #f dec)
   ("SQL_FILE_CATALOG" #f dec)

   ";;; SQL_GETDATA_EXTENSIONS values"
   ("SQL_GD_BLOCK" #f dec)
   ("SQL_GD_BOUND" #f dec)

   ";;; SQL_POSITIONED_STATEMENTS masks"
   ("SQL_PS_POSITIONED_DELETE" #f dec)
   ("SQL_PS_POSITIONED_UPDATE" #f dec)
   ("SQL_PS_SELECT_FOR_UPDATE" #f dec)

   ";;; SQL_GROUP_BY values"
   ("SQL_GB_NOT_SUPPORTED" #f dec)
   ("SQL_GB_GROUP_BY_EQUALS_SELECT" #f dec)
   ("SQL_GB_GROUP_BY_CONTAINS_SELECT" #f dec)
   ("SQL_GB_NO_RELATION" #f dec)
   ("SQL_GB_COLLATE" #f dec)

   ";;; SQL_OWNER_USAGE masks"
   ("SQL_OU_DML_STATEMENTS" #f dec)
   ("SQL_OU_PROCEDURE_INVOCATION" #f dec)
   ("SQL_OU_TABLE_DEFINITION" #f dec)
   ("SQL_OU_INDEX_DEFINITION" #f dec)
   ("SQL_OU_PRIVILEGE_DEFINITION" #f dec)

   ";;; SQL_SCHEMA_USAGE masks"
   ("SQL_SU_DML_STATEMENTS" #f dec)
   ("SQL_SU_PROCEDURE_INVOCATION" #f dec)
   ("SQL_SU_TABLE_DEFINITION" #f dec)
   ("SQL_SU_INDEX_DEFINITION" #f dec)
   ("SQL_SU_PRIVILEGE_DEFINITION" #f dec)

   ";;; SQL_QUALIFIER_USAGE masks"
   ("SQL_QU_DML_STATEMENTS" #f dec)
   ("SQL_QU_PROCEDURE_INVOCATION" #f dec)
   ("SQL_QU_TABLE_DEFINITION" #f dec)
   ("SQL_QU_INDEX_DEFINITION" #f dec)
   ("SQL_QU_PRIVILEGE_DEFINITION" #f dec)

   ";;; SQL_CATALOG_USAGE masks"
   ("SQL_CU_DML_STATEMENTS" #f dec)
   ("SQL_CU_PROCEDURE_INVOCATION" #f dec)
   ("SQL_CU_TABLE_DEFINITION" #f dec)
   ("SQL_CU_INDEX_DEFINITION" #f dec)
   ("SQL_CU_PRIVILEGE_DEFINITION" #f dec)

   ";;; SQL_SUBQUERIES masks"
   ("SQL_SQ_COMPARISON" #f dec)
   ("SQL_SQ_EXISTS" #f dec)
   ("SQL_SQ_IN" #f dec)
   ("SQL_SQ_QUANTIFIED" #f dec)
   ("SQL_SQ_CORRELATED_SUBQUERIES" #f dec)

   ";;; SQL_UNION masks"
   ("SQL_U_UNION" #f dec)
   ("SQL_U_UNION_ALL" #f dec)

   ";;; SQL_BOOKMARK_PERSISTENCE values"
   ("SQL_BP_CLOSE" #f dec)
   ("SQL_BP_DELETE" #f dec)
   ("SQL_BP_DROP" #f dec)
   ("SQL_BP_TRANSACTION" #f dec)
   ("SQL_BP_UPDATE" #f dec)
   ("SQL_BP_OTHER_HSTMT" #f dec)
   ("SQL_BP_SCROLL" #f dec)

   ";;; SQL_STATIC_SENSITIVITY values"
   ("SQL_SS_ADDITIONS" #f dec)
   ("SQL_SS_DELETIONS" #f dec)
   ("SQL_SS_UPDATES" #f dec)

   ";;; SQL_VIEW values"
   ("SQL_CV_CREATE_VIEW" #f dec)
   ("SQL_CV_CHECK_OPTION" #f dec)
   ("SQL_CV_CASCADED" #f dec)
   ("SQL_CV_LOCAL" #f dec)

   ";;; SQL_LOCK_TYPES masks"
   ("SQL_LCK_NO_CHANGE" #f dec)
   ("SQL_LCK_EXCLUSIVE" #f dec)
   ("SQL_LCK_UNLOCK" #f dec)

   ";;; SQL_POS_OPERATIONS masks"
   ("SQL_POS_POSITION" #f dec)
   ("SQL_POS_REFRESH" #f dec)
   ("SQL_POS_UPDATE" #f dec)
   ("SQL_POS_DELETE" #f dec)
   ("SQL_POS_ADD" #f dec)

   ";;; SQL_QUALIFIER_LOCATION values"
   ("SQL_QL_START" #f dec)
   ("SQL_QL_END" #f dec)

   ";;; Here start return values for ODBC 3.0 SQLGetInfo"
   ";;; SQL_AGGREGATE_FUNCTIONS bitmasks"
   ("SQL_AF_AVG" #f dec)
   ("SQL_AF_COUNT" #f dec)
   ("SQL_AF_MAX" #f dec)
   ("SQL_AF_MIN" #f dec)
   ("SQL_AF_SUM" #f dec)
   ("SQL_AF_DISTINCT" #f dec)
   ("SQL_AF_ALL" #f dec)

   ";;; SQL_SQL_CONFORMANCE bit masks"
   ("SQL_SC_FIPS127_2_TRANSITIONAL" #f dec)
   ("SQL_SC_SQL92_INTERMEDIATE" #f dec)
   ("SQL_SC_SQL92_FULL" #f dec)

   ";;; SQL_DATETIME_LITERALS masks"
   ("SQL_DL_SQL92_DATE" #f dec)
   ("SQL_DL_SQL92_TIME" #f dec)
   ("SQL_DL_SQL92_TIMESTAMP" #f dec)
   ("SQL_DL_SQL92_INTERVAL_YEAR" #f dec)
   ("SQL_DL_SQL92_INTERVAL_MONTH" #f dec)
   ("SQL_DL_SQL92_INTERVAL_DAY" #f dec)
   ("SQL_DL_SQL92_INTERVAL_HOUR" #f dec)
   ("SQL_DL_SQL92_INTERVAL_MINUTE" #f dec)
   ("SQL_DL_SQL92_INTERVAL_SECOND" #f dec)
   ("SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH" #f dec)
   ("SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR" #f dec)
   ("SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE" #f dec)
   ("SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND" #f dec)
   ("SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE" #f dec)
   ("SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND" #f dec)
   ("SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND" #f dec)

   ";;; SQL_CATALOG_LOCATION values"
   ("SQL_CL_START" #f dec)
   ("SQL_CL_END" #f dec)

   ";;; values for SQL_BATCH_ROW_COUNT"
   ("SQL_BRC_PROCEDURES" #f dec)
   ("SQL_BRC_EXPLICIT" #f dec)
   ("SQL_BRC_ROLLED_UP" #f dec)
   
   ";;; bitmasks for SQL_BATCH_SUPPORT"
   ("SQL_BS_SELECT_EXPLICIT" #f dec)
   ("SQL_BS_ROW_COUNT_EXPLICIT" #f dec)
   ("SQL_BS_SELECT_PROC" #f dec)
   ("SQL_BS_ROW_COUNT_PROC" #f dec)

   ";;; Values for SQL_PARAM_ARRAY_ROW_COUNTS getinfo"
   ("SQL_PARC_BATCH" #f dec)
   ("SQL_PARC_NO_BATCH" #f dec)

   ";;; values for SQL_PARAM_ARRAY_SELECTS"
   ("SQL_PAS_BATCH" #f dec)
   ("SQL_PAS_NO_BATCH" #f dec)
   ("SQL_PAS_NO_SELECT" #f dec)
   
   ";;; Bitmasks for SQL_INDEX_KEYWORDS"
   ("SQL_IK_NONE" #f dec)
   ("SQL_IK_ASC" #f dec)
   ("SQL_IK_DESC" #f dec)
   ("SQL_IK_ALL" #f dec)

   ";;; Bitmasks for SQL_INFO_SCHEMA_VIEWS"
   ("SQL_ISV_ASSERTIONS" #f dec)
   ("SQL_ISV_CHARACTER_SETS" #f dec)
   ("SQL_ISV_CHECK_CONSTRAINTS" #f dec)
   ("SQL_ISV_COLLATIONS" #f dec)
   ("SQL_ISV_COLUMN_DOMAIN_USAGE" #f dec)
   ("SQL_ISV_COLUMN_PRIVILEGES" #f dec)
   ("SQL_ISV_COLUMNS" #f dec)
   ("SQL_ISV_CONSTRAINT_COLUMN_USAGE" #f dec)
   ("SQL_ISV_CONSTRAINT_TABLE_USAGE" #f dec)
   ("SQL_ISV_DOMAIN_CONSTRAINTS" #f dec)
   ("SQL_ISV_DOMAINS" #f dec)
   ("SQL_ISV_KEY_COLUMN_USAGE" #f dec)
   ("SQL_ISV_REFERENTIAL_CONSTRAINTS" #f dec)
   ("SQL_ISV_SCHEMATA" #f dec)
   ("SQL_ISV_SQL_LANGUAGES" #f dec)
   ("SQL_ISV_TABLE_CONSTRAINTS" #f dec)
   ("SQL_ISV_TABLE_PRIVILEGES" #f dec)
   ("SQL_ISV_TABLES" #f dec)
   ("SQL_ISV_TRANSLATIONS" #f dec)
   ("SQL_ISV_USAGE_PRIVILEGES" #f dec)
   ("SQL_ISV_VIEW_COLUMN_USAGE" #f dec)
   ("SQL_ISV_VIEW_TABLE_USAGE" #f dec)
   ("SQL_ISV_VIEWS" #f dec)

   ";;; Bitmasks for SQL_ASYNC_MODE"
   ("SQL_AM_NONE" #f dec)
   ("SQL_AM_CONNECTION" #f dec)
   ("SQL_AM_STATEMENT" #f dec)

   ";;; Bitmasks for SQL_ALTER_DOMAIN"
   ("SQL_AD_CONSTRAINT_NAME_DEFINITION" #f dec)
   ("SQL_AD_ADD_DOMAIN_CONSTRAINT" #f dec)
   ("SQL_AD_DROP_DOMAIN_CONSTRAINT" #f dec)
   ("SQL_AD_ADD_DOMAIN_DEFAULT" #f dec)
   ("SQL_AD_DROP_DOMAIN_DEFAULT" #f dec)
   ("SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED" #f dec)
   ("SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE" #f dec)
   ("SQL_AD_ADD_CONSTRAINT_DEFERRABLE" #f dec)
   ("SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE" #f dec)

   ";;; SQL_CREATE_SCHEMA bitmasks"
   ("SQL_CS_CREATE_SCHEMA" #f dec)
   ("SQL_CS_AUTHORIZATION" #f dec)
   ("SQL_CS_DEFAULT_CHARACTER_SET" #f dec)

   ";;; SQL_CREATE_TRANSLATION bitmasks"
   ("SQL_CTR_CREATE_TRANSLATION" #f dec)
   
   ";;; SQL_CREATE_ASSERTION bitmasks"
   ("SQL_CA_CREATE_ASSERTION" #f dec)
   ("SQL_CA_CONSTRAINT_INITIALLY_DEFERRED" #f dec)
   ("SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE" #f dec)
   ("SQL_CA_CONSTRAINT_DEFERRABLE" #f dec)
   ("SQL_CA_CONSTRAINT_NON_DEFERRABLE" #f dec)

   ";;; SQL_CREATE_CHARACTER_SET bitmasks"
   ("SQL_CCS_CREATE_CHARACTER_SET" #f dec)
   ("SQL_CCS_COLLATE_CLAUSE" #f dec)
   ("SQL_CCS_LIMITED_COLLATION" #f dec)

   ";;; SQL_CREATE_COLLATION bitmasks"
   ("SQL_CCOL_CREATE_COLLATION" #f dec)

   ";;; SQL_CREATE_DOMAIN bitmasks"
   ("SQL_CDO_CREATE_DOMAIN" #f dec)
   ("SQL_CDO_DEFAULT" #f dec)
   ("SQL_CDO_CONSTRAINT" #f dec)
   ("SQL_CDO_COLLATION" #f dec)
   ("SQL_CDO_CONSTRAINT_NAME_DEFINITION" #f dec)
   ("SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED" #f dec)
   ("SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE" #f dec)
   ("SQL_CDO_CONSTRAINT_DEFERRABLE" #f dec)
   ("SQL_CDO_CONSTRAINT_NON_DEFERRABLE" #f dec)

   ";;; SQL_CREATE_TABLE bitmasks"
   ("SQL_CT_CREATE_TABLE" #f dec)
   ("SQL_CT_COMMIT_PRESERVE" #f dec)
   ("SQL_CT_COMMIT_DELETE" #f dec)
   ("SQL_CT_GLOBAL_TEMPORARY" #f dec)
   ("SQL_CT_LOCAL_TEMPORARY" #f dec)
   ("SQL_CT_CONSTRAINT_INITIALLY_DEFERRED" #f dec)
   ("SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE" #f dec)
   ("SQL_CT_CONSTRAINT_DEFERRABLE" #f dec)
   ("SQL_CT_CONSTRAINT_NON_DEFERRABLE" #f dec)
   ("SQL_CT_COLUMN_CONSTRAINT" #f dec)
   ("SQL_CT_COLUMN_DEFAULT" #f dec)
   ("SQL_CT_COLUMN_COLLATION" #f dec)
   ("SQL_CT_TABLE_CONSTRAINT" #f dec)
   ("SQL_CT_CONSTRAINT_NAME_DEFINITION" #f dec)

   ";;; SQL_DDL_INDEX bitmasks"
   ("SQL_DI_CREATE_INDEX" #f dec)
   ("SQL_DI_DROP_INDEX" #f dec)
   
   ";;; SQL_DROP_COLLATION bitmasks"
   ("SQL_DC_DROP_COLLATION" #f dec)

   ";;; SQL_DROP_DOMAIN bitmask"
   ("SQL_DD_DROP_DOMAIN" #f dec)
   ("SQL_DD_RESTRICT" #f dec)
   ("SQL_DD_CASCADE" #f dec)

   ";;; SQL_DROP_SCHEMA bitmasks"
   ("SQL_DS_DROP_SCHEMA" #f dec)
   ("SQL_DS_RESTRICT" #f dec)
   ("SQL_DS_CASCADE" #f dec)

   ";;; SQL_DROP_CHARACTER_SET bitmasks"
   ("SQL_DCS_DROP_CHARACTER_SET" #f dec)

   ";;; SQL_DROP_ASSERTION bitmasks"
   ("SQL_DA_DROP_ASSERTION" #f dec)

   ";;; SQL_DROP_TABLE bitmasks"
   ("SQL_DT_DROP_TABLE" #f dec)
   ("SQL_DT_RESTRICT" #f dec)
   ("SQL_DT_CASCADE" #f dec)

   ";;; SQL_DROP_TRANSLATION bitmasks"
   ("SQL_DTR_DROP_TRANSLATION" #f dec)

   ";;; SQL_DROP_VIEW bitmasks"
   ("SQL_DV_DROP_VIEW" #f dec)
   ("SQL_DV_RESTRICT" #f dec)
   ("SQL_DV_CASCADE" #f dec)

   ";;; SQL_INSERT_STATEMENT bitmasks"
   ("SQL_IS_INSERT_LITERALS" #f dec)
   ("SQL_IS_INSERT_SEARCHED" #f dec)
   ("SQL_IS_SELECT_INTO" #f dec)

   ";;; SQL_ODBC_INTERFACE_CONFORMANCE values"
   ("SQL_OIC_CORE" #f dec)
   ("SQL_OIC_LEVEL1" #f dec)
   ("SQL_OIC_LEVEL2" #f dec)

   ";;; SQL_SQL92_FOREIGN_KEY_DELETE_RULE bitmasks"
   ("SQL_SFKD_CASCADE" #f dec)
   ("SQL_SFKD_NO_ACTION" #f dec)
   ("SQL_SFKD_SET_DEFAULT" #f dec)
   ("SQL_SFKD_SET_NULL" #f dec)

   ";;; SQL_SQL92_FOREIGN_KEY_UPDATE_RULE bitmasks"
   ("SQL_SFKU_CASCADE" #f dec)
   ("SQL_SFKU_NO_ACTION" #f dec)
   ("SQL_SFKU_SET_DEFAULT" #f dec)
   ("SQL_SFKU_SET_NULL" #f dec)

   ";;; SQL_SQL92_GRANT	bitmasks"
   ("SQL_SG_USAGE_ON_DOMAIN" #f dec)
   ("SQL_SG_USAGE_ON_CHARACTER_SET" #f dec)
   ("SQL_SG_USAGE_ON_COLLATION" #f dec)
   ("SQL_SG_USAGE_ON_TRANSLATION" #f dec)
   ("SQL_SG_WITH_GRANT_OPTION" #f dec)
   ("SQL_SG_DELETE_TABLE" #f dec)
   ("SQL_SG_INSERT_TABLE" #f dec)
   ("SQL_SG_INSERT_COLUMN" #f dec)
   ("SQL_SG_REFERENCES_TABLE" #f dec)
   ("SQL_SG_REFERENCES_COLUMN" #f dec)
   ("SQL_SG_SELECT_TABLE" #f dec)
   ("SQL_SG_UPDATE_TABLE" #f dec)
   ("SQL_SG_UPDATE_COLUMN" #f dec)

   ";;; SQL_SQL92_PREDICATES bitmasks"
   ("SQL_SP_EXISTS" #f dec)
   ("SQL_SP_ISNOTNULL" #f dec)
   ("SQL_SP_ISNULL" #f dec)
   ("SQL_SP_MATCH_FULL" #f dec)
   ("SQL_SP_MATCH_PARTIAL" #f dec)
   ("SQL_SP_MATCH_UNIQUE_FULL" #f dec)
   ("SQL_SP_MATCH_UNIQUE_PARTIAL" #f dec)
   ("SQL_SP_OVERLAPS" #f dec)
   ("SQL_SP_UNIQUE" #f dec)
   ("SQL_SP_LIKE" #f dec)
   ("SQL_SP_IN" #f dec)
   ("SQL_SP_BETWEEN" #f dec)
   ("SQL_SP_COMPARISON" #f dec)
   ("SQL_SP_QUANTIFIED_COMPARISON" #f dec)

   ";;; SQL_SQL92_RELATIONAL_JOIN_OPERATORS bitmasks"
   ("SQL_SRJO_CORRESPONDING_CLAUSE" #f dec)
   ("SQL_SRJO_CROSS_JOIN" #f dec)
   ("SQL_SRJO_EXCEPT_JOIN" #f dec)
   ("SQL_SRJO_FULL_OUTER_JOIN" #f dec)
   ("SQL_SRJO_INNER_JOIN" #f dec)
   ("SQL_SRJO_INTERSECT_JOIN" #f dec)
   ("SQL_SRJO_LEFT_OUTER_JOIN" #f dec)
   ("SQL_SRJO_NATURAL_JOIN" #f dec)
   ("SQL_SRJO_RIGHT_OUTER_JOIN" #f dec)
   ("SQL_SRJO_UNION_JOIN" #f dec)

   ";;; SQL_SQL92_REVOKE bitmasks"
   ("SQL_SR_USAGE_ON_DOMAIN" #f dec)
   ("SQL_SR_USAGE_ON_CHARACTER_SET" #f dec)
   ("SQL_SR_USAGE_ON_COLLATION" #f dec)
   ("SQL_SR_USAGE_ON_TRANSLATION" #f dec)
   ("SQL_SR_GRANT_OPTION_FOR" #f dec)
   ("SQL_SR_CASCADE" #f dec)
   ("SQL_SR_RESTRICT" #f dec)
   ("SQL_SR_DELETE_TABLE" #f dec)
   ("SQL_SR_INSERT_TABLE" #f dec)
   ("SQL_SR_INSERT_COLUMN" #f dec)
   ("SQL_SR_REFERENCES_TABLE" #f dec)
   ("SQL_SR_REFERENCES_COLUMN" #f dec)
   ("SQL_SR_SELECT_TABLE" #f dec)
   ("SQL_SR_UPDATE_TABLE" #f dec)
   ("SQL_SR_UPDATE_COLUMN" #f dec)

   ";;; SQL_SQL92_ROW_VALUE_CONSTRUCTOR bitmasks"
   ("SQL_SRVC_VALUE_EXPRESSION" #f dec)
   ("SQL_SRVC_NULL" #f dec)
   ("SQL_SRVC_DEFAULT" #f dec)
   ("SQL_SRVC_ROW_SUBQUERY" #f dec)

   ";;; SQL_SQL92_VALUE_EXPRESSIONS bitmasks"
   ("SQL_SVE_CASE" #f dec)
   ("SQL_SVE_CAST" #f dec)
   ("SQL_SVE_COALESCE" #f dec)
   ("SQL_SVE_NULLIF" #f dec)

   ";;; SQL_STANDARD_CLI_CONFORMANCE bitmasks"
   ("SQL_SCC_XOPEN_CLI_VERSION1" #f dec)
   ("SQL_SCC_ISO92_CLI" #f dec)

   ";;; SQL_UNION_STATEMENT bitmasks"
   ("SQL_US_UNION" #f dec)
   ("SQL_US_UNION_ALL" #f dec)

   ";;; SQL_DTC_TRANSITION_COST bitmasks"
   ("SQL_DTC_ENLIST_EXPENSIVE" #f dec)
   ("SQL_DTC_UNENLIST_EXPENSIVE" #f dec)

   ";;; additional SQLDataSources fetch directions"
   ("SQL_FETCH_FIRST_USER" #f dec)
   ("SQL_FETCH_FIRST_SYSTEM" #f dec)

   ";;; Defines for SQLSetPos"
   ("SQL_ENTIRE_ROWSET" #f dec)

   ";;; Operations in SQLSetPos"
   ("SQL_POSITION" #f dec)
   ("SQL_REFRESH" #f dec)
   ("SQL_UPDATE" #f dec)
   ("SQL_DELETE" #f dec)

   ";;; Operations in SQLBulkOperations"
   ("SQL_ADD" #f dec)
   ("SQL_SETPOS_MAX_OPTION_VALUE" #f dec)
   ("SQL_UPDATE_BY_BOOKMARK" #f dec)
   ("SQL_DELETE_BY_BOOKMARK" #f dec)
   ("SQL_FETCH_BY_BOOKMARK" #f dec)

   ";;; Lock options in SQLSetPos"
   ("SQL_LOCK_NO_CHANGE" #f dec)
   ("SQL_LOCK_EXCLUSIVE" #f dec)
   ("SQL_LOCK_UNLOCK" #f dec)

   ("SQL_SETPOS_MAX_LOCK_VALUE" #f dec)

   ";;; Column types and scopes in SQLSpecialColumns"
   ("SQL_BEST_ROWID" #f dec)
   ("SQL_ROWVER" #f dec)

   ";;; Defines for SQLSpecialColumns (returned in the result set) SQL_PC_UNKNOWN and SQL_PC_PSEUDO are defined in sql.h"
   ("SQL_PC_NOT_PSEUDO" #f dec)

   ";;; Defines for SQLStatistics"
   ("SQL_QUICK" #f dec)
   ("SQL_ENSURE" #f dec)

   ";;; Defines for SQLStatistics (returned in the result set) SQL_INDEX_CLUSTERED, SQL_INDEX_HASHED, and SQL_INDEX_OTHER are defined in sql.h"
   ("SQL_TABLE_STAT" #f dec)

   ";;; Defines for SQLTables"
   ("SQL_ALL_CATALOGS" #f string)
   ("SQL_ALL_SCHEMAS" #f string)
   ("SQL_ALL_TABLE_TYPES" #f string)

   ";;; Options for SQLDriverConnect"
   ("SQL_DRIVER_NOPROMPT" #f dec)
   ("SQL_DRIVER_COMPLETE" #f dec)
   ("SQL_DRIVER_PROMPT" #f dec)
   ("SQL_DRIVER_COMPLETE_REQUIRED" #f dec)

   ";;; SQLExtendedFetch fFetchType values"
   ("SQL_FETCH_BOOKMARK" #f dec)

   ";;; SQLExtendedFetch rgfRowStatus element values"
   ("SQL_ROW_SUCCESS" #f dec)
   ("SQL_ROW_DELETED" #f dec)
   ("SQL_ROW_UPDATED" #f dec)
   ("SQL_ROW_NOROW" #f dec)
   ("SQL_ROW_ADDED" #f dec)
   ("SQL_ROW_ERROR" #f dec)
   ("SQL_ROW_SUCCESS_WITH_INFO" #f dec)
   ("SQL_ROW_PROCEED" #f dec)
   ("SQL_ROW_IGNORE" #f dec)

   ";;; value for SQL_DESC_ARRAY_STATUS_PTR"
   ("SQL_PARAM_SUCCESS" #f dec)
   ("SQL_PARAM_SUCCESS_WITH_INFO" #f dec)
   ("SQL_PARAM_ERROR" #f dec)
   ("SQL_PARAM_UNUSED" #f dec)
   ("SQL_PARAM_DIAG_UNAVAILABLE" #f dec)
   ("SQL_PARAM_PROCEED" #f dec)
   ("SQL_PARAM_IGNORE" #f dec)

   ";;; Defines for SQLForeignKeys (UPDATE_RULE and DELETE_RULE)"
   ("SQL_CASCADE" #f dec)
   ("SQL_RESTRICT" #f dec)
   ("SQL_SET_NULL" #f dec)
   ("SQL_NO_ACTION" #f dec)
   ("SQL_SET_DEFAULT" #f dec)

   ";;; Note that the following are in a different column of SQLForeignKeys than the previous #defines.   These are for DEFERRABILITY."
   ("SQL_INITIALLY_DEFERRED" #f dec)
   ("SQL_INITIALLY_IMMEDIATE" #f dec)
   ("SQL_NOT_DEFERRABLE" #f dec)

   ";;; Defines for SQLBindParameter and  SQLProcedureColumns (returned in the result set)"
   ("SQL_PARAM_TYPE_UNKNOWN" #f dec)
   ("SQL_PARAM_INPUT" #f dec)
   ("SQL_PARAM_INPUT_OUTPUT" #f dec)
   ("SQL_RESULT_COL" #f dec)
   ("SQL_PARAM_OUTPUT" #f dec)
   ("SQL_RETURN_VALUE" #f dec)

   ";;; Defines for SQLProcedures (returned in the result set)"
   ("SQL_PT_UNKNOWN" #f dec)
   ("SQL_PT_PROCEDURE" #f dec)
   ("SQL_PT_FUNCTION" #f dec)

   ";;; This define is too large for RC (very funny!!!)"
   ("SQL_ODBC_KEYWORDS" #f string)))

(with-output-to-file "constants.c"
  (lambda ()
    (generate-constants odbc-constants-from-sql-h)
    (generate-constants odbc-constants-from-sqlext-h)
    (close-output-port (current-output-port))))

(with-output-to-file "constants.txt" 
  (lambda () 
    (for-each (lambda (name) 
		(format (current-output-port) "\t~A~%" name))
	      clist) 
    (close-output-port (current-output-port))))
