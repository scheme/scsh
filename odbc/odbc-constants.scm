

;;; some return values
(define sql-null-data -1)
(define sql-data-at-exec -2)
(define sql-success 0)
(define sql-success-with-info 1)
(define sql-no-data 100)
(define sql-error -1)
(define sql-invalid-handle -2)
(define sql-still-executing 2)
(define sql-need-data 99)


;;; use these to indicate string termination to some function
(define sql-nts -3)
(define sql-ntsl -3)


;;; maximum message length
(define sql-max-message-length 512)


;;; date/time length constants
(define sql-date-len 10)
(define sql-time-len 8)
(define sql-timestamp-len 19)


;;; handle type identifiers
(define sql-handle-env 1)
(define sql-handle-dbc 2)
(define sql-handle-stmt 3)
(define sql-handle-desc 4)


;;; environment attribute
(define sql-attr-output-nts 10001)


;;; connection attributes
(define sql-attr-auto-ipd 10001)
(define sql-attr-metadata-id 10014)


;;; statement attributes
(define sql-attr-app-row-desc 10010)
(define sql-attr-app-param-desc 10011)
(define sql-attr-imp-row-desc 10012)
(define sql-attr-imp-param-desc 10013)
(define sql-attr-cursor-scrollable -1)
(define sql-attr-cursor-sensitivity -2)
(define sql-nonscrollable 0)
(define sql-scrollable 1)


;;; identifiers of fields in the SQL descriptor
(define sql-desc-count 1001)
(define sql-desc-type 1002)
(define sql-desc-length 1003)
(define sql-desc-octet-length-ptr 1004)
(define sql-desc-precision 1005)
(define sql-desc-scale 1006)
(define sql-desc-datetime-interval-code 1007)
(define sql-desc-nullable 1008)
(define sql-desc-indicator-ptr 1009)
(define sql-desc-data-ptr 1010)
(define sql-desc-name 1011)
(define sql-desc-unnamed 1012)
(define sql-desc-octet-length 1013)
(define sql-desc-alloc-type 1099)


;;; identifiers of fields in the diagnostics area
(define sql-diag-returncode 1)
(define sql-diag-number 2)
(define sql-diag-row-count 3)
(define sql-diag-sqlstate 4)
(define sql-diag-native 5)
(define sql-diag-message-text 6)
(define sql-diag-dynamic-function 7)
(define sql-diag-class-origin 8)
(define sql-diag-subclass-origin 9)
(define sql-diag-connection-name 10)
(define sql-diag-server-name 11)
(define sql-diag-dynamic-function-code 12)
(define sql-diag-alter-domain 3)
(define sql-diag-alter-table 4)
(define sql-diag-call 7)
(define sql-diag-create-assertion 6)
(define sql-diag-create-character-set 8)
(define sql-diag-create-collation 10)
(define sql-diag-create-domain 23)
(define sql-diag-create-index -1)
(define sql-diag-create-schema 64)
(define sql-diag-create-table 77)
(define sql-diag-create-translation 79)
(define sql-diag-create-view 84)
(define sql-diag-delete-where 19)
(define sql-diag-drop-assertion 24)
(define sql-diag-drop-character-set 25)
(define sql-diag-drop-collation 26)
(define sql-diag-drop-domain 27)
(define sql-diag-drop-index -2)
(define sql-diag-drop-schema 31)
(define sql-diag-drop-table 32)
(define sql-diag-drop-translation 33)
(define sql-diag-drop-view 36)
(define sql-diag-dynamic-delete-cursor 38)
(define sql-diag-dynamic-update-cursor 81)
(define sql-diag-grant 48)
(define sql-diag-insert 50)
(define sql-diag-revoke 59)
(define sql-diag-select-cursor 85)
(define sql-diag-unknown-statement 0)
(define sql-diag-update-where 82)


;;; SQL data type codes
(define sql-unknown-type 0)
(define sql-char 1)
(define sql-numeric 2)
(define sql-decimal 3)
(define sql-integer 4)
(define sql-smallint 5)
(define sql-float 6)
(define sql-real 7)
(define sql-double 8)
(define sql-datetime 9)
(define sql-varchar 12)
(define sql-type-date 91)
(define sql-type-time 92)
(define sql-type-timestamp 93)
(define sql-unspecified 0)
(define sql-insensitive 1)
(define sql-sensitive 2)
(define sql-all-types 0)
(define sql-default 99)
(define sql-ard-type -99)
(define sql-code-date 1)
(define sql-code-time 2)
(define sql-code-timestamp 3)
(define sql-false 0)
(define sql-true 1)
(define sql-no-nulls 0)
(define sql-nullable 1)
(define sql-nullable-unknown 2)
(define sql-pred-none 0)
(define sql-pred-char 1)
(define sql-pred-basic 2)
(define sql-named 0)
(define sql-unnamed 1)
(define sql-desc-alloc-auto 1)
(define sql-desc-alloc-user 2)
(define sql-close 0)
(define sql-drop 1)
(define sql-unbind 2)
(define sql-reset-params 3)
(define sql-fetch-next 1)
(define sql-fetch-first 2)
(define sql-fetch-last 3)
(define sql-fetch-prior 4)
(define sql-fetch-absolute 5)
(define sql-fetch-relative 6)
(define sql-commit 0)
(define sql-rollback 1)
(define sql-null-henv 0)
(define sql-null-hdbc 0)
(define sql-null-hstmt 0)
(define sql-null-hdesc 0)
(define sql-null-handle 0)
(define sql-scope-currow 0)
(define sql-scope-transaction 1)
(define sql-scope-session 2)
(define sql-pc-unknown 0)
(define sql-pc-non-pseudo 1)
(define sql-pc-pseudo 2)
(define sql-row-identifier 1)
(define sql-index-unique 0)
(define sql-index-all 1)
(define sql-index-clustered 1)
(define sql-index-hashed 2)
(define sql-index-other 3)
(define sql-api-sqlallocconnect 1)
(define sql-api-sqlallocenv 2)
(define sql-api-sqlallochandle 1001)
(define sql-api-sqlallocstmt 3)
(define sql-api-sqlbindcol 4)
(define sql-api-sqlbindparam 1002)
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
(define sql-api-sqltransact 23)


;;; Information requested by SQLGetInfo()
(define sql-max-driver-connections 0)
(define sql-maximum-driver-connections 0)
(define sql-max-concurrent-activities 1)
(define sql-maximum-driver-connections 0)
(define sql-data-source-name 2)
(define sql-fetch-direction 8)
(define sql-server-name 13)
(define sql-search-pattern-escape 14)
(define sql-dbms-name 17)
(define sql-dbms-ver 18)
(define sql-accessible-tables 19)
(define sql-accessible-procedures 20)
(define sql-cursor-commit-behavior 23)
(define sql-data-source-read-only 25)
(define sql-default-txn-isolation 26)
(define sql-identifier-case 28)
(define sql-identifier-quote-char 29)
(define sql-max-column-name-len 30)
(define sql-maximum-column-name-length 30)
(define sql-max-cursor-name-len 31)
(define sql-maximum-cursor-name-length 31)
(define sql-max-schema-name-len 32)
(define sql-maximum-schema-name-length 32)
(define sql-max-catalog-name-len 34)
(define sql-maximum-catalog-name-length 34)
(define sql-max-table-name-len 35)
(define sql-scroll-concurrency 43)
(define sql-txn-capable 46)
(define sql-transaction-capable 46)
(define sql-user-name 47)
(define sql-txn-isolation-option 72)
(define sql-transaction-isolation-option 72)
(define sql-integrity 73)
(define sql-getdata-extensions 81)
(define sql-null-collation 85)
(define sql-alter-table 86)
(define sql-order-by-columns-in-select 90)
(define sql-special-characters 94)
(define sql-max-columns-in-group-by 97)
(define sql-maximum-columns-in-group-by 97)
(define sql-max-columns-in-index 98)
(define sql-maximum-columns-in-index 98)
(define sql-max-columns-in-order-by 99)
(define sql-maximum-columns-in-order-by 99)
(define sql-max-columns-in-select 100)
(define sql-maximum-columns-in-select 100)
(define sql-max-columns-in-table 101)
(define sql-max-index-size 102)
(define sql-maximum-index-size 102)
(define sql-max-row-size 104)
(define sql-maximum-row-size 104)
(define sql-max-statement-len 105)
(define sql-maximum-statement-length 105)
(define sql-max-tables-in-select 106)
(define sql-maximum-tables-in-select 106)
(define sql-max-user-name-len 107)
(define sql-maximum-user-name-length 107)
(define sql-oj-capabilities 115)
(define sql-outer-join-capabilities 115)
(define sql-xopen-cli-year 10000)
(define sql-cursor-sensitivity 10001)
(define sql-describe-parameter 10002)
(define sql-catalog-name 10003)
(define sql-collation-seq 10004)
(define sql-max-identifier-len 10005)
(define sql-maximum-identifier-length 10005)
(define sql-at-add-column 1)
(define sql-at-drop-column 2)
(define sql-at-add-constraint 8)


;;; SQL_ASYNC_MODE values
(define sql-am-none 0)
(define sql-am-connection 1)
(define sql-am-statement 2)


;;; SQL_CURSOR_COMMIT_BEHAVIOR values
(define sql-cb-delete 0)
(define sql-cb-close 1)
(define sql-cb-preserve 2)


;;; SQL_FETCH_DIRECTION bitmasks
(define sql-fd-fetch-next 1)
(define sql-fd-fetch-first 2)
(define sql-fd-fetch-last 4)
(define sql-fd-fetch-prior 8)
(define sql-fd-fetch-absolute 16)
(define sql-fd-fetch-relative 32)


;;; SQL_GETDATA_EXTENSIONS bitmasks
(define sql-gd-any-column 1)
(define sql-gd-any-order 2)


;;; SQL_IDENTIFIER_CASE values
(define sql-ic-upper 1)
(define sql-ic-lower 2)
(define sql-ic-sensitive 3)
(define sql-ic-mixed 4)


;;; SQL_OJ_CAPABILITIES bitmasks
(define sql-oj-left 1)
(define sql-oj-right 2)
(define sql-oj-full 4)
(define sql-oj-nested 8)
(define sql-oj-not-ordered 16)
(define sql-oj-inner 32)
(define sql-oj-all-comparison-ops 64)
(define sql-scco-read-only 1)
(define sql-scco-lock 2)
(define sql-scco-opt-rowver 4)
(define sql-scco-opt-values 8)
(define sql-tc-none 0)
(define sql-tc-dml 1)
(define sql-tc-all 2)
(define sql-tc-ddl-commit 3)
(define sql-tc-ddl-ignore 4)
(define sql-txn-read-uncommitted 1)
(define sql-transaction-read-uncommitted 1)
(define sql-txn-read-uncommitted 1)
(define sql-transaction-read-committed 2)
(define sql-txn-repeatable-read 4)
(define sql-transaction-repeatable-read 4)
(define sql-txn-serializable 8)
(define sql-transaction-serializable 8)
(define sql-nc-high 0)
(define sql-nc-low 1)


;;; constants from sqlext.h


;;; generally useful constants
(define sql-spec-major 3)
(define sql-spec-minor 51)
(define sql-spec-string 03.51)
(define sql-sqlstate-size 5)
(define sql-max-dsn-length 32)
(define sql-max-option-string-length 256)
(define sql-handle-senv 5)
(define sql-attr-odbc-version 200)
(define sql-attr-connection-pooling 201)
(define sql-attr-cp-match 202)


;;; values for SQL_ATTR_CONNECTION_POOLING
(define sql-cp-one-per-driver 1)
(define sql-cp-one-per-henv 2)
(define sql-cp-default 0)


;;; values for SQL_ATTR_CP_MATCH
(define sql-cp-strict-match 0)
(define sql-cp-relaxed-match 1)
(define sql-cp-match-default 0)


;;; values for SQL_ATTR_ODBC_VERSION
(define sql-ov-odbc2 2)
(define sql-ov-odbc3 3)


;;; connection attributes
(define sql-access-mode 101)
(define sql-autocommit 102)
(define sql-login-timeout 103)
(define sql-opt-trace 104)
(define sql-opt-tracefile 105)
(define sql-translate-dll 106)
(define sql-translate-option 107)
(define sql-txn-isolation 108)
(define sql-current-qualifier 109)
(define sql-odbc-cursors 110)
(define sql-quiet-mode 111)
(define sql-packet-size 112)


;;; connection attributes with new names
(define sql-attr-access-mode 101)
(define sql-attr-autocommit 102)
(define sql-attr-connection-timeout 113)
(define sql-attr-current-catalog 109)
(define sql-attr-disconnect-behavior 114)
(define sql-attr-enlist-in-dtc 1207)
(define sql-attr-enlist-in-xa 1208)
(define sql-attr-login-timeout 103)
(define sql-attr-odbc-cursors 110)
(define sql-attr-packet-size 112)
(define sql-attr-quiet-mode 111)
(define sql-attr-trace 104)
(define sql-attr-tracefile 105)
(define sql-attr-translate-lib 106)
(define sql-attr-translate-option 107)
(define sql-attr-txn-isolation 108)
(define sql-attr-connection-dead 1209)


;;; values for SQL_ATTR_DISCONNECT_BEHAVIOR
(define sql-db-return-to-pool 0)
(define sql-db-disconnect 1)
(define sql-db-default 0)
(define sql-dtc-done 0)


;;; values for SQL_ATTR_CONNECTION_DEAD
(define sql-cd-true 1)
(define sql-cd-false 0)


;;; statement attributes
(define sql-query-timeout 0)
(define sql-max-rows 1)
(define sql-noscan 2)
(define sql-max-length 3)
(define sql-async-enable 4)
(define sql-bind-type 5)
(define sql-cursor-type 6)
(define sql-concurrency 7)
(define sql-keyset-size 8)
(define sql-rowset-size 9)
(define sql-simulate-cursor 10)
(define sql-retrieve-data 11)
(define sql-use-bookmarks 12)
(define sql-get-bookmark 13)
(define sql-row-number 14)


;;; statement attributes for ODBC 3.0
(define sql-attr-async-enable 4)
(define sql-attr-concurrency 7)
(define sql-attr-cursor-type 6)
(define sql-attr-enable-auto-ipd 15)
(define sql-attr-fetch-bookmark-ptr 16)
(define sql-attr-keyset-size 8)
(define sql-attr-max-length 3)
(define sql-attr-max-rows 1)
(define sql-attr-noscan 2)
(define sql-attr-param-bind-offset-ptr 17)
(define sql-attr-param-bind-type 18)
(define sql-attr-param-operation-ptr 19)
(define sql-attr-param-status-ptr 20)
(define sql-attr-params-processed-ptr 21)
(define sql-attr-paramset-size 22)
(define sql-attr-query-timeout 0)
(define sql-attr-retrieve-data 11)
(define sql-attr-row-bind-offset-ptr 23)
(define sql-attr-row-bind-type 5)
(define sql-attr-row-number 14)
(define sql-attr-row-operation-ptr 24)
(define sql-attr-row-status-ptr 25)
(define sql-attr-rows-fetched-ptr 26)
(define sql-attr-row-array-size 27)
(define sql-attr-simulate-cursor 10)
(define sql-attr-use-bookmarks 12)


;;; New defines for SEARCHABLE column in SQLGetTypeInfo
(define sql-col-pred-char 1)
(define sql-col-pred-basic 2)


;;; whether an attribute is a pointer or not
(define sql-is-pointer -4)
(define sql-is-uinteger -5)
(define sql-is-integer -6)
(define sql-is-usmallint -7)
(define sql-is-smallint -8)


;;; the value of SQL_ATTR_PARAM_BIND_TYPE
(define sql-param-bind-by-column 0)
(define sql-param-bind-type-default 0)
(define sql-query-timeout-default 0)
(define sql-max-rows-default 0)
(define sql-noscan-off 0)
(define sql-noscan-on 1)
(define sql-noscan-default 0)
(define sql-max-length-default 0)
(define sql-async-enable-off 0)
(define sql-async-enable-on 1)
(define sql-async-enable-default 0)
(define sql-bind-by-column 0)
(define sql-bind-type-default 0)
(define sql-concur-read-only 1)
(define sql-concur-lock 2)
(define sql-concur-rowver 3)
(define sql-concur-values 4)
(define sql-concur-default 1)
(define sql-cursor-forward-only 0)
(define sql-cursor-keyset-driven 1)
(define sql-cursor-dynamic 2)
(define sql-cursor-static 3)
(define sql-cursor-type-default 0)
(define sql-rowset-size-default 1)
(define sql-keyset-size-default 0)
(define sql-sc-non-unique 0)
(define sql-sc-try-unique 1)
(define sql-sc-unique 2)
(define sql-rd-off 0)
(define sql-rd-on 1)
(define sql-rd-default 1)
(define sql-ub-off 0)
(define sql-ub-on 1)
(define sql-ub-default 0)
(define sql-ub-fixed 1)
(define sql-ub-variable 2)


;;; extended descriptor field
(define sql-desc-array-size 20)
(define sql-desc-array-status-ptr 21)
(define sql-desc-auto-unique-value 11)
(define sql-desc-base-column-name 22)
(define sql-desc-base-table-name 23)
(define sql-desc-bind-offset-ptr 24)
(define sql-desc-bind-type 25)
(define sql-desc-case-sensitive 12)
(define sql-desc-catalog-name 17)
(define sql-desc-concise-type 2)
(define sql-desc-datetime-interval-precision 26)
(define sql-desc-display-size 6)
(define sql-desc-fixed-prec-scale 9)
(define sql-desc-label 18)
(define sql-desc-literal-prefix 27)
(define sql-desc-literal-suffix 28)
(define sql-desc-local-type-name 29)
(define sql-desc-maximum-scale 30)
(define sql-desc-minimum-scale 31)
(define sql-desc-num-prec-radix 32)
(define sql-desc-parameter-type 33)
(define sql-desc-rows-processed-ptr 34)
(define sql-desc-schema-name 16)
(define sql-desc-searchable 13)
(define sql-desc-type-name 14)
(define sql-desc-table-name 15)
(define sql-desc-unsigned 8)
(define sql-desc-updatable 10)
(define sql-diag-cursor-row-count -1249)
(define sql-diag-row-number -1248)
(define sql-diag-column-number -1247)


;;; SQL extended datatypes
(define sql-date 9)
(define sql-interval 10)
(define sql-time 10)
(define sql-timestamp 11)
(define sql-longvarchar -1)
(define sql-binary -2)
(define sql-varbinary -3)
(define sql-longvarbinary -4)
(define sql-bigint -5)
(define sql-tinyint -6)
(define sql-bit -7)


;;; interval code
(define sql-code-year 1)
(define sql-code-month 2)
(define sql-code-day 3)
(define sql-code-hour 4)
(define sql-code-minute 5)
(define sql-code-second 6)
(define sql-code-year-to-month 7)
(define sql-code-day-to-hour 8)
(define sql-code-day-to-minute 9)
(define sql-code-day-to-second 10)
(define sql-code-hour-to-minute 11)
(define sql-code-hour-to-second 12)
(define sql-code-minute-to-second 13)
(define sql-interval-year 101)
(define sql-interval-month 102)
(define sql-interval-day 103)
(define sql-interval-hour 104)
(define sql-interval-minute 105)
(define sql-interval-second 106)
(define sql-interval-year-to-month 107)
(define sql-interval-day-to-hour 108)
(define sql-interval-day-to-minute 109)
(define sql-interval-day-to-second 110)
(define sql-interval-hour-to-minute 111)
(define sql-interval-hour-to-second 112)
(define sql-interval-minute-to-second 113)


;;; C datatype to SQL datatype mapping
(define sql-c-char 1)
(define sql-c-long 4)
(define sql-c-short 5)
(define sql-c-float 7)
(define sql-c-double 8)
(define sql-c-numeric 2)
(define sql-signed-offset -20)
(define sql-unsigned-offset -22)


;;; C datatype to SQL datatype mapping
(define sql-c-date 9)
(define sql-c-time 10)
(define sql-c-timestamp 11)
(define sql-c-type-date 91)
(define sql-c-type-time 92)
(define sql-c-type-timestamp 93)
(define sql-c-interval-year 101)
(define sql-c-interval-month 102)
(define sql-c-interval-day 103)
(define sql-c-interval-hour 104)
(define sql-c-interval-minute 105)
(define sql-c-interval-second 106)
(define sql-c-interval-year-to-month 107)
(define sql-c-interval-day-to-hour 108)
(define sql-c-interval-day-to-minute 109)
(define sql-c-interval-day-to-second 110)
(define sql-c-interval-hour-to-minute 111)
(define sql-c-interval-hour-to-second 112)
(define sql-c-interval-minute-to-second 113)
(define sql-c-binary -2)
(define sql-c-bit -7)
(define sql-c-sbigint -25)
(define sql-c-ubigint -27)
(define sql-c-tinyint -6)
(define sql-c-slong -16)
(define sql-c-sshort -15)
(define sql-c-stinyint -26)
(define sql-c-ulong -18)
(define sql-c-ushort -17)
(define sql-c-utinyint -28)
(define sql-c-bookmark -18)
(define sql-c-varbookmark -2)


;;; define for SQL_DIAG_ROW_NUMBER and SQL_DIAG_COLUMN_NUMBER
(define sql-no-row-number -1)
(define sql-no-column-number -1)
(define sql-row-number-unknown -2)
(define sql-column-number-unknown -2)


;;; SQLBindParameter extensions
(define sql-default-param -5)
(define sql-ignore -6)
(define sql-column-ignore -6)
(define sql-len-data-at-exec-offset -100)
(define sql-param-type-default 2)
(define sql-setparam-value-max -1)


;;; SQLColAttributes defines
(define sql-column-count 0)
(define sql-column-name 1)
(define sql-column-type 2)
(define sql-column-length 3)
(define sql-column-precision 4)
(define sql-column-scale 5)
(define sql-column-display-size 6)
(define sql-column-nullable 7)
(define sql-column-unsigned 8)
(define sql-column-money 9)
(define sql-column-updatable 10)
(define sql-column-auto-increment 11)
(define sql-column-case-sensitive 12)
(define sql-column-searchable 13)
(define sql-column-type-name 14)
(define sql-column-table-name 15)
(define sql-column-owner-name 16)
(define sql-column-qualifier-name 17)
(define sql-column-label 18)
(define sql-colatt-opt-max 18)
(define sql-colatt-opt-min 0)


;;; SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE
(define sql-attr-readonly 0)
(define sql-attr-write 1)
(define sql-attr-readwrite-unknown 2)


;;; SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE. These are also used by SQLGetInfo
(define sql-unsearchable 0)
(define sql-like-only 1)
(define sql-all-except-like 2)
(define sql-searchable 3)
(define sql-pred-searchable 3)


;;; Special return values for SQLGetData
(define sql-no-total -4)


;;; SQLGetFunctions: additional values for Function to represent functions that are not in the X/Open spec.
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
(define sql-api-sqlprocedures 67)
(define sql-api-sqlsetpos 68)
(define sql-api-sqlsetscrolloptions 69)
(define sql-api-sqltableprivileges 70)
(define sql-api-all-functions 0)
(define sql-api-loadbyordinal 199)
(define sql-api-odbc3-all-functions 999)
(define sql-api-odbc3-all-functions-size 250)


;;; Extended definitions for SQLGetInfo
(define sql-info-first 0)
(define sql-active-connections 0)
(define sql-active-statements 1)
(define sql-driver-hdbc 3)
(define sql-driver-henv 4)
(define sql-driver-hstmt 5)
(define sql-driver-name 6)
(define sql-driver-ver 7)
(define sql-odbc-api-conformance 9)
(define sql-odbc-ver 10)
(define sql-row-updates 11)
(define sql-odbc-sag-cli-conformance 12)
(define sql-odbc-sql-conformance 15)
(define sql-procedures 21)
(define sql-concat-null-behavior 22)
(define sql-cursor-rollback-behavior 24)
(define sql-expressions-in-orderby 27)
(define sql-max-owner-name-len 32)
(define sql-max-procedure-name-len 33)
(define sql-max-qualifier-name-len 34)
(define sql-mult-result-sets 36)
(define sql-multiple-active-txn 37)
(define sql-outer-joins 38)
(define sql-owner-term 39)
(define sql-procedure-term 40)
(define sql-qualifier-name-separator 41)
(define sql-qualifier-term 42)
(define sql-scroll-options 44)
(define sql-table-term 45)
(define sql-convert-functions 48)
(define sql-numeric-functions 49)
(define sql-string-functions 50)
(define sql-system-functions 51)
(define sql-timedate-functions 52)
(define sql-convert-bigint 53)
(define sql-convert-binary 54)
(define sql-convert-bit 55)
(define sql-convert-char 56)
(define sql-convert-date 57)
(define sql-convert-decimal 58)
(define sql-convert-double 59)
(define sql-convert-float 60)
(define sql-convert-integer 61)
(define sql-convert-longvarchar 62)
(define sql-convert-numeric 63)
(define sql-convert-real 64)
(define sql-convert-smallint 65)
(define sql-convert-time 66)
(define sql-convert-timestamp 67)
(define sql-convert-tinyint 68)
(define sql-convert-varbinary 69)
(define sql-convert-varchar 70)
(define sql-convert-longvarbinary 71)
(define sql-odbc-sql-opt-ief 73)
(define sql-correlation-name 74)
(define sql-non-nullable-columns 75)
(define sql-driver-hlib 76)
(define sql-driver-odbc-ver 77)
(define sql-lock-types 78)
(define sql-pos-operations 79)
(define sql-positioned-statements 80)
(define sql-bookmark-persistence 82)
(define sql-static-sensitivity 83)
(define sql-file-usage 84)
(define sql-column-alias 87)
(define sql-group-by 88)
(define sql-keywords 89)
(define sql-owner-usage 91)
(define sql-qualifier-usage 92)
(define sql-quoted-identifier-case 93)
(define sql-subqueries 95)
(define sql-union 96)
(define sql-max-row-size-includes-long 103)
(define sql-max-char-literal-len 108)
(define sql-timedate-add-intervals 109)
(define sql-timedate-diff-intervals 110)
(define sql-need-long-data-len 111)
(define sql-max-binary-literal-len 112)
(define sql-like-escape-clause 113)
(define sql-qualifier-location 114)


;;; ODBC 3.0 SQLGetInfo values that are not part of the X/Open standard at this time.   X/Open standard values are in sql.h.
(define sql-active-environments 116)
(define sql-alter-domain 117)
(define sql-sql-conformance 118)
(define sql-datetime-literals 119)
(define sql-async-mode 10021)
(define sql-batch-row-count 120)
(define sql-batch-support 121)
(define sql-catalog-location 114)
(define sql-catalog-name-separator 41)
(define sql-catalog-term 42)
(define sql-catalog-usage 92)
(define sql-convert-wchar 122)
(define sql-convert-interval-day-time 123)
(define sql-convert-interval-year-month 124)
(define sql-convert-wlongvarchar 125)
(define sql-convert-wvarchar 126)
(define sql-create-assertion 127)
(define sql-create-character-set 128)
(define sql-create-collation 129)
(define sql-create-domain 130)
(define sql-create-schema 131)
(define sql-create-table 132)
(define sql-create-translation 133)
(define sql-create-view 134)
(define sql-driver-hdesc 135)
(define sql-drop-assertion 136)
(define sql-drop-character-set 137)
(define sql-drop-collation 138)
(define sql-drop-domain 139)
(define sql-drop-schema 140)
(define sql-drop-table 141)
(define sql-drop-translation 142)
(define sql-drop-view 143)
(define sql-dynamic-cursor-attributes1 144)
(define sql-dynamic-cursor-attributes2 145)
(define sql-forward-only-cursor-attributes1 146)
(define sql-forward-only-cursor-attributes2 147)
(define sql-index-keywords 148)
(define sql-info-schema-views 149)
(define sql-keyset-cursor-attributes1 150)
(define sql-keyset-cursor-attributes2 151)
(define sql-max-async-concurrent-statements 10022)
(define sql-odbc-interface-conformance 152)
(define sql-param-array-row-counts 153)
(define sql-param-array-selects 154)
(define sql-schema-term 39)
(define sql-schema-usage 91)
(define sql-sql92-datetime-functions 155)
(define sql-sql92-foreign-key-delete-rule 156)
(define sql-sql92-foreign-key-update-rule 157)
(define sql-sql92-grant 158)
(define sql-sql92-numeric-value-functions 159)
(define sql-sql92-predicates 160)
(define sql-sql92-relational-join-operators 161)
(define sql-sql92-revoke 162)
(define sql-sql92-row-value-constructor 163)
(define sql-sql92-string-functions 164)
(define sql-sql92-value-expressions 165)
(define sql-standard-cli-conformance 166)
(define sql-static-cursor-attributes1 167)
(define sql-static-cursor-attributes2 168)
(define sql-aggregate-functions 169)
(define sql-ddl-index 170)
(define sql-dm-ver 171)
(define sql-insert-statement 172)
(define sql-union-statement 96)


;;; SQL_ALTER_TABLE bitmasks
(define sql-at-add-column-single 32)
(define sql-at-add-column-default 64)
(define sql-at-add-column-collation 128)
(define sql-at-set-column-default 256)
(define sql-at-drop-column-default 512)
(define sql-at-drop-column-cascade 1024)
(define sql-at-drop-column-restrict 2048)
(define sql-at-add-table-constraint 4096)
(define sql-at-drop-table-constraint-cascade 8192)
(define sql-at-drop-table-constraint-restrict 16384)
(define sql-at-constraint-name-definition 32768)
(define sql-at-constraint-initially-deferred 65536)
(define sql-at-constraint-initially-immediate 131072)
(define sql-at-constraint-deferrable 262144)
(define sql-at-constraint-non-deferrable 524288)


;;; SQL_CONVERT_*  return value bitmasks
(define sql-cvt-char 1)
(define sql-cvt-numeric 2)
(define sql-cvt-decimal 4)
(define sql-cvt-integer 8)
(define sql-cvt-smallint 16)
(define sql-cvt-float 32)
(define sql-cvt-real 64)
(define sql-cvt-double 128)
(define sql-cvt-varchar 256)
(define sql-cvt-longvarchar 512)
(define sql-cvt-binary 1024)
(define sql-cvt-varbinary 2048)
(define sql-cvt-bit 4096)
(define sql-cvt-tinyint 8192)
(define sql-cvt-bigint 16384)
(define sql-cvt-date 32768)
(define sql-cvt-time 65536)
(define sql-cvt-timestamp 131072)
(define sql-cvt-longvarbinary 262144)
(define sql-cvt-interval-year-month 524288)
(define sql-cvt-interval-day-time 1048576)
(define sql-cvt-wchar 2097152)
(define sql-cvt-wlongvarchar 4194304)
(define sql-cvt-wvarchar 8388608)


;;; SQL_STRING_FUNCTIONS functions
(define sql-fn-str-concat 1)
(define sql-fn-str-insert 2)
(define sql-fn-str-left 4)
(define sql-fn-str-ltrim 8)
(define sql-fn-str-length 16)
(define sql-fn-str-locate 32)
(define sql-fn-str-lcase 64)
(define sql-fn-str-repeat 128)
(define sql-fn-str-replace 256)
(define sql-fn-str-right 512)
(define sql-fn-str-rtrim 1024)
(define sql-fn-str-substring 2048)
(define sql-fn-str-ucase 4096)
(define sql-fn-str-ascii 8192)
(define sql-fn-str-char 16384)
(define sql-fn-str-difference 32768)
(define sql-fn-str-locate-2 65536)
(define sql-fn-str-soundex 131072)
(define sql-fn-str-space 262144)
(define sql-fn-str-bit-length 524288)
(define sql-fn-str-char-length 1048576)
(define sql-fn-str-character-length 2097152)
(define sql-fn-str-octet-length 4194304)
(define sql-fn-str-position 8388608)


;;; SQL_SQL92_STRING_FUNCTIONS
(define sql-ssf-convert 1)
(define sql-ssf-lower 2)
(define sql-ssf-upper 4)
(define sql-ssf-substring 8)
(define sql-ssf-translate 16)
(define sql-ssf-trim-both 32)
(define sql-ssf-trim-leading 64)
(define sql-ssf-trim-trailing 128)


;;; SQL_NUMERIC_FUNCTIONS functions
(define sql-fn-num-abs 1)
(define sql-fn-num-acos 2)
(define sql-fn-num-asin 4)
(define sql-fn-num-atan 8)
(define sql-fn-num-atan2 16)
(define sql-fn-num-ceiling 32)
(define sql-fn-num-cos 64)
(define sql-fn-num-cot 128)
(define sql-fn-num-exp 256)
(define sql-fn-num-floor 512)
(define sql-fn-num-log 1024)
(define sql-fn-num-mod 2048)
(define sql-fn-num-sign 4096)
(define sql-fn-num-sin 8192)
(define sql-fn-num-sqrt 16384)
(define sql-fn-num-tan 32768)
(define sql-fn-num-pi 65536)
(define sql-fn-num-rand 131072)
(define sql-fn-num-degrees 262144)
(define sql-fn-num-log10 524288)
(define sql-fn-num-power 1048576)
(define sql-fn-num-radians 2097152)
(define sql-fn-num-round 4194304)
(define sql-fn-num-truncate 8388608)
(define sql-snvf-bit-length 1)
(define sql-snvf-char-length 2)
(define sql-snvf-character-length 4)
(define sql-snvf-extract 8)
(define sql-snvf-octet-length 16)
(define sql-snvf-position 32)


;;; SQL_TIMEDATE_FUNCTIONS functions
(define sql-fn-td-now 1)
(define sql-fn-td-curdate 2)
(define sql-fn-td-dayofmonth 4)
(define sql-fn-td-dayofweek 8)
(define sql-fn-td-dayofyear 16)
(define sql-fn-td-month 32)
(define sql-fn-td-quarter 64)
(define sql-fn-td-week 128)
(define sql-fn-td-year 256)
(define sql-fn-td-curtime 512)
(define sql-fn-td-hour 1024)
(define sql-fn-td-minute 2048)
(define sql-fn-td-second 4096)
(define sql-fn-td-timestampadd 8192)
(define sql-fn-td-timestampdiff 16384)
(define sql-fn-td-dayname 32768)
(define sql-fn-td-monthname 65536)
(define sql-fn-td-current-date 131072)
(define sql-fn-td-current-time 262144)
(define sql-fn-td-current-timestamp 524288)
(define sql-fn-td-extract 1048576)


;;; SQL_SQL92_DATETIME_FUNCTIONS
(define sql-sdf-current-date 1)
(define sql-sdf-current-time 2)
(define sql-sdf-current-timestamp 4)


;;; SQL_SYSTEM_FUNCTIONS functions
(define sql-fn-sys-username 1)
(define sql-fn-sys-dbname 2)
(define sql-fn-sys-ifnull 4)


;;; SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions
(define sql-fn-tsi-frac-second 1)
(define sql-fn-tsi-second 2)
(define sql-fn-tsi-minute 4)
(define sql-fn-tsi-hour 8)
(define sql-fn-tsi-day 16)
(define sql-fn-tsi-week 32)
(define sql-fn-tsi-month 64)
(define sql-fn-tsi-quarter 128)
(define sql-fn-tsi-year 256)


;;; supported SQLFetchScroll FetchOrientation's
(define sql-ca1-next 1)
(define sql-ca1-absolute 2)
(define sql-ca1-relative 4)
(define sql-ca1-bookmark 8)


;;; supported SQLSetPos LockType's
(define sql-ca1-lock-no-change 64)
(define sql-ca1-lock-exclusive 128)
(define sql-ca1-lock-unlock 256)


;;; supported SQLSetPos Operations
(define sql-ca1-pos-position 512)
(define sql-ca1-pos-update 1024)
(define sql-ca1-pos-delete 2048)
(define sql-ca1-pos-refresh 4096)


;;; positioned updates and delete
(define sql-ca1-positioned-update 8192)
(define sql-ca1-positioned-delete 16384)
(define sql-ca1-select-for-update 32768)


;;; supported SQLBulkOperations operations
(define sql-ca1-bulk-add 65536)
(define sql-ca1-bulk-update-by-bookmark 131072)
(define sql-ca1-bulk-delete-by-bookmark 262144)
(define sql-ca1-bulk-fetch-by-bookmark 524288)


;;; supported values for SQL_ATTR_SCROLL_CONCURRENCY 
(define sql-ca2-read-only-concurrency 1)
(define sql-ca2-lock-concurrency 2)
(define sql-ca2-opt-rowver-concurrency 4)
(define sql-ca2-opt-values-concurrency 8)


;;; sensitivity of the cursor to its own inserts, deletes, and updates
(define sql-ca2-sensitivity-additions 16)
(define sql-ca2-sensitivity-deletions 32)
(define sql-ca2-sensitivity-updates 64)


;;; semantics of SQL_ATTR_MAX_ROWS
(define sql-ca2-max-rows-select 128)
(define sql-ca2-max-rows-insert 256)
(define sql-ca2-max-rows-delete 512)
(define sql-ca2-max-rows-update 1024)
(define sql-ca2-max-rows-catalog 2048)
(define sql-ca2-max-rows-affects-all 3968)


;;; semantics of SQL_DIAG_CURSOR_ROW_COUNT
(define sql-ca2-crc-exact 4096)
(define sql-ca2-crc-approximate 8192)


;;; the kinds of positioned statements that can be simulated
(define sql-ca2-simulate-non-unique 16384)
(define sql-ca2-simulate-try-unique 32768)
(define sql-ca2-simulate-unique 65536)


;;; SQL_ODBC_API_CONFORMANCE values
(define sql-oac-none 0)
(define sql-oac-level1 1)
(define sql-oac-level2 2)


;;; SQL_ODBC_SAG_CLI_CONFORMANCE values
(define sql-oscc-not-compliant 0)
(define sql-oscc-compliant 1)


;;; SQL_ODBC_SQL_CONFORMANCE values
(define sql-osc-minimum 0)
(define sql-osc-core 1)
(define sql-osc-extended 2)


;;; SQL_CONCAT_NULL_BEHAVIOR values
(define sql-cb-null 0)
(define sql-cb-non-null 1)


;;; SQL_SCROLL_OPTIONS masks
(define sql-so-forward-only 1)
(define sql-so-keyset-driven 2)
(define sql-so-dynamic 4)
(define sql-so-mixed 8)
(define sql-so-static 16)
(define sql-fd-fetch-bookmark 128)


;;; SQL_CORRELATION_NAME values
(define sql-cn-none 0)
(define sql-cn-different 1)
(define sql-cn-any 2)


;;; SQL_NON_NULLABLE_COLUMNS values
(define sql-nnc-null 0)
(define sql-nnc-non-null 1)


;;; SQL_NULL_COLLATION values
(define sql-nc-start 2)
(define sql-nc-end 4)


;;; SQL_FILE_USAGE values
(define sql-file-not-supported 0)
(define sql-file-table 1)
(define sql-file-qualifier 2)
(define sql-file-catalog 2)


;;; SQL_GETDATA_EXTENSIONS values
(define sql-gd-block 4)
(define sql-gd-bound 8)


;;; SQL_POSITIONED_STATEMENTS masks
(define sql-ps-positioned-delete 1)
(define sql-ps-positioned-update 2)
(define sql-ps-select-for-update 4)


;;; SQL_GROUP_BY values
(define sql-gb-not-supported 0)
(define sql-gb-group-by-equals-select 1)
(define sql-gb-group-by-contains-select 2)
(define sql-gb-no-relation 3)
(define sql-gb-collate 4)


;;; SQL_OWNER_USAGE masks
(define sql-ou-dml-statements 1)
(define sql-ou-procedure-invocation 2)
(define sql-ou-table-definition 4)
(define sql-ou-index-definition 8)
(define sql-ou-privilege-definition 16)


;;; SQL_SCHEMA_USAGE masks
(define sql-su-dml-statements 1)
(define sql-su-procedure-invocation 2)
(define sql-su-table-definition 4)
(define sql-su-index-definition 8)
(define sql-su-privilege-definition 16)


;;; SQL_QUALIFIER_USAGE masks
(define sql-qu-dml-statements 1)
(define sql-qu-procedure-invocation 2)
(define sql-qu-table-definition 4)
(define sql-qu-index-definition 8)
(define sql-qu-privilege-definition 16)


;;; SQL_CATALOG_USAGE masks
(define sql-cu-dml-statements 1)
(define sql-cu-procedure-invocation 2)
(define sql-cu-table-definition 4)
(define sql-cu-index-definition 8)
(define sql-cu-privilege-definition 16)


;;; SQL_SUBQUERIES masks
(define sql-sq-comparison 1)
(define sql-sq-exists 2)
(define sql-sq-in 4)
(define sql-sq-quantified 8)
(define sql-sq-correlated-subqueries 16)


;;; SQL_UNION masks
(define sql-u-union 1)
(define sql-u-union-all 2)


;;; SQL_BOOKMARK_PERSISTENCE values
(define sql-bp-close 1)
(define sql-bp-delete 2)
(define sql-bp-drop 4)
(define sql-bp-transaction 8)
(define sql-bp-update 16)
(define sql-bp-other-hstmt 32)
(define sql-bp-scroll 64)


;;; SQL_STATIC_SENSITIVITY values
(define sql-ss-additions 1)
(define sql-ss-deletions 2)
(define sql-ss-updates 4)


;;; SQL_VIEW values
(define sql-cv-create-view 1)
(define sql-cv-check-option 2)
(define sql-cv-cascaded 4)
(define sql-cv-local 8)


;;; SQL_LOCK_TYPES masks
(define sql-lck-no-change 1)
(define sql-lck-exclusive 2)
(define sql-lck-unlock 4)


;;; SQL_POS_OPERATIONS masks
(define sql-pos-position 1)
(define sql-pos-refresh 2)
(define sql-pos-update 4)
(define sql-pos-delete 8)
(define sql-pos-add 16)


;;; SQL_QUALIFIER_LOCATION values
(define sql-ql-start 1)
(define sql-ql-end 2)


;;; Here start return values for ODBC 3.0 SQLGetInfo


;;; SQL_AGGREGATE_FUNCTIONS bitmasks
(define sql-af-avg 1)
(define sql-af-count 2)
(define sql-af-max 4)
(define sql-af-min 8)
(define sql-af-sum 16)
(define sql-af-distinct 32)
(define sql-af-all 64)


;;; SQL_SQL_CONFORMANCE bit masks
(define sql-sc-fips127-2-transitional 2)
(define sql-sc-sql92-intermediate 4)
(define sql-sc-sql92-full 8)


;;; SQL_DATETIME_LITERALS masks
(define sql-dl-sql92-date 1)
(define sql-dl-sql92-time 2)
(define sql-dl-sql92-timestamp 4)
(define sql-dl-sql92-interval-year 8)
(define sql-dl-sql92-interval-month 16)
(define sql-dl-sql92-interval-day 32)
(define sql-dl-sql92-interval-hour 64)
(define sql-dl-sql92-interval-minute 128)
(define sql-dl-sql92-interval-second 256)
(define sql-dl-sql92-interval-year-to-month 512)
(define sql-dl-sql92-interval-day-to-hour 1024)
(define sql-dl-sql92-interval-day-to-minute 2048)
(define sql-dl-sql92-interval-day-to-second 4096)
(define sql-dl-sql92-interval-hour-to-minute 8192)
(define sql-dl-sql92-interval-hour-to-second 16384)
(define sql-dl-sql92-interval-minute-to-second 32768)


;;; SQL_CATALOG_LOCATION values
(define sql-cl-start 1)
(define sql-cl-end 2)


;;; values for SQL_BATCH_ROW_COUNT
(define sql-brc-procedures 1)
(define sql-brc-explicit 2)
(define sql-brc-rolled-up 4)


;;; bitmasks for SQL_BATCH_SUPPORT
(define sql-bs-select-explicit 1)
(define sql-bs-row-count-explicit 2)
(define sql-bs-select-proc 4)
(define sql-bs-row-count-proc 8)


;;; Values for SQL_PARAM_ARRAY_ROW_COUNTS getinfo
(define sql-parc-batch 1)
(define sql-parc-no-batch 2)


;;; values for SQL_PARAM_ARRAY_SELECTS
(define sql-pas-batch 1)
(define sql-pas-no-batch 2)
(define sql-pas-no-select 3)


;;; Bitmasks for SQL_INDEX_KEYWORDS
(define sql-ik-none 0)
(define sql-ik-asc 1)
(define sql-ik-desc 2)
(define sql-ik-all 3)


;;; Bitmasks for SQL_INFO_SCHEMA_VIEWS
(define sql-isv-assertions 1)
(define sql-isv-character-sets 2)
(define sql-isv-check-constraints 4)
(define sql-isv-collations 8)
(define sql-isv-column-domain-usage 16)
(define sql-isv-column-privileges 32)
(define sql-isv-columns 64)
(define sql-isv-constraint-column-usage 128)
(define sql-isv-constraint-table-usage 256)
(define sql-isv-domain-constraints 512)
(define sql-isv-domains 1024)
(define sql-isv-key-column-usage 2048)
(define sql-isv-referential-constraints 4096)
(define sql-isv-schemata 8192)
(define sql-isv-sql-languages 16384)
(define sql-isv-table-constraints 32768)
(define sql-isv-table-privileges 65536)
(define sql-isv-tables 131072)
(define sql-isv-translations 262144)
(define sql-isv-usage-privileges 524288)
(define sql-isv-view-column-usage 1048576)
(define sql-isv-view-table-usage 2097152)
(define sql-isv-views 4194304)


;;; Bitmasks for SQL_ASYNC_MODE
(define sql-am-none 0)
(define sql-am-connection 1)
(define sql-am-statement 2)


;;; Bitmasks for SQL_ALTER_DOMAIN
(define sql-ad-constraint-name-definition 1)
(define sql-ad-add-domain-constraint 2)
(define sql-ad-drop-domain-constraint 4)
(define sql-ad-add-domain-default 8)
(define sql-ad-drop-domain-default 16)
(define sql-ad-add-constraint-initially-deferred 32)
(define sql-ad-add-constraint-initially-immediate 64)
(define sql-ad-add-constraint-deferrable 128)
(define sql-ad-add-constraint-non-deferrable 256)


;;; SQL_CREATE_SCHEMA bitmasks
(define sql-cs-create-schema 1)
(define sql-cs-authorization 2)
(define sql-cs-default-character-set 4)


;;; SQL_CREATE_TRANSLATION bitmasks
(define sql-ctr-create-translation 1)


;;; SQL_CREATE_ASSERTION bitmasks
(define sql-ca-create-assertion 1)
(define sql-ca-constraint-initially-deferred 16)
(define sql-ca-constraint-initially-immediate 32)
(define sql-ca-constraint-deferrable 64)
(define sql-ca-constraint-non-deferrable 128)


;;; SQL_CREATE_CHARACTER_SET bitmasks
(define sql-ccs-create-character-set 1)
(define sql-ccs-collate-clause 2)
(define sql-ccs-limited-collation 4)


;;; SQL_CREATE_COLLATION bitmasks
(define sql-ccol-create-collation 1)


;;; SQL_CREATE_DOMAIN bitmasks
(define sql-cdo-create-domain 1)
(define sql-cdo-default 2)
(define sql-cdo-constraint 4)
(define sql-cdo-collation 8)
(define sql-cdo-constraint-name-definition 16)
(define sql-cdo-constraint-initially-deferred 32)
(define sql-cdo-constraint-initially-immediate 64)
(define sql-cdo-constraint-deferrable 128)
(define sql-cdo-constraint-non-deferrable 256)


;;; SQL_CREATE_TABLE bitmasks
(define sql-ct-create-table 1)
(define sql-ct-commit-preserve 2)
(define sql-ct-commit-delete 4)
(define sql-ct-global-temporary 8)
(define sql-ct-local-temporary 16)
(define sql-ct-constraint-initially-deferred 32)
(define sql-ct-constraint-initially-immediate 64)
(define sql-ct-constraint-deferrable 128)
(define sql-ct-constraint-non-deferrable 256)
(define sql-ct-column-constraint 512)
(define sql-ct-column-default 1024)
(define sql-ct-column-collation 2048)
(define sql-ct-table-constraint 4096)
(define sql-ct-constraint-name-definition 8192)


;;; SQL_DDL_INDEX bitmasks
(define sql-di-create-index 1)
(define sql-di-drop-index 2)


;;; SQL_DROP_COLLATION bitmasks
(define sql-dc-drop-collation 1)


;;; SQL_DROP_DOMAIN bitmask
(define sql-dd-drop-domain 1)
(define sql-dd-restrict 2)
(define sql-dd-cascade 4)


;;; SQL_DROP_SCHEMA bitmasks
(define sql-ds-drop-schema 1)
(define sql-ds-restrict 2)
(define sql-ds-cascade 4)


;;; SQL_DROP_CHARACTER_SET bitmasks
(define sql-dcs-drop-character-set 1)


;;; SQL_DROP_ASSERTION bitmasks
(define sql-da-drop-assertion 1)


;;; SQL_DROP_TABLE bitmasks
(define sql-dt-drop-table 1)
(define sql-dt-restrict 2)
(define sql-dt-cascade 4)


;;; SQL_DROP_TRANSLATION bitmasks
(define sql-dtr-drop-translation 1)


;;; SQL_DROP_VIEW bitmasks
(define sql-dv-drop-view 1)
(define sql-dv-restrict 2)
(define sql-dv-cascade 4)


;;; SQL_INSERT_STATEMENT bitmasks
(define sql-is-insert-literals 1)
(define sql-is-insert-searched 2)
(define sql-is-select-into 4)


;;; SQL_ODBC_INTERFACE_CONFORMANCE values
(define sql-oic-core 1)
(define sql-oic-level1 2)
(define sql-oic-level2 3)


;;; SQL_SQL92_FOREIGN_KEY_DELETE_RULE bitmasks
(define sql-sfkd-cascade 1)
(define sql-sfkd-no-action 2)
(define sql-sfkd-set-default 4)
(define sql-sfkd-set-null 8)


;;; SQL_SQL92_FOREIGN_KEY_UPDATE_RULE bitmasks
(define sql-sfku-cascade 1)
(define sql-sfku-no-action 2)
(define sql-sfku-set-default 4)
(define sql-sfku-set-null 8)


;;; SQL_SQL92_GRANT	bitmasks
(define sql-sg-usage-on-domain 1)
(define sql-sg-usage-on-character-set 2)
(define sql-sg-usage-on-collation 4)
(define sql-sg-usage-on-translation 8)
(define sql-sg-with-grant-option 16)
(define sql-sg-delete-table 32)
(define sql-sg-insert-table 64)
(define sql-sg-insert-column 128)
(define sql-sg-references-table 256)
(define sql-sg-references-column 512)
(define sql-sg-select-table 1024)
(define sql-sg-update-table 2048)
(define sql-sg-update-column 4096)


;;; SQL_SQL92_PREDICATES bitmasks
(define sql-sp-exists 1)
(define sql-sp-isnotnull 2)
(define sql-sp-isnull 4)
(define sql-sp-match-full 8)
(define sql-sp-match-partial 16)
(define sql-sp-match-unique-full 32)
(define sql-sp-match-unique-partial 64)
(define sql-sp-overlaps 128)
(define sql-sp-unique 256)
(define sql-sp-like 512)
(define sql-sp-in 1024)
(define sql-sp-between 2048)
(define sql-sp-comparison 4096)
(define sql-sp-quantified-comparison 8192)


;;; SQL_SQL92_RELATIONAL_JOIN_OPERATORS bitmasks
(define sql-srjo-corresponding-clause 1)
(define sql-srjo-cross-join 2)
(define sql-srjo-except-join 4)
(define sql-srjo-full-outer-join 8)
(define sql-srjo-inner-join 16)
(define sql-srjo-intersect-join 32)
(define sql-srjo-left-outer-join 64)
(define sql-srjo-natural-join 128)
(define sql-srjo-right-outer-join 256)
(define sql-srjo-union-join 512)


;;; SQL_SQL92_REVOKE bitmasks
(define sql-sr-usage-on-domain 1)
(define sql-sr-usage-on-character-set 2)
(define sql-sr-usage-on-collation 4)
(define sql-sr-usage-on-translation 8)
(define sql-sr-grant-option-for 16)
(define sql-sr-cascade 32)
(define sql-sr-restrict 64)
(define sql-sr-delete-table 128)
(define sql-sr-insert-table 256)
(define sql-sr-insert-column 512)
(define sql-sr-references-table 1024)
(define sql-sr-references-column 2048)
(define sql-sr-select-table 4096)
(define sql-sr-update-table 8192)
(define sql-sr-update-column 16384)


;;; SQL_SQL92_ROW_VALUE_CONSTRUCTOR bitmasks
(define sql-srvc-value-expression 1)
(define sql-srvc-null 2)
(define sql-srvc-default 4)
(define sql-srvc-row-subquery 8)


;;; SQL_SQL92_VALUE_EXPRESSIONS bitmasks
(define sql-sve-case 1)
(define sql-sve-cast 2)
(define sql-sve-coalesce 4)
(define sql-sve-nullif 8)


;;; SQL_STANDARD_CLI_CONFORMANCE bitmasks
(define sql-scc-xopen-cli-version1 1)
(define sql-scc-iso92-cli 2)


;;; SQL_UNION_STATEMENT bitmasks
(define sql-us-union 1)
(define sql-us-union-all 2)


;;; SQL_DTC_TRANSITION_COST bitmasks
(define sql-dtc-enlist-expensive 1)
(define sql-dtc-unenlist-expensive 2)


;;; additional SQLDataSources fetch directions
(define sql-fetch-first-user 31)
(define sql-fetch-first-system 32)


;;; Defines for SQLSetPos
(define sql-entire-rowset 0)


;;; Operations in SQLSetPos
(define sql-position 0)
(define sql-refresh 1)
(define sql-update 2)
(define sql-delete 3)


;;; Operations in SQLBulkOperations
(define sql-add 4)
(define sql-setpos-max-option-value 4)
(define sql-update-by-bookmark 5)
(define sql-delete-by-bookmark 6)
(define sql-fetch-by-bookmark 7)


;;; Lock options in SQLSetPos
(define sql-lock-no-change 0)
(define sql-lock-exclusive 1)
(define sql-lock-unlock 2)
(define sql-setpos-max-lock-value 2)


;;; Column types and scopes in SQLSpecialColumns
(define sql-best-rowid 1)
(define sql-rowver 2)


;;; Defines for SQLSpecialColumns (returned in the result set) SQL_PC_UNKNOWN and SQL_PC_PSEUDO are defined in sql.h
(define sql-pc-not-pseudo 1)


;;; Defines for SQLStatistics
(define sql-quick 0)
(define sql-ensure 1)


;;; Defines for SQLStatistics (returned in the result set) SQL_INDEX_CLUSTERED, SQL_INDEX_HASHED, and SQL_INDEX_OTHER are defined in sql.h
(define sql-table-stat 0)


;;; Options for SQLDriverConnect
(define sql-driver-noprompt 0)
(define sql-driver-complete 1)
(define sql-driver-prompt 2)
(define sql-driver-complete-required 3)


;;; SQLExtendedFetch fFetchType values
(define sql-fetch-bookmark 8)


;;; SQLExtendedFetch rgfRowStatus element values
(define sql-row-success 0)
(define sql-row-deleted 1)
(define sql-row-updated 2)
(define sql-row-norow 3)
(define sql-row-added 4)
(define sql-row-error 5)
(define sql-row-success-with-info 6)
(define sql-row-proceed 0)
(define sql-row-ignore 1)


;;; value for SQL_DESC_ARRAY_STATUS_PTR
(define sql-param-success 0)
(define sql-param-success-with-info 6)
(define sql-param-error 5)
(define sql-param-unused 7)
(define sql-param-diag-unavailable 1)
(define sql-param-proceed 0)
(define sql-param-ignore 1)


;;; Defines for SQLForeignKeys (UPDATE_RULE and DELETE_RULE)
(define sql-cascade 0)
(define sql-restrict 1)
(define sql-set-null 2)
(define sql-no-action 3)
(define sql-set-default 4)


;;; Note that the following are in a different column of SQLForeignKeys than the previous #defines.   These are for DEFERRABILITY.
(define sql-initially-deferred 5)
(define sql-initially-immediate 6)
(define sql-not-deferrable 7)


;;; Defines for SQLBindParameter and  SQLProcedureColumns (returned in the result set)
(define sql-param-type-unknown 0)
(define sql-param-input 1)
(define sql-param-input-output 2)
(define sql-result-col 3)
(define sql-param-output 4)
(define sql-return-value 5)


;;; Defines for SQLProcedures (returned in the result set)
(define sql-pt-unknown 0)
(define sql-pt-procedure 1)
(define sql-pt-function 2)
