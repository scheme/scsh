#include "scheme48.h"
#include <stdio.h>

/* ODBC header files */
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

/* darwin 6.1 sql header files do not define SQLLEN */
#ifndef SQLLEN 
#define SQLLEN SQLINTEGER
#endif

#define ERROR_MSG_BUFFER_LEN          255

/* turn debug messages on/off. */
#define ODBC_DEBUG_MSGS            1

#ifdef ODBC_DEBUG_MSGS
#define ODBC_DEBUG_DIAGREC(ht, h) odbc_debug_msgs(ht, h);
#else
#define ODBC_DEBUG_DIAGREC(ht, h) ;
#endif

#ifdef ODBC_DEBUG_MSGS
#define ODBC_DEBUG_PRINTF_1(str) printf(str);
#define ODBC_DEBUG_PRINTF_2(str, arg) printf(str, arg);
#define ODBC_DEBUG_PRINTF_3(str, arg1, arg2) printf(str, arg1, arg2);
#define ODBC_DEBUG_PRINTF_4(str, arg1, arg2, arg3) printf(str, arg1, arg2, arg3);
#define ODBC_DEBUG_PRINTF_5(str, arg1, arg2, arg3, arg4) printf(str, arg1, arg2, arg3, arg4);
#else
#define ODBC_DEBUG_PRINTF_1(str) ;
#define ODBC_DEBUG_PRINTF_2(str, arg) ;
#define ODBC_DEBUG_PRINTF_3(str, arg1, arg2) ;
#define ODBC_DEBUG_PRINTF_4(str, arg1, arg2, arg3) ;
#define ODBC_DEBUG_PRINTF_5(str, arg1, arg2, arg3, arg4) ;
#endif

/* import conditions */
static s48_value raise_odbc_api_version_mismatch_error = S48_FALSE;

#define RAISE_API_VERSION_MISMATCH(FUNNAME, APIVER, APIVERNEEDED) \
 s48_call_scheme(S48_SHARED_BINDING_REF(raise_odbc_api_version_mismatch_error), \
                 3, s48_enter_string(FUNNAME), \
		 s48_enter_integer(APIVER), s48_enter_integer(APIVERNEEDED));

static s48_value raise_odbc_unknown_integer_type_error = S48_FALSE;

#define RAISE_UNKNOWN_INTEGER_TYPE_ERROR(FUNNAME, TYPEID) \
 s48_call_scheme(S48_SHARED_BINDING_REF(raise_odbc_unknown_integer_type_error), \
                 2, s48_enter_string(FUNNAME),  s48_enter_integer(TYPEID));

static s48_value raise_odbc_buffer_alloc_error = S48_FALSE;

#define RAISE_ODBC_BUFFER_ALLOC_ERROR(BUFFERLEN) \
 s48_call_scheme(S48_SHARED_BINDING_REF(raise_odbc_buffer_alloc_error), \
                 1, s48_enter_integer(BUFFERLEN));

static s48_value raise_odbc_unknown_c_type_identifier_error = S48_FALSE;

#define RAISE_ODBC_UNKNOWN_C_TYPE_IDENTIFIER_ERROR(BUFFER, TYPEID) \
  s48_call_scheme(S48_SHARED_BINDING_REF(raise_odbc_unknown_c_type_identifier_error), \
                  2, s48_enter_integer(BUFFER), s48_enter_integer(TYPEID));

static s48_value raise_odbc_bindcol_unbound_column_error = S48_FALSE;

#define RAISE_ODBC_BINDCOL_UNBOUND_COLUMN_ERROR(STMTHANDLE, COLUMNNO) \
 s48_call_scheme(S48_SHARED_BINDING_REF(raise_odbc_bindcol_unbound_column_error), \
                 2, s48_enter_integer(STMTHANDLE), s48_enter_integer(COLUMNNO));

static s48_value raise_odbc_bindcol_rebinding_error = S48_FALSE;

#define RAISE_ODBC_BINDCOL_REBINDING_ERROR(TEXTMSG) \
 s48_call_scheme(S48_SHARED_BINDING_REF(raise_odbc_bindcol_rebinding_error), \
		 1, s48_enter_string(TEXTMSG));

/* corresponds to sql-date */
static s48_value sql_date_record_type = S48_FALSE;

#define SR_SQL_DATE_YEAR         0
#define SR_SQL_DATE_MONTH        1
#define SR_SQL_DATE_DAY          2

/* corresponds to sql-time */
static s48_value sql_time_record_type = S48_FALSE;

#define SR_SQL_TIME_HOUR         0
#define SR_SQL_TIME_MINUTE       1
#define SR_SQL_TIME_SECOND       2

/* corresponds to sql-timestamp */
static s48_value sql_timestamp_record_type = S48_FALSE;

#define SR_SQL_TIMESTAMP_YEAR     0
#define SR_SQL_TIMESTAMP_MONTH    1
#define SR_SQL_TIMESTAMP_DAY      2
#define SR_SQL_TIMESTAMP_HOUR     3
#define SR_SQL_TIMESTAMP_MINUTE   4
#define SR_SQL_TIMESTAMP_SECOND   5
#define SR_SQL_TIMESTAMP_FRACTION 6

/* corresponds to sql-numeric */
#define SR_SQL_NUMERIC_PRECISION 0
#define SR_SQL_NUMERIC_SCALE     1
#define SR_SQL_NUMERIC_SIGN      2
#define SR_SQL_NUMERIC_VALUE     3

/* corresponds to odbc-column */
static s48_value odbc_column_record_type = S48_FALSE;

#define SR_ODBC_COLUMN_NAME      0
#define SR_ODBC_COLUMN_TYPE      1
#define SR_ODBC_COLUMN_SIZE      2
#define SR_ODBC_COLUMN_DIGITS    3
#define SR_ODBC_COLUMN_NULLABLE  4

/* corresponds to odbc-diag */
static s48_value odbc_diag_record_type = S48_FALSE;

#define SR_ODBC_DIAG_SQL_STATE     0
#define SR_ODBC_DIAG_NATIVE_ERROR  1
#define SR_ODBC_DIAG_MESSAGE       2

/* corresponds to odbc-parameter */
static s48_value odbc_parameter_record_type = S48_FALSE;

#define SR_ODBC_PARAMETER_TYPE     0
#define SR_ODBC_PARAMETER_SIZE     1
#define SR_ODBC_PARAMETER_DIGITS   2
#define SR_ODBC_PARAMETER_NULLABLE 3

/* stuff needed for SQLBindCol() */

/* correspons to bindcol-buffer */
static s48_value bindcol_buffer_record_type = S48_FALSE;

#define SR_BINDCOL_BUFFER_POINTER  0
#define SR_BINDCOL_BUFFER_LENGTH   1
#define SR_BINDCOL_BUFFER_TARGET_TYPE 2

typedef struct bindcol_col_rec *ColumnRecPtr;

typedef struct bindcol_col_rec {
  SQLUSMALLINT col_no;
  void *col_buffer;
  SQLSMALLINT target_type;
  SQLLEN buffer_len;
  SQLLEN buffer_needed;
  ColumnRecPtr next;
} ColumnRec;

typedef struct bindcol_stmt_rec *StmtRecPtr;

typedef struct bindcol_stmt_rec {
  SQLHSTMT stmt_handle;
  ColumnRecPtr col_recs;
  StmtRecPtr next;
} StmtRec;

/* global variables */
StmtRecPtr global_bindcol_list = NULL;

/* helper functions needed for SQLBindCol() */
ColumnRecPtr bindcol_lookup_binding(SQLHSTMT stmt_handle, SQLUSMALLINT column_no);
s48_value bindcol_lookup_binding_scheme(s48_value stmt_handle, s48_value column_no);

void bindcol_bind_column(SQLHSTMT stmt_handle, SQLUSMALLINT column_no, ColumnRecPtr new_col);
void bindcol_unbind_colum(SQLHSTMT stmt_handle, SQLUSMALLINT column_no);
s48_value bindcol_finalize_bindcols(s48_value stmt_handle);
s48_value odbc_sql_bindcol(s48_value stmt_handle, s48_value column_no,
			   s48_value target_type, s48_value buffer_len);

/*
 *
 * PART 1
 *
 * Connecting to a data source
 *
 */

/* Call SQLAllocHandle and get an environment handle. After that
 * call odbc_set_environment to set the ODBC version */
s48_value odbc_alloc_environment_handle();

/* given a valid environment handle (type SQLHENV) this function
 * sets the environment attributes. This needs to be done before
 * allocating a connection handle */
s48_value odbc_sql_set_env_attr(SQLHENV env_handle);

/* Given a valid environment handle get a connection handle */
s48_value odbc_alloc_connection_handle(s48_value env_handle);

/* Given a valid connection handle get a statement handle */
s48_value odbc_alloc_statement_handle(s48_value stmt_handle);

/* Connect to a server */
s48_value odbc_sql_connect(s48_value connection_handle,
			   s48_value ds_name,
			   s48_value user_name,
			   s48_value authentication);

s48_value odbc_sql_browse_connect(s48_value conn_handle, s48_value conn_string);

/*
 *
 * PART 2
 *
 * Obtaining information about a driver and data source
 *
 */

/* Returns a list of available data sources. */
s48_value odbc_sql_data_sources(s48_value env_handle);

/* Returns the list of installed drivers and their attributes. */
s48_value odbc_sql_drivers(s48_value env_handle);

/* Returns information about a specific driver and data source. 
 * (use if the information is an integer) */
s48_value odbc_sql_get_info_int(s48_value conn_handle, s48_value info_key);

/* Returns information about a specific driver and data source. 
 * (use if the information is a string) */
s48_value odbc_sql_get_info_string(s48_value conn_handle, s48_value info_key);

/* Returns supported driver functions. */
s48_value odbc_sql_get_func_exists(s48_value conn_handle, s48_value fun_id);

/* Returns information about supported data types. */
s48_value odbc_sql_get_type_info(s48_value stmt_handle, s48_value data_type);

/*
 *
 * PART 3
 *
 * Setting and retrieving driver attributes
 *
 */

s48_value odbc_sql_set_connect_attr_int(s48_value conn_handle,
					s48_value attribute,
					s48_value value);

s48_value odbc_sql_set_connect_attr_string(s48_value conn_handle,
					   s48_value attribute,
					   s48_value value);

s48_value odbc_sql_get_connect_attr_string(s48_value conn_handle,
					   s48_value attribute);

s48_value odbc_sql_get_connect_attr_int(s48_value conn_handle,
					s48_value attribute);

s48_value odbc_sql_set_env_attr_int(s48_value env_handle,
				    s48_value attribute,
				    s48_value value);

s48_value odbc_sql_get_env_attr_int(s48_value env_handle,
				    s48_value attribute,
				    s48_value value);

s48_value odbc_sql_set_stmt_attr_int(s48_value stmt_handle,
				     s48_value attribute,
				     s48_value value);

s48_value odbc_sql_set_stmt_attr_string(s48_value stmt_handle,
					s48_value attribute,
					s48_value value);

s48_value odbc_sql_get_stmt_attr_int(s48_value stmt_handle, 
				     s48_value attribute);

s48_value odbc_sql_get_stmt_attr_string(s48_value stmt_handle,
					s48_value attribute);

/*
 *
 * part 4
 *
 * Setting and retrieving descriptor fields
 *
 */

/* Returns the value of a single descriptor field (for integers) */
s48_value odbc_sql_get_desc_field_int(s48_value desc_handle, s48_value rec_number,
				      s48_value field_id);

/* Returns the value of a single descriptor field (for strings/binary data) */
s48_value odbc_sql_get_desc_field_string(s48_value desc_handle, s48_value rec_number,
					 s48_value field_id);

/*
 *
 * PART 5
 *
 * Preparing SQL requests
 *
 */

/* Prepare a SQL statement for execution */
s48_value odbc_sql_prepare(s48_value stmt_handle, s48_value stmt_txt);

s48_value odbc_sql_get_cursor_name(s48_value stmt_handle);

s48_value odbc_sql_set_cursor_name(s48_value stmt_handle, s48_value cursorname);

/*
 *
 * PART 6
 *
 * Submitting requests
 *
 */
s48_value odbc_sql_execute(s48_value stmt_handle);

s48_value odbc_sql_execute_direct(s48_value stmt_handle, s48_value stmt);

/* Returns the text of an SQL statement as translated by the driver. */
s48_value odbc_sql_native_sql(s48_value conn_handle, s48_value stmt_txt);

/* Returns the description for a specific parameter in a statement */
s48_value odbc_sql_describe_param(s48_value stmt_handle, s48_value parameter_no);

/* Returns the number of parameters in a statement */
s48_value odbc_sql_num_params(s48_value stmt_handle);


/*
 *
 * PART 7
 *
 * Retrieving results and information about results
 *
 */

s48_value odbc_sql_row_count(s48_value stmt_handle);

s48_value odbc_sql_get_data(s48_value stmt_handle, s48_value column_number, 
			    s48_value target_type);

/* Positions a cursor within a fetched block of data and allows an application
   to refresh data in the rowset or to update or delete data in the result
   set */
s48_value odbc_sql_set_pos(s48_value stmt_handle, s48_value row_number,
			   s48_value operation, s48_value lock_type);

/* Performs bulk insertions and bulk bookmark operations, including
   update, delete, and fetch by bookmark. */
s48_value odbc_sql_bulk_operations(s48_value stmt_handle, s48_value operation);

/* Determines whether there are more result sets available and, if so,
   initializes processing for the next result set */
s48_value odbc_sql_more_results(s48_value stmt_handle);

s48_value odbc_sql_fetch(s48_value stmt_handle);

s48_value odbc_sql_num_result_cols(s48_value stmt_handle);

s48_value odbc_sql_describe_col(s48_value stmt_handle, s48_value column_number);

/* Describes attributes of a column in the result set */
s48_value odbc_sql_col_attribute(s48_value stmt_handle, s48_value column_number,
				 s48_value field_id);

/*
 *
 * PART 8
 *
 * Obtaining information about the data source's
 * system tables (catalog functions)
 *
 */

/* Returns a list of columns and associated privileges for one or more tables */
s48_value odbc_sql_column_privileges(s48_value stmt_handle, s48_value catalog_name,
				     s48_value schema_name, s48_value table_name,
				     s48_value column_name);

/* Returns the list of column names in a specified table */
s48_value odbc_sql_columns(s48_value stmt_handle, s48_value catalog_name,
			   s48_value schema_name, s48_value table_name,
			   s48_value column_name);

/* Returns a list of columns names that make up foreign keys, 
   if the exist for a specified table */
s48_value odbc_sql_foreign_keys(s48_value stmt_handle, s48_value pk_catalog_name,
				s48_value pk_schema_name, s48_value pk_table_name,
				s48_value fk_catalog_name, s48_value fk_schema_name,
				s48_value fk_table_name);

/* Returns the list of column names that make up the primary key for a table */
s48_value odbc_sql_primary_keys(s48_value stmt_handle, s48_value catalog_name,
				s48_value schema_name, s48_value table_name);

/* Returns the list of input and output parameters, as well as the columns
   that make up the result set for the specified procedures */
s48_value odbc_sql_procedure_columns(s48_value stmt_handle, s48_value catalog_name,
				     s48_value schema_name, s48_value proc_name,
				     s48_value column_name);

/* Returns the list of procedure names stored in a specific data source. */
s48_value odbc_sql_procedures(s48_value stmt_handle, s48_value catalog_name,
			      s48_value schema_name, s48_value proc_name);

/* Returns information about the optimal set of columns that uniquely identifies
   a row in a specified table, or the columns that are automatically updated
   when any value in the row is updated by a transaction */
s48_value odbc_sql_special_columns(s48_value stmt_handle, s48_value identifier_type,
				   s48_value catalog_name, s48_value schema_name,
				   s48_value table_name, s48_value scope,
				   s48_value nullable);

/* Returns statistics about a single table and the list of indexes associated 
   with the table */
s48_value odbc_sql_statistics(s48_value stmt_handle, s48_value catalog_name,
			      s48_value schema_name, s48_value table_name,
			      s48_value unique, s48_value reserved);

/* Returns a list of tables and the privileges associated with each table */
s48_value odbc_sql_table_privileges(s48_value stmt_handle, s48_value catalog_name,
				    s48_value schema_name, s48_value table_name);

/* Returns the list of table names stored in a specific data source */
s48_value odbc_sql_tables(s48_value stmt_handle, s48_value catalog_name,
			  s48_value schema_name, s48_value table_name,
			  s48_value table_type);

/*
 *
 * PART 9
 *
 * Terminating a statement
 *
 */

/* Ends statement processing, discards pending resilt, and, 
 * optionally, frees all resources associated with the
 * statement handle */ 
s48_value odbc_sql_free_statement(s48_value stmt_handle, s48_value option);

/* Closes a cursor that has been opened on a statement handle */
s48_value odbc_sql_close_cursor(s48_value stmt_handle);

/* Cancels an SQL statement */
s48_value odbc_sql_cancel(s48_value stmt_handle);

/* Commits or rolls back a transaction */
s48_value odbc_sql_endtran(s48_value handle_type, s48_value handle,
			   s48_value completion_type);

/*
 *
 * PART 10
 *
 * Terminating a connection
 *
 */

/* Closes the connection */
s48_value odbc_sql_disconnect(s48_value conn_handle);

/* Free a handle */
s48_value odbc_sql_free_handle(s48_value handle_type, s48_value handle);


/*
 *
 * PART 11
 *
 * misc. functions
 *
 */

s48_value odbc_sql_get_diag_recs(s48_value handle_type, s48_value handle);

#ifdef ODBC_DEBUG_MSGS
/* print detailed debug information */
void odbc_debug_msgs(SQLSMALLINT handle_type, SQLHANDLE handle);
#endif

/* convert Scheme sql-date record to SQL_DATE_STRUCT */
void sql_date_record_to_struct(s48_value sql_date, SQL_DATE_STRUCT *ds);

/* convert SQL_DATE_STRUCT to Scheme sql-date record */
s48_value struct_to_sql_date_record(SQL_DATE_STRUCT *ds);

/* convert Scheme sql-time record to SQL_TIME_STRUCT */
void sql_time_record_to_struct(s48_value sql_time, SQL_TIME_STRUCT *ts);

/* convert SQL_TIME_STRUCT to Scheme sql-time record */
s48_value struct_to_sql_time_record(SQL_TIME_STRUCT *ts);

/* convert SQL_TIME_STRUCT to Scheme sql-time record */
s48_value struct_to_sql_time_record(SQL_TIME_STRUCT *ts);

/* convert Scheme sql-timestamp record to SQL_TIMESTAMP_STRUCT */ 
void sql_timestamp_record_to_struct(s48_value sql_timestamp, 
				    SQL_TIMESTAMP_STRUCT *ts);

/* initial return value buffer size */
#define ODBC_RETVAL_BUFFER_DEFAULT_SIZE   255
static SQLUSMALLINT odbc_initial_retval_buffer_size = ODBC_RETVAL_BUFFER_DEFAULT_SIZE;

/* set initial return value buffer size */
s48_value odbc_set_initial_retval_buffer_size(s48_value nobytes);

/* get initial return value buffer size */
s48_value odbc_get_intial_retval_buffer_size();

void odbc_sql_alloc(void **buffer, size_t buffer_len, size_t type_len);

size_t sizeof_sql_c_type_identifier(SQLSMALLINT ctypeid);

s48_value buffer_to_s48_value(void *buffer, SQLSMALLINT ctypeid);

void s48_init_odbc(void);

