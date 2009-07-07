#include "odbc.h"

/*
 *
 * PART 1
 *
 * Connecting to a data source
 *
 */

/* Call SQLAllocHandle and get an environment handle. After that
 * call odbc_set_environment to set the ODBC version */
s48_value odbc_alloc_environment_handle() 
{
  SQLHENV henv;
  SQLRETURN retval;

#if (ODBCVER >= 0x300)
  retval = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &henv);
  ODBC_DEBUG_PRINTF_2("SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, ...): %x\n", henv);
#else
  retval = SQLAllocEnv(&henv);
  ODBC_DEBUG_PRINTF_2("SQLAllocEnv(...): %x\n", henv);
#endif /* (ODBCVER >= 0x300) */

  return s48_list_2(s48_enter_integer(retval), 
		    SQL_SUCCEEDED(retval) ? s48_enter_integer((long)henv) :
		    S48_UNSPECIFIC);
}

/* given a valid environment handle (type SQLHENV) this function
 * sets the environment attributes. This needs to be done before
 * allocating a connection handle */
s48_value odbc_sql_set_env_attr(SQLHENV env_handle) 
{
  SQLRETURN retval;

  retval = SQLSetEnvAttr(env_handle, SQL_ATTR_ODBC_VERSION, 
			 (void *)SQL_OV_ODBC3, 0);
  ODBC_DEBUG_PRINTF_2("odbc_set_environment() %x\n", env_handle);  

  return s48_enter_integer(retval);
}

/* Given a valid environment handle get a connection handle */
s48_value odbc_alloc_connection_handle(s48_value env_handle) 
{
  SQLHDBC hdbc;
  SQLRETURN retval;
  SQLHENV envh;

  envh = (SQLHENV) s48_extract_integer(env_handle);

#if (ODBCVER >= 0x300)
  retval = SQLAllocHandle(SQL_HANDLE_DBC, envh, &hdbc);
  ODBC_DEBUG_PRINTF_3("SQLAllocHandle(SQL_HANDLE_DBC, %x, ...): %x\n", envh, hdbc);
#else
  retval = SQLAllocConnect(envh, &hdbc);
  ODBC_DEBUG_PRINTF_3("SQLAllocConnect(%x, ...): %x\n", envh, hdbc);
#endif /* ODBCVER >= 0x300 */

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer((long)hdbc) : 
		    S48_UNSPECIFIC);
}

/* Given a valid connection handle get a statement handle */
s48_value odbc_alloc_statement_handle(s48_value conn_handle) 
{
  SQLHSTMT hstmt;
  SQLRETURN retval;
  SQLHANDLE ch;

  ch = (SQLHANDLE) s48_extract_integer(conn_handle);

#if (ODBCVER >= 0x300)
  retval = SQLAllocHandle(SQL_HANDLE_STMT, ch, &hstmt);
  ODBC_DEBUG_PRINTF_3("SQLAllocHandle() %x %x\n", ch, hstmt);
#else
  retval = SQLAllocStmt(ch, &hstmt);
  ODBC_DEBUG_PRINTF_3("SQLAllocStmt() %x %x\n", ch, hstmt);
#endif /* ODBCVER >= 0x300 */

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer((long)hstmt) : 
		    S48_UNSPECIFIC);
}

/* Connect to a server */
s48_value odbc_sql_connect(s48_value connection_handle,
			   s48_value ds_name,
			   s48_value user_name,
			   s48_value authentication) 
{
  SQLHDBC ch;
  SQLCHAR *dsn, *user, *auth;
  SQLRETURN retval;
  
  dsn = (SQLCHAR *) s48_extract_string(ds_name);
  user = (SQLCHAR *) s48_extract_string(user_name);
  auth = (SQLCHAR *) s48_extract_string(authentication);
  ch = (SQLHDBC) s48_extract_integer(connection_handle);

  ODBC_DEBUG_PRINTF_5("odbc_sql_connect() %x '%s' '%s' '%s'\n",
		      ch, dsn, user,auth);
  retval = SQLConnect(ch,
		      dsn, S48_STRING_LENGTH(ds_name),
		      user, S48_STRING_LENGTH(user_name),
		      auth, S48_STRING_LENGTH(authentication));

  return s48_enter_integer(retval);
}

s48_value odbc_sql_browse_connect(s48_value conn_handle, s48_value conn_string)
{
  SQLHDBC ch;
  SQLCHAR *buffer = NULL;
  SQLRETURN retval;
  SQLSMALLINT buffer_needed, buffer_len;
  s48_value res = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(res);
  ch = (SQLHDBC) s48_extract_integer(conn_handle);
  ODBC_DEBUG_PRINTF_3("odbc_sql_browse_connect() %x '%s'\n", 
		      ch, s48_extract_string(conn_string));
  buffer_len = odbc_initial_retval_buffer_size;

  for (;;)
    {
      odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));

      retval = SQLBrowseConnect(ch, (SQLCHAR *) s48_extract_string(conn_string),
				(SQLSMALLINT) S48_STRING_LENGTH(conn_string),
				buffer, buffer_len, &buffer_needed);

      if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	buffer_len = buffer_needed+1;
      else
	break;
    }

  res = s48_list_2(s48_enter_integer(retval),
		   SQL_SUCCEEDED(retval) ? s48_enter_string(buffer) : 
		   S48_UNSPECIFIC);
  free(buffer);
  S48_GC_UNPROTECT();
  return res;
}

/*
 *
 * PART 2
 *
 * Obtaining information about a driver and data source
 *
 */

/* Returns a list of available data sources. */
s48_value odbc_sql_data_sources(s48_value env_handle)
{
  SQLHENV eh;
  SQLUSMALLINT dir;
  SQLRETURN retval;
  int first;
  SQLSMALLINT server_name_len, driver_descr_len;
  SQLSMALLINT server_name_needed, driver_descr_needed;
  SQLCHAR *server_name = NULL;
  SQLCHAR *driver_descr = NULL;
  s48_value res_list = S48_NULL;
  s48_value res = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res_list, res);
  eh = (SQLHENV) s48_extract_integer(env_handle);
  ODBC_DEBUG_PRINTF_1("odbc_sql_data_sources\n");
  server_name_len = driver_descr_len = odbc_initial_retval_buffer_size;

  first = 1; 
  for (;;) 
    {
      for (;;)
	{
	  odbc_sql_alloc((void **) &server_name, server_name_len, sizeof(SQLCHAR));
	  odbc_sql_alloc((void **) &driver_descr, driver_descr_len, sizeof(SQLCHAR));
	  
	  retval = SQLDataSources(eh, 
				  (first ? SQL_FETCH_FIRST : SQL_FETCH_NEXT),
				  server_name, sizeof(SQLCHAR)*server_name_len, 
				  &server_name_needed,
				  driver_descr, sizeof(SQLCHAR)*driver_descr_len, 
				  &driver_descr_needed);
	  
	  if (SQL_SUCCEEDED(retval) && ((server_name_needed > server_name_len) || 
					(driver_descr_needed > driver_descr_len)))
	    {
	      if (server_name_needed > server_name_len)
		server_name_len = server_name_needed + 1;
	      if (driver_descr_needed > driver_descr_len)
		driver_descr_len = driver_descr_needed + 1;
	    }
	  else
	    break;
	}

      if (SQL_SUCCEEDED(retval))
	res_list = s48_cons(s48_cons(s48_enter_string(server_name), 
				     s48_enter_string(driver_descr)),
			    res_list);
      else
	res_list = S48_UNSPECIFIC;
      
      if (retval == SQL_NO_DATA)
	{
	  res = s48_list_2(s48_enter_integer(retval), res_list);
	  free(server_name);
	  free(driver_descr);
	  S48_GC_UNPROTECT();
	  return res;
	}
      first = 0;
    }
  
  /* never reached */
  S48_GC_UNPROTECT();
  return s48_list_2(s48_enter_integer(retval), S48_UNSPECIFIC);
}

/* Returns the list of installed drivers and their attributes. */
s48_value odbc_sql_drivers(s48_value env_handle) 
{
#if (ODBCVER >= 0x0200)
  SQLHENV eh;
  SQLRETURN retval;
  int first;
  SQLSMALLINT driver_descr_len, driver_attr_len;
  SQLSMALLINT driver_descr_needed, driver_attr_needed;
  SQLCHAR *driver_attr = NULL;
  SQLCHAR *driver_descr = NULL;
  s48_value res_list = S48_NULL;
  s48_value res = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res, res_list);
  eh = (SQLHENV) s48_extract_integer(env_handle);
  ODBC_DEBUG_PRINTF_1("odbc_sql_drivers\n");
  driver_descr_len = driver_attr_len = odbc_initial_retval_buffer_size;
  first = 0;
 
  for (;;)
    {
      for (;;)
	{
	  odbc_sql_alloc((void **) &driver_descr, driver_descr_len, sizeof(SQLCHAR));
	  odbc_sql_alloc((void **) &driver_attr, driver_attr_len, sizeof(SQLCHAR));

	  retval = SQLDrivers(eh,
			      (SQLUSMALLINT) (first ? SQL_FETCH_FIRST : SQL_FETCH_NEXT),
			      driver_descr, driver_descr_len, &driver_descr_needed,
			      driver_attr, driver_attr_len, &driver_attr_needed);

	  if (SQL_SUCCEEDED(retval) && ((driver_descr_needed > driver_descr_len) ||
					(driver_attr_needed > driver_attr_len)))
	    {
	      if (driver_descr_needed > driver_descr_len)
		driver_descr_len = driver_descr_needed + 1;
	      if (driver_attr_needed > driver_attr_len)
		driver_attr_len = driver_attr_needed + 1;
	    }
	  else
	    break;
	}

      if (SQL_SUCCEEDED(retval))
	res_list = s48_cons(s48_cons(s48_enter_string(driver_descr),
				     s48_enter_string(driver_attr)),
			    res_list);
	  
      if (retval == SQL_NO_DATA)
	{
	  res = s48_list_2(s48_enter_integer(retval), res_list);
	  free(driver_descr);
	  free(driver_attr);
	  S48_GC_UNPROTECT();
	  return res;
	}

      first = 1;
    }

  /* not reached */
  S48_GC_UNPROTECT();
  return res;
#else
  RAISE_API_VERSION_MISMATCH("SQLDrivers", ODBCVER, 0x200);
#endif /* ODBCVER >= 0x200 */
}

/* Returns information about a specific driver and data source. 
 * (use if the information is an integer) */
s48_value odbc_sql_get_info_int(s48_value conn_handle, s48_value info_key)
{
  SQLHDBC ch;
  SQLUSMALLINT ik;
  SQLRETURN retval;
  SQLUINTEGER info;
  SQLSMALLINT buffer_size;

  ODBC_DEBUG_PRINTF_1("odbc_sql_get_info_int\n");
  ch = (SQLHDBC) s48_extract_integer(conn_handle);
  ik = (SQLUSMALLINT) s48_extract_integer(info_key);
  retval = SQLGetInfo(ch, ik, &info, sizeof(SQLUINTEGER), &buffer_size);

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer(info) : 
		    S48_UNSPECIFIC);
}

/* Returns information about a specific driver and data source. 
 * (use if the information is a string) */
s48_value odbc_sql_get_info_string(s48_value conn_handle, s48_value info_key)
{
  SQLHDBC ch;
  SQLUSMALLINT ik;
  SQLRETURN retval;
  SQLCHAR *buffer = NULL;
  SQLSMALLINT buffer_needed, buffer_len;
  s48_value res = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(res);
  ODBC_DEBUG_PRINTF_1("odbc_sql_get_info_string\n");
  ch = (SQLHDBC) s48_extract_integer(conn_handle);
  ik = (SQLUSMALLINT) s48_extract_integer(info_key);
  buffer_len = odbc_initial_retval_buffer_size;

  for (;;)
    {
      odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));
      retval = SQLGetInfo(ch, ik, buffer, buffer_len, &buffer_needed);
      
      if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	buffer_len = buffer_needed+1;
      else 
	break;
    }

  res = s48_list_2(s48_enter_integer(retval),
		   SQL_SUCCEEDED(retval) ? s48_enter_string(buffer) : 
		   S48_UNSPECIFIC);
  free(buffer);
  S48_GC_UNPROTECT();
  return res;
}

/* Returns supported driver functions. (for a multiple functions) */
s48_value odbc_sql_get_func_exists(s48_value conn_handle, s48_value fun_id)
{
  SQLHDBC ch;
  SQLUSMALLINT fi, supported[SQL_API_ODBC3_ALL_FUNCTIONS_SIZE];
  SQLRETURN retval;
  int i;
  
  ODBC_DEBUG_PRINTF_1("odbc_sql_func_exists\n");
  ch = (SQLHDBC) s48_extract_integer(conn_handle);
  fi = (SQLUSMALLINT) s48_extract_integer(fun_id);
  retval = SQLGetFunctions(ch, fi, supported);
 
  if (SQL_SUCCEEDED(retval))
    return 
      s48_list_2(s48_enter_integer(retval),
		 SQL_FUNC_EXISTS(supported, fi) == SQL_TRUE ? S48_TRUE : 
		 S48_FALSE);
  else
    return s48_list_2(s48_enter_integer(retval), S48_UNSPECIFIC);
}

/* Returns information about supported data types. */
s48_value odbc_sql_get_type_info(s48_value stmt_handle, s48_value data_type)
{
  SQLHSTMT sh;
  SQLSMALLINT dt;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_get_type_info\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  dt = (SQLSMALLINT) s48_extract_integer(data_type);
  retval = SQLGetTypeInfo(sh, dt);

  return s48_enter_integer(retval);
}

/*
 *
 * PART 3
 *
 * Setting and retrieving driver attributes
 *
 */

s48_value odbc_sql_set_connect_attr_int(s48_value conn_handle,
					s48_value attribute,
					s48_value value)
{
#if (ODBCVER >= 0x300)
  SQLHDBC ch;
  SQLINTEGER attr;
  SQLUINTEGER val;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_set_connect_attr_int\n");
  ch = (SQLHDBC) s48_extract_integer(conn_handle);
  attr = (SQLINTEGER) s48_extract_integer(attribute);
  val = (SQLUINTEGER) s48_extract_integer(value);
  retval = SQLSetConnectAttr(ch, attr, &val, 0);

  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLSetConnectAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

s48_value odbc_sql_set_connect_attr_string(s48_value conn_handle,
					   s48_value attribute,
					   s48_value value)
{
#if (ODBCVER >= 0x300)
  SQLHDBC ch;
  SQLINTEGER attr;
  SQLCHAR *val;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_set_connect_attr_string\n");
  ch = (SQLHDBC) s48_extract_integer(conn_handle);
  attr = (SQLINTEGER) s48_extract_integer(attribute);
  val = (SQLCHAR *) s48_extract_string(value);
  retval = SQLSetConnectAttr(ch, attr, val, S48_STRING_LENGTH(value));

  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLSetConnectAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

s48_value odbc_sql_get_connect_attr_string(s48_value conn_handle, 
					   s48_value attribute)
{
#if (ODBCVER >= 0x300)
  SQLHDBC ch;
  SQLINTEGER attr;
  SQLCHAR *buffer = NULL;
  SQLINTEGER buffer_needed, buffer_len;
  SQLRETURN retval;
  s48_value res = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(res);
  ODBC_DEBUG_PRINTF_1("odbc_sql_get_connect_attr_string\n");
  buffer_len = odbc_initial_retval_buffer_size;

  for (;;) {
    odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));
    retval = SQLGetConnectAttr(ch, attr, buffer, buffer_len, &buffer_needed);

    if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
      buffer_needed = buffer_len;
    else
      break;
  }
  
  res = s48_list_2(s48_enter_integer(retval),
		   SQL_SUCCEEDED(retval) ? s48_enter_string(buffer) : 
		   S48_UNSPECIFIC);
  free(buffer);
  S48_GC_UNPROTECT();
  return res;
#else
  RAISE_API_VERSION_MISMATCH("SQLGetConnectAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

s48_value odbc_sql_get_connect_attr_int(s48_value conn_handle,
					s48_value attribute) 
{
#if (ODBCVER >= 0x300)  
  SQLHDBC ch;
  SQLINTEGER attr;
  SQLUINTEGER buffer;
  SQLINTEGER buffer_size;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_get_connect_attr_int\n");
  retval = SQLGetConnectAttr(ch, attr,
			     &buffer, sizeof(SQLUINTEGER), &buffer_size);

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer(buffer) :
		    S48_UNSPECIFIC);
#else
  RAISE_API_VERSION_MISMATCH("SQLGetConnectAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

s48_value odbc_sql_set_env_attr_int(s48_value env_handle,
				    s48_value attribute,
				    s48_value value)
{
#if (ODBCVER >= 0x300)
  SQLHENV eh;
  SQLINTEGER attr;
  SQLUINTEGER val;
  SQLRETURN retval;

  eh = (SQLHENV) s48_extract_integer(env_handle);
  attr = (SQLINTEGER) s48_extract_integer(attribute);
  val = s48_extract_integer(value);
  ODBC_DEBUG_PRINTF_4("odbc_sql_set_env_attr_int eh %x attr %d value %d\n", eh, attr, val);
  retval = SQLSetEnvAttr(eh, attr, (SQLPOINTER) &val, sizeof(SQLUINTEGER));
  
  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLSetConnectAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

s48_value odbc_sql_get_env_attr_int(s48_value env_handle,
				    s48_value attribute,
				    s48_value value) 
{
#if (ODBCVER >= 0x300)
  SQLHENV eh;
  SQLINTEGER attr;
  SQLUINTEGER val, str_len;
  SQLRETURN retval;

  eh = (SQLHENV) s48_extract_integer(env_handle);
  attr = (SQLINTEGER) s48_extract_integer(attribute);
  val = (SQLUINTEGER) s48_extract_integer(value);
  ODBC_DEBUG_PRINTF_4("odbc_sql_get_env_attr_int eh %x attr %d value %d\n", eh, attr, val);
  retval = SQLGetEnvAttr(eh, attr, &val, sizeof(SQLUINTEGER), &str_len);

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer(val) : 
		    S48_UNSPECIFIC);
#else
  RAISE_API_VERSION_MISMATCH("SQLGetEnvAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

/* Sets a statement attribute */
s48_value odbc_sql_set_stmt_attr_int(s48_value stmt_handle,
				     s48_value attribute,
				     s48_value value)
{
#if (ODBCVER >= 0x300)
  SQLHSTMT sh;
  SQLINTEGER attr;
  SQLUINTEGER val;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_get_stmt_attr_int\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  attr = (SQLINTEGER) s48_extract_integer(attribute);
  val = (SQLUINTEGER) s48_extract_integer(value);
  retval = SQLSetStmtAttr(sh, attr, &val, 0);

  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLSetStmtAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

s48_value odbc_sql_set_stmt_attr_string(s48_value stmt_handle,
					s48_value attribute,
					s48_value value)
{
#if (ODBCVER >= 0x300)
  SQLHSTMT sh;
  SQLINTEGER attr;
  SQLCHAR *val;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_set_stmt_attr_string\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  attr = (SQLINTEGER) s48_extract_integer(attribute);
  val = (SQLCHAR *) s48_extract_string(value);
  retval = SQLSetStmtAttr(sh, attr, val, S48_STRING_LENGTH(value));

  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLSetStmtAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

s48_value odbc_sql_get_stmt_attr_int(s48_value stmt_handle, s48_value attribute)
{
#if (ODBCVER >= 0x300)
  SQLHSTMT sh;
  SQLINTEGER attr, val, buf_size;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_get_stmt_attr_int\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  attr = (SQLINTEGER) s48_extract_integer(attribute);
  retval = SQLGetStmtAttr(sh, attr, &val, sizeof(SQLINTEGER), &buf_size);

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer(val) : 
		    S48_UNSPECIFIC);
#else
  RAISE_API_VERSION_MISMATCH("SQLGetStmtAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

s48_value odbc_sql_get_stmt_attr_string(s48_value stmt_handle,
					s48_value attribute) 
{
#if (ODBCVER >= 0x300)
  SQLHSTMT sh;
  SQLINTEGER attr, buffer_len, buffer_needed;
  SQLCHAR *buffer = NULL;
  SQLRETURN retval;
  s48_value res = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(res);
  ODBC_DEBUG_PRINTF_1("odbc_sql_get_stmt_attr_string\n");
  buffer_len = odbc_initial_retval_buffer_size;

  for (;;)
    {
      odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));
      retval = SQLGetStmtAttr(sh, attr, &buffer, buffer_len, &buffer_needed);
      
      if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	buffer_len = buffer_needed+1;
      else 
	break;
    }

  res = s48_list_2(s48_enter_integer(retval),
		   SQL_SUCCEEDED(retval) ? s48_enter_string(buffer) : 
		   S48_UNSPECIFIC);
  free(buffer);
  S48_GC_UNPROTECT();
  return res;
#else
  RAISE_API_VERSION_MISMATCH("SQLGetStmtAttr", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}
  
/*
 *
 * PART 4
 *
 * Setting and retrieving descriptor fields
 *
 */

/* Returns the value of a single descriptor field (for integers) */
s48_value odbc_sql_get_desc_field_int(s48_value desc_handle, s48_value rec_number,
				      s48_value field_id)
{
#if (ODBCVER >= 0x300)
  SQLHDESC dh;
  SQLSMALLINT rn, fi;
  SQLINTEGER value, buffer_len;
  SQLRETURN retval;
  s48_value res = S48_UNSPECIFIC;
  s48_value scheme_int = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res, scheme_int);
  ODBC_DEBUG_PRINTF_1("odbc_sql_get_desc_field_int\n");  
  dh = (SQLHDESC) s48_extract_integer(desc_handle);
  rn = (SQLSMALLINT) s48_extract_integer(rec_number);
  fi = (SQLSMALLINT) s48_extract_integer(field_id);

  retval = SQLGetDescField(dh, rn, fi, (SQLPOINTER) &value, 
			   sizeof(SQLINTEGER), &buffer_len);

  if (SQL_SUCCEEDED(retval))
    switch (buffer_len)
      {
      case SQL_IS_INTEGER:
	scheme_int = s48_enter_integer((SQLINTEGER) value);
	break;
      case SQL_IS_UINTEGER:
	scheme_int = s48_enter_integer((SQLUINTEGER) value);
	break;
      case SQL_IS_SMALLINT:
	  scheme_int = s48_enter_integer((SQLSMALLINT) value);
	  break;
      case SQL_IS_USMALLINT:
	scheme_int = s48_enter_integer((SQLUSMALLINT) value);
	break;
      default:
	RAISE_UNKNOWN_INTEGER_TYPE_ERROR("SQLGetDescField", buffer_len);
      }

  res = s48_list_2(s48_enter_integer(retval), scheme_int);
  S48_GC_UNPROTECT();
  return res;
#else
  RAISE_API_VERSION_MISMATCH("SQLGetDescField", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

/* Returns the value of a single descriptor field (for strings/binary data) */
s48_value odbc_sql_get_desc_field_string(s48_value desc_handle, s48_value rec_number,
					 s48_value field_id)
{
#if (ODBCVER >= 0x300)
  SQLHDESC dh;
  SQLSMALLINT rn, fi;
  SQLCHAR *buffer = NULL;
  SQLINTEGER buffer_len, buffer_needed;
  SQLRETURN retval;
  s48_value res = S48_UNSPECIFIC;
  s48_value scheme_str = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res, scheme_str);
  ODBC_DEBUG_PRINTF_1("odbc_sql_get_desc_field_string\n");
  dh = (SQLHDESC) s48_extract_integer(desc_handle);
  rn = (SQLSMALLINT) s48_extract_integer(rec_number);
  fi = (SQLSMALLINT) s48_extract_integer(field_id);

  buffer_len = odbc_initial_retval_buffer_size;
  for (;;)
    {
      odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));
      retval = SQLGetDescField(dh, rn, fi, (SQLPOINTER) buffer,
			       buffer_len, &buffer_needed);

      if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	buffer_len = buffer_needed+1;
      else
	break;
    }

  if (SQL_SUCCEEDED(retval))
    scheme_str = s48_enter_string(buffer);
  
  free(buffer);
  res = s48_list_2(s48_enter_integer(retval), scheme_str);
  S48_GC_UNPROTECT();
  return res;
#else
  RAISE_API_VERSION_MISMATCH("SQLGetDescField", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

/*
 *
 * PART 5
 *
 * Preparing SQL requests
 *
 */

/* Prepare a SQL statement for execution */
s48_value odbc_sql_prepare(s48_value stmt_handle, s48_value stmt_txt)
{
  SQLHSTMT sh;
  SQLCHAR *query;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_prepare\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  query = (SQLCHAR *) s48_extract_string(stmt_txt);
  retval = SQLPrepare(sh, query, S48_STRING_LENGTH(stmt_txt));

  return s48_enter_integer(retval);
}

/* FIXME: implement SQLBindParameter */

s48_value odbc_sql_get_cursor_name(s48_value stmt_handle)
{
  SQLHSTMT sh;
  SQLRETURN retval;
  SQLCHAR *buffer = NULL;
  SQLSMALLINT buffer_len, buffer_needed;
  s48_value res = S48_UNSPECIFIC;
  s48_value scheme_str = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res, scheme_str);
  ODBC_DEBUG_PRINTF_1("odbc_sql_get_cursor_name\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  buffer_len = odbc_initial_retval_buffer_size;
  for (;;)
    {
      odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));
      retval = SQLGetCursorName(sh, buffer, buffer_len, &buffer_needed);
      
      if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	buffer_len = buffer_needed+1;
      else
	break;
    }

  if (SQL_SUCCEEDED(retval))
    scheme_str = s48_enter_string(buffer);

  free(buffer);
  res = s48_list_2(s48_enter_integer(retval), scheme_str);
  S48_GC_UNPROTECT();
  return res;
}

s48_value odbc_sql_set_cursor_name(s48_value stmt_handle, s48_value cursorname)
{
  SQLHSTMT sh;
  SQLCHAR *cn;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_set_cursor_name\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  cn = (SQLCHAR *) s48_extract_string(cursorname);
  retval = SQLSetCursorName(sh, cn, S48_STRING_LENGTH(cursorname));

  return s48_enter_integer(retval);
}

/*
 *
 * PART 6
 *
 * Submitting requests
 *
 */

s48_value odbc_sql_execute(s48_value stmt_handle)
{
  SQLHSTMT sh;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_execute\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  retval = SQLExecute(sh);

  return s48_enter_integer(retval);
}
  
s48_value odbc_sql_execute_direct(s48_value stmt_handle,
				  s48_value stmt)
{
  SQLHSTMT sh;
  SQLCHAR  *query;
  SQLRETURN retval;
  
  ODBC_DEBUG_PRINTF_1("odbc_sql_execute_direct\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  query = (SQLCHAR *) s48_extract_string(stmt);
  retval = SQLExecDirect(sh, query, S48_STRING_LENGTH(stmt));

  return s48_enter_integer(retval);
}

/* Returns the text of an SQL statement as translated by the driver. */
s48_value odbc_sql_native_sql(s48_value conn_handle, s48_value stmt_txt)
{
  SQLHDBC ch;
  SQLCHAR *stmt_in, *stmt_out = NULL;
  SQLINTEGER buffer_len, buffer_needed;
  SQLRETURN retval;
  s48_value res = S48_UNSPECIFIC;
  s48_value scheme_str = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res, scheme_str);
  ch = (SQLHDBC) s48_extract_integer(conn_handle);
  stmt_in = (SQLCHAR *) s48_extract_string(stmt_txt);
  ODBC_DEBUG_PRINTF_1("odbc_sql_native_sql\n");
  
  buffer_len = odbc_initial_retval_buffer_size;
  for (;;) 
    {
      odbc_sql_alloc((void **) &stmt_out, buffer_len, sizeof(SQLCHAR));
      retval = SQLNativeSql(ch, stmt_in, S48_STRING_LENGTH(stmt_txt),
			    stmt_out, buffer_len, &buffer_needed);

      if (SQL_SUCCEEDED(retval) && buffer_needed > buffer_len)
	buffer_len = buffer_needed+1;
      else
	break;
    }

  if (SQL_SUCCEEDED(retval))
    scheme_str = s48_enter_string(stmt_out);
  
  free(stmt_out);
  res = s48_list_2(s48_enter_integer(retval), scheme_str);
  S48_GC_UNPROTECT();
  return res;
}

/* Returns the description for a specific parameter in a statement */
s48_value odbc_sql_describe_param(s48_value stmt_handle, s48_value parameter_no)
{
  SQLHSTMT sh;
  SQLUSMALLINT pn;
  SQLUINTEGER ps;
  SQLSMALLINT dt, dd, n;
  SQLRETURN retval;
  s48_value res = S48_UNSPECIFIC;
  s48_value scheme_rec = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res, scheme_rec);
  ODBC_DEBUG_PRINTF_1("odbc_sql_describe_param\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  pn = (SQLUSMALLINT) s48_extract_integer(parameter_no);
  retval = SQLDescribeParam(sh, pn, &dt, &ps, &dd, &n);

  if (SQL_SUCCEEDED(retval))
    {
      scheme_rec = s48_make_record(odbc_parameter_record_type);
      S48_RECORD_SET(scheme_rec, SR_ODBC_PARAMETER_TYPE, s48_enter_integer(dt));
      S48_RECORD_SET(scheme_rec, SR_ODBC_PARAMETER_SIZE, s48_enter_integer(ps));
      S48_RECORD_SET(scheme_rec, SR_ODBC_PARAMETER_DIGITS, s48_enter_integer(dd));
      S48_RECORD_SET(scheme_rec, SR_ODBC_PARAMETER_NULLABLE, s48_enter_integer(n));
    }

  res = s48_list_2(s48_enter_integer(retval), scheme_rec);
  S48_GC_UNPROTECT();
  return res;
}

/* Returns the number of parameters in a statement */
s48_value odbc_sql_num_params(s48_value stmt_handle)
{
  SQLHSTMT sh;
  SQLSMALLINT params;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_num_params\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  retval = SQLNumParams(sh, &params);

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer(params) :
		    S48_UNSPECIFIC);
}

/*
 *
 * PART 7
 *
 * Retrieving results and information about results
 *
 */

s48_value odbc_sql_row_count(s48_value stmt_handle)
{
  SQLHSTMT sh;
  SQLRETURN retval;
  SQLINTEGER rowcount;

  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  ODBC_DEBUG_PRINTF_1("odbc_sql_row_count\n");
  retval = SQLRowCount(sh, &rowcount);

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer(rowcount) :
		    S48_UNSPECIFIC);
}
  
s48_value odbc_sql_get_data(s48_value stmt_handle, s48_value column_number, 
			    s48_value target_type)
{
  SQLHSTMT sh;
  SQLUSMALLINT cn;
  SQLSMALLINT tt, buffer_len;
  SQLINTEGER buffer_needed;
  SQLRETURN retval;
  void *buffer = NULL;
  s48_value result = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(result);
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  cn = (SQLUSMALLINT) s48_extract_integer(column_number);
  tt = (SQLSMALLINT) s48_extract_integer(target_type);
  ODBC_DEBUG_PRINTF_4("odbc_sql_get_data() sh:%x cn:%d tt:%d\n", sh, cn, tt);

  buffer_len = odbc_initial_retval_buffer_size;
  for (;;)
    {
      odbc_sql_alloc((void **) &buffer, buffer_len, sizeof_sql_c_type_identifier(tt));
      retval = SQLGetData(sh, cn, tt, buffer, buffer_len, &buffer_needed);

      if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	buffer_len = buffer_needed+1;
      else
	break;
    }

  if (SQL_SUCCEEDED(retval))
    {
      result = s48_list_2(s48_enter_integer(retval), buffer_to_s48_value(buffer, tt));
      free(buffer);
    }
  else
    result = s48_list_2(s48_enter_integer(retval), S48_UNSPECIFIC);

  S48_GC_UNPROTECT();
  return result;
}

/* Positions a cursor within a fetched block of data and allows an application
   to refresh data in the rowset or to update or delete data in the result
   set */
s48_value odbc_sql_set_pos(s48_value stmt_handle, s48_value row_number,
			   s48_value operation, s48_value lock_type)
{
  SQLHSTMT sh;
  SQLUSMALLINT rn, op, lt;
  SQLRETURN retval;

  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  rn = (SQLUSMALLINT) s48_extract_integer(row_number);
  op = (SQLUSMALLINT) s48_extract_integer(operation);
  lt = (SQLUSMALLINT) s48_extract_integer(lock_type);
  ODBC_DEBUG_PRINTF_5("odbc_sql_set_pos() sh:%x rn:%d op:%d lt:%d\n", sh, rn, op, lt);
  retval = SQLSetPos(sh, rn, op, lt);

  return s48_enter_integer(retval);
}

/* Performs bulk insertions and bulk bookmark operations, including
   update, delete, and fetch by bookmark. */
s48_value odbc_sql_bulk_operations(s48_value stmt_handle, s48_value operation)
{
#if (ODBCVER >= 0x300)
  SQLHSTMT sh;
  SQLUSMALLINT op;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_bulk_operations\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  op = (SQLUSMALLINT) s48_extract_integer(operation);
  retval = SQLBulkOperations(sh, op);
  
  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLBulkOperations", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

/* Determines whether there are more result sets available and, if so,
   initializes processing for the next result set */
s48_value odbc_sql_more_results(s48_value stmt_handle)
{
  SQLHSTMT sh;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_more_results\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  retval = SQLMoreResults(sh);

  return s48_enter_integer(retval);
}

s48_value odbc_sql_fetch(s48_value stmt_handle)
{
  SQLHSTMT sh;
  SQLRETURN retval;
  
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  ODBC_DEBUG_PRINTF_2("odbc_sql_fetch() %x\n", sh);
  retval = SQLFetch(sh);

  return s48_enter_integer(retval);
}

s48_value odbc_sql_num_result_cols(s48_value stmt_handle)
{
  SQLHSTMT sh;
  SQLSMALLINT numcols;
  SQLRETURN retval;

  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  ODBC_DEBUG_PRINTF_1("odbc_sql_num_result_cols\n");
  retval = SQLNumResultCols(sh, &numcols);

  return s48_list_2(s48_enter_integer(retval),
		    SQL_SUCCEEDED(retval) ? s48_enter_integer(numcols) :
		    S48_UNSPECIFIC);
}

s48_value odbc_sql_describe_col(s48_value stmt_handle, s48_value column_number)
{
  SQLHSTMT sh;
  SQLSMALLINT cn;
  SQLCHAR *buffer = NULL;
  SQLSMALLINT buffer_len, buffer_needed, data_type, digits, nullable;
  SQLUINTEGER col_size;
  SQLRETURN retval;
  s48_value res = S48_UNSPECIFIC;
  s48_value col_rec = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res, col_rec);
  ODBC_DEBUG_PRINTF_1("odbc_sql_describe_col\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  cn = (SQLSMALLINT) s48_extract_integer(column_number);

  buffer_len = odbc_initial_retval_buffer_size;
  for (;;)
    {
      odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));
      retval = SQLDescribeCol(sh, cn,
			      buffer, buffer_len, &buffer_needed,
			      &data_type, &col_size, &digits,
			      &nullable);
      
      if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	buffer_len = buffer_needed+1;
      else
	break;
    }

  if (SQL_SUCCEEDED(retval))
    {
      col_rec = s48_make_record(odbc_column_record_type);
      S48_RECORD_SET(col_rec, SR_ODBC_COLUMN_NAME, 
		     s48_enter_string(buffer));
      S48_RECORD_SET(col_rec, SR_ODBC_COLUMN_TYPE,
		     s48_enter_integer(data_type));
      S48_RECORD_SET(col_rec, SR_ODBC_COLUMN_SIZE,
		     s48_enter_integer(col_size));
      S48_RECORD_SET(col_rec, SR_ODBC_COLUMN_DIGITS,
		     s48_enter_integer(digits));
      S48_RECORD_SET(col_rec, SR_ODBC_COLUMN_NULLABLE,
		     s48_enter_integer(nullable));
    }

  res = s48_list_2(s48_enter_integer(retval), col_rec);
  free(buffer);
  S48_GC_UNPROTECT();
  return res;
}

/* Describes attributes of a column in the result set */
s48_value odbc_sql_col_attribute(s48_value stmt_handle, s48_value column_number,
				 s48_value field_id)
{
#if (ODBCVER >= 0x300)
  SQLHSTMT sh;
  SQLUSMALLINT cn, fi;
  SQLCHAR *buffer = NULL;
  SQLSMALLINT buffer_len, buffer_needed;
  SQLINTEGER intbuffer;
  SQLRETURN retval;
  s48_value res_string = S48_UNSPECIFIC;
  s48_value res_pair = S48_UNSPECIFIC;
  s48_value res = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(3);
  S48_GC_PROTECT_3(res, res_pair, res_string);
  ODBC_DEBUG_PRINTF_1("odbc_sql_col_attribute\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  cn = (SQLUSMALLINT) s48_extract_integer(column_number);
  fi = (SQLUSMALLINT) s48_extract_integer(field_id);

  buffer_len = odbc_initial_retval_buffer_size;
  for (;;)
    {
      odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));
      
      retval = SQLColAttribute(sh, cn, fi, 
			       buffer, buffer_len, &buffer_needed,
			       &intbuffer);

      if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	buffer_len = buffer_needed+1;
      else
	break;
    }

  if (SQL_SUCCEEDED(retval))
    {
      res_string = s48_enter_string(buffer);
      res_pair = s48_cons(res_string, s48_enter_integer(intbuffer));
    }

  free(buffer);
  res = s48_list_2(s48_enter_integer(retval), res_pair);
  S48_GC_UNPROTECT();
  return res;
#else
  RAISE_API_VERSION_MISMATCH("SQLColAttribute", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

ColumnRecPtr bindcol_lookup_binding(SQLHSTMT stmt_handle, SQLUSMALLINT column_no)
{
  StmtRecPtr stmt;
  ColumnRecPtr col;

  ODBC_DEBUG_PRINTF_3("bindcol_lookup_binding() %x %d\n", stmt_handle, column_no);
  stmt = global_bindcol_list;

  while (stmt != NULL)
    {
      if (stmt->stmt_handle == stmt_handle)
	{
	  col = stmt->col_recs;
	  while ((col != NULL) && (col->col_no != column_no))
	    col = col->next;
	  return col;
	}
      col = col->next;
    }
  return NULL;
}

s48_value bindcol_lookup_binding_scheme(s48_value stmt_handle, s48_value column_no)
{
  SQLHSTMT sh;
  SQLUSMALLINT cn;
  ColumnRecPtr col;
  
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  cn = (SQLUSMALLINT) s48_extract_integer(column_no);

  ODBC_DEBUG_PRINTF_3("bindcol_lookup_binding_scheme() sh:%x cn:%d\n", sh, cn);
  col = bindcol_lookup_binding(sh, cn);

  if (col == NULL)
    RAISE_ODBC_BINDCOL_UNBOUND_COLUMN_ERROR((long)sh, cn)
  else
    {
      ODBC_DEBUG_PRINTF_2("bindcol_lookup_binding_scheme(): col_buf:%x\n", col->col_buffer);
      return buffer_to_s48_value(col->col_buffer, col->target_type);
    }
}

/* Semantics differs from original SQLBindCol(), due to the need of
   bookkeeping a target buffer list in C. odbc_sql_bindcol() handles
   column binding and rebinding, but not unbinding. To unbind a column
   use bindcol_unbind_column() or bindcol_finalize_bindcols(). */
s48_value odbc_sql_bindcol(s48_value stmt_handle, s48_value column_no,
			   s48_value target_type, s48_value buffer_len)
{
  SQLHSTMT sh;
  SQLUSMALLINT cn;
  SQLSMALLINT tt;
  SQLLEN bl;
  SQLRETURN retval;
  ColumnRecPtr col = NULL;

  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  cn = (SQLUSMALLINT) s48_extract_integer(column_no);
  tt = (SQLSMALLINT) s48_extract_integer(target_type);
  bl = (SQLLEN) s48_extract_integer(buffer_len);

  ODBC_DEBUG_PRINTF_5("odbc_sql_bindcol() %x %d %d %d\n", sh, cn, tt, bl);
  
  /* try to look up ColumRec */
  col = bindcol_lookup_binding(sh, cn);
  
  if (col == NULL)
    {
      /* There is no binding for this stmt-handle/column-no yet */
      ODBC_DEBUG_PRINTF_1("odbc_sql_bindcol(): new binding\n");
      col = (ColumnRec *) malloc(sizeof(ColumnRec));
      if (col == NULL)
	RAISE_ODBC_BUFFER_ALLOC_ERROR(sizeof(ColumnRec));

      /* initialize it */
      col->col_no = cn;
      col->target_type = tt;
      col->buffer_len = bl;
      col->buffer_needed = 0;
      col->next = NULL;
      
      col->col_buffer = (void *) malloc(sizeof_sql_c_type_identifier(tt)*bl);
      ODBC_DEBUG_PRINTF_3("odbc_sql_bindcol() malloc %x %d\n",
			  col->col_buffer, sizeof_sql_c_type_identifier(tt)*bl);
      if (col->col_buffer == NULL)
	RAISE_ODBC_BUFFER_ALLOC_ERROR(sizeof_sql_c_type_identifier(tt)*bl);

      /* store col in global_bindcol_list */
      bindcol_bind_column(sh, cn, col);
    }
  else 
    {
      /* user wishes to rebind column for whatever reason */
      ODBC_DEBUG_PRINTF_1("odbc_sql_bindcol(): rebinding existing binding\n");
  
      if (tt != col->target_type)
	RAISE_ODBC_BINDCOL_REBINDING_ERROR("While rebinding buffers: old/new target type do not match")
      
      /* free the old buffer, allocate a new one */
      free(col->col_buffer);
      col->col_buffer = (void *) malloc(sizeof_sql_c_type_identifier(tt)*bl);
      col->buffer_len = sizeof_sql_c_type_identifier(tt)*bl;
      ODBC_DEBUG_PRINTF_3("odbc_sql_bindcol() reallocate %x %d\n", col->col_buffer,
			  sizeof_sql_c_type_identifier(tt)*bl);
      if (col->col_buffer == NULL)
	RAISE_ODBC_BUFFER_ALLOC_ERROR(sizeof_sql_c_type_identifier(tt)*bl);
    }

  /* at this point ColumRecPtr col has been created or updated, call SQLBindCol() */
  retval = SQLBindCol(sh, cn, tt, col->col_buffer, col->buffer_len, &col->buffer_needed);

  return s48_enter_integer(retval);
}

void bindcol_bind_column(SQLHSTMT stmt_handle, SQLUSMALLINT column_no, ColumnRecPtr new_col)
{
  StmtRecPtr stmt, prev_stmt, new_stmt;
  ColumnRecPtr col, prev_col;

  ODBC_DEBUG_PRINTF_4("bindcol_bind_column() %x %d %x\n", stmt_handle, column_no, new_col);
  prev_stmt = stmt = global_bindcol_list;  
  while (stmt != NULL)
    {
      if (stmt->stmt_handle == stmt_handle)
	{
	  prev_col = col = stmt->col_recs;
	  while ((col != NULL) && (col->col_no != column_no))
	    {
	      prev_col = col;
	      col = col->next;
	    }
	  if (col == NULL)
	    {
	      prev_col->next = new_col;
	      return;
	    }
	}
      prev_stmt = stmt;
      stmt = stmt->next;
    }
  if (stmt == NULL)
    {
      ODBC_DEBUG_PRINTF_1("bindcol_bind_column() global_bindcol_list is empty\n");
      new_stmt = (StmtRecPtr) malloc(sizeof(StmtRec));
      if (new_stmt == NULL)
	RAISE_ODBC_BUFFER_ALLOC_ERROR(sizeof(StmtRec));
      new_stmt->next = NULL;
      new_stmt->stmt_handle = stmt_handle;
      new_stmt->col_recs = new_col;
      if (global_bindcol_list == NULL)
	global_bindcol_list = new_stmt;
      else
	prev_stmt->next = new_stmt;
    }
}

void bindcol_unbind_colum(SQLHSTMT stmt_handle, SQLUSMALLINT column_no)
{
  StmtRecPtr stmt, prev_stmt;
  ColumnRecPtr col, prev_col;

  ODBC_DEBUG_PRINTF_3("bindcol_unbind_colum() %x %d\n", stmt_handle, column_no);

  prev_stmt = stmt = global_bindcol_list;
  while (stmt != NULL)
    {
      if (stmt->stmt_handle == stmt_handle)
	{
	  prev_col = col = stmt->col_recs;
	  while ((col != NULL) && (col->col_no != column_no))
	    {
	      prev_col = col;
	      col = col->next;
	    }
	  if (col == NULL)
	    RAISE_ODBC_BINDCOL_UNBOUND_COLUMN_ERROR((long)stmt_handle, column_no)
	  ODBC_DEBUG_PRINTF_2("bindcol_unbind_colum() free %x\n", col->col_buffer);
	  free(col->col_buffer);
	  prev_col->next = col->next;
	  free(col);
	  if (stmt->col_recs == NULL)
	    {
	      prev_stmt->next = stmt->next;
	      free(stmt);
	    }
	  return;
	}
      prev_stmt = stmt;
      stmt = stmt->next;
    }
  RAISE_ODBC_BINDCOL_UNBOUND_COLUMN_ERROR((long)stmt_handle, column_no)
}

s48_value bindcol_finalize_bindcols(s48_value stmt_handle)
{
  SQLHSTMT sh;
  StmtRecPtr stmt, prev_stmt;
  ColumnRecPtr col, prev_col, first_col;
  SQLRETURN retval;

  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  ODBC_DEBUG_PRINTF_2("bindcol_finalize_bindcols() %x\n", sh);

  prev_stmt = stmt = global_bindcol_list;
  while (stmt != NULL)
    {
      if (stmt->stmt_handle == sh)
	{
	  /* a rather dumb approach... */
	  first_col = stmt->col_recs;
	  while (first_col != NULL)
	    {
	      col = first_col;
	      while (col != NULL)
		{
		  if (col->next == NULL)
		    {
		      /* this bindcol_rec is the last in chain */
		      SQLLEN dummy;

 		      retval = SQLBindCol(sh, col->col_no, col->target_type, 
					  NULL, 0, &dummy);
		      free(col->col_buffer);
		      free(col);
		      prev_col->next = NULL;
		    }
		  prev_col = col;
		  col = col->next;
		}
	    }
	}
      stmt = stmt->next;
    }
  return S48_UNSPECIFIC;
}

/*
 *
 * PART 8
 *
 * Obtaining information about the data source's
 * system tables (catalog functxbions)
 *
 */

/* Returns a list of columns and associated privileges for one or more tables */
s48_value odbc_sql_column_privileges(s48_value stmt_handle, s48_value catalog_name,
				s48_value schema_name, s48_value table_name,
				s48_value column_name)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *table, *column;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_column_privileges\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  catalog = (SQLCHAR *) s48_extract_string(catalog_name);
  schema = (SQLCHAR *) s48_extract_string(schema_name);
  table = (SQLCHAR *) s48_extract_string(table_name);
  column = (SQLCHAR *) s48_extract_string(column_name);
  retval = SQLColumnPrivileges(sh, 
			       catalog, S48_STRING_LENGTH(catalog_name),
			       schema, S48_STRING_LENGTH(schema_name),
			       table, S48_STRING_LENGTH(table_name),
			       column, S48_STRING_LENGTH(column_name));

  return s48_enter_integer(retval);
}

/* Returns the list of column names in a specified table */
s48_value odbc_sql_columns(s48_value stmt_handle, s48_value catalog_name,
			   s48_value schema_name, s48_value table_name,
			   s48_value column_name)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *table, *column;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_columns\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  catalog = (SQLCHAR *) s48_extract_string(catalog_name);
  schema = (SQLCHAR *) s48_extract_string(schema_name);
  table = (SQLCHAR *) s48_extract_string(table_name);
  column = (SQLCHAR *) s48_extract_string(column_name);
  retval = SQLColumns(sh, 
		      catalog, S48_STRING_LENGTH(catalog_name),
		      schema, S48_STRING_LENGTH(schema_name),
		      table, S48_STRING_LENGTH(table_name),
		      column, S48_STRING_LENGTH(column_name));

  return s48_enter_integer(retval);
}

/* Returns a list of columns names that make up foreign keys, 
   if the exist for a specified table */
s48_value odbc_sql_foreign_keys(s48_value stmt_handle, s48_value pk_catalog_name,
				s48_value pk_schema_name, s48_value pk_table_name,
				s48_value fk_catalog_name, s48_value fk_schema_name,
				s48_value fk_table_name)
{
  SQLHSTMT sh;
  SQLCHAR *pk_catalog, *pk_schema, *pk_table;
  SQLCHAR *fk_catalog, *fk_schema, *fk_table;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_foreign_keys\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  pk_catalog = (SQLCHAR *) s48_extract_string(pk_catalog_name);
  pk_schema = (SQLCHAR *) s48_extract_string(pk_schema_name);
  pk_table = (SQLCHAR *) s48_extract_string(pk_table_name);
  fk_catalog = (SQLCHAR *) s48_extract_string(fk_catalog_name);
  fk_schema = (SQLCHAR *) s48_extract_string(fk_schema_name);
  fk_table = (SQLCHAR *) s48_extract_string(fk_table_name);
  retval = SQLForeignKeys(sh,
			  pk_catalog, S48_STRING_LENGTH(pk_catalog_name),
			  pk_schema, S48_STRING_LENGTH(pk_schema_name),
			  pk_table, S48_STRING_LENGTH(pk_table_name),
			  fk_catalog, S48_STRING_LENGTH(fk_catalog_name),
			  fk_schema, S48_STRING_LENGTH(fk_schema_name),
			  fk_table, S48_STRING_LENGTH(fk_table_name));

  return s48_enter_integer(retval);
}

/* Returns the list of column names that make up the primary key for a table */
s48_value odbc_sql_primary_keys(s48_value stmt_handle, s48_value catalog_name,
				s48_value schema_name, s48_value table_name)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *table;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_primary_keys\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  catalog = (SQLCHAR *) s48_extract_string(catalog_name);
  schema = (SQLCHAR *) s48_extract_string(schema_name);
  table = (SQLCHAR *) s48_extract_string(table_name);
  retval = SQLPrimaryKeys(sh, 
			  catalog, S48_STRING_LENGTH(catalog_name),
			  schema, S48_STRING_LENGTH(schema_name),
			  table, S48_STRING_LENGTH(table_name));
  
  return s48_enter_integer(retval);
}

/* Returns the list of input and output parameters, as well as the columns
   that make up the result set for the specified procedures */
s48_value odbc_sql_procedure_columns(s48_value stmt_handle, s48_value catalog_name,
				     s48_value schema_name, s48_value proc_name,
				     s48_value column_name)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *proc, *column;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_procedure_columns\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  catalog = (SQLCHAR *) s48_extract_string(catalog_name);
  schema = (SQLCHAR *) s48_extract_string(schema_name);
  proc = (SQLCHAR *) s48_extract_string(proc_name);
  column = (SQLCHAR *) s48_extract_string(column_name);
  retval = SQLProcedureColumns(sh,
			       catalog, S48_STRING_LENGTH(catalog_name),
			       schema, S48_STRING_LENGTH(schema_name),
			       proc, S48_STRING_LENGTH(proc_name),
			       column, S48_STRING_LENGTH(column_name));
  
  return s48_enter_integer(retval);
}

/* Returns the list of procedure names stored in a specific data source. */
s48_value odbc_sql_procedures(s48_value stmt_handle, s48_value catalog_name,
			      s48_value schema_name, s48_value proc_name)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *proc;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_procedures\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  catalog = (SQLCHAR *) s48_extract_string(catalog_name);
  schema = (SQLCHAR *) s48_extract_string(schema_name);
  proc = (SQLCHAR *) s48_extract_string(proc_name);
  retval = SQLProcedures(sh,
			 catalog, S48_STRING_LENGTH(catalog_name),
			 schema, S48_STRING_LENGTH(schema_name),
			 proc, S48_STRING_LENGTH(proc_name));

  return s48_enter_integer(retval);
}

/* Returns information about the optimal set of columns that uniquely identifies
   a row in a specified table, or the columns that are automatically updated
   when any value in the row is updated by a transaction */
s48_value odbc_sql_special_columns(s48_value stmt_handle, s48_value identifier_type,
				   s48_value catalog_name, s48_value schema_name,
				   s48_value table_name, s48_value scope,
				   s48_value nullable)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *table;
  SQLSMALLINT s, n, it;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_special_columns\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  it = (SQLSMALLINT) s48_extract_integer(identifier_type);
  catalog = (SQLCHAR *) s48_extract_string(catalog_name);
  schema = (SQLCHAR *) s48_extract_string(schema_name);
  table = (SQLCHAR *) s48_extract_string(table_name);
  s = (SQLSMALLINT) s48_extract_integer(scope);
  n = (SQLSMALLINT) s48_extract_integer(nullable);
  retval = SQLSpecialColumns(sh, it,
			     catalog, S48_STRING_LENGTH(catalog_name),
			     schema, S48_STRING_LENGTH(schema_name),
			     table, S48_STRING_LENGTH(table_name),
			     s, n);
  
  return s48_enter_integer(retval);
}

/* Returns statistics about a single table and the list of indexes associated 
   with the table */
s48_value odbc_sql_statistics(s48_value stmt_handle, s48_value catalog_name,
			      s48_value schema_name, s48_value table_name,
			      s48_value unique, s48_value reserved)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *table;
  SQLSMALLINT u, r;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_statistics\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  catalog = (SQLCHAR *) s48_extract_string(catalog_name);
  schema = (SQLCHAR *) s48_extract_string(schema_name);
  table = (SQLCHAR *) s48_extract_string(table_name);
  u = (SQLSMALLINT) s48_extract_integer(unique);
  r = (SQLSMALLINT) s48_extract_integer(reserved);
  retval = SQLStatistics(sh,
			 catalog, S48_STRING_LENGTH(catalog_name),
			 schema, S48_STRING_LENGTH(schema_name),
			 table, S48_STRING_LENGTH(table_name),
			 u, r);
  
  return s48_enter_integer(retval);
}

/* Returns a list of tables and the privileges associated with each table */
s48_value odbc_sql_table_privileges(s48_value stmt_handle, s48_value catalog_name,
				    s48_value schema_name, s48_value table_name)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *table;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_table_privileges\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  catalog = (SQLCHAR *) s48_extract_string(catalog_name);
  schema = (SQLCHAR *) s48_extract_string(schema_name);
  table = (SQLCHAR *) s48_extract_string(table_name);
  retval = SQLTablePrivileges(sh,
			      catalog, S48_STRING_LENGTH(catalog_name),
			      schema, S48_STRING_LENGTH(schema_name),
			      table, S48_STRING_LENGTH(table_name));

  return s48_enter_integer(retval);
}

/* Returns the list of table names stored in a specific data source */
s48_value odbc_sql_tables(s48_value stmt_handle, s48_value catalog_name,
			  s48_value schema_name, s48_value table_name,
			  s48_value table_type)
{
  SQLHSTMT sh;
  SQLCHAR *catalog, *schema, *tablen, *tablet;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_tables\n");
  retval = SQLTables(sh,
		     catalog, S48_STRING_LENGTH(catalog_name),
		     schema, S48_STRING_LENGTH(schema_name),
		     tablen, S48_STRING_LENGTH(table_name),
		     tablet, S48_STRING_LENGTH(table_type));

  return s48_enter_integer(retval);
}

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
s48_value odbc_sql_free_statement(s48_value stmt_handle, s48_value option)
{
  SQLHSTMT sh;
  SQLUSMALLINT opt;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_free_statement\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  opt = (SQLUSMALLINT) s48_extract_integer(option);
  retval = SQLFreeStmt(sh, opt);

  return s48_enter_integer(retval);
}

/* Closes a cursor that has been opened on a statement handle */
s48_value odbc_sql_close_cursor(s48_value stmt_handle)
{
#if (ODBCVER >= 0x300)
  SQLHSTMT sh;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_close_cursor\n");
  sh = (SQLHSTMT) s48_extract_integer(stmt_handle);
  retval = SQLCloseCursor(sh);

  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLCloseCursor", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

/* Cancels an SQL statement */
s48_value odbc_sql_cancel(s48_value stmt_handle)
{
  SQLHSTMT sh;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_cancel\n");
  retval = SQLCancel(sh);
  
  return s48_enter_integer(retval);
}

/* Commits or rolls back a transaction */
s48_value odbc_sql_endtran(s48_value handle_type, s48_value handle,
			   s48_value completion_type)
{
#if (ODBCVER >= 0x300)
  SQLSMALLINT ht, ct;
  SQLHANDLE h;
  SQLRETURN retval;

  ODBC_DEBUG_PRINTF_1("odbc_sql_endtran\n");
  ht = (SQLSMALLINT) s48_extract_integer(handle_type);
  h = (SQLHANDLE) s48_extract_integer(handle);
  ct = (SQLSMALLINT) s48_extract_integer(completion_type);

  retval = SQLEndTran(ht, h, ct);
  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLEndTran", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

/*
 *
 * PART 10
 *
 * Terminating a connection
 *
 */

/* Closes the connection */
s48_value odbc_sql_disconnect(s48_value conn_handle)
{
  SQLHDBC ch;
  SQLRETURN retval;

  ch = (SQLHDBC) s48_extract_integer(conn_handle);
  ODBC_DEBUG_PRINTF_2("odbc_sql_disconnect() %x\n", ch);
  retval = SQLDisconnect(ch);

  return s48_enter_integer(retval);
}

/* Free a handle */
s48_value odbc_sql_free_handle(s48_value handle_type, s48_value handle) 
{
#if (ODBCVER >= 0x300)
  SQLSMALLINT ht;
  SQLHANDLE h;
  SQLRETURN retval;

  ht = (SQLSMALLINT) s48_extract_integer(handle_type);
  h = (SQLHANDLE) s48_extract_integer(handle);
  ODBC_DEBUG_PRINTF_3("odbc_sql_free_handle() %x %d\n", h, ht);
  retval = SQLFreeHandle(ht, h);
  
  return s48_enter_integer(retval);
#else
  RAISE_API_VERSION_MISMATCH("SQLFreeHandle", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}

/*
 *
 * PART 11
 *
 * misc. functions
 *
 */

s48_value odbc_sql_get_diag_recs(s48_value handle_type, s48_value handle)
{
#if (ODBCVER >= 0x300)
  SQLSMALLINT ht;
  SQLHANDLE h;
  SQLCHAR sql_state[6];
  SQLINTEGER native_error;
  SQLCHAR *buffer = NULL;
  SQLSMALLINT i, more_recs, buffer_len, buffer_needed;
  SQLRETURN retval;
  s48_value res = S48_UNSPECIFIC;
  s48_value rec_list = S48_NULL;
  s48_value diag_rec = S48_UNSPECIFIC;

  S48_DECLARE_GC_PROTECT(3);
  S48_GC_PROTECT_3(res, rec_list, diag_rec);
  ht = (SQLSMALLINT) s48_extract_integer(handle_type);
  h = (SQLHANDLE) s48_extract_integer(handle);
  ODBC_DEBUG_PRINTF_4("SQLGetDiagRec(%d, %x, %i)\n", ht, h, i);

  i = more_recs = 1;
  buffer_len = odbc_initial_retval_buffer_size;
  while (more_recs) {

    diag_rec = s48_make_record(odbc_diag_record_type);

    for (;;)
      {
	odbc_sql_alloc((void **) &buffer, buffer_len, sizeof(SQLCHAR));
	retval = SQLGetDiagRec(ht, h, i, 
			       sql_state, &native_error,
			       buffer, buffer_len, &buffer_needed);

	if (SQL_SUCCEEDED(retval) && (buffer_needed > buffer_len))
	  {
	    ODBC_DEBUG_PRINTF_3("buffer_needed %d buffer_len %d\n", buffer_needed, buffer_len);
	    buffer_len = buffer_needed+1;
	  }
	else
	  break;
      }

    switch (retval)
      {
      case SQL_SUCCESS: case SQL_SUCCESS_WITH_INFO:
	{
	  S48_RECORD_SET(diag_rec, SR_ODBC_DIAG_SQL_STATE, s48_enter_string(sql_state));
	  S48_RECORD_SET(diag_rec, SR_ODBC_DIAG_NATIVE_ERROR, s48_enter_integer(native_error));
	  S48_RECORD_SET(diag_rec, SR_ODBC_DIAG_MESSAGE, s48_enter_string(buffer));
	  rec_list = s48_cons(diag_rec, rec_list);
	  free(buffer);
	  buffer = NULL;
	  break;
	}
      case SQL_NO_DATA:
	{ 
	  more_recs = 0;
	  break;
	}
      default:
	{
	  free(buffer);
	  res = s48_list_2(s48_enter_integer(retval), S48_UNSPECIFIC);
	  S48_GC_UNPROTECT();
	  return res;
	}
      }
    i++;
  }
  
  res = s48_list_2(s48_enter_integer(retval), rec_list);
  free(buffer);
  S48_GC_UNPROTECT(); 
  return res;
#else
  RAISE_API_VERSION_MISMATCH("SQLGetDiagRec", ODBCVER, 0x300);
#endif /* ODBCVER >= 0x300 */
}    
    
#if ODBC_DEBUG_MSGS
/* print detailed debug information */

void odbc_debug_msgs(SQLSMALLINT handle_type, SQLHANDLE handle) 
{

  SQLCHAR sql_state[5];
  SQLINTEGER native_error;
  SQLCHAR message[ERROR_MSG_BUFFER_LEN];
  SQLSMALLINT i, message_len;
  SQLRETURN retval;

  printf("fetching diag_rec from odbc driver.\n");
  i = 1;
  while (1) {

    retval = SQLGetDiagRec(handle_type, handle,
			   i, sql_state,
			   &native_error, 
			   message, ERROR_MSG_BUFFER_LEN-1, 
			   &message_len);
	 
    if (retval == SQL_NO_DATA)
      break;
    
    if (retval == SQL_INVALID_HANDLE) {
      printf("ODBC: Could not get debug information: invalid handle provided\n");
      break;
    }
    
    if (retval == SQL_ERROR) {
      printf("ODBC: SQLGetDiagRec returned SQL_ERROR\n");
      break;
    }
    
    if (retval == SQL_SUCCESS_WITH_INFO)
      printf("ODBC warning: error message buffer too small.\n");
    
    if (retval == SQL_SUCCESS) {
      printf("\nODBC status record %d:\n", i);
      printf("SQL state: %s\n", (char *)sql_state);
      /* TODO: 
       * Need to find out how to printf the
       * native_error here 
       */
      printf("error msg: %s\n", message);
    }
    
    i++;
  }
}
#endif
 
/* convert Scheme sql-date record to SQL_DATE_STRUCT */
void sql_date_record_to_struct(s48_value sql_date, SQL_DATE_STRUCT *ds)
{  
  ds->year = (SQLSMALLINT) 
    s48_extract_integer(S48_RECORD_REF(sql_date, SR_SQL_DATE_YEAR));

  ds->month = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_date, SR_SQL_DATE_MONTH));
  
  ds->day = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_date, SR_SQL_DATE_DAY));
}

/* convert SQL_DATE_STRUCT to Scheme sql-date record */
s48_value struct_to_sql_date_record(SQL_DATE_STRUCT *ds)
{
  s48_value sql_date;

  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(sql_date);

  sql_date = s48_make_record(sql_date_record_type);

  S48_RECORD_SET(sql_date, SR_SQL_DATE_YEAR,
		 s48_enter_integer(ds->year));
  S48_RECORD_SET(sql_date, SR_SQL_DATE_MONTH,
		 s48_enter_integer(ds->month));
  S48_RECORD_SET(sql_date, SR_SQL_DATE_DAY,
		 s48_enter_integer(ds->day));

  S48_GC_UNPROTECT();
  return sql_date;
}
  
/* convert Scheme sql-time record to SQL_TIME_STRUCT */
void sql_time_record_to_struct(s48_value sql_time, SQL_TIME_STRUCT *ts)
{
  ts->hour = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_time, SR_SQL_TIME_HOUR));
  
  ts->minute = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_time, SR_SQL_TIME_MINUTE));
  
  ts->second = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_time, SR_SQL_TIME_SECOND));
}

/* convert SQL_TIME_STRUCT to Scheme sql-time record */
s48_value struct_to_sql_time_record(SQL_TIME_STRUCT *ts)
{
  s48_value sql_time;

  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(sql_time);

  sql_time = s48_make_record(sql_time_record_type);
  
  S48_RECORD_SET(sql_time, SR_SQL_TIME_HOUR,
		 s48_extract_integer(ts->hour));
  
  S48_RECORD_SET(sql_time, SR_SQL_TIME_MINUTE,
		 s48_extract_integer(ts->minute));
  
  S48_RECORD_SET(sql_time, SR_SQL_TIME_SECOND,
		 s48_extract_integer(ts->second));

  S48_GC_UNPROTECT();
  return sql_time;
}

/* convert Scheme sql-timestamp record to SQL_TIMESTAMP_STRUCT */ 
void sql_timestamp_record_to_struct(s48_value sql_timestamp, 
				    SQL_TIMESTAMP_STRUCT *ts)
{
  ts->year = (SQLSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_timestamp, SR_SQL_TIMESTAMP_YEAR));
  
  ts->month = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_timestamp, SR_SQL_TIMESTAMP_MONTH));
  
  ts->day = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_timestamp, SR_SQL_TIMESTAMP_DAY));
  
  ts->hour = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_timestamp, SR_SQL_TIMESTAMP_HOUR));
  
  ts->minute = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_timestamp, SR_SQL_TIMESTAMP_MINUTE));

  ts->second = (SQLUSMALLINT)
    s48_extract_integer(S48_RECORD_REF(sql_timestamp, SR_SQL_TIMESTAMP_SECOND));

  ts->fraction = (SQLUINTEGER)
    s48_extract_integer(S48_RECORD_REF(sql_timestamp, SR_SQL_TIMESTAMP_FRACTION));
}

/* convert SQL_TIMESTAMP_STRUCT to Scheme sql-timestamp record */
s48_value struct_to_sql_timestamp_record(SQL_TIMESTAMP_STRUCT *ts)
{
  s48_value sql_timestamp;

  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(sql_timestamp);

  sql_timestamp = s48_make_record(sql_timestamp_record_type);

  S48_RECORD_SET(sql_timestamp, SR_SQL_TIMESTAMP_YEAR,
		 s48_extract_integer(ts->year));
  
  S48_RECORD_SET(sql_timestamp, SR_SQL_TIMESTAMP_MONTH,
		 s48_extract_integer(ts->month));
  
  S48_RECORD_SET(sql_timestamp, SR_SQL_TIMESTAMP_DAY,
		 s48_extract_integer(ts->day));

  S48_RECORD_SET(sql_timestamp, SR_SQL_TIMESTAMP_HOUR,
		 s48_extract_integer(ts->hour));
  
  S48_RECORD_SET(sql_timestamp, SR_SQL_TIMESTAMP_MINUTE,
		 s48_extract_integer(ts->minute));

  S48_RECORD_SET(sql_timestamp, SR_SQL_TIMESTAMP_SECOND,
		 s48_extract_integer(ts->second));

  S48_RECORD_SET(sql_timestamp, SR_SQL_TIMESTAMP_FRACTION,
		 s48_extract_integer(ts->fraction));

  S48_GC_UNPROTECT();
  return sql_timestamp;
}

/* gets a C type identifier and returns the size of this type,
   according to Appendix D of ODBC Programmer's reference */
size_t sizeof_sql_c_type_identifier(SQLSMALLINT ctypeid) 
{ 
  switch (ctypeid) 
    { 
    case SQL_C_CHAR: return sizeof(SQLCHAR);
    case SQL_C_SSHORT: case SQL_C_SHORT: return sizeof(SQLSMALLINT);
    case SQL_C_USHORT: return sizeof(SQLUSMALLINT);
    case SQL_C_SLONG: case SQL_C_LONG: return sizeof(SQLINTEGER);
    case SQL_C_ULONG: return sizeof(SQLUINTEGER);
    case SQL_C_FLOAT: return sizeof(SQLREAL);
    case SQL_C_DOUBLE: return sizeof(SQLDOUBLE);
    case SQL_C_BIT: return sizeof(SQLCHAR);
    case SQL_C_STINYINT: return sizeof(SQLSCHAR);
    case SQL_C_UTINYINT: return sizeof(SQLCHAR);
/*     case SQL_SBIGINT: return sizeof(SQLBIGINT); */
/*     case SQL_UBIGINT: return sizeof(SQLUBIGINT); */
    case SQL_C_BINARY: return sizeof(SQLCHAR);
      /* I don't know what to do with these types... */
/*     case SQL_C_BOOKMARK: return sizeof(BOOKMARK); */
/*     case SQL_C_VARBOOKMARK: return sizeof(SQLCHAR); */
    case SQL_C_TYPE_DATE: return sizeof(SQL_DATE_STRUCT);
    case SQL_C_TYPE_TIME: return sizeof(SQL_TIME_STRUCT);
    case SQL_C_TYPE_TIMESTAMP: return sizeof(SQL_TIMESTAMP_STRUCT);
    case SQL_C_NUMERIC: return sizeof(SQL_NUMERIC_STRUCT);
      /*     case SQL_C_GUID: return sizeof(SQLGUID); */
    default: 
      RAISE_ODBC_UNKNOWN_C_TYPE_IDENTIFIER_ERROR(0, ctypeid);
      break;
    }
}

	
void odbc_sql_alloc(void **buffer, size_t buffer_len, size_t type_len)
{
  ODBC_DEBUG_PRINTF_4("odbc_sql_alloc(): bf:%x bl:%d tl: %d\n", buffer, buffer_len, type_len);
  if (*buffer == NULL)
    {
      ODBC_DEBUG_PRINTF_3("calloc bl:%d tl:%d\n", buffer_len+1, type_len);
      *buffer = (void *) calloc(buffer_len+1, type_len);
    }
  else
    {
      ODBC_DEBUG_PRINTF_2("realloc bl:%d\n", buffer_len*type_len);
      *buffer = (void *) realloc(*buffer, (buffer_len+1)*type_len);
    }

  if (*buffer == NULL)
    RAISE_ODBC_BUFFER_ALLOC_ERROR((buffer_len+1)*type_len);
}

s48_value buffer_to_s48_value(void *buffer, SQLSMALLINT ctypeid)
{
  ODBC_DEBUG_PRINTF_3("buffer_to_s48_value(): %x %d\n", buffer, ctypeid);
  switch (ctypeid)
    {
    case SQL_C_CHAR:
      return s48_enter_string((SQLCHAR *) buffer);
    case SQL_C_SSHORT: case SQL_C_SHORT:
      return s48_enter_integer(*((SQLSMALLINT*) buffer));
    case SQL_C_USHORT:
      return s48_enter_integer(*((SQLUSMALLINT*) buffer));
    case SQL_C_SLONG: case SQL_C_LONG:
      return s48_enter_integer(*((SQLINTEGER*) buffer));
    case SQL_C_ULONG:
      return s48_enter_integer(*((SQLUINTEGER*) buffer));
      /* case SQL_C_FLOAT: */
      /* case SQL_C_DOUBLE: */
      /* case SQL_C_BIT: */
    case SQL_C_STINYINT:
      return s48_enter_integer(*((SQLSCHAR *) buffer));
    case SQL_C_UTINYINT:
      return s48_enter_integer(*((SQLCHAR *) buffer));
    case SQL_C_BINARY:
      return s48_enter_string((SQLCHAR *) buffer);
    case SQL_C_TYPE_DATE: 
      return struct_to_sql_date_record((SQL_DATE_STRUCT *) buffer);
    case SQL_C_TYPE_TIME:
      return struct_to_sql_time_record((SQL_TIME_STRUCT *) buffer);
    case SQL_C_TYPE_TIMESTAMP:
      return struct_to_sql_timestamp_record((SQL_TIMESTAMP_STRUCT *) buffer);
      /*    case SQL_C_NUMERIC: */
    default:
      RAISE_ODBC_UNKNOWN_C_TYPE_IDENTIFIER_ERROR((long)buffer, ctypeid);
      break;
    }
}

/* set initial return value buffer size */
s48_value odbc_set_initial_retval_buffer_size(s48_value nobytes)
{
  odbc_initial_retval_buffer_size = (SQLUSMALLINT) s48_extract_integer(nobytes);
}

/* get initial return value buffer size */
s48_value odbc_get_intial_retval_buffer_size()
{
  return s48_enter_integer((int) odbc_initial_retval_buffer_size);
}

void s48_init_odbc(void)
{
  /* bindings for record types */
  S48_GC_PROTECT_GLOBAL(odbc_diag_record_type);
  odbc_diag_record_type = s48_get_imported_binding("odbc-diag");

  S48_GC_PROTECT_GLOBAL(odbc_column_record_type);
  odbc_column_record_type = s48_get_imported_binding("odbc-column");

  S48_GC_PROTECT_GLOBAL(odbc_parameter_record_type);
  odbc_column_record_type = s48_get_imported_binding("odbc-parameter");

  S48_GC_PROTECT_GLOBAL(bindcol_buffer_record_type);
  bindcol_buffer_record_type = s48_get_imported_binding("bindcol-buffer");

  /* import conditions */
  S48_GC_PROTECT_GLOBAL(raise_odbc_api_version_mismatch_error);
  raise_odbc_api_version_mismatch_error 
    = s48_get_imported_binding("raise-odbc-api-version-mismatch-error");
  S48_GC_PROTECT_GLOBAL(raise_odbc_unknown_integer_type_error);
  raise_odbc_unknown_integer_type_error 
    = s48_get_imported_binding("raise-odbc-unknown-integer-type-error");
  S48_GC_PROTECT_GLOBAL(raise_odbc_buffer_alloc_error);
  raise_odbc_buffer_alloc_error
    = s48_get_imported_binding("raise-odbc-buffer-alloc-error");
  S48_GC_PROTECT_GLOBAL(raise_odbc_unknown_c_type_identifier_error);
  raise_odbc_unknown_c_type_identifier_error
    = s48_get_imported_binding("raise-odbc-unknown-c-type-identifier-error");
  S48_GC_PROTECT_GLOBAL(raise_odbc_bindcol_unbound_column_error);
  raise_odbc_bindcol_unbound_column_error
    = s48_get_imported_binding("raise-odbc-bindcol-unbound-column-error");
  S48_GC_PROTECT_GLOBAL(raise_odbc_bindcol_rebinding_error);
  raise_odbc_bindcol_rebinding_error
    = s48_get_imported_binding("raise-odbc-bindcol-rebinding-error");

  /* init global variables */
  global_bindcol_list = NULL;

  /* functions for SQLBindCol() */
  S48_EXPORT_FUNCTION(bindcol_lookup_binding_scheme);
  S48_EXPORT_FUNCTION(odbc_sql_bindcol);

  /* PART 1 */
  S48_EXPORT_FUNCTION(odbc_alloc_environment_handle);
  S48_EXPORT_FUNCTION(odbc_alloc_connection_handle);
  S48_EXPORT_FUNCTION(odbc_alloc_statement_handle);
    
  S48_EXPORT_FUNCTION(odbc_sql_connect);
  S48_EXPORT_FUNCTION(odbc_sql_browse_connect);

  /* PART 2 */
  S48_EXPORT_FUNCTION(odbc_sql_data_sources);
  S48_EXPORT_FUNCTION(odbc_sql_drivers);
  S48_EXPORT_FUNCTION(odbc_sql_get_info_int); 
  S48_EXPORT_FUNCTION(odbc_sql_get_info_string);
  S48_EXPORT_FUNCTION(odbc_sql_get_func_exists); 
  S48_EXPORT_FUNCTION(odbc_sql_get_type_info);

  /* PART 3 */
   S48_EXPORT_FUNCTION(odbc_sql_set_connect_attr_int);
   S48_EXPORT_FUNCTION(odbc_sql_set_connect_attr_string);
   S48_EXPORT_FUNCTION(odbc_sql_get_connect_attr_string);
   S48_EXPORT_FUNCTION(odbc_sql_get_connect_attr_int);
   S48_EXPORT_FUNCTION(odbc_sql_set_env_attr_int);
   S48_EXPORT_FUNCTION(odbc_sql_get_env_attr_int);
   S48_EXPORT_FUNCTION(odbc_sql_set_stmt_attr_int);
   S48_EXPORT_FUNCTION(odbc_sql_set_stmt_attr_string);
   S48_EXPORT_FUNCTION(odbc_sql_get_stmt_attr_int); 
   S48_EXPORT_FUNCTION(odbc_sql_get_stmt_attr_string);
  
   /* PART 4 */
   S48_EXPORT_FUNCTION(odbc_sql_get_desc_field_int);
   S48_EXPORT_FUNCTION(odbc_sql_get_desc_field_string);

   /* PART 5 */
   S48_EXPORT_FUNCTION(odbc_sql_prepare);
   S48_EXPORT_FUNCTION(odbc_sql_get_cursor_name);
   S48_EXPORT_FUNCTION(odbc_sql_set_cursor_name);

  /* PART 6 */
  S48_EXPORT_FUNCTION(odbc_sql_execute);
  S48_EXPORT_FUNCTION(odbc_sql_execute_direct);
  S48_EXPORT_FUNCTION(odbc_sql_native_sql);
  S48_EXPORT_FUNCTION(odbc_sql_describe_param);
  S48_EXPORT_FUNCTION(odbc_sql_num_params);

  /* PART 7 */
  S48_EXPORT_FUNCTION(odbc_sql_row_count);
  S48_EXPORT_FUNCTION(odbc_sql_get_data);
  S48_EXPORT_FUNCTION(odbc_sql_set_pos);
  S48_EXPORT_FUNCTION(odbc_sql_bulk_operations);
  S48_EXPORT_FUNCTION(odbc_sql_more_results);
  S48_EXPORT_FUNCTION(odbc_sql_fetch);
  S48_EXPORT_FUNCTION(odbc_sql_num_result_cols);
  S48_EXPORT_FUNCTION(odbc_sql_describe_col);
  S48_EXPORT_FUNCTION(odbc_sql_col_attribute);

  /* PART 8 */
  S48_EXPORT_FUNCTION(odbc_sql_column_privileges);
  S48_EXPORT_FUNCTION(odbc_sql_columns);
  S48_EXPORT_FUNCTION(odbc_sql_foreign_keys);
  S48_EXPORT_FUNCTION(odbc_sql_primary_keys);
  S48_EXPORT_FUNCTION(odbc_sql_procedure_columns);
  S48_EXPORT_FUNCTION(odbc_sql_procedures);
  S48_EXPORT_FUNCTION(odbc_sql_special_columns);
  S48_EXPORT_FUNCTION(odbc_sql_statistics);
  S48_EXPORT_FUNCTION(odbc_sql_table_privileges);
  S48_EXPORT_FUNCTION(odbc_sql_tables);

  /* PART 9 */
  S48_EXPORT_FUNCTION(odbc_sql_free_statement);
  S48_EXPORT_FUNCTION(odbc_sql_close_cursor);
  S48_EXPORT_FUNCTION(odbc_sql_cancel);
  S48_EXPORT_FUNCTION(odbc_sql_endtran);

  /* PART 10 */
  S48_EXPORT_FUNCTION(odbc_sql_disconnect);
  S48_EXPORT_FUNCTION(odbc_sql_free_handle);

  /* PART 11 */
  S48_EXPORT_FUNCTION(odbc_sql_get_diag_recs);

  /* misc functions */
  S48_EXPORT_FUNCTION(bindcol_finalize_bindcols);
  S48_EXPORT_FUNCTION(odbc_set_initial_retval_buffer_size);
  S48_EXPORT_FUNCTION(odbc_get_intial_retval_buffer_size);
  
}
