/* Copyright (c) 1993-2000 by Richard Kelsey and Jonathan Rees.
   See file COPYING. */

/*
 * Scheme 48/POSIX regex interface
 */

#include <sys/types.h>
#include <regex.h> /* POSIX.2 */
#include <stdlib.h>
#include <unistd.h>

#include "scheme48.h"

extern void		s48_init_posix_regex(void);
static s48_value	posix_compile_regexp(s48_value pattern,
					     s48_value extended_p,
					     s48_value ignore_case_p,
					     s48_value submatches_p,
					     s48_value newline_p),
			posix_regexp_match(s48_value sch_regex,
					   s48_value string,
					   s48_value submatches_p,
					   s48_value bol_p,
					   s48_value eol_p,
					   s48_value sch_start),
			posix_regexp_error_message(s48_value pattern,
					     s48_value extended_p,
					     s48_value ignore_case_p,
					     s48_value submatches_p,
					     s48_value newline_p),
			posix_free_regexp(s48_value sch_regex);

/*
 * Record type imported from Scheme.
 */
static s48_value 	posix_regexp_match_type_binding = S48_FALSE;

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_posix_regexp(void)
{
  /* Export our stuff. */
  S48_EXPORT_FUNCTION(posix_compile_regexp);
  S48_EXPORT_FUNCTION(posix_regexp_match);
  S48_EXPORT_FUNCTION(posix_regexp_error_message);
  S48_EXPORT_FUNCTION(posix_free_regexp);

  /* Protect and import the regex-match type. */
  S48_GC_PROTECT_GLOBAL(posix_regexp_match_type_binding);
  posix_regexp_match_type_binding =
    s48_get_imported_binding("posix-regexp-match-type");
}

/*
 * Interface to regcomp.  We encode the flags, make the return value, and
 * then call regcomp() to fill it in.
 */
static s48_value
posix_compile_regexp(s48_value pattern,
		     s48_value extended_p, s48_value ignore_case_p,
		     s48_value submatches_p, s48_value newline_p)
{
  s48_value sch_regex;
  int status;
  S48_DECLARE_GC_PROTECT(1);
  int flags = S48_EXTRACT_BOOLEAN(extended_p)    ? REG_EXTENDED : 0 |
              S48_EXTRACT_BOOLEAN(ignore_case_p) ? REG_ICASE    : 0 |
              S48_EXTRACT_BOOLEAN(submatches_p)  ? 0 : REG_NOSUB |
              S48_EXTRACT_BOOLEAN(newline_p)     ? REG_NEWLINE  : 0;

  S48_GC_PROTECT_1(pattern);

  S48_CHECK_STRING(pattern);

  sch_regex = S48_MAKE_VALUE(regex_t);

  status = regcomp(S48_UNSAFE_EXTRACT_VALUE_POINTER(sch_regex, regex_t),
		   S48_UNSAFE_EXTRACT_STRING(pattern),
		   flags);

  S48_GC_UNPROTECT();

  if (status == 0)
    return sch_regex;
  else
    return S48_UNSAFE_ENTER_FIXNUM(status);   /* not that it can do them much good */
}

/*
 * Interface to regexec.
 *
 * Returns #f if there is no match, #t if there is a match and submatches_p
 * is false, and a list of regex-match records otherwise.
 *
 * Most of this is making the buffer for the match structs and then translating
 * them into Scheme match records.
 */
static s48_value
posix_regexp_match(s48_value sch_regex, s48_value string,
		   s48_value submatches_p,
		   s48_value bol_p, s48_value eol_p,
		   s48_value sch_start)
{
  int status;
  s48_value result;
  int start, len;
  /* re_nsub doesn't include the full pattern */
  size_t nmatch   = 1 + S48_EXTRACT_VALUE_POINTER(sch_regex, regex_t)->re_nsub;
  regmatch_t *pmatch,
             pmatch_buffer[32];
  int flags = S48_EXTRACT_BOOLEAN(bol_p) ? 0 : REG_NOTBOL |
              S48_EXTRACT_BOOLEAN(eol_p) ? 0 : REG_NOTEOL;

  start = s48_extract_fixnum(sch_start);
  len = S48_STRING_LENGTH(string);
  if ((start < 0) || (start > len))
    s48_raise_range_error(sch_start,
			  s48_enter_fixnum(0), 
			  s48_enter_fixnum(len));

  if (nmatch <= 32)
    pmatch = pmatch_buffer;
  else {
    pmatch = (regmatch_t *) malloc(nmatch * sizeof(regmatch_t));
    if (pmatch == NULL)
      s48_raise_out_of_memory_error(); }
    
  status = regexec(S48_EXTRACT_VALUE_POINTER(sch_regex, regex_t),
		   S48_UNSAFE_EXTRACT_STRING(string) + start,
		   nmatch, pmatch, flags);

  if (status == REG_NOMATCH)
    result = S48_FALSE;
  else if (! S48_EXTRACT_BOOLEAN(submatches_p))
    result = S48_TRUE;
  else {
    s48_value match = S48_FALSE;
    s48_value matches = S48_NULL;
    int i;
    S48_DECLARE_GC_PROTECT(2);
  
    S48_GC_PROTECT_2(match, matches);
    
    for(i = nmatch - 1; i > -1; i--) {
      if (pmatch[i].rm_so == -1)
	match = S48_FALSE;
      else {
	match = s48_make_record(posix_regexp_match_type_binding);
	S48_UNSAFE_RECORD_SET(match, 0,
			      s48_enter_fixnum(pmatch[i].rm_so + start));
	S48_UNSAFE_RECORD_SET(match, 1,
			      s48_enter_fixnum(pmatch[i].rm_eo + start));
	S48_UNSAFE_RECORD_SET(match, 2, S48_FALSE); }  /* submatches */
      matches = s48_cons(match, matches); }
    
    S48_GC_UNPROTECT();

    result = matches; }

  if (nmatch > 32)
    free(pmatch);
  
  return result;
}

/*
 * Interface to regcomp.
 *
 * This takes the same arguments as `compile_regexp' but returns the error
 * message, if any, that `regcomp()' returns.  For some reason `regerror()'
 * requires both the status code and the compiled pattern buffer returned
 * by `regcomp()'.  `compile_regexp' only returned the status so we have to
 * redo the compilation.
 *
 */
static s48_value
posix_regexp_error_message(s48_value pattern,
			   s48_value extended_p, s48_value ignore_case_p,
			   s48_value submatches_p, s48_value newline_p)
{
  regex_t compiled_regex;
  int status;
  int flags = S48_EXTRACT_BOOLEAN(extended_p)    ? REG_EXTENDED : 0 |
              S48_EXTRACT_BOOLEAN(ignore_case_p) ? REG_ICASE    : 0 |
              S48_EXTRACT_BOOLEAN(submatches_p)  ? 0 : REG_NOSUB |
              S48_EXTRACT_BOOLEAN(newline_p)     ? REG_NEWLINE  : 0;

  S48_CHECK_STRING(pattern);

  status = regcomp(&compiled_regex, S48_UNSAFE_EXTRACT_STRING(pattern), flags);

  if (status == 0)
    return S48_FALSE;
  else {
    size_t buffer_size;
    s48_value buffer;
    
    buffer_size = regerror(status, &compiled_regex, NULL, 0);
    /* For string lengths C counts the nul, Scheme doesn't. */
    buffer = s48_make_string(buffer_size - 1, ' ');
    regerror(status,
	     &compiled_regex,
	     S48_UNSAFE_EXTRACT_STRING(buffer),
	     buffer_size);
    
    return buffer; }
}

/*
 * Stub for regfree().
 */

static s48_value
posix_free_regexp(s48_value sch_regex)
{
  regfree(S48_EXTRACT_VALUE_POINTER(sch_regex, regex_t));

  return S48_UNSPECIFIC;
}
