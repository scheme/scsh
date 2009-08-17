/* Copyright (c) 1993-2000 by Richard Kelsey and Jonathan Rees.
   See file COPYING. */

#include <syslog.h>
#include "scheme48.h"

static s48_value	sch_openlog(s48_value sch_ident,
				    s48_value sch_option,
				    s48_value sch_facility),
			sch_setlogmask(s48_value sch_logmask),
			sch_syslog(s48_value sch_level,
				   s48_value sch_facility,
				   s48_value sch_message),
			sch_closelog(void);

/*
 * Record types imported from Scheme.
 */
static s48_value	is_syslog_options_binding = S48_FALSE;
static s48_value	syslog_facility_type_binding = S48_FALSE;
static s48_value	syslog_facilities_binding = S48_FALSE;
static s48_value	syslog_level_type_binding = S48_FALSE;
static s48_value	syslog_levels_binding = S48_FALSE;
static s48_value	is_syslog_mask_binding = S48_FALSE;
static s48_value	syslog_mask_type_binding = S48_FALSE;

static s48_value	is_enum_set_binding = S48_FALSE;
static s48_value	enum_set2integer_binding = S48_FALSE;
static s48_value	integer2enum_set_binding = S48_FALSE;

/*
 * Install all exported functions in Scheme 48.
 */
void
s48_init_syslog(void)
{
  S48_EXPORT_FUNCTION(sch_openlog);
  S48_EXPORT_FUNCTION(sch_syslog);
  S48_EXPORT_FUNCTION(sch_setlogmask);
  S48_EXPORT_FUNCTION(sch_closelog);

  S48_GC_PROTECT_GLOBAL(is_syslog_options_binding);
  is_syslog_options_binding =
    s48_get_imported_binding("syslog-options?");

  S48_GC_PROTECT_GLOBAL(syslog_facility_type_binding);
  syslog_facility_type_binding =
    s48_get_imported_binding("syslog-facility-type");
  S48_GC_PROTECT_GLOBAL(syslog_facilities_binding);
  syslog_facilities_binding =
    s48_get_imported_binding("syslog-facilities");

  S48_GC_PROTECT_GLOBAL(syslog_level_type_binding);
  syslog_level_type_binding =
    s48_get_imported_binding("syslog-level-type");
  S48_GC_PROTECT_GLOBAL(syslog_levels_binding);
  syslog_levels_binding =
    s48_get_imported_binding("syslog-levels");

  S48_GC_PROTECT_GLOBAL(is_syslog_mask_binding);
  is_syslog_mask_binding =
    s48_get_imported_binding("syslog-mask?");

  S48_GC_PROTECT_GLOBAL(syslog_mask_type_binding);
  syslog_mask_type_binding =
    s48_get_imported_binding(":syslog-mask");

  S48_GC_PROTECT_GLOBAL(is_enum_set_binding);
  is_enum_set_binding =
    s48_get_imported_binding("enum-set?");

  S48_GC_PROTECT_GLOBAL(enum_set2integer_binding);
  enum_set2integer_binding =
    s48_get_imported_binding("enum-set->integer");

  S48_GC_PROTECT_GLOBAL(integer2enum_set_binding);
  integer2enum_set_binding =
    s48_get_imported_binding("integer->enum-set");
}

/* ************************************************************
 * General procedures
 */

static int
is_enum_set(s48_value sch_thing)
{
  S48_SHARED_BINDING_CHECK(is_enum_set_binding);

  return !S48_FALSE_P
    (s48_call_scheme(S48_SHARED_BINDING_REF(is_enum_set_binding),
		     1,
		     sch_thing));
}

static void
check_enum_set(s48_value sch_thing)
{
  if (!is_enum_set(sch_thing))
    s48_raise_argument_type_error(sch_thing);
}

static long
enum_set2integer(s48_value sch_enum_set)
{
  check_enum_set(sch_enum_set);

  S48_SHARED_BINDING_CHECK(enum_set2integer_binding);

  return s48_extract_fixnum
    (s48_call_scheme(S48_SHARED_BINDING_REF(enum_set2integer_binding),
		     1,
		     sch_enum_set));
}

static s48_value
integer2enum_set(s48_value sch_enum_type, long mask)
{
  S48_SHARED_BINDING_CHECK(integer2enum_set_binding);

  return s48_call_scheme(S48_SHARED_BINDING_REF(integer2enum_set_binding),
			 2,
			 sch_enum_type,
			 s48_enter_fixnum(mask));
}

/* ************************************************************ */
/* Syslog options.
 *
 * We translate the our own bits into local bits
 */

static int
is_syslog_options(s48_value sch_thing)
{
  S48_SHARED_BINDING_CHECK(is_syslog_options_binding);

  return !S48_FALSE_P
    (s48_call_scheme(S48_SHARED_BINDING_REF(is_syslog_options_binding),
		     1,
		     sch_thing));
}

static void
check_syslog_options(s48_value sch_thing)
{
  if (!is_syslog_options(sch_thing))
    s48_raise_argument_type_error(sch_thing);
}

static int
s48_extract_syslog_options(s48_value sch_syslog_options)
{
  int	c_syslog_options;
  long	syslog_options;

  check_syslog_options(sch_syslog_options);

  syslog_options = enum_set2integer(sch_syslog_options);

  c_syslog_options =
    (00001 & syslog_options ? LOG_CONS   : 0) |
    (00002 & syslog_options ? LOG_ODELAY : 0) |
    (00004 & syslog_options ? LOG_NDELAY : 0) |
    (00010 & syslog_options ? LOG_PID    : 0);

  return c_syslog_options;
}

/* ************************************************************ */
/* Syslog facility.
 *
 * We translate the local facility into our own encoding and vice versa.
 */

/* The order of these is known to the Scheme code. */
static int syslog_facilities[] = {
  LOG_AUTH,
  LOG_CRON,
  LOG_DAEMON,
  LOG_KERN,
  LOG_LPR,
  LOG_MAIL,
  LOG_NEWS,
  LOG_USER,
  LOG_UUCP,
  LOG_LOCAL0, LOG_LOCAL1, LOG_LOCAL2, LOG_LOCAL3,
  LOG_LOCAL4, LOG_LOCAL5, LOG_LOCAL6, LOG_LOCAL7
};


static s48_value
s48_extract_syslog_facility(s48_value sch_syslog_facility)
{
  int	c_syslog_facility;
  int	syslog_facility;

  s48_check_record_type(sch_syslog_facility, syslog_facility_type_binding);

  syslog_facility =
    s48_extract_fixnum(S48_UNSAFE_RECORD_REF(sch_syslog_facility, 1));

  c_syslog_facility = syslog_facilities[syslog_facility];

  return c_syslog_facility;
}

/* ************************************************************ */
/* Syslog level.
 *
 * We translate the local level into our own encoding and vice versa.
 */

/* The order of these is known to the Scheme code. */
static int syslog_levels[] = {
  LOG_EMERG,
  LOG_ALERT,
  LOG_CRIT,
  LOG_ERR,
  LOG_WARNING,
  LOG_NOTICE,
  LOG_INFO,
  LOG_DEBUG
};


static s48_value
s48_extract_syslog_level(s48_value sch_syslog_level)
{
  int	c_syslog_level;
  int	syslog_level;

  s48_check_record_type(sch_syslog_level, syslog_level_type_binding);

  syslog_level =
    s48_extract_fixnum(S48_UNSAFE_RECORD_REF(sch_syslog_level, 1));

  c_syslog_level = syslog_levels[syslog_level];

  return c_syslog_level;
}

/* ************************************************************ */
/* Syslog mask.
 *
 * We translate the local bits into our own bits and vice versa.
 */

static s48_value
s48_enter_syslog_mask(int syslog_mask)
{
  int		my_syslog_mask;

  my_syslog_mask =
    (LOG_MASK(LOG_EMERG)   & syslog_mask ? 00001 : 0) |
    (LOG_MASK(LOG_ALERT)   & syslog_mask ? 00002 : 0) |
    (LOG_MASK(LOG_CRIT)    & syslog_mask ? 00004 : 0) |
    (LOG_MASK(LOG_ERR)     & syslog_mask ? 00010 : 0) |
    (LOG_MASK(LOG_WARNING) & syslog_mask ? 00020 : 0) |
    (LOG_MASK(LOG_NOTICE)  & syslog_mask ? 00040 : 0) |
    (LOG_MASK(LOG_INFO)    & syslog_mask ? 00100 : 0) |
    (LOG_MASK(LOG_DEBUG)   & syslog_mask ? 00200 : 0);


  return integer2enum_set
    (S48_SHARED_BINDING_REF(syslog_mask_type_binding),
     my_syslog_mask);
}

static int
is_syslog_mask(s48_value sch_thing)
{
  S48_SHARED_BINDING_CHECK(is_enum_set_binding);

  return !S48_FALSE_P
    (s48_call_scheme(S48_SHARED_BINDING_REF(is_syslog_mask_binding),
					    1,
					    sch_thing));
}

static void
check_syslog_mask(s48_value sch_thing)
{
  if (!is_syslog_mask(sch_thing))
    s48_raise_argument_type_error(sch_thing);
}

static int
s48_extract_syslog_mask(s48_value sch_syslog_mask)
{
  int	c_syslog_mask;
  int	syslog_mask;

  check_syslog_mask(sch_syslog_mask);

  syslog_mask = enum_set2integer(sch_syslog_mask);
   
  c_syslog_mask =
    (00001 & syslog_mask ? LOG_MASK(LOG_EMERG)   : 0) |
    (00002 & syslog_mask ? LOG_MASK(LOG_ALERT)   : 0) |
    (00004 & syslog_mask ? LOG_MASK(LOG_CRIT)    : 0) |
    (00010 & syslog_mask ? LOG_MASK(LOG_ERR)     : 0) |
    (00010 & syslog_mask ? LOG_MASK(LOG_WARNING) : 0) |
    (00010 & syslog_mask ? LOG_MASK(LOG_NOTICE)  : 0) |
    (00010 & syslog_mask ? LOG_MASK(LOG_INFO)    : 0) |
    (00020 & syslog_mask ? LOG_MASK(LOG_DEBUG)   : 0);

  return c_syslog_mask;
}

/*
 * Interface to openlog, setlogmask, syslog, and closelog.
 * ### Must still prevent cores.
 */

#define MAX_SYSLOG_IDENT 256 /* should be ample */
static int syslog_open = 0;
static char syslog_ident[MAX_SYSLOG_IDENT];

static s48_value
sch_openlog(s48_value sch_ident,
	    s48_value sch_options,
	    s48_value sch_facility)
{
  int i;
  char *syslog_ident_arg;

  if (syslog_open)
    s48_raise_string_os_error("syslog is already open");

  /* sch_ident may be copied to a different location by GC,
     and openlog doesn't copy the input string, at least not
     on every system.  That's just great. */

  /* strncpy doesn't really do what we want */
  syslog_ident_arg = s48_extract_string(sch_ident);
  for (i = 0;
       (i < MAX_SYSLOG_IDENT-1) && (syslog_ident_arg[i] != '\0');
       ++i)
    syslog_ident[i] = syslog_ident_arg[i];
  syslog_ident[i] = '\0';

  openlog(syslog_ident, 
	  s48_extract_syslog_options(sch_options),
	  s48_extract_syslog_facility(sch_facility));
  syslog_open = 1;
  return S48_UNSPECIFIC;
}

static s48_value
sch_setlogmask(s48_value sch_logmask)
{
  int logmask = s48_extract_syslog_mask(sch_logmask);
  int previous_logmask = setlogmask(logmask);

  return s48_enter_syslog_mask(previous_logmask);
}

static s48_value
sch_syslog(s48_value sch_level, s48_value sch_opt_facility,
	   s48_value sch_message)
{
  int facility =
    S48_EQ_P(S48_FALSE, sch_opt_facility)
    ? 0 : s48_extract_syslog_facility(sch_opt_facility);
  int level = s48_extract_syslog_level(sch_level);

  if (!syslog_open)
    s48_raise_string_os_error("syslog isn't open");
  syslog(facility | level, "%s", s48_extract_string (sch_message));
  return S48_UNSPECIFIC;
}

static s48_value
sch_closelog(void)
{
  if (!syslog_open)
    s48_raise_string_os_error("syslog isn't open");
  closelog();
  syslog_open = 0;
  return S48_UNSPECIFIC;
}

