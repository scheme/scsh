/* Posix time support for scsh.
** Copyright (c) 1994 by Olin Shivers.
*/

/* The source code is conditionalised by three #ifdef feature macros:
** HAVE_STRUCT_TM_TM_ZONE/HAVE_TZNAME
**   The char *tzname[2] global variable is POSIX. Everyone provides
**   it...except some "classic" versions of SunOS that we still care about
**   running (People in LCS/AI refuse to switch to Solaris). So, we kluge
**   around not having it.
**
** HAVE_GMTOFF
**   Some systems (NetBSD, NeXTSTEP, Solaris) have a non-standard field in the
**   tm struct, the tm_gmtoff field. localtime() sets it to the offset from
**   UTC for the current time. If you have this field, it is trivial to
**   compute the the UTC time zone offset. If you have a strict POSIX system,
**   and don't have it, then the offset can be computed with a slower
**   technique.
**
** NeXT
**   The presence of this feature macro means that, basically, you are
**   screwed, and should go download yourself a real Unix system off the
**   Net. For free.
**
**   More specifically, it means that (1) the presence of the strftime()
**   function will cause the whole system build to die at link time,
**   when compiled with the -posix flag. (NeXT bug #59098) There is no fix
**   for this as of November 1994.  Thanks, guys.
**   
**   We handle this problem by abandoning ship. When compiled under NeXT, 
**   your time zone is always computed to be the empty string.
**
**   The other problem is that (2) NeXT's mktime() procedure pays attention
**   to the gmt_offset field of the tm struct you give it, instead of
**   the $TZ environment variable. So there is no way to convert a date
**   to a time without knowing in advance what the UTC offset is in seconds.
**   This screws up scsh's DATE->TIME procedure.
*/

#include <time.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "sysdep.h"
#include "cstuff.h"
#include "time1.h"	/* Make sure the .h interface agrees with the code. */

#include <stdio.h>     /* JMG debug */

extern char **environ;

/* To work in the UTC time zone, do "environ = utc_env;". */
static char *utc_env[] = {"TZ=UCT0", 0};

#if defined(HAVE_TZNAME) && !defined(CYGWIN)
extern char *tzname[];	/* Why isn't this defined in time.h? */
#endif

/* These two functions allow you to temporarily override
** the current time zone with one of your choice. make_newenv()
** takes a time zone string as an argument, and constructs a Unix environ
** vector with a single entry: "TZ=<zone>". You pass the new environ vector
** as an argument. It installs the new environment, and returns the old
** one. You can later pass the old environment back to revert_env()
** to reinstall the old environment and free up malloc'd storage.
**
** On error, make_newenv returns NULL.
*/

static char **make_newenv(s48_value zone, char *newenv[2])
{	
    int zonelen = S48_STRING_LENGTH(zone);
    char **oldenv = environ,
         *tz = Malloc(char, 4+zonelen);
    
    char * extracted_zone = s48_extract_string(zone);
    
    /* s48_value temp; */
    if( !tz ) return NULL;
    strcpy(tz, "TZ=");

    /*    temp = S48_STRING_REF(zone,0);*/ /* JMG */
    /* strncpy(tz+3, &temp, zonelen);*/
    strncpy(tz+3, extracted_zone, zonelen);
    tz[zonelen+3] = '\0';
    newenv[0] = tz;
    newenv[1] = NULL;

    environ = newenv;	/* Install it. */
    return oldenv;
    }

static void revert_env(char **old_env)
{
    char *tz = *environ;
    environ = old_env;
    Free(tz);
    }


/*****************************************************************************/




s48_value scheme_time()
{
    time_t t;
    errno = 0;
    t = time(NULL);
    if( t == -1 && errno ) s48_raise_os_error (errno);
    return s48_enter_integer (t);
}

/* Zone:
**   #f		Local time
**   int	Offset from GMT in seconds.
**   string	Time zone understood by OS.
*/
s48_value time2date(s48_value sch_t, s48_value sch_zone)
     /*		       int *sec, int *min, int *hour,
		       int *mday, int *month, int *year,
		       const char **tz_name, int *tz_secs,
		       int *summer,
		       int *wday, int *yday)
     */
{
   struct tm d;
   time_t t = s48_extract_integer(sch_t);
   s48_value sch_tz_name = S48_UNSPECIFIC;
   s48_value sch_tz_secs = S48_UNSPECIFIC;

   if( S48_FIXNUM_P(sch_zone) ) {		/* Offset from GMT in secs. */
	int offset = s48_extract_fixnum(sch_zone);
	t += s48_extract_fixnum(sch_zone);
	d = *gmtime(&t);
	sch_tz_name = S48_FALSE;
	sch_tz_secs = s48_enter_fixnum (offset);
	}
    else {
	char *newenv[2], **oldenv = NULL;

	if( S48_STRING_P(sch_zone) ) {			/* Time zone */
	  
	  oldenv = make_newenv(sch_zone, newenv); 	/* Install new TZ. */
	  if( !oldenv ) s48_raise_os_error_2(errno, sch_t, sch_zone);
	  d = *localtime(&t);			   	/* Do it. */
	}
	else					/* Local time */
	  d = *localtime(&t);

	/* This little chunk of code copies the calculated time zone into
	** a malloc'd buffer and assigns it to *tz_name. It's a little
	** complicated because we have to clean up after detecting an
	** error w/o walking on errno.
	**
	** The time zone has to be stashed into a malloc'd buffer because
	** when revert_env resets to the original time zone, it will
	** overwrite the static buffer tzname. We have to copy it out before
	** that happens.
	*/
	{ int error = 0;
#ifdef HAVE_TZNAME
	  char *zone = tzname[d.tm_isdst];
#else
#ifdef HAVE_STRUCT_TM_TM_ZONE
	  const char *zone = d.tm_zone; /* Hack it for SunOS. */
#endif
#endif
	  char *newzone = Malloc(char, 1+strlen(zone));
	  if( newzone ){
	    strcpy(newzone, zone);
	    sch_tz_name = s48_enter_string (newzone);
	  }
	  else error = errno;
	  
	  if( oldenv ) revert_env(oldenv); 		/* Revert TZ & env. */

	  if( !newzone ) s48_raise_os_error_2(error, sch_t, sch_zone);
	  }

	/* Calculate the time-zone offset in seconds from UTC. */
#ifdef HAVE_GMTOFF	
	sch_tz_secs = s48_enter_fixnum (d.tm_gmtoff);
	
#else
	{ char **oldenv = environ;			/* Set TZ to UTC     */
	  environ=utc_env;				/* time temporarily. */
	  tzset(); /* NetBSD, SunOS POSIX-noncompliance requires this. */
	  sch_tz_secs = s48_enter_fixnum (mktime(&d) - t);
	  environ=oldenv;
	  
	  }
#endif
	}
   return s48_list_11 (s48_enter_fixnum (d.tm_sec),
		       s48_enter_fixnum (d.tm_min),
		       s48_enter_fixnum (d.tm_hour),
		       s48_enter_fixnum (d.tm_mday),
		       s48_enter_fixnum (d.tm_mon),
		       s48_enter_fixnum (d.tm_year),
		       sch_tz_name,
		       sch_tz_secs,
		       d.tm_isdst ? S48_TRUE : S48_FALSE,
		       s48_enter_fixnum (d.tm_wday),
		       s48_enter_fixnum (d.tm_yday));	
}

/* Oops
** There's a fundamental problem with the Posix mktime() function used below
** -- it's error return value (-1) is also a valid return value, for date
**     11:59:00 UTC, 12/31/1969
**
** 1. We choose to err on the paranoid side. If mktime() returns -1, it is
**    considered an error.
** 2. If we return an error, we try to return a useful errno value, if we can.
**
** Who designed this interface?
*/

s48_value date2time(s48_value sec, s48_value min, s48_value hour,
		    s48_value mday, s48_value month, s48_value year,
		    s48_value tz_name, s48_value tz_secs,
		    s48_value summer)
{
    time_t t;
    struct tm d;
    int err = 0;
    d.tm_sec  = s48_extract_fixnum (sec); 
    d.tm_min  = s48_extract_fixnum (min);
    d.tm_hour = s48_extract_fixnum (hour);
    d.tm_mday = s48_extract_fixnum (mday);
    d.tm_mon  = s48_extract_fixnum (month);
    d.tm_year = s48_extract_fixnum (year);
    d.tm_wday = 0;	d.tm_yday = 0;	      

    if( S48_FIXNUM_P(tz_secs) ) {		/* Offset from GMT in seconds. */
	char **oldenv = environ;			/* Set TZ to UTC     */
	environ = utc_env;				/* time temporarily. */
	tzset(); /* NetBSD, SunOS POSIX-noncompliance requires this. */
	d.tm_isdst = 0; /* FreeBSD, at least, needs this or it sulks. */
	t = mktime(&d);
	if (t == -1) err = 1;
	t -= s48_extract_fixnum(tz_secs);
	environ = oldenv;
	}

    /* ### Note that we *still* don't implement the manual paragraph
       with "When calcultating with time-zones, the date's SUMMER?
       field is used to resolve ambiguities. */
    else if( S48_STRING_P(tz_name) ) {	/* Time zone */
	char *newenv[2];
	char **oldenv = make_newenv(tz_name, newenv);
	if( !oldenv ) return s48_enter_integer(errno);
	tzset(); /* NetBSD, SunOS POSIX-noncompliance requires this. */
	errno = 0;

	d.tm_isdst = -1;
	t = mktime(&d);
	if (t == -1) err = 1;
	revert_env(oldenv);
	}

    else {				/* Local time */
	tzset(); /* NetBSD, SunOS POSIX-noncompliance requires this. */
	errno = 0;
	d.tm_isdst = -1;
	t = mktime(&d);
	if (t == -1) err = 1;
    }

    return s48_cons (err ? S48_TRUE : S48_FALSE, s48_enter_integer(t));
}


/* WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
**
** This code doesn't work under NeXTSTEP.  I have cleverly #included the
** critical call to strftime() out for NeXT. This is because the compiler
** blows up on Posix compiles involving strftime(). Go figure.
*/


/* It's disgusting how long and tortuous this function is, just
** to interface to the strftime() function. -Olin
**
** There's a weird screw case this code is careful to handle. Exhibiting
** classic Unix design (we use the term loosely), strftime()'s error
** return (0) is also a legal return value for some boundary cases.
** For example, if the format string is empty, or it is "%Z" and
** the time-zone is not available, then the result string is 0 chars long.
** We distinguish this case by suffixing an "x" to the format string,
** and flushing the last char in the formatted result.
**
** Don't consider *prefixing* an "x" instead, because then you'd
** probably pass back &result[1] to skip the x, and that would lose --
** the guy we are handing the string to will later pass it to free(),
** so we can't pass back a pointer to anything other than the very front
** of the block.
**
** Professional programmers sacrifice their pride that others may live.
** Why me? Why Unix?
*/
s48_value format_date(s48_value sch_fmt, s48_value sch_sec, s48_value sch_min,
		      s48_value sch_hour, s48_value sch_mday, 
		      s48_value sch_month, s48_value sch_year,
		      s48_value sch_tz, s48_value sch_summer,
		      s48_value sch_week_day, s48_value sch_year_day)
{
    struct tm d;
    char *fmt = s48_extract_string(sch_fmt);
    int fmt_len = strlen(fmt);
    char *fmt2 = Malloc(char, 2+2*fmt_len); /* 1 extra for prefixed "x" char.*/
    int target_len = 1; 	/* 1 for the prefixed "x" char. Ugh. */
    int zone = 0; 		/* Are we using the time-zone? */
    char *q, *target;
    const char *p;
    char *newenv[2], **oldenv = NULL;
    int result_len;
    s48_value sch_ans = S48_UNSPECIFIC;

    if( !fmt2 ) s48_raise_os_error_1(errno, sch_fmt); 

    d.tm_sec  = s48_extract_fixnum(sch_sec);
    d.tm_min  = s48_extract_fixnum(sch_min);
    d.tm_hour  = s48_extract_fixnum(sch_hour);
    d.tm_mday = s48_extract_fixnum(sch_mday);
    d.tm_mon  = s48_extract_fixnum(sch_month);
    d.tm_year  = s48_extract_fixnum(sch_year);
    d.tm_wday = s48_extract_fixnum(sch_week_day);
    d.tm_yday = s48_extract_fixnum(sch_year_day);
    d.tm_isdst = (S48_EQ_P (sch_summer, S48_FALSE)) ? 0 : 1;

#ifdef HAVE_STRUCT_TM_TM_ZONE
    /* FreeBSD's strftime reads this */
    d.tm_zone = (S48_EQ_P(sch_tz, S48_FALSE)) ? NULL : s48_extract_string(sch_tz);
#endif

    /* Copy fmt -> fmt2, converting ~ escape codes to % escape codes.
    ** Set zone=1 if fmt has a ~Z.
    ** Build up an estimate of how large the target buffer needs to be.
    ** The length calculation is not required to be accurate.
    */
    for(q=fmt2, p=fmt; *p; p++) {
	if( *p != '~' ) {
	    target_len++;
	    *q++ = *p;
	    if( *p == '%' ) *q++ = '%';	/* Percents get doubled. */
	    }
	else {
	    char c = *++p;
	    if( ! c ) {
		Free(fmt2);
		return S48_FALSE;	/* % has to be followed by something. */
		}
	    else if( c == '~' ) {
		*q++ = '~';
		target_len++;
		}
	    else {
		*q++ = '%';
		*q++ = c;
		switch (c) {
		    case 'a': target_len += 3;  break;
		    case 'A': target_len += 9;  break;
		    case 'b': target_len += 3;  break;
		    case 'B': target_len += 9;  break;
		    case 'c': target_len += 10; break;	/* wtf */
		    case 'd': target_len += 2;  break;
		    case 'H': target_len += 2;  break;
		    case 'I': target_len += 2;  break;
		    case 'j': target_len += 3;  break;
		    case 'm': target_len += 2;  break;
		    case 'M': target_len += 2;  break;
		    case 'p': target_len += 2;  break;
		    case 'S': target_len += 2;  break;
		    case 'U': target_len += 2;  break;
		    case 'w': target_len += 1;  break;
		    case 'W': target_len += 2;  break;
		    case 'x': target_len += 10; break;	/* wtf */
		    case 'X': target_len += 10; break;	/* wtf */
		    case 'y': target_len += 2;  break;
		    case 'Y': target_len += 4;  break;
		    case 'Z': target_len += 6;  zone++; break;
		  default:
		         target_len += 5; break;	/* wtf */
		    }
		}
	    }
	}
    *q++ = 'x'; *q = '\0'; /* Append the guard "x" suffix and nul-terminate. */

    /* Fix up the time-zone if it is being used and the user passed one in. */
    if( zone && S48_STRING_P(sch_tz) ) {
	oldenv = make_newenv(sch_tz, newenv);
	if( !oldenv ) {
	    Free(fmt);
	    s48_raise_os_error_1(errno, sch_fmt);
	    }
	}

    /* Call strftime with increasingly larger buffers until the result fits. */
    target = Malloc(char, target_len);
    if( !target ) goto lose; /* Alloc lost. */
	
#ifndef NeXT
    while( !(result_len=strftime(target, target_len, fmt2, &d)) ) {
	target_len *= 2;
	target = Realloc(char, target, target_len);
	if( !target ) goto lose;
	}
    target[result_len-1] = '\0'; /* Flush the trailing "x". */
#endif
    sch_ans = s48_enter_string(target);
    Free(fmt2);
    Free(target);
    if( oldenv ) revert_env(oldenv);
    return sch_ans;

lose:
    /* We lost trying to allocate space for the strftime() target buffer. */
    {int err = errno;
     if( oldenv ) revert_env(oldenv); /* Clean up */
     Free(fmt2);
     s48_raise_os_error_1(err, sch_fmt);
     }
}

#if 0
/* This is a kludge one can use should the tzname variable
** not be present on the system. Only SunOS is broken this way,
** and it has a non-standard alternative we can use for this application.
** So this code is commented out.
**
** tzname_loser(int dst) returns a string containing the current time zone
** for loser OS's. The string is statically allocated. If the time zone
** is longer than some hidden, arbitrary length, the function simply
** returns the empty string. It is a workaround for tzname[dp->tm_isdst].
** 
*/
char *tzname_loser(struct tm *dp)
{
    static char buf[1024];
    return strftime(buf, 1024, "x%Z", dp) ? buf+1 : "";
    }
#endif

/* clear errno before mktime() and time(), if -1 ret, return errno.
**     This is defined to work under HP-UX at least; 
**     other man pages are silent.
** gettimeofday() returns -1/errno
** localtime() & gmtime() don't error.
*/

void s48_init_time(void)
{
    S48_EXPORT_FUNCTION(time_plus_ticks);
    S48_EXPORT_FUNCTION(scheme_time);
    S48_EXPORT_FUNCTION(date2time);
    S48_EXPORT_FUNCTION(time2date);
    S48_EXPORT_FUNCTION(format_date);
}
