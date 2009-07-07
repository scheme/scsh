/* OS-dependent support for fine-grained timer.
** Copyright (c) 1995 by Olin Shivers.
**
** We return the current time in seconds and sub-second "ticks" where the 
** number of ticks/second is OS dependent (and is defined in time_dep.scm).
** This definition works on any BSD Unix with the gettimeofday() 
** microsecond-resolution timer.
*/

#include <errno.h>
#include <sys/time.h>
#include "scheme48.h"
#include "../time1.h"

s48_value time_plus_ticks()
{
    struct timeval t;
    s48_value sch_tv_sec = S48_UNSPECIFIC;
    s48_value sch_tv_usec = S48_UNSPECIFIC;
    s48_value sch_listval = S48_UNSPECIFIC;
    s48_value sch_retval = S48_UNSPECIFIC;
    S48_DECLARE_GC_PROTECT(3);
    
    S48_GC_PROTECT_3(sch_tv_sec, sch_tv_usec, sch_listval);

    if( gettimeofday(&t, NULL) ) s48_raise_os_error (errno);


    sch_tv_sec = s48_enter_integer(t.tv_sec);
    sch_tv_usec = s48_enter_integer(t.tv_usec);
    sch_listval = s48_cons (sch_tv_usec, S48_NULL);
    sch_retval = s48_cons (sch_tv_sec, sch_listval);
    
    S48_GC_UNPROTECT();

    return sch_retval;

}
