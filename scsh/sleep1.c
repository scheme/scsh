#include "sysdep.h"

#include <sys/types.h>
#include <sys/time.h>
#if defined(HAVE_SELECT)
#  include <sys/types.h>	/* for FD_SET and friends (BSD) */
#if defined(HAVE_SYS_SELECT_H)
#  include <sys/select.h>
#endif
#endif
#include <unistd.h>
#include <time.h>
#include "../c/scheme48.h"


/* Sleep until time hisecs/losecs (return #t),
** or until interrupted (return #f).
**
** We make you pass in an absolute time so that if you have to loop
** making multiple tries to sleep due to interrupts, you don't get
** drift.
**
** Posix sleep() is not too well defined. This one uses select(),
** and is pretty straightforward.
*/

s48_value sleep_until(s48_value scm_when)
{
  time_t now = time(0);
  int delta = s48_extract_integer(scm_when) - now;
  if( delta > 0 ) {
    fd_set r, w, e;
    struct timeval tv;
    tv.tv_sec = delta;
    tv.tv_usec = 0;
    FD_ZERO(&r);
    FD_ZERO(&w);
    FD_ZERO(&e);
    if( select(0, &r, &w, &e, &tv) ) return S48_FALSE;	/* Lose */
    }
  return S48_TRUE; /* Win */
  }
  
