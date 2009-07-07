/* Scheme48/scsh Unix system interface.
** Routines that require custom C support.
** Copyright (c) 1995 by David Albertz.
*/

/*  File locking routines  */

#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

/* Make sure our exports match up w/the implementation: */
#include "scheme48.h"
#include "flock1.h"

s48_value set_lock(s48_value fd, s48_value cmd, s48_value type, 
		   s48_value whence, s48_value start, s48_value len)
{
    struct flock lock;
    int retval;
    lock.l_type   = s48_extract_integer (type);
    lock.l_whence = s48_extract_integer (whence);
    lock.l_start  = s48_extract_integer (start);
    lock.l_len    = s48_extract_integer (len);
    retval = fcntl(s48_extract_fixnum (fd), s48_extract_integer (cmd), &lock);
    if (retval == -1)
      s48_raise_os_error_6 (errno, fd, cmd, type, whence, start, len);
    return S48_UNSPECIFIC;
}

s48_value get_lock(s48_value fd, s48_value cmd, s48_value type, 
		   s48_value whence, s48_value start, s48_value len)
{
    struct flock lock;
    int ret;
    s48_value sch_type = S48_UNSPECIFIC;
    s48_value sch_whence = S48_UNSPECIFIC;
    s48_value sch_start = S48_UNSPECIFIC;
    s48_value sch_len = S48_UNSPECIFIC;
    s48_value sch_pid = S48_UNSPECIFIC;
    s48_value sch_retval = S48_UNSPECIFIC;

    S48_DECLARE_GC_PROTECT(6);

    S48_GC_PROTECT_6(sch_type, sch_whence, sch_start, sch_len, 
		     sch_pid, sch_retval);

    lock.l_type   = s48_extract_integer (type);
    lock.l_whence = s48_extract_integer (whence);
    lock.l_start  = s48_extract_integer (start);
    lock.l_len    = s48_extract_integer (len);
    ret = fcntl(s48_extract_fixnum (fd), F_GETLK, &lock);
    if (ret == -1) {
      S48_GC_UNPROTECT();
      s48_raise_os_error_6 (errno, fd, cmd, type, whence, start, len);
    } else {

      sch_type = s48_enter_integer (lock.l_type);
      sch_whence = s48_enter_integer (lock.l_whence);
      sch_start = s48_enter_integer (lock.l_start);
      sch_len = s48_enter_integer (lock.l_len);
      sch_pid = s48_enter_integer (lock.l_pid);

      sch_retval = s48_list_5(sch_type, sch_whence, sch_start, sch_len, 
			      sch_pid);
      S48_GC_UNPROTECT();

      return sch_retval;
    }
}

void s48_init_flock(void)
{
    S48_EXPORT_FUNCTION(set_lock);
    S48_EXPORT_FUNCTION(get_lock);
}
