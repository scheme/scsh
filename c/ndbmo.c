/* Scheme48/scsh Unix system interface.
** Routines that require custom C support.
** Copyright (c) 1995 by David Albertz.
*/

/*  Vanilla ndbm version of the database open function */

#include <stdio.h>
#include <errno.h>
#include <ndbm.h>

#include "cstuff.h"

extern int errno;

/* stub used to return FALSE when scheme checks for Berkeley dbm */
int db_check()
{
  return 0;
  }

s48_value db_open_default (char *file, int flags, int mode, DBM **db_ptr)
{
    *db_ptr = dbm_open(file, flags, mode);
    if(*db_ptr == NULL)
	return -1;
    else
	return 0;
    }
