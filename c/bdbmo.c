/* Scheme48/scsh Unix system interface.
** Routines that require custom C support.
** Copyright (c) 1995 by David Albertz.
*/

#include <stdio.h>
#include <errno.h>
#include <db.h>
#include <sys/types.h>
#include <limits.h>

#include "cstuff.h"

extern int errno;

/* stub used to return TRUE when scheme checks for Berkeley dbm */
int db_check()
{
  return 1;
  }

s48_value db_open_default(char *file, int flags, int mode, DB **db_ptr)
{
    *db_ptr = dbopen(file, flags, mode, DB_BTREE, NULL);
    if(*db_ptr == NULL)
	return -1;
    else
	return 0;
    }

/*  Open a btree type database
    Note: if pass_info is set to zero, all subsequent arguments are
          ignored, otherwise they are loaded into a BTREEINFO structure
          and passed to the open routine */
s48_value db_open_btree (char *file, int flags, int mode, int pass_info,
		     int access_flags, int cachesize, int maxkeypage,
		     int minkeypage, int psize, int lorder, DB **db_ptr)
{
    BTREEINFO btree;
    if (pass_info)
	{
	btree.flags = access_flags;
	btree.cachesize = cachesize;
	btree.maxkeypage = maxkeypage;
	btree.minkeypage = minkeypage;
	btree.psize = psize;
	btree.compare = NULL;
	btree.prefix = NULL;
	btree.lorder = lorder;
	*db_ptr = dbopen(file, flags, mode, DB_BTREE, &btree);
	}
    else
	*db_ptr = dbopen(file, flags, mode, DB_BTREE, NULL);
    if(*db_ptr == NULL)
	return -1;
    else
	return 0;
    }

/*  Open a hash type database (same use of pass_info as in btree) */
s48_value db_open_hash (char *file, int flags, int mode, int pass_info,
		     int bsize, int ffactor, int nelem, int cachesize,
		     int lorder, DB **db_ptr)
{
    HASHINFO hash;
    if (pass_info)
	{
	hash.bsize = bsize;
	hash.ffactor = ffactor;
	hash.nelem = nelem;
	hash.cachesize = cachesize;
	hash.hash = NULL;
	hash.lorder = lorder;
	*db_ptr = dbopen(file, flags, mode, DB_HASH, &hash);
	}
    else
	*db_ptr = dbopen(file, flags, mode, DB_HASH, NULL);
    if(*db_ptr == NULL)
	return -1;
    else
	return 0;
    }

/*  Open a recno type database (with same use of pass_info again) */
s48_value db_open_recno (char *file, int flags, int mode, int pass_info,
		     int access_flags, int cachesize, int psize, int lorder,
		     int reclen, char bval, char *bfname, DB **db_ptr)
{
    RECNOINFO recno;
    if (pass_info)
	{
	recno.flags = access_flags;
	recno.cachesize = cachesize;
	recno.psize = psize;
	recno.lorder = lorder;
	recno.reclen = reclen;
	recno.bval = bval;
	recno.bfname = bfname;
	*db_ptr = dbopen(file, flags, mode, DB_RECNO, &recno);
	}
    else
	*db_ptr = dbopen(file, flags, mode, DB_RECNO, NULL);
    if(*db_ptr == NULL)
	return -1;
    else
	return 0;
    }
