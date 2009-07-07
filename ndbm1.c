/* Scheme48/scsh Unix system interface.
** Routines that require custom C support.
** Copyright (c) 1995 by David Albertz.
*/

/*  Common dbm access routines */

#include <stdio.h>
#include <errno.h>
#include <ndbm.h>

#include "cstuff.h"

extern int errno;

int database_close (DBM *db)
{
    dbm_close(db);
    return 0;
    }

int database_error(DBM *db)
{
    return(dbm_error(db));
    }

int database_clearerr(DBM *db)
{
    return(dbm_clearerr(db));
    }

int database_delete(DBM *db, char *key)
{
    datum work_key;
    work_key.dptr = key;
    work_key.dsize = strlen(key);
    return(dbm_delete(db, work_key));
    }

char *database_fetch(DBM *db, char *key)
{
    datum work_key, work_data;
    char *work_string;
    work_key.dptr = key;
    work_key.dsize = strlen(key);
    work_data = dbm_fetch(db, work_key);
    if (work_data.dptr == NULL)
	{
	work_string = Malloc(char, 1);
	work_string[0] = '\0';
	}
    else
	{
	work_string = Malloc(char, work_data.dsize+1);
	strncpy(work_string,work_data.dptr,work_data.dsize);
	work_string[work_data.dsize] = '\0';
	}
    return(work_string);
    }

int database_store(DBM *db, char *key, char *data, int flags)
{
    datum work_key, work_data;
    work_key.dptr = key;
    work_key.dsize = strlen(key);
    work_data.dptr = data;
    work_data.dsize = strlen(data);
    return(dbm_store(db, work_key, work_data, flags));
    }

char *database_first(DBM *db)
{
    datum work_key;
    char *work_string;
    work_key = dbm_firstkey(db);
    if (work_key.dptr == NULL)
	{
	work_string = Malloc(char, 1);
	work_string[0] = '\0';
	}
    else
	{
	work_string = Malloc(char, work_key.dsize+1);
	strncpy(work_string,work_key.dptr,work_key.dsize);
	work_string[work_key.dsize] = '\0';
	}
    return(work_string);
    }

char *database_next(DBM *db)
{
    datum work_key;
    char *work_string;
    work_key = dbm_nextkey(db);
    if (work_key.dptr == NULL)
	{
	work_string = Malloc(char, 1);
	work_string[0] = '\0';
	}
    else
	{
	work_string = Malloc(char, work_key.dsize+1);
	strncpy(work_string,work_key.dptr,work_key.dsize);
	work_string[work_key.dsize] = '\0';
	}
    return(work_string);
    }
