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

int database_delete(DBM *db, s48_value key)
{
    datum work_key;
    work_key.dptr = ADDRESS_AFTER_HEADER(key, char);
    work_key.dsize = S48_STRING_LENGTH(key);
    return(dbm_delete(db, work_key));
    }

char *database_fetch(DBM *db, s48_value key)
{
    datum work_key, work_data;
    char *work_string;
    work_key.dptr = ADDRESS_AFTER_HEADER(key, char);
    work_key.dsize = S48_STRING_LENGTH(key);
    work_data = dbm_fetch(db, work_key);
    if (work_data.dptr == NULL)
	{
	work_string = Malloc(char, 1);
	work_string[0] = '\0';
	}
    else
	{
	work_string = Malloc(char, work_data.dsize+1);
	memcpy(work_string,work_data.dptr,work_data.dsize);
	work_string[work_data.dsize] = '\0';
	}
    return(work_string);
    }

int database_store(DBM *db, s48_value key, s48_value data, int flags)
{
    datum work_key, work_data;
    work_key.dptr = ADDRESS_AFTER_HEADER(key, char);
    work_key.dsize = S48_STRING_LENGTH(key);
    work_data.dptr = ADDRESS_AFTER_HEADER(data, char);
    work_data.dsize = S48_STRING_LENGTH(data);
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
	memcpy(work_string,work_key.dptr,work_key.dsize);
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
	memcpy(work_string,work_key.dptr,work_key.dsize);
	work_string[work_key.dsize] = '\0';
	}
    return(work_string);
    }




