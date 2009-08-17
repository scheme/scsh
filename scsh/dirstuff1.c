/* Here is stuff for interfacing to directories.
** Copyright (c) 1993, 1994 by Olin Shivers.
*/

#include <sys/types.h>
#include <stdio.h>
#include <dirent.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/* Make sure our exports match up w/the implementation: */
#include "scheme48.h"
#include "dirstuff1.h"

s48_value directory_files(s48_value sch_dirname)
{
  struct dirent *dirent;
  DIR *d;
  s48_value dirlist = S48_NULL;

  S48_DECLARE_GC_PROTECT(1);
  
  S48_GC_PROTECT_1(dirlist);
  
  if( NULL == (d = opendir(s48_extract_string (sch_dirname))) ) 
    s48_raise_os_error_1 (errno, sch_dirname);
  
  while( NULL != (dirent = readdir(d)) ) {
    if((strcmp(dirent->d_name,".") == 0) || (strcmp(dirent->d_name,"..") == 0))
      continue;
    
    dirlist = s48_cons (s48_enter_string (dirent->d_name),
			dirlist);
    
  }
  if (closedir(d) == -1)
    s48_raise_os_error_1 (errno, sch_dirname);
  
  S48_GC_UNPROTECT ();
  return dirlist;
}
s48_value
scm_opendir(s48_value dirname)
{
  DIR		*dp;
  s48_value	res;
  char		*c_name;

  c_name = s48_extract_string(dirname);
  dp = opendir(c_name);
  if (dp == NULL)
    s48_raise_os_error_1(errno, dirname);
  res = S48_MAKE_VALUE(DIR *);
  S48_UNSAFE_EXTRACT_VALUE(res, DIR *) = dp;
  return (res);
}

/*
 * Interface to closedir.
 * Note, it is ok to call closedir on an already closed directory.
 */
s48_value
scm_closedir(s48_value dirstream)
{
  DIR	**dpp;

  dpp = S48_EXTRACT_VALUE_POINTER(dirstream, DIR *);
  if (*dpp != (DIR *)NULL) {
    int	status = closedir(*dpp);
    if (status == -1)
      s48_raise_os_error_1(errno, dirstream);
    *dpp = (DIR *)NULL;
  }
  return (S48_UNSPECIFIC);
}

/*
 * Interface to readdir.
 * If we have already read all the files that are in the directory,
 * #F is returned.  Otherwise, a string with the next file name.
 * Note, "." and ".." are never returned.
 */
s48_value
scm_readdir(s48_value dirstream)
{
  DIR		**dpp;
  struct dirent	*dep;
  char		*name;

  dpp = S48_EXTRACT_VALUE_POINTER(dirstream, DIR *);
  if (*dpp == (DIR *)NULL)
    s48_raise_argument_type_error(dirstream);	/* not really correct error */
  do {
    errno = 0;
    dep = readdir(*dpp);
    if (dep == (struct dirent *)NULL) {
      if (errno != 0)
	s48_raise_os_error_1(errno, dirstream);
      return (S48_FALSE);
    }
    name = dep->d_name;
  } while ((name[0] == '.')
	   && (name[1] == '\0' || (name[1] == '.' && name[2] == '\0')));
  return s48_enter_string(name);
}

void s48_init_dirstuff (){
  S48_EXPORT_FUNCTION(directory_files);
  S48_EXPORT_FUNCTION(scm_opendir);
  S48_EXPORT_FUNCTION(scm_readdir);
  S48_EXPORT_FUNCTION(scm_closedir);
}
