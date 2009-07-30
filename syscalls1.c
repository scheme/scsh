/* Scheme48/scsh Unix system interface.
** Routines that require custom C support.
** Copyright (c) 1993,1994 by Olin Shivers.
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/signal.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <fcntl.h>		/* for O_RDWR */
#include <sys/stat.h>
#include <sys/param.h> /* For gethostname() */
#include <sys/utsname.h> /* for uname() */

#include <netdb.h>
/* This lossage brought to you by Solaris and BIND */
/* We thank Solaris for forcing users to get a new BIND */
/* We thank BIND for blowing away the Solaris includea for MAXHOSTNAMELEN */
#ifndef MAXHOSTNAMELEN
#include <arpa/nameser.h>
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN      MAXDNAME
#endif
#endif

#include <pwd.h>
#include <errno.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <utime.h>
#ifdef HAVE_CRYPT_H
#include <crypt.h>
#endif
#include "cstuff.h"
#include <limits.h>     /* NGROUPS_MAX on solaris */

/* Make sure our exports match up w/the implementation: */
#include "syscalls1.h"

extern int errno;
extern char **environ;

/* Sux because it's dependent on 32-bitness. */
#define hi8(i)  (((i)>>24) & 0xff)
#define lo24(i) ((i) & 0xffffff)
#define comp8_24(hi, lo) (((hi)<<24) + (lo))


/* Process stuff
*******************************************************************************
** wait, exec
*/

/* Args: pid, flags; returns [retval, status] */

s48_ref_t wait_pid(s48_call_t call, s48_ref_t s48_pid, s48_ref_t s48_flags)
{
  int status=0;
  pid_t pid;
  int flags;
  pid_t result_pid;
  s48_ref_t sch_retval = s48_unspecific_2(call);
  s48_ref_t sch_result_pid = s48_unspecific_2(call);
  s48_ref_t sch_status = s48_unspecific_2(call);
  s48_ref_t sch_status_list = s48_unspecific_2(call);

  pid = (pid_t) s48_extract_long_2(call, s48_pid);
  flags = s48_extract_long_2(call, s48_flags);

  result_pid = waitpid(pid, &status, flags);
  if (result_pid == -1)
    if (errno == ECHILD) sch_result_pid = s48_enter_long_2(call, 0);
    else s48_os_error_2(call, "wait_pid", errno, 2, s48_pid, s48_flags);
  else {
    sch_result_pid = s48_enter_long_2(call, result_pid);
    sch_status = s48_enter_long_2(call, status);
  }
  sch_status_list = s48_cons_2 (call, sch_status, s48_null_2(call));
  sch_retval = s48_cons_2 (call, sch_result_pid, sch_status_list);

  return sch_retval;
}

s48_ref_t scsh_exit(s48_call_t call, s48_ref_t status)
{
  exit(s48_extract_long_2(call, status));
  return s48_false_2(call);
}

s48_ref_t scsh__exit(s48_call_t call, s48_ref_t status)
{
  _exit(s48_extract_long_2(call, status));
  return s48_false_2(call);
}

s48_ref_t scsh_fork(s48_call_t call)
{
  pid_t pid = fork();
  if (pid == -1)
    s48_os_error_2(call, "scsh_fork", errno, 0);
  else return s48_enter_long_2(call, pid);
}

/* Random file and I/O stuff
*******************************************************************************
*/

/* Returns (r w) */
s48_ref_t scheme_pipe()
{
  int fds[2];
  s48_ref_t sch_retval = s48_unspecific_2(call);

  if(pipe(fds) == -1)
    s48_raise_os_error(errno);
  else {
    sch_retval = s48_cons(s48_enter_fixnum(fds[0]),
			  s48_cons(s48_enter_fixnum (fds[1]), S48_NULL));
  }

    return sch_retval;
}

s48_ref_t scsh_kill (s48_ref_t sch_pid, s48_ref_t sch_signal)
{
  int retval = kill ((pid_t) s48_extract_fixnum (sch_pid),
		     s48_extract_fixnum (sch_signal));
  if (retval == -1)
    s48_raise_os_error_2(errno, sch_pid, sch_signal);
  else return s48_enter_fixnum (retval);
}


/* Read the symlink. */
#ifdef MAXPATHLEN
s48_ref_t scsh_readlink(s48_ref_t sch_path)
{
  char linkpath[MAXPATHLEN+1];
  int retval = readlink(s48_extract_string (sch_path), linkpath, MAXPATHLEN);
  if (retval == -1)
    s48_raise_os_error_1(errno, sch_path);
  else
    {
      linkpath[retval] = '\0';
      return s48_enter_string(linkpath);
    }
}
#else
s48_ref_t scsh_readlink(s48_ref_t sch_path)
{
  char *linkpath;
  int size;
  int retval;
  s48_ref_t sch_sym_link_path = s48_unspecific_2(call);

  for (size = 256;; size *=2){

    linkpath = Malloc(char,size);
    if (!linkpath)
      s48_raise_os_error_1(errno, sch_path);

    retval = readlink(s48_extract_string (sch_path), linkpath, size);

    if (retval == -1){
      free (linkpath);
      s48_raise_os_error_1(errno, sch_path);
    }

    if (retval < size){
      sch_sym_link_path = s48_enter_substring (linkpath, retval);
      free (linkpath);
      return sch_sym_link_path;
    }
    free (linkpath);
  }
}
#endif


s48_ref_t scsh_rename(s48_ref_t sch_from, s48_ref_t sch_to)
{
  int retval = rename (s48_extract_string (sch_from),
		       s48_extract_string (sch_to));
  if (retval == -1)
    s48_raise_os_error_2(errno, sch_from, sch_to);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_rmdir(s48_ref_t sch_path)
{
  int retval = rmdir (s48_extract_string (sch_path));
  if (retval == -1)
    s48_raise_os_error_1(errno, sch_path);
  return s48_unspecific_2(call);
}



/* Scheme interfaces to utime().
** Complicated by need to pass real 32-bit quantities.
*/

s48_ref_t scm_utime(s48_ref_t sch_path, s48_ref_t sch_ac, s48_ref_t sch_mod)
{
  struct utimbuf t;
  int retval;
  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(sch_path, sch_ac, sch_mod);

  t.actime = s48_extract_integer (sch_ac);
  t.modtime = s48_extract_integer (sch_mod);
  retval = utime(s48_extract_string (sch_path), &t);

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_3(errno, sch_path, sch_ac, sch_mod);
  return s48_unspecific_2(call);
}

s48_ref_t scm_utime_now(s48_ref_t sch_path){
  int retval = utime (s48_extract_string (sch_path), 0);
  if (retval == -1)
    s48_raise_os_error_1(errno, sch_path);
  return s48_unspecific_2(call);
}


s48_ref_t set_cloexec(s48_ref_t _fd, s48_ref_t _val)
{
  int fd = s48_extract_fixnum (_fd);
  int val = (_val == S48_TRUE) ? 1 : 0;
  int flags = fcntl(fd, F_GETFD);
  if( flags == -1 ) s48_raise_os_error_2(errno, _fd, _val);
  val = -val;	/* 0 -> 0 and 1 -> -1 */

  /* If it's already what we want, just return. */
  if( (flags & FD_CLOEXEC) == (FD_CLOEXEC & val) ) return S48_FALSE;

  flags = (flags & ~FD_CLOEXEC) | (val & FD_CLOEXEC);
  if (fcntl(fd, F_SETFD, flags) == -1)
    s48_raise_os_error_2(errno, _fd, _val);
  else return S48_FALSE;
}

s48_ref_t scsh_chdir(s48_ref_t directory)
{
  int retval = chdir (s48_extract_string (directory));
  if (retval == -1)
    s48_raise_os_error_1(errno, directory);
  return S48_TRUE;
}

/* Two versions of CWD
*******************************************************************************
*/

/* Posix rules: If PATH_MAX is defined, it's the length of longest path.
** Otherwise, _POSIX_PATH_MAX = 255, and is a lower bound on said length.
** I'm writing out 255 as a literal because HP-UX isn't finding
** _POSIX_PATH_MAX.
*/
#ifdef PATH_MAX
#define scsh_path_max (PATH_MAX)
#else
#define scsh_path_max (255)
#endif

/* Simple-minded POSIX version. */
s48_ref_t scheme_cwd()
{
  char *buf;
  int size = scsh_path_max + 1; /* +1 for terminating nul byte... */
  s48_ref_t cwd = s48_unspecific_2(call);

  buf = Malloc(char,size);
  if(!buf) goto lose;

  while( !getcwd(buf, size) )
    if( errno != ERANGE ) goto lose;
    else {
      /* Double the buf and retry. */
      char *nbuf = Realloc(char, buf, size += size);
      if( !nbuf ) goto lose;
      buf = nbuf;
    }
  cwd = s48_enter_string(buf);		/* win */

  Free(buf);

  return cwd;

 lose:
  {int e = errno;
   Free(buf);
   s48_raise_os_error(e);}
}

/* Process times
*******************************************************************************
*/

/* Sleazing on the types here -- the ret values should be clock_t, not int,
** but cig can't handle it.
*/

s48_ref_t process_times()
{
  struct tms tms;

  s48_ref_t sch_result_utime = s48_unspecific_2(call);
  s48_ref_t sch_result_stime = s48_unspecific_2(call);
  s48_ref_t sch_result_cutime = s48_unspecific_2(call);
  s48_ref_t sch_result_cstime = s48_unspecific_2(call);
  s48_ref_t sch_result_retval = s48_unspecific_2(call);

  S48_DECLARE_GC_PROTECT(4);

  clock_t t = times(&tms);
  if (t == -1) s48_raise_os_error(errno);

  S48_GC_PROTECT_4(sch_result_utime,
		   sch_result_stime,
		   sch_result_cutime,
		   sch_result_cstime);

  sch_result_utime = s48_enter_integer(tms.tms_utime);
  sch_result_stime = s48_enter_integer(tms.tms_stime);
  sch_result_cutime = s48_enter_integer(tms.tms_cutime);
  sch_result_cstime = s48_enter_integer(tms.tms_cstime);


  sch_result_retval = s48_cons(sch_result_utime,
			       s48_cons(sch_result_stime,
					s48_cons(sch_result_cutime,
						 s48_cons(sch_result_cstime, S48_NULL))));

  S48_GC_UNPROTECT();

  return sch_result_retval;
}

s48_ref_t cpu_clock_ticks_per_sec()
{
#ifdef _SC_CLK_TCK
  static long clock_tick = 0;

  if (clock_tick == 0){
    clock_tick = sysconf(_SC_CLK_TCK); /* POSIX.1, POSIX.2 */
    if (clock_tick == -1)
      s48_raise_os_error(errno);
  }
  return s48_enter_integer(clock_tick);
#else
#ifdef CLK_TCK
  return s48_enter_integer(CLK_TCK);
#else
  return s48_enter_fixnum(60);
#endif
#endif
}

s48_ref_t scsh_chmod(s48_ref_t sch_path, s48_ref_t sch_mode)
{
  int retval;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sch_path, sch_mode);

  retval = chmod (s48_extract_string(sch_path),
		  s48_extract_integer(sch_mode));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_2(errno, sch_path, sch_mode);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_fchmod(s48_ref_t sch_fd, s48_ref_t sch_mode)
{
  int retval;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sch_fd, sch_mode);

  retval = fchmod (s48_extract_fixnum(sch_fd),
		   s48_extract_integer(sch_mode));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_2(errno, sch_fd, sch_mode);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_chown(s48_ref_t sch_path, s48_ref_t sch_uid, s48_ref_t sch_gid)
{
  int retval;
  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(sch_path, sch_uid, sch_gid);

  retval = chown(s48_extract_string(sch_path),
		 s48_extract_integer(sch_uid),
		 s48_extract_integer(sch_gid));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_3(errno, sch_path, sch_uid, sch_gid);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_fchown(s48_ref_t sch_fd, s48_ref_t sch_uid, s48_ref_t sch_gid)
{
  int retval;
  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(sch_fd, sch_uid, sch_gid);

  retval = fchown(s48_extract_fixnum(sch_fd),
		  s48_extract_integer(sch_uid),
		  s48_extract_integer(sch_gid));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_3(errno, sch_fd, sch_uid, sch_gid);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_access(s48_ref_t sch_path, s48_ref_t sch_mode)
{
  int retval;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sch_path, sch_mode);

  retval = access (s48_extract_string(sch_path),
		   s48_extract_integer(sch_mode));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_2(errno, sch_path, sch_mode);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_link(s48_ref_t sch_name1, s48_ref_t sch_name2)
{
  int retval = link (s48_extract_string (sch_name1),
		     s48_extract_string (sch_name2));
  if (retval == -1)
    s48_raise_os_error_2(errno, sch_name1, sch_name2);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_mkfifo(s48_ref_t sch_path, s48_ref_t sch_mode)
{
  int retval = mkfifo (s48_extract_string (sch_path),
		       s48_extract_fixnum (sch_mode));
  if (retval == -1)
    s48_raise_os_error_2(errno, sch_path, sch_mode);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_mkdir(s48_ref_t sch_path, s48_ref_t sch_mode)
{
  int retval = mkdir (s48_extract_string (sch_path),
		      s48_extract_fixnum (sch_mode));
  if (retval == -1)
    s48_raise_os_error_2(errno, sch_path, sch_mode);
  return s48_unspecific_2(call);
}

/* S_ISSOCK(mode) and S_ISLNK(mode) are not POSIX. You lose on a NeXT. Ugh. */
#ifndef S_ISSOCK
#define S_ISSOCK(mode) (((mode) & S_IFMT) == S_IFSOCK)
#endif
#ifndef S_ISLNK
#define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)
#endif

#define low24(x) ((x) & 0xffffff)
#define hi_but24(x) (((x) >> 24) & 0xff)

/* Note that hi_but24 assumes value is a *32 bit* signed value. We have to
** do this, because C's right-shift operator exposes word width. A suckful
** language.
*/

/* Internal aux function -- loads stat values into Scheme vector: */
s48_ref_t really_stat(struct stat *s, s48_ref_t vec)
{
  int modes, typecode = -1;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(vec);

  modes = s->st_mode;
  if( S_ISBLK(modes) )       typecode = 0;
  else if( S_ISCHR(modes) )  typecode = 1;
  else if( S_ISDIR(modes) )  typecode = 2;
  else if( S_ISFIFO(modes) ) typecode = 3;
  else if( S_ISREG(modes) )  typecode = 4;
  else if( S_ISSOCK(modes) ) typecode = 5;
  else if( S_ISLNK(modes) )  typecode = 6;

  S48_VECTOR_SET(vec,0,s48_enter_fixnum(typecode));
  S48_VECTOR_SET(vec,1, s48_enter_integer(s->st_dev));
  S48_VECTOR_SET(vec,2, s48_enter_integer(s->st_ino));
  S48_VECTOR_SET(vec,3, s48_enter_integer(s->st_mode));
  S48_VECTOR_SET(vec,4, s48_enter_integer(s->st_nlink));
  S48_VECTOR_SET(vec,5, s48_enter_integer(s->st_uid));
  S48_VECTOR_SET(vec,6, s48_enter_integer(s->st_gid));
  S48_VECTOR_SET(vec,7, s48_enter_integer(s->st_size));
  S48_VECTOR_SET(vec,8, s48_enter_integer(s->st_atime));
  S48_VECTOR_SET(vec,9, s48_enter_integer(s->st_mtime));
  S48_VECTOR_SET(vec,10, s48_enter_integer(s->st_ctime));

  /* We also used to do st_rdev, st_blksize, and st_blocks.
     These aren't POSIX, and, e.g., are not around on SGI machines.
     Too bad -- blksize is useful. Unix sux. */
  S48_GC_UNPROTECT();
  return s48_unspecific_2(call);
}

s48_ref_t scheme_stat(s48_ref_t path, s48_ref_t vec, s48_ref_t chase_p)
{
  struct stat s;
  const char * cp_path = s48_extract_string(path);
  int retval = (chase_p != S48_FALSE) ? stat(cp_path, &s) : lstat(cp_path, &s);

  if (retval == -1) s48_raise_os_error_2 (errno, path, chase_p);

  return really_stat (&s, vec);
}

s48_ref_t scheme_fstat(s48_ref_t fd, s48_ref_t vec)
{
  struct stat s;
  int retval = fstat (s48_extract_fixnum (fd), &s);
  if (retval == -1) s48_raise_os_error_1 (errno, fd);
  return really_stat (&s, vec);
}

s48_ref_t scsh_symlink(s48_ref_t sch_name1, s48_ref_t sch_name2)
{
  int retval = symlink (s48_extract_string (sch_name1),
			s48_extract_string (sch_name2));
  if (retval == -1)
    s48_raise_os_error_2(errno, sch_name1, sch_name2);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_truncate(s48_ref_t sch_path, s48_ref_t sch_length)
{
  int retval;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sch_path, sch_length);

  retval = truncate (s48_extract_string (sch_path),
		     s48_extract_integer (sch_length));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_2(errno, sch_path, sch_length);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_ftruncate(s48_ref_t sch_fdes, s48_ref_t sch_length)
{
  int retval;

  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sch_fdes, sch_length);

  retval = ftruncate (s48_extract_fixnum (sch_fdes),
		      s48_extract_integer (sch_length));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_2(errno, sch_fdes, sch_length);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_unlink(s48_ref_t sch_path)
{
  int retval = unlink (s48_extract_string (sch_path));
  if (retval == -1)
    s48_raise_os_error_1(errno, sch_path);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_fsync(s48_ref_t sch_fdes)
{
  int retval = fsync (s48_extract_fixnum (sch_fdes));
  if (retval == -1)
    s48_raise_os_error_1(errno, sch_fdes);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_sync()
{
#ifdef HAVE_SYNC
  sync();
#endif
  return s48_unspecific_2(call);
}

s48_ref_t scsh_close(s48_ref_t sch_fdes)
{
  int retval = close (s48_extract_fixnum (sch_fdes));
  if (retval == 0)
    return S48_TRUE;
  else if (errno == EBADF)
    return S48_FALSE;
  else s48_raise_os_error_1 (errno, sch_fdes);
}

s48_ref_t scsh_dup(s48_ref_t sch_fdes)
{
  int retval = dup (s48_extract_fixnum (sch_fdes));
  if (retval == -1)
    s48_raise_os_error_1 (errno, sch_fdes);
  return s48_enter_fixnum (retval);
}

s48_ref_t scsh_dup2(s48_ref_t sch_oldd,  s48_ref_t sch_newd)
{
  int retval = dup2 (s48_extract_fixnum (sch_oldd),
		     s48_extract_fixnum (sch_newd));
  if (retval == -1)
    s48_raise_os_error_2 (errno, sch_oldd, sch_newd);
  return s48_enter_fixnum (retval);
}

s48_ref_t scsh_lseek(s48_ref_t sch_fdes, s48_ref_t sch_offset,
		     s48_ref_t sch_whence)
{
  int retval;
  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(sch_fdes, sch_offset, sch_whence);

  retval = lseek (s48_extract_fixnum (sch_fdes),
		  s48_extract_integer (sch_offset),
		  s48_extract_fixnum (sch_whence));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_3 (errno, sch_fdes, sch_offset, sch_whence);
  return s48_enter_integer (retval);
}

s48_ref_t scsh_open(s48_ref_t sch_path, s48_ref_t sch_flags, s48_ref_t sch_mode)
{
  int retval = open (s48_extract_string (sch_path),
		     s48_extract_fixnum (sch_flags),
		     s48_extract_fixnum (sch_mode));
  if (retval == -1)
    s48_raise_os_error_3 (errno, sch_path, sch_flags, sch_mode);

  return s48_enter_fixnum (retval);
}

s48_ref_t char_ready_fdes(s48_ref_t sch_fd)
{
  fd_set readfds;
  struct timeval timeout;
  int result;
  int fd = s48_extract_fixnum (sch_fd);
  FD_ZERO(&readfds);
  FD_SET(fd, &readfds);

  timeout.tv_sec=0;
  timeout.tv_usec=0;

  result=select(fd+1, &readfds, NULL, NULL, &timeout);

  if(result == -1 )
    s48_raise_os_error_1(errno, sch_fd);
  if(result)
    return(S48_TRUE);
  return(S48_FALSE);
}


/* Supplementary groups access
*******************************************************************************
*/

s48_ref_t scsh_getgid()
{
  return s48_enter_integer(getgid());
}

s48_ref_t scsh_getegid()
{
  return s48_enter_integer(getegid());
}

s48_ref_t scsh_setgid(s48_ref_t gid)
{
  int retval;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(gid);

  retval = setgid (s48_extract_integer (gid));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_1(errno, gid);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_setegid(s48_ref_t gid)
{
  int retval;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(gid);

#ifdef HAVE_SETEGID
  retval = setegid (s48_extract_integer (gid));
#else
  retval = setregid (-1, s48_extract_integer (gid));
#endif

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_1(errno, gid);
  return s48_unspecific_2(call);
}

/* Load the supplementary groups into a list */

s48_ref_t get_groups()
{
  int retval;
  gid_t gvec[NGROUPS_MAX + 1];
  int veclen = getgroups(NGROUPS_MAX + 1, gvec);
  int i = veclen;
  s48_ref_t l = S48_NULL;

  if (veclen == -1)
    s48_raise_os_error(errno);
  else {
    S48_DECLARE_GC_PROTECT(1);
    S48_GC_PROTECT_1(l);

    while (i > 0)
      l = s48_cons(s48_enter_integer(gvec[--i]), l);

    S48_GC_UNPROTECT();

    return l;
  }
}
s48_ref_t scsh_getuid()
{
  return s48_enter_integer(getuid());
}

s48_ref_t scsh_geteuid()
{
  return s48_enter_integer(geteuid());
}

s48_ref_t scsh_setuid(s48_ref_t uid)
{
  int retval;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(uid);

  retval = setuid (s48_extract_integer (uid));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_1(errno, uid);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_seteuid(s48_ref_t uid)
{
  int retval;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(uid);

#ifdef HAVE_SETEUID
  retval = seteuid (s48_extract_integer (uid));
#else
  retval = setreuid (-1, s48_extract_integer (uid));
#endif

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_1(errno, uid);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_getpid()
{
  return s48_enter_integer(getpid());
}

s48_ref_t scsh_getppid()
{
  return s48_enter_integer(getppid());
}

s48_ref_t scsh_getpgrp()
{
  return s48_enter_integer(getpgrp());
}

s48_ref_t scsh_setpgid(s48_ref_t sch_pid, s48_ref_t sch_pgrp)
{
  int retval;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sch_pid, sch_pgrp);

  retval = setpgid(s48_extract_integer(sch_pid),
		   s48_extract_integer(sch_pgrp));

  S48_GC_UNPROTECT();

  if (retval == -1)
    s48_raise_os_error_2(errno, sch_pid, sch_pgrp);
  return s48_unspecific_2(call);
}

s48_ref_t scsh_setsid()
{
  pid_t retval = setsid();
   if (retval == -1)
     s48_raise_os_error(errno);
   return s48_enter_integer(retval);
}

s48_ref_t scsh_umask(s48_ref_t sch_mask)
{
  return s48_enter_integer(umask(s48_extract_integer(sch_mask)));
}

/* Environment hackery
*******************************************************************************
*/
static s48_ref_t envvec_record_type_binding = S48_FALSE;
static s48_ref_t add_envvec_finalizerB_binding = S48_FALSE;

#define ENVVEC_ENVIRON(envvec) \
  ((char**) s48_extract_integer(S48_RECORD_REF((envvec),0)))

/* The envvec corresponding to the current environment.
** Null if the current environment has no corresponding envvec struct
** (which should only be true of the initial environment at process
** startup time.) That is,
**     !current_env || current_env->env == environ
*/
s48_ref_t current_env = S48_FALSE;

s48_ref_t align_env(s48_ref_t envvec)
  {
    environ = ENVVEC_ENVIRON(envvec);
    current_env = envvec;
    return S48_TRUE;
}

char** original_environ = 0;

s48_ref_t free_envvec (s48_ref_t envvec)
{
  char** env = ENVVEC_ENVIRON(envvec);
  int i=0;
  if (env == original_environ)
    {
      return S48_FALSE;
    }
  while (env[i] != 0){
    Free(env[i]);
    i++;
  }
  Free(env);
  return S48_TRUE;
}

s48_ref_t make_envvec(char** newenv){
  s48_ref_t thread_env;

  thread_env = s48_make_record(envvec_record_type_binding);

  S48_RECORD_SET(thread_env, 0, s48_enter_integer((long)newenv));
  s48_call_scheme(S48_SHARED_BINDING_REF(add_envvec_finalizerB_binding),
		  1,
		  thread_env);
  return thread_env;
}

s48_ref_t scm_envvec(){
  s48_ref_t thread_env;
  if (current_env == 0){
    thread_env = make_envvec(environ);
    current_env = thread_env;
  }
  else thread_env = current_env;

  if (original_environ == 0)
    original_environ = environ;

  return s48_cons (char_pp_2_string_list(environ),
		   thread_env);
}


/* Load the (Scheme) strings in the (Scheme) vector VEC into environ.
*/

s48_ref_t create_env(s48_ref_t vec)
{
  int i, envsize;
  char **newenv;
  s48_ref_t thread_env;
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(vec);

  envsize = S48_VECTOR_LENGTH(vec);

  newenv = Malloc(char*, envsize+1);
  if( !newenv ) s48_raise_out_of_memory_error();


  for( i=0; i<envsize; i++ ) {
    char *s = scheme2c_strcpy(S48_VECTOR_REF(vec,i));
    if (!s) {
      /* Return all the memory and bail out. */
      while(--i) Free(newenv[i]);
      Free(newenv);
      Free(thread_env);
      s48_raise_out_of_memory_error();
    }
    newenv[i] = s;
  }

  newenv[envsize] = NULL;

  thread_env = make_envvec(newenv);
  environ = newenv;
  current_env = thread_env;

  S48_GC_UNPROTECT();
  return thread_env;

}

/*****************************************************************************/

/* N.B.: May be unaligned. */

s48_ref_t scm_gethostname(void)
{
   char hostname[MAXHOSTNAMELEN+1];
    /* different OS's declare differently, so punt the prototype. */
    int gethostname();
    int retval = gethostname(hostname, MAXHOSTNAMELEN);
    if (retval == -1) s48_raise_os_error(errno);
    return s48_enter_string(hostname);
}

#include <errno.h>

s48_ref_t errno_msg(s48_ref_t sch_i)
{
  int i = s48_extract_fixnum (sch_i);
#ifdef HAVE_STRERROR
  return(s48_enter_string (strerror(i)));
#else
    /* temp hack until we figure out what to do about losing sys_errlist's */
  extern
#ifdef HAVE_CONST_SYS_ERRLIST
    const
#endif
    char *sys_errlist[];
  extern int sys_nerr;
  return ( i < 0 || i > sys_nerr ) ? s48_raise_argument_type_error(sch_i)
	: s48_enter_string (sys_errlist[i]);
#endif /* !HAVE_STRERROR */
}

/* Some of fcntl()
******************
*/

s48_ref_t fcntl_read(s48_ref_t fd, s48_ref_t command)
{
  int ret;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(fd, command);

  ret = fcntl(s48_extract_fixnum (fd),
	      s48_extract_integer (command));

  S48_GC_UNPROTECT();

  if (ret == -1)
    s48_raise_os_error_2(errno, fd, command);
  else return s48_enter_fixnum (ret);
}


s48_ref_t fcntl_write(s48_ref_t fd, s48_ref_t command, s48_ref_t value)
{
  int ret;

  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(fd, command, value);

  ret = fcntl(s48_extract_fixnum (fd),
	      s48_extract_integer (command),
	      s48_extract_integer (value));

  S48_GC_UNPROTECT();

  if (ret == -1)
    s48_raise_os_error_3(errno, fd, command, value);
  else return s48_enter_fixnum (ret);
}

/* crypt()
******************
*/
s48_ref_t scm_crypt(s48_ref_t key, s48_ref_t salt)
{
  char * ret = crypt (s48_extract_string (key),
		      s48_extract_string(salt));

  /* FreeBSD does this on error:*/
  if (ret == NULL) return s48_enter_string("");

  return s48_enter_string (ret);
}

static s48_ref_t uname_record_type_binding = S48_FALSE;

s48_ref_t scm_uname(void)
{
  s48_ref_t uname_record = s48_unspecific_2(call);
  struct utsname uname_struct;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(uname_record);
  if (uname(&uname_struct) == -1)
    s48_raise_os_error(errno);

  uname_record = s48_make_record(uname_record_type_binding);
  S48_RECORD_SET(uname_record, 0, s48_enter_string (uname_struct.sysname));
  S48_RECORD_SET(uname_record, 1, s48_enter_string (uname_struct.nodename));
  S48_RECORD_SET(uname_record, 2, s48_enter_string (uname_struct.release));
  S48_RECORD_SET(uname_record, 3, s48_enter_string (uname_struct.version));
  S48_RECORD_SET(uname_record, 4, s48_enter_string (uname_struct.machine));
  S48_GC_UNPROTECT();
  return uname_record;
}

void s48_init_syscalls (){
  S48_EXPORT_FUNCTION(scheme_exec);
  S48_EXPORT_FUNCTION(scsh_exit);
  S48_EXPORT_FUNCTION(scsh__exit);
  S48_EXPORT_FUNCTION(scsh_fork);
  S48_EXPORT_FUNCTION(wait_pid);
  S48_EXPORT_FUNCTION(scsh_chdir);
  S48_EXPORT_FUNCTION(scheme_cwd);
  S48_EXPORT_FUNCTION(scsh_getgid);
  S48_EXPORT_FUNCTION(scsh_getegid);
  S48_EXPORT_FUNCTION(scsh_setgid);
  S48_EXPORT_FUNCTION(scsh_setegid);
  S48_EXPORT_FUNCTION(get_groups);
  S48_EXPORT_FUNCTION(scsh_getuid);
  S48_EXPORT_FUNCTION(scsh_geteuid);
  S48_EXPORT_FUNCTION(scsh_setuid);
  S48_EXPORT_FUNCTION(scsh_seteuid);
  S48_EXPORT_FUNCTION(scsh_getpid);
  S48_EXPORT_FUNCTION(scsh_getppid);
  S48_EXPORT_FUNCTION(scsh_getpgrp);
  S48_EXPORT_FUNCTION(scsh_setpgid);
  S48_EXPORT_FUNCTION(scsh_setsid);
  S48_EXPORT_FUNCTION(scsh_umask);
  S48_EXPORT_FUNCTION(process_times);
  S48_EXPORT_FUNCTION(cpu_clock_ticks_per_sec);
  S48_EXPORT_FUNCTION(scsh_chmod);
  S48_EXPORT_FUNCTION(scsh_fchmod);
  S48_EXPORT_FUNCTION(scsh_chown);
  S48_EXPORT_FUNCTION(scsh_fchown);
  S48_EXPORT_FUNCTION(scsh_access);
  S48_EXPORT_FUNCTION(scsh_link);
  S48_EXPORT_FUNCTION(scsh_mkfifo);
  S48_EXPORT_FUNCTION(scsh_mkdir);
  S48_EXPORT_FUNCTION(scsh_readlink);
  S48_EXPORT_FUNCTION(scsh_rename);
  S48_EXPORT_FUNCTION(scsh_rmdir);
  S48_EXPORT_FUNCTION(scm_utime);
  S48_EXPORT_FUNCTION(scm_utime_now);
  S48_EXPORT_FUNCTION(scheme_stat);
  S48_EXPORT_FUNCTION(scheme_fstat);
  S48_EXPORT_FUNCTION(scsh_symlink);
  S48_EXPORT_FUNCTION(scsh_truncate);
  S48_EXPORT_FUNCTION(scsh_ftruncate);
  S48_EXPORT_FUNCTION(scsh_unlink);
  S48_EXPORT_FUNCTION(scsh_fsync);
  S48_EXPORT_FUNCTION(scsh_sync);
  S48_EXPORT_FUNCTION(scsh_close);
  S48_EXPORT_FUNCTION(scsh_dup);
  S48_EXPORT_FUNCTION(scsh_dup2);
  S48_EXPORT_FUNCTION(scsh_lseek);
  S48_EXPORT_FUNCTION(char_ready_fdes);
  S48_EXPORT_FUNCTION(scsh_open);
  S48_EXPORT_FUNCTION(scheme_pipe);
  S48_EXPORT_FUNCTION(scsh_kill);
  S48_EXPORT_FUNCTION(scm_envvec);
  S48_EXPORT_FUNCTION(create_env);
  S48_EXPORT_FUNCTION(align_env);
  S48_EXPORT_FUNCTION(free_envvec);
  S48_EXPORT_FUNCTION(set_cloexec);
  S48_EXPORT_FUNCTION(fcntl_read);
  S48_EXPORT_FUNCTION(fcntl_write);
  S48_EXPORT_FUNCTION(sleep_until);
  S48_EXPORT_FUNCTION(scm_gethostname);
  S48_EXPORT_FUNCTION(errno_msg);
  S48_EXPORT_FUNCTION(scm_crypt);
  S48_EXPORT_FUNCTION(scm_uname);
  S48_GC_PROTECT_GLOBAL(envvec_record_type_binding);
  S48_GC_PROTECT_GLOBAL(add_envvec_finalizerB_binding);
  S48_GC_PROTECT_GLOBAL(current_env);
  S48_GC_PROTECT_GLOBAL(uname_record_type_binding);
  envvec_record_type_binding = s48_get_imported_binding("envvec-record-type");
  uname_record_type_binding = s48_get_imported_binding("uname-record-type");
  add_envvec_finalizerB_binding =
    s48_get_imported_binding("add-envvec-finalizer!");
}
