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

#include <grp.h>
#include <pwd.h>
#include <errno.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <utime.h>
#include "cstuff.h"
#include <limits.h>     /* NGROUPS_MAX on solaris */

/* Make sure our exports match up w/the implementation: */
#include "syscalls.h"

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
  pid_t pid;

  RETRY_OR_RAISE_NEG(pid, fork(), "scsh_fork");

  return s48_enter_long_2(call, pid);
}

/* Random file and I/O stuff
*******************************************************************************
*/

/* Returns (r w) */
s48_ref_t scheme_pipe(s48_call_t call)
{
  int fds[2];
  s48_ref_t sch_retval = s48_unspecific_2(call);

  if(pipe(fds) == -1)
    s48_os_error_2(call, "scheme_pipe", errno, 0);
  else {
    sch_retval = s48_cons_2(call, s48_enter_long_2(call, fds[0]),
                            s48_cons_2(call, s48_enter_long_2(call, fds[1]),
                                       s48_null_2(call)));
  }

  return sch_retval;
}

s48_ref_t scsh_kill (s48_call_t call, s48_ref_t sch_pid, s48_ref_t sch_signal)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     kill ((pid_t) s48_extract_long_2 (call, sch_pid),
                           s48_extract_long_2 (call, sch_signal)),
                     "scsh_kill");

  return s48_enter_long_2(call, retval);
}

/* Read the symlink. */
#ifdef MAXPATHLEN
s48_ref_t scsh_readlink(s48_call_t call, s48_ref_t sch_path)
{
  char linkpath[MAXPATHLEN+1];
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     readlink(s48_extract_byte_vector_2(call, sch_path),
                              linkpath, MAXPATHLEN),
                     "scsh_readlink");

  linkpath[retval] = '\0';
  return s48_enter_byte_string_2(call, linkpath);
}

#else
s48_ref_t scsh_readlink(s48_call_t call, s48_ref_t sch_path)
{
  char *linkpath;
  int size;
  int retval;
  s48_ref_t sch_sym_link_path = s48_unspecific_2(call);

  for (size = 256;; size *=2){

    linkpath = Malloc(char,size);
    if (!linkpath)
      s48_os_error_2(call, "scsh_readlink", errno, 1, sch_path);

    retval = readlink(s48_extract_byte_vector_2(call, sch_path), linkpath, size);

    if (retval == -1){
      free (linkpath);
      s48_os_error_2(call, "scsh_readlink", errno, 1, sch_path);
    }

    if (retval < size){
      sch_sym_link_path = s48_enter_byte_substring_2(call, linkpath, retval);
      free (linkpath);
      return sch_sym_link_path;
    }
    free (linkpath);
  }
}
#endif

/* Scheme interfaces to utime().
** Complicated by need to pass real 32-bit quantities.
*/

s48_ref_t scm_utime(s48_call_t call, s48_ref_t sch_path,
                    s48_ref_t sch_ac, s48_ref_t sch_mod)
{
  struct utimbuf t;
  int retval;

  t.actime = s48_extract_long_2(call, sch_ac);
  t.modtime = s48_extract_long_2(call, sch_mod);

  RETRY_OR_RAISE_NEG(retval,
                     utime(s48_extract_byte_vector_2(call, sch_path), &t),
                     "scm_utime");

  return s48_unspecific_2(call);
}

s48_ref_t scm_utime_now(s48_call_t call, s48_ref_t sch_path){
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     utime(s48_extract_byte_vector_2(call, sch_path), 0),
                     "scm_utime_now");

  return s48_unspecific_2(call);
}


s48_ref_t set_cloexec(s48_call_t call, s48_ref_t _fd, s48_ref_t _val)
{
  int fd = s48_extract_long_2(call, _fd);
  int val = s48_true_p_2(call, _val) ? 1 : 0;
  int flags;

  RETRY_OR_RAISE_NEG(flags, fcntl(fd, F_GETFD), "set_cloexec");

  val = -val;	/* 0 -> 0 and 1 -> -1 */

  /* If it's already what we want, just return. */
  if( (flags & FD_CLOEXEC) == (FD_CLOEXEC & val) ) return s48_false_2(call);

  flags = (flags & ~FD_CLOEXEC) | (val & FD_CLOEXEC);
  if (fcntl(fd, F_SETFD, flags) == -1)
    s48_os_error_2(call, "set_cloexec", errno, 2, _fd, _val);
  else return s48_false_2(call);
}

/* Process times
*******************************************************************************
*/

s48_ref_t process_times(s48_call_t call)
{
  struct tms tms;

  s48_ref_t sch_result_utime = s48_unspecific_2(call);
  s48_ref_t sch_result_stime = s48_unspecific_2(call);
  s48_ref_t sch_result_cutime = s48_unspecific_2(call);
  s48_ref_t sch_result_cstime = s48_unspecific_2(call);
  s48_ref_t sch_result_retval = s48_unspecific_2(call);

  clock_t t = times(&tms);
  if (t == -1) s48_os_error_2(call, "process_times", errno, 0);

  sch_result_utime = s48_enter_long_2(call, tms.tms_utime);
  sch_result_stime = s48_enter_long_2(call, tms.tms_stime);
  sch_result_cutime = s48_enter_long_2(call, tms.tms_cutime);
  sch_result_cstime = s48_enter_long_2(call, tms.tms_cstime);


  sch_result_retval = s48_cons_2(call, sch_result_utime,
                                 s48_cons_2(call, sch_result_stime,
                                            s48_cons_2(call, sch_result_cutime,
                                                       s48_cons_2(call,
                                                                  sch_result_cstime,
                                                                  s48_null_2(call)))));


  return sch_result_retval;
}

s48_ref_t cpu_clock_ticks_per_sec(s48_call_t call)
{
#ifdef _SC_CLK_TCK
  static long clock_tick = 0;

  if (clock_tick == 0){
    /* POSIX.1, POSIX.2 */
    RETRY_OR_RAISE_NEG(clock_tick, sysconf(_SC_CLK_TCK),
                       "cpu_clock_ticks_per_sec");
  }
  return s48_enter_long_2(call, clock_tick);
#else
#ifdef CLK_TCK
  return s48_enter_long_2(call, CLK_TCK);
#else
  return s48_enter_long_2(call, 60);
#endif
#endif
}

s48_ref_t scsh_chmod(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_mode)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     chmod(s48_extract_byte_vector_2(call, sch_path),
                           s48_extract_long_2(call, sch_mode)),
                     "scsh_chmod");

  return s48_unspecific_2(call);
}

s48_ref_t scsh_fchmod(s48_call_t call, s48_ref_t sch_fd, s48_ref_t sch_mode)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     fchmod(s48_extract_long_2(call, sch_fd),
                            s48_extract_long_2(call, sch_mode)),
                     "scsh_fchmod");

  return s48_unspecific_2(call);
}

s48_ref_t scsh_chown(s48_call_t call, s48_ref_t sch_path,
                     s48_ref_t sch_uid, s48_ref_t sch_gid)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     chown(s48_extract_byte_vector_2(call, sch_path),
                           s48_extract_long_2(call, sch_uid),
                           s48_extract_long_2(call, sch_gid)),
                     "scsh_chown");

  return s48_unspecific_2(call);
}

s48_ref_t scsh_fchown(s48_call_t call, s48_ref_t sch_fd,
                      s48_ref_t sch_uid, s48_ref_t sch_gid)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     fchown(s48_extract_long_2(call, sch_fd),
                            s48_extract_long_2(call, sch_uid),
                            s48_extract_long_2(call, sch_gid)),
                     "scsh_fchown");

  return s48_unspecific_2(call);
}

s48_ref_t scsh_access(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_mode)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     access (s48_extract_byte_vector_2(call, sch_path),
                             s48_extract_long_2(call, sch_mode)),
                     "scsh_access");

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
s48_ref_t really_stat(s48_call_t call, struct stat *s, s48_ref_t vec)
{
  int modes, typecode = -1;

  modes = s->st_mode;
  if( S_ISBLK(modes) )       typecode = 0;
  else if( S_ISCHR(modes) )  typecode = 1;
  else if( S_ISDIR(modes) )  typecode = 2;
  else if( S_ISFIFO(modes) ) typecode = 3;
  else if( S_ISREG(modes) )  typecode = 4;
  else if( S_ISSOCK(modes) ) typecode = 5;
  else if( S_ISLNK(modes) )  typecode = 6;

  s48_vector_set_2(call, vec, 0, s48_enter_long_2(call, typecode));
  s48_vector_set_2(call, vec, 1, s48_enter_long_2(call, s->st_dev));
  s48_vector_set_2(call, vec, 2, s48_enter_long_2(call, s->st_ino));
  s48_vector_set_2(call, vec, 3, s48_enter_long_2(call, s->st_mode));
  s48_vector_set_2(call, vec, 4, s48_enter_long_2(call, s->st_nlink));
  s48_vector_set_2(call, vec, 5, s48_enter_long_2(call, s->st_uid));
  s48_vector_set_2(call, vec, 6, s48_enter_long_2(call, s->st_gid));
  s48_vector_set_2(call, vec, 7, s48_enter_long_2(call, s->st_size));
  s48_vector_set_2(call, vec, 8, s48_enter_long_2(call, s->st_atime));
  s48_vector_set_2(call, vec, 9, s48_enter_long_2(call, s->st_mtime));
  s48_vector_set_2(call, vec, 10, s48_enter_long_2(call, s->st_ctime));

  /* We also used to do st_rdev, st_blksize, and st_blocks.
     These aren't POSIX, and, e.g., are not around on SGI machines.
     Too bad -- blksize is useful. Unix sux. */
  return s48_unspecific_2(call);
}

s48_ref_t scheme_stat(s48_call_t call, s48_ref_t path,
                      s48_ref_t vec, s48_ref_t chase_p)
{
  struct stat s;
  const char * cp_path = s48_extract_byte_vector_2(call, path);
  int retval = !(s48_false_p_2(call, chase_p)) ? stat(cp_path, &s) : lstat(cp_path, &s);

  if (retval == -1) s48_os_error_2(call, "scheme_stat", errno, 2, path, chase_p);

  return really_stat (call, &s, vec);
}

s48_ref_t scheme_fstat(s48_call_t call, s48_ref_t fd, s48_ref_t vec)
{
  struct stat s;
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     fstat(s48_extract_long_2(call, fd), &s),
                     "scheme_fstat");

  return really_stat (call, &s, vec);
}

s48_ref_t scsh_symlink(s48_call_t call, s48_ref_t sch_name1, s48_ref_t sch_name2)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     symlink(s48_extract_byte_vector_2(call, sch_name1),
                             s48_extract_byte_vector_2(call, sch_name2)),
                     "scsh_symlink");

  return s48_unspecific_2(call);
}

s48_ref_t scsh_truncate(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_length)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     truncate(s48_extract_byte_vector_2(call, sch_path),
                              s48_extract_long_2(call, sch_length)),
                     "scsh_truncate");

  return s48_unspecific_2(call);
}

s48_ref_t scsh_ftruncate(s48_call_t call, s48_ref_t sch_fdes, s48_ref_t sch_length)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     ftruncate(s48_extract_long_2(call, sch_fdes),
                               s48_extract_long_2(call, sch_length)),
                     "scsh_ftruncate");

  return s48_unspecific_2(call);
}

s48_ref_t scsh_fsync(s48_call_t call, s48_ref_t sch_fdes)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     fsync(s48_extract_long_2(call, sch_fdes)),
                     "scsh_fsync");

  return s48_unspecific_2(call);
}

s48_ref_t scsh_sync(s48_call_t call)
{
#ifdef HAVE_SYNC
  sync();
#endif
  return s48_unspecific_2(call);
}

s48_ref_t scsh_close(s48_call_t call, s48_ref_t sch_fdes)
{
  int retval = close (s48_extract_long_2(call, sch_fdes));
  if (retval == 0)
    return s48_true_2(call);
  else if (errno == EBADF)
    return s48_false_2(call);
  else s48_os_error_2(call, "scsh_close", errno, 1, sch_fdes);
}

s48_ref_t scsh_dup(s48_call_t call, s48_ref_t sch_fdes)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     dup(s48_extract_long_2(call, sch_fdes)),
                     "scsh_dup");

  return s48_enter_long_2(call, retval);
}

s48_ref_t scsh_dup2(s48_call_t call, s48_ref_t sch_oldd,  s48_ref_t sch_newd)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     dup2(s48_extract_long_2(call, sch_oldd),
                          s48_extract_long_2(call, sch_newd)),
                     "scsh_dup2");

  return s48_enter_long_2(call, retval);
}

s48_ref_t scsh_lseek(s48_call_t call, s48_ref_t sch_fdes,
                     s48_ref_t sch_offset, s48_ref_t sch_whence)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     lseek(s48_extract_long_2(call, sch_fdes),
                           s48_extract_long_2(call, sch_offset),
                           s48_extract_long_2(call, sch_whence)),
                     "scsh_lseek");

  return s48_enter_long_2(call, retval);
}

s48_ref_t scsh_open(s48_call_t call, s48_ref_t sch_path,
                    s48_ref_t sch_flags, s48_ref_t sch_mode)
{
  int retval;

  RETRY_OR_RAISE_NEG(retval,
                     open(s48_extract_byte_vector_2(call, sch_path),
                          s48_extract_long_2(call, sch_flags),
                          s48_extract_long_2(call, sch_mode)),
                     "scsh_open");

  return s48_enter_long_2(call, retval);
}

s48_ref_t char_ready_fdes(s48_call_t call, s48_ref_t sch_fd)
{
  fd_set readfds;
  struct timeval timeout;
  int result;
  int fd = s48_extract_long_2(call, sch_fd);
  FD_ZERO(&readfds);
  FD_SET(fd, &readfds);

  timeout.tv_sec=0;
  timeout.tv_usec=0;

  result = select(fd+1, &readfds, NULL, NULL, &timeout);

  if(result == -1 )
    s48_os_error_2(call, "char_ready_fdes", errno, 1, sch_fd);
  if(result)
    return s48_true_2(call);
  return s48_false_2(call);
}


/* Supplementary groups access
*******************************************************************************
*/

s48_ref_t scsh_getpgrp(s48_call_t call)
{
  return s48_enter_long_2(call, getpgrp());
}

s48_ref_t scsh_setpgid(s48_call_t call, s48_ref_t sch_pid, s48_ref_t sch_pgrp)
{
  int retval;

  retval = setpgid(s48_extract_long_2(call, sch_pid),
		   s48_extract_long_2(call, sch_pgrp));

  if (retval == -1)
    s48_os_error_2(call, "scsh_setpgid", errno, 2, sch_pid, sch_pgrp);

  return s48_unspecific_2(call);
}

s48_ref_t scsh_setsid(s48_call_t call)
{
  pid_t retval = setsid();

  if (retval == -1)
    s48_os_error_2(call, "scsh_setsid", errno, 0);

  return s48_enter_long_2(call, retval);
}

/* Environment hackery
*******************************************************************************
*/
static s48_ref_t envvec_record_type_binding;
static s48_ref_t add_envvec_finalizerB_binding;


/* This assumes there's a call type name call around.
 */
#define ENVVEC_ENVIRON(ENVVEC) \
  ((char**) s48_extract_long_2(call, s48_record_ref_2(call, (ENVVEC),0)))

/* The envvec corresponding to the current environment.
** Null if the current environment has no corresponding envvec struct
** (which should only be true of the initial environment at process
** startup time.) That is,
**     !current_env || current_env->env == environ
*/
s48_ref_t current_env;

s48_ref_t align_env(s48_call_t call, s48_ref_t envvec)
{
  environ = ENVVEC_ENVIRON(envvec);
  current_env = envvec;
  return s48_true_2(call);
}

char** original_environ = 0;

s48_ref_t free_envvec(s48_call_t call, s48_ref_t envvec)
{
  char** env = ENVVEC_ENVIRON(envvec);
  int i = 0;

  if(env == original_environ)
    return s48_false_2(call);

  Free(env);
  return s48_true_2(call);
}

s48_ref_t make_envvec(s48_call_t call, char** newenv){
  s48_ref_t thread_env;

  thread_env = s48_make_record_2(call, envvec_record_type_binding);

  s48_record_set_2(call, thread_env, 0, s48_enter_long_2(call, (long)newenv));
  s48_call_scheme_2(call,
                    s48_shared_binding_ref_2(call, add_envvec_finalizerB_binding),
                    1,
                    thread_env);
  return thread_env;
}

s48_ref_t scm_envvec(s48_call_t call){
  s48_ref_t thread_env;

  if (current_env == 0){
    thread_env = make_envvec(call, environ);
    current_env = thread_env;
  }
  else thread_env = current_env;

  if (original_environ == 0)
    original_environ = environ;

  return s48_cons_2(call, char_pp_2_string_list(call, environ),
                    thread_env);
}


/* Load the (Scheme) strings in the (Scheme) vector VEC into environ.
*/

s48_ref_t create_env(s48_call_t call, s48_ref_t vec)
{
  int i, envsize;
  char **newenv;
  s48_ref_t thread_env;

  envsize = s48_vector_length_2(call, vec);

  newenv = Malloc(char*, envsize+1);
  if( !newenv ) s48_out_of_memory_error_2(call);


  for( i=0; i<envsize; i++ ) {
    char *s = s48_extract_byte_vector_2(call, s48_vector_ref_2(call, vec, i));
    if (!s) {
      /* Return all the memory and bail out. */
      while(--i) Free(newenv[i]);
      Free(newenv);
      s48_out_of_memory_error_2(call);
    }
    newenv[i] = s;
  }

  newenv[envsize] = NULL;

  thread_env = make_envvec(call, newenv);
  environ = newenv;
  current_env = thread_env;

  return thread_env;

}

/*****************************************************************************/

/* N.B.: May be unaligned. */

s48_ref_t scm_gethostname(s48_call_t call)
{
   char hostname[MAXHOSTNAMELEN+1];
    /* different OS's declare differently, so punt the prototype. */
    int gethostname();
    int retval = gethostname(hostname, MAXHOSTNAMELEN);
    if (retval == -1) s48_os_error_2(call, "scm_gethostname", errno, 0);
    return s48_enter_byte_string_2(call, hostname);
}

s48_ref_t errno_msg(s48_call_t call, s48_ref_t sch_i)
{
  int i = s48_extract_long_2(call, sch_i);
#ifdef HAVE_STRERROR
  return(s48_enter_byte_string_2(call, strerror(i)));
#else
  if( i < 0 || i > sys_nerr )
    s48_error_2(call, "errno_msg", "", 1, sch_i);
  else
    return s48_enter_byte_string_2(call, sys_errlist[i]);
#endif /* !HAVE_STRERROR */
}

/* Some of fcntl()
******************
*/

s48_ref_t fcntl_read(s48_call_t call, s48_ref_t fd, s48_ref_t command)
{
  int ret;

  RETRY_OR_RAISE_NEG(ret,
                     fcntl(s48_extract_long_2(call, fd),
                           s48_extract_long_2(call, command)),
                     "fcntl_read");

  return s48_enter_long_2(call, ret);
}


s48_ref_t fcntl_write(s48_call_t call, s48_ref_t fd,
                      s48_ref_t command, s48_ref_t value)
{
  int ret;

  RETRY_OR_RAISE_NEG(ret,
                     fcntl(s48_extract_long_2(call, fd),
                           s48_extract_long_2(call, command),
                           s48_extract_long_2(call, value)),
                     "fcntl_write");

  return s48_enter_long_2(call, ret);
}

s48_ref_t scm_uname(s48_call_t call)
{
  s48_ref_t uname_list = s48_null_2(call);
  struct utsname uname_struct;

  if (uname(&uname_struct) == -1)
    s48_os_error_2(call, "scm_uname", errno, 0);

  uname_list = s48_cons_2(call, s48_enter_byte_string_2(call, uname_struct.sysname),
                          uname_list);
  uname_list = s48_cons_2(call, s48_enter_byte_string_2(call, uname_struct.nodename),
                          uname_list);
  uname_list = s48_cons_2(call, s48_enter_byte_string_2(call, uname_struct.release),
                          uname_list);
  uname_list = s48_cons_2(call, s48_enter_byte_string_2(call, uname_struct.version),
                          uname_list);
  uname_list = s48_cons_2(call, s48_enter_byte_string_2(call, uname_struct.machine),
                          uname_list);

  return uname_list;
}

/*
 *  User db access routines
 */

s48_ref_t user_info_uid(s48_call_t call, s48_ref_t scheme_uid,
                        s48_ref_t user_info_record)
{
  struct passwd *pwd = getpwuid(s48_extract_long_2(call, scheme_uid));

  if( !pwd ) {
    return s48_false_2(call);
  }

  s48_record_set_2(call, user_info_record, 0, s48_enter_byte_string_2(call, pwd->pw_name));
  s48_record_set_2(call, user_info_record, 2, s48_enter_long_2(call, pwd->pw_gid));
  s48_record_set_2(call, user_info_record, 3, s48_enter_byte_string_2(call, pwd->pw_dir));
  s48_record_set_2(call, user_info_record, 4, s48_enter_byte_string_2(call, pwd->pw_shell));

  return s48_true_2(call);
}

s48_ref_t user_info_name(s48_call_t call, s48_ref_t scheme_name,
                         s48_ref_t user_info_record)
{
  struct passwd *pwd = getpwnam(s48_extract_byte_vector_2(call, scheme_name));

  if(!pwd) {
    return s48_false_2(call);
  }

  s48_record_set_2(call, user_info_record, 1, s48_enter_long_2(call, pwd->pw_uid));
  s48_record_set_2(call, user_info_record, 2, s48_enter_long_2(call, pwd->pw_gid));
  s48_record_set_2(call, user_info_record, 3, s48_enter_byte_string_2(call, pwd->pw_dir));
  s48_record_set_2(call, user_info_record, 4, s48_enter_byte_string_2(call, pwd->pw_shell));

  return s48_true_2(call);
}

s48_ref_t group_info_gid (s48_call_t call, s48_ref_t scheme_gid,
                          s48_ref_t group_info_record)
{
  struct group *grp = getgrgid(s48_extract_long_2(call, scheme_gid));
  s48_ref_t member_list;

  if(!grp) {
    return s48_false_2(call);
  }

  s48_record_set_2(call, group_info_record, 0,
                   s48_enter_byte_string_2(call, grp->gr_name));

  member_list = char_pp_2_string_list(call, grp->gr_mem);

  if(!member_list) {
    return s48_false_2(call);
  }

  s48_record_set_2(call, group_info_record, 2, member_list);

  return s48_true_2(call);
}

s48_ref_t group_info_name(s48_call_t call, s48_ref_t scheme_name,
                          s48_ref_t group_info_record)
{
  struct group *grp = getgrnam(s48_extract_byte_vector_2(call, scheme_name));
  s48_ref_t member_list;

  if(!grp) {
    return s48_false_2(call);
  }

  s48_record_set_2(call, group_info_record, 1,
                   s48_enter_long_2(call, grp->gr_gid));

  member_list = char_pp_2_string_list(call, grp->gr_mem);

  if(!member_list) {
    return s48_false_2(call);
  }

  s48_record_set_2(call, group_info_record, 2, member_list);

  return s48_true_2(call);
}

s48_ref_t sleep_until(s48_call_t call, s48_ref_t scm_when)
{
    time_t now = time(0);
    int delta = s48_extract_long_2(call, scm_when) - now;
    if( delta > 0 ) {
        fd_set r, w, e;
        struct timeval tv;
        tv.tv_sec = delta;
        tv.tv_usec = 0;
        FD_ZERO(&r);
        FD_ZERO(&w);
        FD_ZERO(&e);
        if( select(0, &r, &w, &e, &tv) ) return s48_false_2(call);	/* Lose */
    }
    return s48_true_2(call); /* Win */
}

void scsh_init_syscalls (){
  S48_EXPORT_FUNCTION(scsh_exit);
  S48_EXPORT_FUNCTION(scsh__exit);
  S48_EXPORT_FUNCTION(scsh_fork);
  S48_EXPORT_FUNCTION(wait_pid);
  S48_EXPORT_FUNCTION(scsh_getpgrp);
  S48_EXPORT_FUNCTION(scsh_setpgid);
  S48_EXPORT_FUNCTION(scsh_setsid);
  S48_EXPORT_FUNCTION(process_times);
  S48_EXPORT_FUNCTION(cpu_clock_ticks_per_sec);
  S48_EXPORT_FUNCTION(scsh_chmod);
  S48_EXPORT_FUNCTION(scsh_fchmod);
  S48_EXPORT_FUNCTION(scsh_chown);
  S48_EXPORT_FUNCTION(scsh_fchown);
  S48_EXPORT_FUNCTION(scsh_access);
  S48_EXPORT_FUNCTION(scsh_readlink);
  S48_EXPORT_FUNCTION(scm_utime);
  S48_EXPORT_FUNCTION(scm_utime_now);
  S48_EXPORT_FUNCTION(scheme_stat);
  S48_EXPORT_FUNCTION(scheme_fstat);
  S48_EXPORT_FUNCTION(scsh_symlink);
  S48_EXPORT_FUNCTION(scsh_truncate);
  S48_EXPORT_FUNCTION(scsh_ftruncate);
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
  S48_EXPORT_FUNCTION(scm_uname);
  S48_EXPORT_FUNCTION(user_info_uid);
  S48_EXPORT_FUNCTION(user_info_name);
  S48_EXPORT_FUNCTION(group_info_gid);
  S48_EXPORT_FUNCTION(group_info_name);

  current_env = s48_make_global_ref(_s48_value_false);
  envvec_record_type_binding = s48_get_imported_binding_2("envvec-record-type");
  add_envvec_finalizerB_binding = s48_get_imported_binding_2("add-envvec-finalizer!");
}
