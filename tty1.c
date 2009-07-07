/* To do:
 * - Replace explicit 8/24 splits with macros.
 * - We need to pass the control-chars vecs in as Scheme
 *   strings, and test the length before doing the memcpy.
 */

/* 
 * Scheme48/scsh terminal control interface.
 * Routines that require custom C support.
 * Copyright (c) 1995 by Brian D. Carlstrom
 * Re-written by Olin.
 */
#include "sysdep.h"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>	/* ctermid decl */
#include <termios.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include "scheme48.h"

#ifdef HAVE_STROPTS_H
#include <stropts.h>
#endif

#ifdef HAVE_PTY_H
#include <pty.h>		/* openpty() on Tru64, Linux */
#endif

#ifdef HAVE_LIBUTIL_H
#include <libutil.h>		/* openpty() on FreeBSD */
#endif

#ifdef HAVE_UTIL_H
#include <util.h>		/* openpty() on NetBSD */
#endif

#ifndef countof
# define countof(x) (sizeof (x) / sizeof (*(x)))
#endif

#define INTERRUPTIBLE_CLOSE

int
retry_close (int filedes)
{
#ifdef INTERRUPTIBLE_CLOSE
  int did_retry = 0;
  int rtnval;

  while ((rtnval = close (filedes)) == -1
	 && (errno == EINTR))
    did_retry = 1;

  /* If close is interrupted SunOS 4.1 may or may not have closed the
     file descriptor.  If it did the second close will fail with
     errno = EBADF.  That means we have succeeded.  */
  if (rtnval == -1 && did_retry && errno == EBADF)
    return 0;

  return rtnval;
#else
  return close (filedes);
#endif
}

/* This #include is for the #ifdef'd code in open_ctty() below, and
** is therefor ifdef'd identically.
*/
#if defined(TIOCSCTTY) && !defined(CIBAUD)
#include <sys/ioctl.h>
#endif

#include "tty1.h"	/* Make sure the .h interface agrees with the code. */

/*****************************************************************************/

s48_value scheme_tcgetattr(s48_value sch_fd, s48_value sch_control_chars)
     /*		     int *iflag,
		     int *oflag,
		     int *cflag,
		     int *lflag,
		     int *ispeed,    int *ospeed)*/
{
  S48_DECLARE_GC_PROTECT(6);
  struct termios t;
  int fd = s48_extract_fixnum(sch_fd);
  int i;
  int result;

  s48_value sch_iflag = S48_UNSPECIFIC;
  s48_value sch_oflag = S48_UNSPECIFIC;
  s48_value sch_cflag = S48_UNSPECIFIC;
  s48_value sch_lflag = S48_UNSPECIFIC;
  s48_value sch_ispeed = S48_UNSPECIFIC;
  s48_value sch_ospeed = S48_UNSPECIFIC;
  s48_value sch_retval = S48_UNSPECIFIC;

  if (isatty(fd) == 0) {
    fprintf(stderr, "%d is not a tty\n", fd);
    return S48_FALSE;
  }
    

  result = tcgetattr(s48_extract_fixnum (sch_fd), &t);
  
  S48_GC_PROTECT_6(sch_iflag, sch_oflag, sch_cflag, sch_lflag, sch_ispeed,
		   sch_ospeed);

  if (result == -1) {
    S48_GC_UNPROTECT();
    s48_raise_os_error_2 (errno, sch_fd, sch_control_chars);
  }

  for (i = 0; i < NCCS; i++)
    S48_STRING_SET(sch_control_chars, i, t.c_cc[i]);
  {
    sch_iflag = s48_enter_integer(t.c_iflag);
    sch_oflag = s48_enter_integer(t.c_oflag);
    sch_cflag = s48_enter_integer(t.c_cflag);
    sch_lflag = s48_enter_integer(t.c_lflag);
    sch_ispeed = s48_enter_integer(cfgetispeed(&t));
    sch_ospeed = s48_enter_integer(cfgetospeed(&t));

    sch_retval = s48_list_6 (sch_iflag, sch_oflag, sch_cflag, sch_lflag,
			     sch_ispeed, sch_ospeed);
    S48_GC_UNPROTECT();
    
    return sch_retval;
  }
}

/* The Scheme caller of this is commented out...*/
int scheme_tcgetattrB(int fd, char *control_chars, s48_value scmvec)
{
  struct termios t;
  int result = tcgetattr(fd, &t);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(scmvec);
  /* JMG  int *ivec = ADDRESS_AFTER_HEADER(scmvec, int);*/
  
  if (result != -1) {
      memcpy(control_chars, t.c_cc, NCCS);
      S48_VECTOR_SET(scmvec, 0, s48_enter_integer(t.c_iflag));
      S48_VECTOR_SET(scmvec, 1, s48_enter_integer(t.c_oflag));
      S48_VECTOR_SET(scmvec, 2, s48_enter_integer(t.c_cflag));
      S48_VECTOR_SET(scmvec, 3, s48_enter_integer(t.c_lflag));
      S48_VECTOR_SET(scmvec, 4, s48_enter_fixnum(cfgetispeed(&t)));
      S48_VECTOR_SET(scmvec, 5, s48_enter_fixnum(cfgetospeed(&t)));
      }
  S48_GC_UNPROTECT();
  return result;
  }


/*****************************************************************************/

s48_value scheme_tcsetattr(s48_value sch_fd, s48_value sch_option,
			   s48_value sch_control_chars,
			   s48_value sch_iflag,
			   s48_value sch_oflag,
			   s48_value sch_cflag,
			   s48_value sch_lflag,
			   s48_value sch_ispeed, s48_value sch_ospeed,
			   s48_value sch_min, s48_value sch_time)
{
  struct termios t;

  memcpy(t.c_cc, s48_extract_string (sch_control_chars), NCCS);

  /* This first clause of this conditional test will hopefully
  ** resolve the branch at compile time. However, since VMIN/VEOF
  ** and VTIME/VEOL are allowed by POSIX to colllide, we have to check.
  ** If they do collide, we set EOF & EOL in canonical mode, and MIN & TIME
  ** in raw mode. Ah, Unix.
  */

  t.c_iflag = s48_extract_integer (sch_iflag);
  t.c_oflag = s48_extract_integer (sch_oflag);
  t.c_cflag = s48_extract_integer (sch_cflag);
  t.c_lflag = s48_extract_integer (sch_lflag);

  if( (VMIN != VEOF && VTIME != VEOL) || !(t.c_lflag & ICANON) ) {
      t.c_cc[VMIN] = s48_extract_fixnum (sch_min);
      t.c_cc[VTIME] = s48_extract_integer (sch_time);
      }

  cfsetispeed(&t, s48_extract_integer (sch_ispeed));
  cfsetospeed(&t, s48_extract_integer (sch_ospeed));

  if (tcsetattr(s48_extract_fixnum (sch_fd), s48_extract_integer (sch_option), 
		&t) 
      == -1)
    s48_raise_os_error_1 (errno, sch_fd);
  return S48_UNSPECIFIC;
}


s48_value sch_tcsendbreak (s48_value sch_fd, s48_value sch_duration)
{
  if (tcsendbreak (s48_extract_fixnum (sch_fd), 
		   s48_extract_integer (sch_duration)) == -1)
    s48_raise_os_error_2 (errno, sch_fd, sch_duration);
  return S48_UNSPECIFIC;
}

s48_value sch_tcdrain (s48_value sch_fd)
{
  if (tcdrain (s48_extract_fixnum (sch_fd)) == -1)
    s48_raise_os_error_1 (errno, sch_fd);
  return S48_UNSPECIFIC;
}

s48_value sch_tcflush (s48_value sch_fd, s48_value sch_action)
{
  if (tcflush (s48_extract_fixnum (sch_fd),
	       s48_extract_fixnum (sch_action)) == -1)
    s48_raise_os_error_2 (errno, sch_fd, sch_action);
  return S48_UNSPECIFIC;
}

s48_value sch_tcflow (s48_value sch_fd, s48_value sch_action)
{
  if (tcflow (s48_extract_fixnum (sch_fd),
	      s48_extract_fixnum (sch_action)) == -1)
    s48_raise_os_error_2 (errno, sch_fd, sch_action);
  return S48_UNSPECIFIC;
}

s48_value sch_tcsetpgrp (s48_value sch_fd, s48_value sch_pid)
{
   if (tcsetpgrp (s48_extract_fixnum (sch_fd),
		  s48_extract_fixnum (sch_pid)) == -1)
    s48_raise_os_error_2 (errno, sch_fd, sch_pid);
  return S48_UNSPECIFIC;
}
	       
s48_value sch_tcgetpgrp (s48_value sch_fd)
{
  int ret = tcgetpgrp (s48_extract_fixnum (sch_fd));
  if (ret == -1)
    s48_raise_os_error_1 (errno, sch_fd);
  return s48_enter_integer (ret);
}

/*****************************************************************************/

s48_value open_ctty(s48_value sch_ttyname, s48_value sch_flags)
{
    int fd = open(s48_extract_string (sch_ttyname), 
		  s48_extract_integer (sch_flags));
    
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(__hpux)
    /* 4.3+BSD way to acquire control tty. !CIBAUD rules out SunOS.
    ** This code stolen from Steven's *Advanced Prog. in the Unix Env.*
    */
    if( (fd >= 0) && (ioctl(fd, TIOCSCTTY, (char *) 0) < 0) ) {
	int e = errno;
	close(fd);
	s48_raise_os_error_2 (e, sch_ttyname, sch_flags);
	}
#endif
    if (fd == -1)
      s48_raise_os_error_2 (errno, sch_ttyname, sch_flags);
    return s48_enter_fixnum (fd);
}

s48_value make_ctty(s48_value sch_fd)
{
    int fd = s48_extract_fixnum (sch_fd);
    
#if defined(TIOCSCTTY) && !defined(CIBAUD) && !defined(__hpux)
    /* 4.3+BSD way to acquire control tty. !CIBAUD rules out SunOS.
    ** This code stolen from Steven's *Advanced Prog. in the Unix Env.*
    */
    if( (fd >= 0) && (ioctl(fd, TIOCSCTTY, (char *) 0) < 0) ) {
	s48_raise_os_error_1 (errno, sch_fd);
	}
#endif
    return S48_UNSPECIFIC;
}

s48_value pty2tty(s48_value sch_fd)
{
  int fd = s48_extract_fixnum(sch_fd);

  #if defined (HAVE_ISASTREAM) && defined (I_PUSH)
    if (isastream (fd))
      {
# if defined (I_FIND)
#  define stream_module_pushed(fd, module) (ioctl (fd, I_FIND, module) == 1)
# else
#  define stream_module_pushed(fd, module) 0
# endif
        if ((! stream_module_pushed (fd, "ptem")) &&
            (ioctl (fd, I_PUSH, "ptem") < 0))
          s48_raise_os_error_1 (errno, sch_fd);
        if ((! stream_module_pushed (fd, "ldterm")) &&
            (ioctl (fd, I_PUSH, "ldterm") < 0))
          s48_raise_os_error_1 (errno, sch_fd);
        if ((! stream_module_pushed (fd, "ttcompat")) &&
            (ioctl (fd, I_PUSH, "ttcompat") < 0))
          s48_raise_os_error_1 (errno, sch_fd);
      }
#endif /* defined (HAVE_ISASTREAM) && defined (I_PUSH) */
    return S48_UNSPECIFIC;
}

s48_value sch_isatty (s48_value sch_fd)
{
  return ((isatty (s48_extract_fixnum (sch_fd))) ? S48_TRUE : S48_FALSE);
}

s48_value sch_ttyname (s48_value sch_fd)
{
  char* ret = ttyname (s48_extract_fixnum (sch_fd));
  if (ret == NULL)
    s48_raise_os_error_1 (errno, sch_fd);
  return s48_enter_string (ret);
}

s48_value scm_ctermid() 
{ 
  char* ret = ctermid(0);
  if (ret == NULL)
    s48_raise_os_error (errno);
  return s48_enter_string (ret);
}

static int allocate_master (const char**, const char **);
static const char* allocate_slave_name(int, const char*);

#define BLOCK_SIGNAL(sig) do                    \
{                                               \
  sigset_t sig_mask;                             \
  sigemptyset (&sig_mask);                       \
  sigaddset (&sig_mask, sig);                    \
  sigprocmask (SIG_BLOCK, &sig_mask, NULL);      \
} while (0)
#define UNBLOCK_SIGNAL(sig) do                  \
{                                               \
  sigset_t sig_mask;                             \
  sigemptyset (&sig_mask);                       \
  sigaddset (&sig_mask, sig);                    \
  sigprocmask (SIG_UNBLOCK, &sig_mask, NULL);    \
} while (0)


/* Open an available pty, returning a file descriptor.
   Return -1 on failure. */
allocate_pty (void)
{
  /* Unix98 standardized grantpt, unlockpt, and ptsname, but not the
     functions required to open a master pty in the first place :-(

     Modern Unix systems all seems to have convenience methods to open
     a master pty fd in one function call, but there is little
     agreement on how to do it.

     allocate_pty() tries all the different known easy ways of opening
     a pty.  In case of failure, we resort to the old BSD-style pty
     grovelling code in allocate_pty_the_old_fashioned_way(). */
  int master_fd = -1;
  const char *slave_name = NULL;
  const char* clone = NULL;
  int off = 0;

  s48_value scm_slave_name = S48_UNSPECIFIC;

  master_fd = allocate_master(&slave_name, &clone);

  if (master_fd == -1)
    return S48_FALSE;

  if (slave_name == NULL){
    slave_name = allocate_slave_name(master_fd, clone);
    
    if (slave_name == NULL){
      retry_close (master_fd);
      return S48_FALSE;
    }
  }
  scm_slave_name = s48_enter_string((char *) slave_name);

#ifdef TIOCPKT
  /* In some systems (Linux through 2.0.0, at least), packet mode doesn't
     get cleared when a pty is closed, so we need to clear it here.
     Linux pre2.0.13 contained an attempted fix for this (from Ted Ts'o,
     tytso@mit.edu), but apparently it messed up rlogind and telnetd, so he
     removed the fix in pre2.0.14.     - dkindred@cs.cmu.edu
   */
  ioctl (master_fd, TIOCPKT, (char *)&off);
#endif /* TIOCPKT */

  /* We jump through some hoops to frob the pty.
     It's not obvious that checking the return code here is useful. */

  /* "The grantpt() function will fail if it is unable to successfully
      invoke the setuid root program.  It may also fail if the
      application has installed a signal handler to catch SIGCHLD
      signals." */
#if defined (HAVE_GRANTPT) || defined (HAVE_UNLOCKPT)
  BLOCK_SIGNAL (SIGCHLD);

#if defined (HAVE_GRANTPT)
  grantpt (master_fd);
#endif /* HAVE_GRANTPT */

#if defined (HAVE_UNLOCKPT)
  unlockpt (master_fd);
#endif

  UNBLOCK_SIGNAL (SIGCHLD);
#endif /* HAVE_GRANTPT || HAVE_UNLOCKPT */

  fcntl(master_fd, F_SETFL, O_NONBLOCK);
  
  return s48_cons(s48_enter_fixnum(master_fd), scm_slave_name);
}

static int
allocate_master(const char ** slave_name, const char** clone){
  int master_fd = -1;
  static const char * const clones[] =
    /* Different pty master clone devices */
    {
      "/dev/ptmx",      /* Various systems */
      "/dev/ptm/clone", /* HPUX */
      "/dev/ptc",       /* AIX */
      "/dev/ptmx_bsd"   /* Tru64 */
    };

#ifdef HAVE_GETPT /* glibc */
  master_fd = getpt ();
  if (master_fd >= 0)
    return master_fd;
#endif /* HAVE_GETPT */


#if defined(HAVE_OPENPTY) /* BSD, Tru64, glibc */
  {
    int slave_fd = -1;
    int rc;
    BLOCK_SIGNAL (SIGCHLD);
    rc = openpty (&master_fd, &slave_fd, NULL, NULL, NULL);
    UNBLOCK_SIGNAL (SIGCHLD);
    if (rc == 0)
      {
	*slave_name = ttyname (slave_fd);
	retry_close (slave_fd);
	return master_fd;
      }
    else
      {
	if (master_fd >= 0)
	  retry_close (master_fd);
	if (slave_fd >= 0)
	  retry_close (slave_fd);
      }
  }
#endif /* HAVE_OPENPTY */

#if defined(HAVE__GETPTY) && defined (O_NDELAY) /* SGI */
  master_fd = -1;
  BLOCK_SIGNAL (SIGCHLD);
  *slave_name = _getpty (&master_fd, O_RDWR | O_NDELAY, 0600, 0);
  UNBLOCK_SIGNAL (SIGCHLD);
  if (master_fd >= 0 && *slave_name != NULL)
    return master_fd;
#endif /* HAVE__GETPTY */

  /* Master clone devices are available on most systems */
  {
    int i;
    for (i = 0; i < countof (clones); i++)
      {
	*clone = clones[i];
	master_fd = open ((char *) *clone, // TODO: retry open
                          O_RDWR | O_NONBLOCK, 0);
	if (master_fd >= 0)
	  return master_fd;
      }
    *clone = NULL;
  }
  return -1;
}

static const char*
allocate_slave_name(int master_fd, const char* clone){

  char * slave_name;

#if defined (HAVE_PTSNAME)
  slave_name = ptsname (master_fd);
  if (slave_name)
    return slave_name;
#endif

  /* kludge for AIX */
  if (clone
      && !strcmp (clone, "/dev/ptc")
      && (slave_name = ttyname (master_fd)) != NULL)
    return slave_name;

  return NULL;
}

void s48_init_tty(void)
{
    S48_EXPORT_FUNCTION(scheme_tcgetattr);
    S48_EXPORT_FUNCTION(scheme_tcsetattr);
    S48_EXPORT_FUNCTION(sch_tcsendbreak);
    S48_EXPORT_FUNCTION(sch_tcdrain);
    S48_EXPORT_FUNCTION(sch_tcflush);
    S48_EXPORT_FUNCTION(sch_tcflow);
    S48_EXPORT_FUNCTION(sch_tcsetpgrp);
    S48_EXPORT_FUNCTION(sch_tcgetpgrp);
    S48_EXPORT_FUNCTION(open_ctty);
    S48_EXPORT_FUNCTION(make_ctty);
    S48_EXPORT_FUNCTION(pty2tty);
    S48_EXPORT_FUNCTION(sch_isatty);
    S48_EXPORT_FUNCTION(sch_ttyname);
    S48_EXPORT_FUNCTION(scm_ctermid);
    S48_EXPORT_FUNCTION(allocate_pty);
}
