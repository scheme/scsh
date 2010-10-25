#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([scsh], [0.7], [roderyc@gmail.com])
AC_CONFIG_HEADERS([c/config.h])
AC_CANONICAL_HOST

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h limits.h netdb.h sgtty.h stdlib.h string.h sys/ioctl.h sys/param.h sys/time.h termios.h unistd.h utime.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

# Checks for library functions.
AC_FUNC_CHOWN
AC_FUNC_FORK
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([dup2 ftruncate gethostname memset select utime])
AC_CHECK_FUNC(openpty, have_openpty=yes, [
  AC_CHECK_LIB(util, openpty, have_openpty=yes need_libutil=yes)])
if test "$have_openpty" = "yes"; then
  AC_DEFINE(HAVE_OPENPTY, 1, [Define to 1 if you have the 'openpty' function])
  AC_CHECK_HEADERS(libutil.h util.h, break)
  if test "$need_libutil" = "yes"; then
    LIBS="${LIBS} -lutil"
  fi
fi
S48_LDFLAGS
S48_DYNAMIC_EXTERNALS
AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(LD)
AC_SUBST(DYNAMIC_EXTERNALS_CFLAGS)
AC_SUBST(DYNAMIC_EXTERNALS_LDFLAGS)
AC_SUBST(DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE)
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
