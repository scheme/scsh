#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([scsh], [0.7], [roderyc@gmail.com])
AC_CONFIG_HEADERS([c/config.h])
AC_CANONICAL_HOST

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

dnl  Is there a non-default location for Scheme48?
S48DIR=/usr
AC_SUBST(S48DIR)
AC_ARG_WITH([scheme48],
            [Location of scheme48 installation (parent of bin/scheme48)],
            [S48DIR=$withval])

dnl  Check the location of the scheme48 directory; the following will
dnl  also work if S48DIR has the default value of /usr
AC_PATH_PROG([SCHEME48], [scheme48], [], [$S48DIR/bin:$PATH])
if test -z "$SCHEME48"; then
   AC_MSG_ERROR([Can't find scheme48 -- aborting (consider using --with-scheme48)])
else
   S48DIR=`dirname $SCHEME48`
   S48DIR=`dirname $S48DIR`
fi
echo "SCHEME48=$SCHEME48,  S48DIR=$S48DIR"

CPPFLAGS="$CPPFLAGS -I$S48DIR/include"

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h limits.h netdb.h sgtty.h stdlib.h string.h sys/ioctl.h sys/param.h sys/time.h termios.h unistd.h utime.h])

dnl  We need the scheme48.h headers, so bomb out if we can't find them.
AC_CHECK_HEADERS([scheme48.h], [],
                 [AC_MSG_ERROR([Can't find required scheme48.h (consider using --with-scheme48)])])

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
  AC_CHECK_HEADERS(libutil.h util.h)
  if test "$need_libutil" = "yes"; then
    LIBS="${LIBS} -lutil"
  fi
fi
S48_PIC_OPTION_NAME
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
