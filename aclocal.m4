### s48_ldflags.m4 --- S48_LDFLAGS macro  -*- Autoconf -*-
# serial 1
dnl
dnl
dnl
dnl -rdynamic is needed for loading external code
AC_DEFUN([S48_LDFLAGS], [dnl
	AC_MSG_CHECKING([-rdynamic])
	oldLDFLAGS="$LDFLAGS"
	LDFLAGS="$LDFLAGS -rdynamic"
	AC_TRY_RUN([int main() { return 0;}],
		[AC_MSG_RESULT(yes)],
		[AC_MSG_RESULT(no)
			LDFLAGS="$oldLDFLAGS"],
		[AC_MSG_RESULT(no)
			LDFLAGS="$oldLDFLAGS"])
])dnl
### s48_ldflags.m4 ends here

### s48_dynamic_externals.m4 --- S48_DYNAMIC_EXTERNALS macro  -*- Autoconf -*-
# serial 1
dnl
dnl
dnl
AC_DEFUN([S48_DYNAMIC_EXTERNALS], [dnl
	dnl
	AC_CHECK_PROGS([LD], [ld cc gcc])
	AC_MSG_CHECKING([compile and link flags for dynamic externals])
	DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE=""
	case "$host_os" in
		hpux* )
			dnl +z means position-independent code
			DYNAMIC_EXTERNALS_CFLAGS='+z -D_HPUX_SOURCE'
			DYNAMIC_EXTERNALS_LDFLAGS='-b'
		;;
		aix* )
			DYNAMIC_EXTERNALS_CFLAGS=''
			dnl -bM:SRE means shared object
			dnl -brtl means run-time linking is enabled
			dnl -bnoentry means no default entry point
			DYNAMIC_EXTERNALS_LDFLAGS='-bM:SRE -brtl -bI:\$(incdir)/scheme48.exp -bnoentry -bE:\$(incdir)/scheme48-external.exp -lc'
			DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE='-bM:SRE -brtl -bI:\$(srcdir)/c/scheme48.exp -bnoentry -bE:\$(srcdir)/c/scheme48-external.exp -lc'
		;;
		darwin*|macosx* )
			DYNAMIC_EXTERNALS_CFLAGS='-fno-common'
			DYNAMIC_EXTERNALS_LDFLAGS="$CFLAGS $LDFLAGS -bundle -flat_namespace -undefined suppress"
			dnl linker workaround for MacOSX
			LD="$CC"
		;;
		solaris* )
			DYNAMIC_EXTERNALS_CFLAGS="$PIC"
			dnl -G means shared object
			DYNAMIC_EXTERNALS_LDFLAGS="$LDFLAGS -G"
			LD="$CC"
		;;
		linux* )
			dnl various Linuxen link in strange stuff we don't know about,
			dnl but gcc does
			LD="$CC"
			DYNAMIC_EXTERNALS_CFLAGS="$PIC"
			DYNAMIC_EXTERNALS_LDFLAGS="-shared $LDFLAGS"
		;;
		cygwin* )
			LD="$CC"
			DYNAMIC_EXTERNALS_CFLAGS=""
			DYNAMIC_EXTERNALS_LDFLAGS='-shared $(bindir)/scheme48vm.a'
			DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE='-shared $(srcdir)/scheme48vm.a'
		;;
		* )
			DYNAMIC_EXTERNALS_CFLAGS="$PIC"
			DYNAMIC_EXTERNALS_LDFLAGS='-shared'
		;;
	esac
	if test -z "$DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE"; then
		DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE="$DYNAMIC_EXTERNALS_LDFLAGS"
	fi
	AC_MSG_RESULT([$DYNAMIC_EXTERNALS_CFLAGS, $DYNAMIC_EXTERNALS_LDFLAGS])
])dnl
### s48_dynamic_externals.m4 ends here
