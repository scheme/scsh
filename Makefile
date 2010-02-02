srcdir = .
# DESTDIR = ./test-install

CC = gcc
LD = gcc
DEFS = -DHAVE_CONFIG_H
CFLAGS = -g -O2 -pthread
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
LDFLAGS =  -rdynamic -pthread

prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
libdir = ${exec_prefix}/lib
manext = 1
mandir = ${datarootdir}/man/man$(manext)
datarootdir = ${prefix}/share
datadir = ${datarootdir}


DYNAMIC_EXTERNALS_CFLAGS=-fPIC
DYNAMIC_EXTERNALS_LDFLAGS=-shared  -rdynamic
DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE=-shared  -rdynamic

VERSION = 0.7
RUNNABLE = scsh
LIB = $(libdir)/scsh-$(VERSION)
SHARE = $(datadir)/scsh-$(VERSION)
lib_dirs_list = ("${prefix}/lib/scsh/modules" "${prefix}/lib/scsh/modules/${VERSION}")

SCHEME48VM = /usr/local/lib/scheme48-1.9t/scheme48vm
SCHEME48 = /usr/local/bin/scheme48

scsh: scsh/scsh.so scsh/scsh scsh/scsh.image go

C = scsh/syscalls.o \
    scsh/cstuff.o \
    scsh/scsh.o

scsh/scsh.so: $(C)
	$(LD) -o $@ $(C) $(DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE)

# not a complete list yet.
SCHEME = scsh/condition-handler.scm \
	 scsh/enumconst.scm \
	 scsh/event.scm \
	 scsh/low-interrupt.scm \
	 scsh/fdports.scm \
	 scsh/fileinfo.scm \
	 scsh/filemtch.scm \
	 scsh/filesys.scm \
	 scsh/fname.scm \
	 scsh/fr.scm \
	 scsh/glob.scm \
	 scsh/dot-locking.scm \
	 scsh/here.scm \
	 scsh/lib-dirs.scm \
	 scsh/libscsh.scm \
	 scsh/md5.scm \
	 scsh/meta-arg.scm \
	 scsh/newports.scm \
	 scsh/procobj.scm \
	 scsh/pty.scm \
	 scsh/rdelim.scm \
	 scsh/rw.scm \
	 scsh/scsh-condition.scm \
	 scsh/scsh-interfaces.scm \
	 scsh/scsh-package.scm \
	 scsh/scsh-read.scm \
	 scsh/scsh-version.scm \
	 scsh/scsh.scm \
	 scsh/startup.scm \
	 scsh/stringcoll.scm \
	 scsh/syntax-helpers.scm \
	 scsh/syntax.scm \
	 scsh/syscalls.scm \
	 scsh/top.scm \
	 scsh/tty.scm \
	 scsh/utilities.scm \
	 scsh/weaktables.scm \
	 rx/packages.scm \
	 rx/re-match-syntax.scm \
	 rx/rx-lib.scm \
	 rx/parse.scm \
	 rx/re-subst.scm \
	 rx/simp.scm \
	 rx/posixstr.scm \
	 rx/re-syntax.scm \
	 rx/spencer.scm \
	 rx/re-fold.scm \
	 rx/re.scm \
	 rx/re-high.scm \
	 rx/re-low.scm \
	 rx/regress.scm

go: scsh/scsh-tramp.c
	$(CC) -o $@ $(CFLAGS) \
	-DVM=\"$(SCHEME48VM)\" \
	-DIMAGE=\"scsh/scsh.image\" \
	$(srcdir)/scsh/scsh-tramp.c

scsh/scsh: scsh/scsh-tramp.c
	$(CC) -o $@ $(CFLAGS) \
	-DVM=\"$(SCHEME48VM)\" \
	-DIMAGE=\"$(LIB)/scsh.image\" \
	$(srcdir)/scsh/scsh-tramp.c

LOADS = $(srcdir)/rx/interfaces.scm \
	$(srcdir)/rx/packages.scm \
	$(srcdir)/scsh/scsh-interfaces.scm \
	$(srcdir)/scsh/scsh-package.scm \
	$(srcdir)/scsh/lib/ccp-pack.scm \
	$(srcdir)/scsh/lib/char-package.scm

OPENS = floatnums scsh scsh-top-package scsh-here-string-hax \
	srfi-1 srfi-13 srfi-14 # srfi-14 is also exported by scsh

scsh/scsh.image: $(SCHEME)
	$(srcdir)/build/build-image.sh $(srcdir) \
		"`pwd`/scsh/" '$@' '$(SCHEME48) -h 0' '$(LOADS)' '$(OPENS)'

dirs:
	for dir in $(bindir) $(LIB) $(SHARE); do\
		{ mkdir -p $(DESTDIR)$$dir && [ -w $(DESTDIR)$$dir ]; } || {	\
			echo "$(DESTDIR)$$dir not a writable directory" >&2;	\
			exit 1;						\
	}								\
	done

install: dirs install-scsh

install-scsh: scsh install-scsh-image # install-stripped-scsh-image
        #install runner
	$(RM) $(DESTDIR)$(bindir)/$(RUNNABLE)
	$(INSTALL_PROGRAM) scsh/scsh $(DESTDIR)$(bindir)/$(RUNNABLE)
        #install scsh.so
	$(RM) $(DESTDIR)$(LIB)/scsh.so
	$(INSTALL_DATA) scsh/scsh.so $(DESTDIR)$(LIB)
        #install scheme source files
	for f in $(srcdir)/scsh/*.scm $(srcdir)/scsh/*/*.scm; \
	    do $(INSTALL_DATA) $$f $(DESTDIR)$(SHARE)/; done

install-scsh-image: scsh/scsh.image
	(	echo ',translate =scshexternal/ $(LIB)/)'; \
		echo ',in lib-dirs (set-default-lib-dirs! (quote $(lib_dirs_list)))'; \
		echo ',user'; \
		echo '(dump-scsh "$(DESTDIR)$(LIB)/scsh.image")';	\
		echo ',exit';						\
	) | $(SCHEME48) -h 0 -i scsh/scsh.image -a batch

clean:
	$(RM) scsh/*.o scsh/*.so scsh/*.image scsh/scsh go
