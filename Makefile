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

enough: c/scsh.so scsh scsh.image go

test: enough
	$(srcdir)/build/test.sh $(srcdir)

C = c/syscalls.o \
    c/tty.o \
    c/cstuff.o \
    c/scsh.o

c/scsh.so: $(C)
	$(LD) -o $@ $(C) $(DYNAMIC_EXTERNALS_LDFLAGS_IN_PLACE)

# not a complete list yet.
SCHEME = scheme/command-line.scm \
	 scheme/condition-handler.scm \
	 scheme/continuation.scm \
	 scheme/directory.scm \
	 scheme/enumconst.scm \
	 scheme/environment.scm \
	 scheme/event.scm \
	 scheme/fdports.scm \
	 scheme/file.scm \
	 scheme/fileinfo.scm \
	 scheme/filemtch.scm \
	 scheme/filesys.scm \
	 scheme/fname.scm \
	 scheme/fname-system.scm \
	 scheme/fr.scm \
	 scheme/glob.scm \
	 scheme/dot-locking.scm \
	 scheme/here.scm \
	 scheme/lib-dirs.scm \
	 scheme/libscsh.scm \
	 scheme/low-interrupt.scm \
	 scheme/md5.scm \
	 scheme/meta-arg.scm \
	 scheme/newports.scm \
	 scheme/port-collect.scm \
	 scheme/process-high-level.scm \
	 scheme/process-state.scm \
	 scheme/process.scm \
	 scheme/procobj.scm \
	 scheme/pty.scm \
	 scheme/rdelim.scm \
	 scheme/resource.scm \
	 scheme/rw.scm \
	 scheme/scsh-condition.scm \
	 scheme/scsh-interfaces.scm \
	 scheme/scsh-package.scm \
	 scheme/scsh-read.scm \
	 scheme/scsh-version.scm \
	 scheme/signal.scm \
	 scheme/startup.scm \
	 scheme/stdio.scm \
	 scheme/stringcoll.scm \
	 scheme/syntax-helpers.scm \
	 scheme/syntax.scm \
	 scheme/system.scm \
	 scheme/temp-file.scm \
	 scheme/top.scm \
	 scheme/tty.scm \
	 scheme/user-group.scm \
	 scheme/utilities.scm \
	 scheme/weaktables.scm \
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

go: c/scsh-tramp.c
	$(CC) -o $@ $(CFLAGS) \
	-DVM=\"$(SCHEME48VM)\" \
	-DIMAGE=\"scsh.image\" \
	$(srcdir)/c/scsh-tramp.c

scsh: c/scsh-tramp.c
	$(CC) -o $@ $(CFLAGS) \
	-DVM=\"$(SCHEME48VM)\" \
	-DIMAGE=\"$(LIB)/scsh.image\" \
	$(srcdir)/c/scsh-tramp.c

LOADS = $(srcdir)/rx/interfaces.scm \
	$(srcdir)/rx/packages.scm \
	$(srcdir)/scheme/scsh-interfaces.scm \
	$(srcdir)/scheme/scsh-package.scm \
	$(srcdir)/scheme/lib/ccp-pack.scm \
	$(srcdir)/scheme/lib/char-package.scm

scsh.image: $(SCHEME)
	$(srcdir)/build/build-image.sh $(srcdir) \
		"`pwd`/c/" '$@' '$(SCHEME48) -h 0' '$(LOADS)'

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
	$(INSTALL_PROGRAM) scsh $(DESTDIR)$(bindir)/$(RUNNABLE)
        #install scsh.so
	$(RM) $(DESTDIR)$(LIB)/scsh.so
	$(INSTALL_DATA) c/scsh.so $(DESTDIR)$(LIB)
        #install scheme source files
	for f in $(srcdir)/scheme/*.scm $(srcdir)/scheme/*/*.scm; \
	    do $(INSTALL_DATA) $$f $(DESTDIR)$(SHARE)/; done

install-scsh-image:
	$(srcdir)/build/build-image.sh $(srcdir) \
		"$(LIB)/" '$(DESTDIR)$(LIB)/scsh.image' '$(SCHEME48) -h 0' '$(LOADS)'

clean:
	$(RM) c/*.o c/*.so scsh.image scsh go
