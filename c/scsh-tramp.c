/*
** Shell-script trampoline.
** Copyright (c) 1994 by Olin Shivers.
/*

/* Unix #! shell scripts are not recursive. The interpreter you specify
** on the #! line cannot itself be a shell script. This is a problem for
** the Scheme shell, since it is implemented as a heap image executed
** by the Scheme 48 vm. This means that users cannot write shell scripts
** of the form:
**     #!/usr/local/bin/scsh -s
**     !#
**     ...Scheme code goes here...
**
** They must instead write:
**     #!/usr/local/lib/scsh/scshvm \
**     -o /usr/local/lib/scsh/scshvm -i /usr/local/lib/scsh/scsh.image -s
**     ...Scheme code goes here...
**
** This is gruesome and probably confusing to novices.
**
** What we do is have this tiny little stub program play the role of scsh.
** It is compiled to a real Unix binary, but when it is executed, it simply
** execs the scsh virtual machine, passing it an argv composed of
**     { "-o" "scshvm" "-i" "scsh.image"}
** prepended to whatever argv it was given. Now you can write shell scripts
** with
**     #!/usr/local/bin/scsh -s
** triggers.
**
** There are two downsides to doing things this way.
** 1. You pay an extra exec(2) at startup time.
**    And scsh starts up slow enough as it is.
** 2. You cannot specify extra arguments for the vm this way. The most
**    important one you might want to specify is the heap size arg, -h.
*/

#include <errno.h>
#include <unistd.h>
#include <stdlib.h>

#ifndef VM
#define VM "/usr/local/lib/scheme48/scheme48vm"
#endif
#ifndef IMAGE
#define IMAGE "/usr/local/lib/scsh/scsh.image"
#endif

main(int argc, char *argv[])
{
    char **ap, **aq, **newav;

    /* Insert "-i" IMAGE between argv[0] and argv[1]. */

    argc += 3;					/* We're adding 3 new elts. */
    newav = (char **) malloc((argc+1) * sizeof(char*));	/* Alloc new argv. */
    if( !newav ) {
	perror(argv[0]);
	exit(1);
	}

    newav[0] = argv[0];		/* Install new header args. */
    newav[1] = "-I";
    newav[2] = IMAGE;
    newav[3] = argv[0];

    for(ap=&argv[0], aq=&newav[3]; *ap;)	/* Copy over orignal argv */
	*++aq = *++ap;				/*   & the terminating NULL. */

    execv(VM, newav);				/* Do it. */
    perror(argv[0]);
    exit(-1);
}
