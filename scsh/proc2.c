/* Copyright (c) 1993 by Olin Shivers.
**
** See file COPYING
*/

/* If the above copyright notice is a problem for your app, send me mail. */

/* Using the #! interpreter hack in Unix for making scripts has a big
** problem: you only get 1 argument after the interpreter on the #! line. 
** This subroutine helps to fix that.
**
** Below is a procedure that will arrange for a command-line switch of the 
** form \ <script> to stand for reading more args from line 2 of the file
** <script>. Replace the \ arg with these args. Now you can have Scheme,
** Postscript, Forth, Lisp, Smalltalk, tcl, etc. scripts that look like:
**
** File foo:
**         #!/usr/local/bin/scheme \
**         -heap 4000000 -batch -script
**         !#
**         (define foo ...) ; Scheme code from here on.
**         ...
**
** With this program definition, executing
**     foo arg1 arg2 arg3
** will turn into
**     /usr/local/bin/scheme \ foo arg1 arg2 arg3
** which your Scheme interpreter main() (using this routine) will expand during
** argv processing into:
**     /usr/local/bin/scheme -heap 4000000 -batch -script foo arg1 arg2 arg3
** That is, the argument processing in main() will *replace* the \ argument
** with the arguments read in from line 2 of foo. So we have dodged the
** only-one-argument-on-the-#!-line constraint.
**
** The only other thing that needs to be done in this case is arrange for the
** interpreter to ignore these initial few non-Scheme lines. We can arrange
** for this in our Scheme example by defining a Scheme read macro #! that
** skips characters until newline, bang, splat (somewhat like the ; read macro
** skips characters until newline).
**
** Using backslash as the meta-argument switch is handy for two reasons:
** - It is only one character. Since many Unix systems limit the #!
**   line to 32 characters total, this is important.
** - It is a helpful visual pun -- implying a continuation line for the
**   arguments.
** It is also very unlikely to be an already-used switch. However, -2
** is also a reasonable choice.
**
** All you have to do to get this second-line meta-argument functionality is
** link this file in with your interpreter.  You can tweak this routine for
** various interpreters if you need to have it, for example, skip an initial
** comment character when it begins to scan the second line.
**
** Arguments are parsed from the second line as follows:
** - The only special chars are space, tab, newline, and \.
** - Every space char terminates an argument. 
**   Multiple spaces therefore introduce empty-string arguments.
** - A newline terminates an argument, and also terminates the argument list.
**   This means that an empty line parses to the singleton list whose one
**   element is the empty string: (""). The grammar doesn't admit the empty 
**   list.
** - Tab is not allowed.
**   This is to prevent you from being screwed by thinking you had several
**   spaces where you really had a tab, and vice-versa.
** - The only other special character is \, the knock-down character. 
**   \ escapes \, space, tab, and newline, turning off their special 
**   functions. The ANSI C escape sequences (\b, \n, \r and \t) are 
**   supported; these also produce argument-constituents -- \n doesn't act 
**   like a terminating newline. \nnn for *exactly* three octal digits reads 
**   as the char whose ASCII code is nnn. It is an error if \ is followed by 
**   just 1 or 2 octal digits: \3Q is an error. Octal-escapes are always 
**   constituent chars. \ followed by other chars is not allowed (so we can
**   extend the escape-code space later if we like).
**
** You have to construct these line-2 arg lines carefully. For example,
** beware of trailing spaces at the end of the line. They'll give you
** extra trailing empty-string args.
**
** You should also beware of including nul bytes into your arguments, since
** C's pathetic excuse for a string data-type will lose if you try this.
**
**
** Another way to get this sort of multiple-argument functionality, with
** the extra cost of starting up a shell, is to simply have the following
** trampoline at the beginning of your script:
**     #!/bin/sh -
**     exec /usr/local/bin/scheme -heap 4000000 -batch -script $0 $*
**     !#
** (or use the indir program, same rough idea). This is less appropriate
** for interpreters intended to replace the shell.
**
** Possible extensions:
** - I considered making the argument line syntax hairier -- adding ~user
**   directory expansion and $(envvar) expansion. But I didn't do it.
**
** - Not much error information. If something is wrong -- file can't
**   be read, no second line, illegal syntax on second line, malloc
**   loses -- you just get a NULL return value. You can examine errno
**   if the problem is a Unix error (e.g., file error). But if the call
**   fails for another reason (e.g., bad arg syntax on the second line),
**   then errno won't help. This code could be modified to take an additional
**   &error_code argument, and assign an integer into the var indicating
**   just exactly what the problem was, if that's important to your 
**   application. 
**
** This code is fairly robust, careful code. ANSI standard C. No dependencies 
** on fixed-size buffers. It won't blow up if the inputs are pathological.
** It all type-checks. No core leaks. Feel free to customise it for the
** particular needs of a given interpreter; the core functionality is there.
**
** See the end of this file for a sample program with an arg processing loop.
** Please send me bug reports, fixes, and improvements.
**
** Some interpreters that might use this: tcl (wish, hope), perl, Smalltalk,
** little Schemes (scm, elk, s48, ...), big Schemes, Postscript, emacs, 
** Dylan, Lisp, Prolog.
**     -Olin Shivers 2/93
**     shivers@cs.cmu.edu
**     shivers@csd.hku.hk
*/

#include <stdio.h>
#include <stdlib.h> /* malloc */
#include <ctype.h>

#define Alloc(type) 	((type *) malloc(sizeof(type)))
#define Malloc(type,n)	((type *) malloc(sizeof(type)*(n)))
#define Realloc(type,ptr,size) \
	((type *) realloc((void *)ptr, sizeof(type)*(size)))
#define Free(p)		(free((void *)(p)))

/* Is character c an octal digit? */
#define isodigit(c) (isdigit(c) && (c) != '8' && (c) != '9')

/* Double the vector if we've overflowed it. Return the vector.
** If we double the vector, lenptr is updated with the new length.
** If we fail, return NULL.
*/

static void *maybe_grow_vec(void *vec, int *lenptr, int index, int elt_size)
{
    int len = *lenptr;
    if( index < len ) return vec;
    len *= 2;
    *lenptr = len; /* Update the length pointer. */
    return realloc(vec, len*elt_size);
    }

/* The do ... while(0) is a trick to make this macro accept a terminating
** semicolon.
*/
#define Maybe_Grow_Vec(vec, size, index, elt_t, lose) \
    do {elt_t *mgv_tmp =(elt_t*)maybe_grow_vec((void*)vec, &size, \
					       index, sizeof(elt_t)); \
	if(mgv_tmp) vec = mgv_tmp; else goto lose;} while (0);


/* process_meta_arg(fname, av)
** -----------------------
** The main routine.
**
** Expand a \ <fname> switch. Return NULL on error, otherwise a new arg
** vector composed of (1) the args scanned in from line 2 of fname, followed
** by (2) the arguments in av. The argument vector av starts with the
** argument following the \ switch, i.e., the <fname> argument.
*/

static char* read_arg(FILE*);

char **process_meta_arg(char **av)
{
    char **argv, *arg, **ap;
    int c;
    FILE *script;
    char *fname;
    int av_len;
    int argv_i=0, argv_len=100;

    if( !*av ) return NULL;
    fname = *av;
    script = fopen(fname, "rb");
    if( !script ) return NULL;

    /* Skip line 1. */
    while( '\n' != getc(script) )
	if( feof(script) || ferror(script) ) goto lose3;

    argv = Malloc(char*, argv_len);
    if( !argv ) goto lose3;

    do {
	char *arg;
	arg = read_arg(script);
	if( !arg ) goto lose2;
	Maybe_Grow_Vec(argv, argv_len, argv_i, char*, lose1);
	argv[argv_i++] = arg;
	}
    while( EOF != (c=getc(script)) && '\n' != c );

    for(av_len=0; av[av_len]; av_len++);	/* Compute length of av. */

    /* Precisely re-size argv. */
    if( NULL == (ap=Realloc(char*, argv, argv_len + av_len + 1)) ) goto lose2;
    argv = ap;

    while( argv[argv_i++] = *av++ );	/* Copy over av & null terminate. */

    fclose(script);
    return argv;


    /* Exception handlers: free storage and lose. */
  lose1:
    Free(arg);
  lose2:
    while( argv_i ) Free(argv[--argv_i]);
    Free(argv);
 lose3:
    fclose(script);
    return NULL;
    }

/* Read in one arg, but not its terminating space or newline.
** Return a newly-allocated string containing the arg; 
** NULL if there's an error.
*/
static char *read_arg(FILE *f)
{
    char *buf, *tmp;
    int buflen, i;

    /* Allocate a buffer for the arg. */
    i = 0;
    buflen=20;
    if( !(buf = Malloc(char, buflen)) ) return NULL;

    /* Read in the arg. */
    while(1) {
	int c = getc(f);

	if( c == EOF || c == ' ' || c == '\n' ) {ungetc(c, f); break;}

	/* Do knock-down processing. */
	if( c == '\\' ) {
	    int c1, c2, c3;
	    switch (c1=getc(f)) {
	      case EOF:
		goto lose;

		/* \nnn octal escape. */
	      case '0':		case '1':
	      case '2':		case '3':
	      case '4':		case '5':
	      case '6':		case '7':
		if( EOF == (c2=getc(f)) || !isodigit(c2) ) goto lose;
		if( EOF == (c3=getc(f)) || !isodigit(c3) ) goto lose;
		c = ((c1-'0')<<6) | ((c2-'0')<<3) | (c3-'0');
		break;

		/* ANSI C escapes. */
	      case 'n':	c='\n'; break;
	      case 'r':	c='\r'; break;
	      case 't':	c='\t'; break;
	      case 'b':	c='\b'; break;

		/* Simple knock-down: \, space, tab, newline. */
	      case '\\':	case ' ':
	      case '\t':	case '\n':
		c=c1; break;

		/* Nothing else allowed. */
	      default: goto lose;
		}
	    }

	/* No tab allowed. */
	else if( c == '\t' ) goto lose;

	Maybe_Grow_Vec(buf, buflen, i, char, lose);
	buf[i++] = c;
	}

    /* Null terminate the arg. */
    Maybe_Grow_Vec(buf, buflen, i, char, lose);
    buf[i++] = '\0';

    /* Precisely re-size buf and return. */
    if( tmp=Realloc(char,buf,i) ) return tmp;

  lose:
    Free(buf);
    return NULL;
    }


/*****************************************************************************/
#if 0
/*
** Debugging test stub and example argument scanner. 
** Like echo, but with \ <fname> expansion.
**/

char *prog_name;

static void usage(void)
{
    fprintf(stderr,
	    "Usage: %s [\\ <fname>] [-n] [--] arg1 ... argn\n",
	    prog_name);
    exit(1);
    }

/* Expand away a leading meta-arg if there is one. Die informatively on error.
** I can't think of a reason why you might want to have recursive meta
** arguments, but we handle this case to be complete.
*/
static char **maybe_expand_meta_arg(char **argv)
{
    if( *argv )
	while( strcmp(*argv, "\\") == 0 ) {
	    argv++;
	    if( !*argv ) {
		fprintf(stderr, "%s: \\ switch without following filename.\n",
			prog_name);
		usage();
		}
	    argv = process_meta_arg(argv);
	    if( !argv ) {
		fprintf(stderr, "%s: unable to expand \\ <filename> switch.\n",
			prog_name);
		usage();
		}
	    }
    return argv;
    }

main(int argc, char **argv)
{
    int n_flag=0;

    prog_name = *argv++;

    /* Handle an initial meta-arg expansion. */
    argv = maybe_expand_meta_arg(argv);

    /* Process switches. */
    for(;*argv;argv++) {
	/* Process arg. */
	if( argv[0][0] == '-' )
	    switch( argv[0][1] ) {
	      /* -n means no terminating newline. */
	      case 'n':
		n_flag++;
		break;

	      /* -- terminates args, so you can echo \, -n, -- args. */
	      case '-':
		argv++;
		goto args_done;
		break;

	      default:
		fprintf(stderr, "%s: unknown flag %s.\n", prog_name, *argv);
		usage();
	      }
	else goto args_done; /* Not a switch. We are done. */
	}

  args_done:
    if( *argv ) printf("\"%s\"", *argv++);
    while( *argv ) printf(" \"%s\"", *argv++);
    if( !n_flag ) putchar('\n');
    }
#endif /* 0 */
