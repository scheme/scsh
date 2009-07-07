/* This file contains the code for doing the scsh file ports stuff.
** Copyright (c) 1993, 1994 by Olin Shivers.
**
** Note that this code mutates Scheme records -- it has the layout
** of fdports and their data records wired in. This is somewhat fragile.
*/

/* A note on the clearerr() calls herein: SunOS stdio input routines, 
** contrary to POSIX, will return EOF if the stream's EOF flag is set,
** without trying to read the stream. This is a lose for tty's, which
** can frequently still be read from after the first EOF (e.g., if you
** type a ^D to bag out of a breakpoint, you would like the terminal
** input port to not shut down forever.)
**
** To fix this lossage, we are careful to call clearerr() before every
** input stream op.
*/

#include "sysdep.h"
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include "cstuff.h"
#define NUM_FDPORTS 256
#include "fdports.h"

/* Make sure our exports match up w/the implementation: */
#include "fdports1.h"

extern int errno;

static char const *fdes_modestr(int fd)
{
    int flags = fcntl(fd,F_GETFL);

    if( flags == -1 ) return NULL;
    flags &= O_ACCMODE;

    if( flags == O_WRONLY ) return "w";
    else if( flags == O_RDONLY ) return "r";
    else if( flags == O_RDWR ) return "r+";

    fputs("That's impossible.\n", stderr);
    abort();
    _exit(-1);
    /*NOTREACHED*/
    }

/* The two following routines are for delimited read. */

s48_value read_delim(const char *delims, char *buf,
			int fd, int start, int end,
			int *nread)
{
    char *cptr   = buf+start, /* Location of last char deposited. */
         *bufend = buf+end -1;   /* Last writeable position. */
    
    int retval;

    char c;

    while( 1 ) {

        retval = read( fd, &c, 1 );

	if( retval == 0 ) {		/* Terminal case: EOF. */
	    *nread =  cptr - buf - start;
	    return S48_EOF;
	    }

	else if( retval == -1 ) {       /* Terminal case: error. */
	    *nread =  cptr - buf - start;
	    return s48_enter_integer(errno);
	    }	  

	else if( delims[c] ) {		/* Terminal case: delimiter char. */
	    *nread =  cptr - buf - start;
	    return s48_enter_char(c);
	    }

	else if( cptr == bufend ) {	/* Terminal case: buffer overflow. */
	  *cptr = c;
	  *nread = end-start;
	  return S48_FALSE;
	}
	else if ( cptr > bufend ){
	  fputs("cptr > bufend.\n", stderr);
	  abort();
	  _exit(-1);
	  }
	else { 
	  *cptr++ = c;
	}
    }
}


s48_value skip_chars(const char *skipchars, int fd,  int *nread)
{

    int nr = 0; /* Number of skip chars read. */

    char c;

    while( 1 ) {
	
        int retval = read ( fd, &c, 1 );

	if( retval == 0 ) {		/* Terminal case: EOF. */
	    *nread = nr;
	    return S48_FALSE;
	    }

	if( retval == -1 ) {		/* Terminal case: error. */
	    *nread = nr;
	    return s48_enter_integer(errno);
	    }

	else if( !skipchars[c] ) {	/* Terminal case: non-skip char. */
	    *nread = nr;
	    return s48_enter_char(c);
	    }
	nr++;
	}
    }
