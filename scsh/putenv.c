/* My very own implementation of putenv() and friends,
** because NeXTSTEP is a lame UNIX.
**
** Copyright (c) 1994 by Olin Shivers.
** You may use this software for any purpose provided I am not held
** accountable for its effects and you leave this copyright notice
** intact.
*/

/* This file exports three names:
**     extern int putenv(const char *str);
**     extern int unsetenv(const char *name);
**     extern int setenv(const char *name, const char *val);
*/

/* This code leaks memory. It is unavoidable. */

#include <string.h>

/* Don't want to include stdlib.h because it declares putenv to take
** a const string -- bogus. So we'll just declare malloc directly.
*/
extern void *malloc(size_t size);

#define Malloc(type,n)	((type *) malloc(sizeof(type)*(n)))

extern char **environ;
/*****************************************************************************/

/* Internal utility.
** Copy the entire env to a new block, and add the new definition.
** Drop the old block on the floor; can't free() it.
** Return 0 if win
**        non-zero if the malloc fails.
*/
static int append_envvar(char *str, int old_envsize)
{
    char **envp, **nenvp;
    char **newenv = Malloc(char*, 1+old_envsize);
    if( !newenv ) return 1;

    for( envp=environ, nenvp=newenv;   *envp;   envp++, nenvp++ )
	*nenvp = *envp;
    *nenvp++ = str;
    *nenvp = 0;
    environ = newenv;
    return 0;
    }


/* int putenv(char *str)
***************************
** Change or add a value to the environment.
**
** str is an env string of the form "<var>=<val>".
** The environ vector is scanned for a matching <var> binding.
** If one is found, str is installed in that slot in the vector.
** Otherwise, the environ vector is copied into a new vector of one greater
** size, and str is added in the new slot. Note: in either case, str
** becomes part of the environment structure (until is later replaced by
** another putenv() call), so altering str changes the environment.
**
** Malloc is used to allocate new environ vectors. 
** In neither replacement strategy are we able to free() the unused
** storage; it is simply dropped on the floor.
** Putenv returns
**   0 if it wins;
**   non-zero if str doesn't contain an '=' char or if the malloc fails.
*/

int putenv(char *str)
{
    char **envp;
    char *equalsign = strchr(str, '=');
    int namelen;

    if( ! equalsign ) return 0; /* No equals sign in str! */
    namelen = 1 + equalsign - str; /* Count the terminating =. */

    for(envp = environ; *envp; envp++)
	if( ! memcmp(*envp, str, namelen) ) {
	    *envp = str;
	    return 0;
	    }

    /* The env var wasn't defined. Copy the entire env to a new
    ** block, and add the new definition.
    */
    return append_envvar(str, envp-environ+1);
    }


/* int unsetenv(const char *name)
***********************************   
** Delete an environment var from environ.
** name is an environment variable <var>. All strings in environ
** beginning with "<var>=" are deleted from environ. unsetenv
** returns the number of occurrences it found and deleted; if
** it returns 0, then the variable wasn't in environ to begin with.
** If name is the null pointer, unsetenv returns -1 immediately.
*/

int unsetenv(const char *name)
{
    char **envp, **target;
    int hits;
    int slen;

    if( !name ) return -1;
    slen = strlen(name);
    hits = 0;
    target = environ;

    for( envp=environ; *envp; envp++ )
	if( !strncmp(*envp, name, slen) && (*envp)[slen] == '=' )
	    hits++;
	else
	    *target++ = *envp;
    *target = 0;

    return hits;
    }


/* int setenv(const char *name, const char *val)
************************************************
** Sets an existing env var or adds a new one.
** - If val  is the null pointer, the env var is deleted.
** - If name is the null pointer, setenv() returns an error and does nothing.
**
** If env var <name> is already defined in environ, then
** the new value is copied over the var's old value if
** there is space. Otherwise a fresh string is allocated
** with malloc. If the var is not defined, then environ
** is copied to a fresh block of storage, of size one greater,
** and the new "<name>=<val>" binding installed in that block.
**
** Returns 0 if it wins.
** Returns non-zero if there is an error.
*/

int setenv(const char *name, const char *val)
{
    char **envp;
    char *s;
    int val_len, name_len;

    if( !name ) return 1;
    if( !val ) {
	unsetenv(name);
	return 0;
	}

    name_len = strlen(name);
    val_len  = strlen(val);
    
    for( envp=environ; *envp; envp++ )
	if( !strncmp(*envp, name, name_len) && (*envp)[name_len] == '=' ) {
	    char *equal = name_len + *envp;

	    if( strlen(equal+1) >= val_len )
		memcpy(equal+1, val, val_len+1); /* Copy in place. */
	    else {
		char *s = Malloc(char, name_len + val_len + 2);
		if( !s ) return 1;
		memcpy(s, name, name_len);
		s[name_len] = '=';
		memcpy(s+name_len+1, val, val_len+1);
		*envp=s;
		}
	    return 0;
	    }

    /* Not found. Add. */
    s = Malloc(char, val_len + name_len + 2);
    if( !s ) return 1;
    memcpy(s, name, name_len);
    s[name_len] = '=';
    memcpy(s+name_len+1, val, val_len+1);
    return append_envvar(s, 1+envp-environ);
    }
