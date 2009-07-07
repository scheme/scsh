/* OS-dependent support for what is supposed to be the standard ANSI C Library.
** Copyright (c) 1999 by Brian D. Carlstrom.
*/
struct netent     *getnetbyaddr (long x, int y) {return 0;}
struct netent     *getnetbyname (const char *x) {return 0;}
