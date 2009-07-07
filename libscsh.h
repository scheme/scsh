/* This file is part of scsh.
 * Copyright (c) 2002 by Martin Gasbichler. See file COPYING.
 * Interface to libscsh
 */

#ifndef LIBSCSH_H
#define LIBSCSH_H
#include <stdarg.h>


s48_value s48_command (char* format, ...);
s48_value s48_vcommand (char* format, va_list ap);
s48_value s48_init_libscsh(void);

#endif
