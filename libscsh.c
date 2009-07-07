/* This file is part of scsh.
 * Copyright (c) 2002 by Martin Gasbichler and Richard Kelsey. See file COPYING.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "scheme48.h"
#include "libscsh.h"
#include "sysdep.h"

s48_value s48_command_binding;
s48_value s48_to_string_binding;

s48_value s48_command (char* fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  return s48_vcommand (fmt, ap);
}

s48_value s48_vcommand (char* fmt, va_list ap)
{
  char* command;
  s48_value ret;

#ifdef HAVE_VASPRINTF  
  if (vasprintf(&command, fmt, ap) == -1){
    fprintf(stderr, "error in vasprintf\n");
    exit(1);
  }
#else
  command = (char *)calloc (1000, sizeof (char));
  if (vsprintf(command, fmt, ap) == -1){
    fprintf(stderr, "error in vsprintf\n");
    exit(1);
  }
#endif
  fprintf (stderr,"The command is: %s\n", command);
  S48_SHARED_BINDING_CHECK (s48_command_binding);
 
  ret = s48_call_scheme (S48_SHARED_BINDING_REF (s48_command_binding),
			 1,
			 s48_enter_string (command));
 
  free (command);
  va_end (ap);
  return ret;
}


s48_value s48_init_libscsh(void)
{
  s48_command_binding = s48_get_imported_binding ("s48-command");
  return S48_UNSPECIFIC;
}
