#include <stdio.h>

/* The current stdio lib that Linux uses has a problem that screws up
** scsh's interrupt system: when a stdio function such as getc() or fputs()
** blocks in an i/o system call, and that system call is interrupted, the
** stdio function realises this and loops, retrying the the i/o operation.
** What we need is for the stdio function to return an error, with 
** errno=EINTR -- i.e., we need for the stdio function to give control back
** to the caller, telling it that the i/o call was interrupted.
**
** The EINTR error return is in fact mandated by Posix. The next release
** of the GNU libc will provide this functionality. The current release
** doesn't. So we use the workaround in this file. Calling 
**	remove_bone_from_head_of_linux_libc()
** will smash the call tables of the i/o library so that non-retrying
** functions get called to do the i/o system calls.
**
** Why does scsh need non-retry? Because in scsh, Unix signals are *not*
** handled by the actual Unix signal handler. The signal handler is just
** a piece of C code that sets a bit, notifying the S48 vm that it needs
** to service a signal. When the vm gets to a vm instruction boundary,
** it suspends execution of the program and services the interrupt by
** invoking a *Scheme* function. In this way, we can interrupt on VM
** instruction boundaries with VM interrupt handlers.
**
** If a C function retries when interrupted, we never return to Scheme,
** and so the vm never has a chance to service the interrupt. This is bad.
**
** This code was contributed by Roland McGrath.
*/

#ifdef __GLIBC__

/* GNU libc 2.0 needs no fixing. */
void remove_bone_from_head_of_linux_libc () {}

#else

#include <libio.h>

extern _IO_ssize_t _IO_file_read (_IO_FILE *, void *, _IO_ssize_t);
_IO_ssize_t
my_linux_file_read (_IO_FILE *fp, void *buf, _IO_ssize_t size)
{
  return read (fp->_fileno, buf, size);
}

extern _IO_ssize_t _IO_file_write (_IO_FILE *, const void *, _IO_ssize_t);
_IO_ssize_t
my_linux_file_write (_IO_FILE *fp, const void *buf, _IO_ssize_t size)
{
  return write (fp->_fileno, buf, size);
}

static void
debone (_IO_ssize_t (**jumptable) ())
{
  _IO_ssize_t (**p) ();
  int r, w;
  r = w = 0;
  for (p = jumptable; !r || !w; ++p)
    {
      if (*p == &_IO_file_read)
	++r, *p = &my_linux_file_read;
      else if (*p == &_IO_file_write)
	++w, *p = &my_linux_file_write;
    }
}

void
remove_bone_from_head_of_linux_libc ()
{
  extern _IO_ssize_t (*_IO_file_jumps[]) (); /* used for normal fds */
  extern _IO_ssize_t (*_IO_proc_jumps[]) (); /* used by popen */

  debone (_IO_file_jumps);
  debone (_IO_proc_jumps);
}
#endif
