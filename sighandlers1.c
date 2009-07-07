/* Need to define sig2interrupt vector.
** Interrupt-system mutators should probably hold interrupts while they
**   operate.
*/

#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include "cstuff.h"
#include <errno.h>

/* Make sure our exports match up w/the implementation: */
#include "sighandlers1.h"

/* Import the OS-dependent set of signals and their translations
** to S48 vm interrupts.
*/
#include "signals1.h"

extern int errno;

/* Translate Unix signal numbers to S48 interrupt numbers. */

s48_value sig2interrupt(s48_value _signal)
{
  int signal = s48_extract_fixnum (_signal);
  return s48_enter_fixnum (( signal < 0 || signal > max_sig ) ? -1 : 
			   sig2int[signal]);
}

/* This guy is responsible for making the default action for a
** Unix signal happen. Because S48's signal handler system is
** interposed between delivery-to-the-process and
** delivery-to-the-scheme-handler, when the user sets a signal
** handler to default, we install a Scheme proc that calls this
** guy, instead of just slapping a SIGDFL in as the Unix handler.
** We only have to do this for signals whose default isn't "ignore," i.e.:
**   Posix: SIGALRM SIGHUP SIGINT SIGQUIT SIGTERM SIGUSR1 SIGUSR2
**   Non-Posix: SIGINFO SIGPOLL SIGPROF SIGVTALRM SIGXCPU SIGXFSZ SIGIO
** This way, the S48 signal-blocking mechanism can work.
**
** Weird, I know.
*/
s48_value do_default_sigaction(s48_value _signal)
{
  sigset_t ss, old_ss;
  struct sigaction default_action, old_action;
  int signal = s48_extract_fixnum(_signal);
  /* fprintf(stderr, "Doing default for signal %d\n", signal); */

  sigfillset(&ss);				/* Block everyone. */
  sigprocmask(SIG_SETMASK, &ss, &old_ss);

  default_action.sa_handler = SIG_DFL;		/* Set for default. */
  sigemptyset(&default_action.sa_mask);
  default_action.sa_flags = 0;
  sigaction(signal, &default_action, &old_action);

  raise(signal);			      	/* Raise the signal. */
  sigdelset(&ss, signal);
  sigprocmask(SIG_SETMASK, &ss, 0);		/* Handle it. */

  /* Most likely, we'll never get to here, as the default for
  ** the signals we're handling is "terminate," but we'll play it safe.
  */
  sigaction(signal, &old_action, 0);		/* Restore old handler, */
  sigprocmask(SIG_SETMASK, &old_ss, 0);		/* and mask.            */
  return S48_UNSPECIFIC;
}

s48_value ignore_signal(s48_value _signal)
{
  void (*res)(int) = signal(s48_extract_fixnum(_signal), SIG_IGN);
  if (res == SIG_ERR)
    s48_raise_os_error_1(errno, _signal);
  return S48_UNSPECIFIC;
}

s48_value handle_signal_default(s48_value _signal)
{
  void(*res)(int) = signal(s48_extract_fixnum(_signal), SIG_DFL);
  if (res == SIG_ERR)
    s48_raise_os_error_1(errno, _signal);
  return S48_UNSPECIFIC;
}


s48_value s48_init_sighandlers(void)
{
    S48_EXPORT_FUNCTION(sig2interrupt);
    S48_EXPORT_FUNCTION(do_default_sigaction);
    S48_EXPORT_FUNCTION(ignore_signal);
    S48_EXPORT_FUNCTION(handle_signal_default);

    return S48_UNSPECIFIC;
}

