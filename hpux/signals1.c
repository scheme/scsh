/* Need to turn off synchronous error signals (SIGPIPE, SIGSYS). */

#include "../scsh_aux.h"

/* Make sure our exports match up w/the implementation: */
#include "../signals1.h"

/* This table converts Unix signal numbers to S48/scsh interrupt numbers.
** If the signal doesn't have an interrupt number, the entry is -1.
** (Only asynchronous signals have interrupt numbers.)
**
** Note that we bake into this table the integer values of the signals --
** i.e., we assume that SIGHUP=1, SIGALRM=15, etc. So this definition is
** very system-dependent.
*/
const int sig2int[] = {
	-1,		/* 0 is not a signal */
	scshint_hup,	/* 1: SIGHUP */
	scshint_keyboard,	/* 2: SIGINT */
	scshint_quit,	/* 3: SIGQUIT */
	-1,		/* 4: SIGILL */
	-1,		/* 5: SIGTRAP */
	-1,		/* 6: SIGABRT */
	-1,		/* 7: SIGEMT */
	-1,		/* 8: SIGFPE */
	-1,		/* 9: SIGKILL */
	-1,		/* 10: SIGBUS */
	-1,		/* 11: SIGSEGV */
	-1,		/* 12: SIGSYS */
	-1,		/* 13: SIGPIPE */
	scshint_alarm,	/* 14: SIGALRM */
	scshint_term,	/* 15: SIGTERM */
	scshint_usr1,	/* 16: SIGUSR1 */
	scshint_usr2,	/* 17: SIGUSR2 */
	scshint_chld,	/* 18: SIGCHLD */
	scshint_pwr,	/* 19: SIGPWR */
	scshint_vtalrm,	/* 20: SIGVTALRM */
	scshint_prof,	/* 21: SIGPROF */
	scshint_io,	/* 22: SIGIO */
	scshint_winch,	/* 23: SIGWINCH */
	-1,		/* 24: SIGSTOP */
	scshint_tstp,	/* 25: SIGTSTP */
	scshint_cont,	/* 26: SIGCONT */
	-1,		/* 27: SIGTTIN */	/* scshint_ttyin */
	-1,		/* 28: SIGTTOU */	/* scshint_ttyou */
	scshint_urg,	/* 29: SIGURG */
	-1,		/* 30: SIGLOST */
	-1,		/* 32: SIGDIL */
	scshint_xcpu,	/* 33: SIGXCPU */
	scshint_xfsz	/* 34: SIGXFSZ */
	};

const int max_sig = 34; /* SIGXFSZ */

/*
scshint_alarm
scshint_keyboard
scshint_memory_shortage
scshint_chld
scshint_cont
scshint_hup
scshint_quit
scshint_term
scshint_tstp
scshint_usr1
scshint_usr2
scshint_info
scshint_io
scshint_poll
scshint_prof
scshint_pwr
scshint_urg
scshint_vtalrm
scshint_winch
scshint_xcpu
scshint_xfsz

scshint_alarm
scshint_chld
scshint_cont
scshint_hup
scshint_info
scshint_io
scshint_keyboard
scshint_memory_shortage
scshint_poll
scshint_prof
scshint_pwr
scshint_quit
scshint_term
scshint_tstp
scshint_urg
scshint_usr1
scshint_usr2
scshint_vtalrm
scshint_winch
scshint_xcpu
scshint_xfsz
*/
