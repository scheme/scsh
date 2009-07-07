/* Copyright (c) 2000 by Martin Gasbichler
   See file COPYING. */

#include "scsh_aux.h"
#include <signal.h>
#include <stdio.h>

int main(int argc, char* argv[]){
  int signr = atoi(argv[1]);

  switch (signr) {
  case SIGINT    : printf("scshint_keyboard"); break;
  case SIGALRM   : printf("scshint_alarm"); break;
  case SIGCHLD   : printf("scshint_chld"); break;
  case SIGCONT   : printf("scshint_cont"); break;
  case SIGHUP    : printf("scshint_hup"); break;
  case SIGQUIT   : printf("scshint_quit"); break;
  case SIGTERM   : printf("scshint_term"); break;
  case SIGTSTP   : printf("scshint_tstp"); break; 
  case SIGUSR1   : printf("scshint_usr1"); break;
  case SIGUSR2   : printf("scshint_usr2"); break;
#ifdef SIGINFO
  case SIGINFO   : printf("scshint_info"); break;
#endif
#ifdef SIGIO
  case SIGIO     : printf("scshint_io"); break;
#endif
#if defined SIGPOLL && ((defined SIGIO && SIGPOLL != SIGIO) || \
                         !defined SIGIO)
     case SIGPOLL   : printf("scshint_poll"); break;
#endif
#ifdef SIGPROF
  case SIGPROF   : printf("scshint_prof"); break; 
#endif
#if defined SIGPWR && ((defined SIGINFO && SIGPWR != SIGINFO) || \
                         !defined SIGINFO)
      case SIGPWR    : printf("scshint_pwr"); break;
#endif
#ifdef SIGURG
  case SIGURG    : printf("scshint_urg"); break; 
#endif
#ifdef SIGVTALRM
  case SIGVTALRM : printf("scshint_vtalrm"); break; 
#endif
#ifdef SIGWINCH
  case SIGWINCH  : printf("scshint_winch"); break; 
#endif
#ifdef SIGXCPU
  case SIGXCPU   : printf("scshint_xcpu"); break;
#endif
#ifdef SIGXFSZ
  case SIGXFSZ   : printf("scshint_xfsz"); break; 
#endif
  default: printf("-1");}

  return 0;
}

 
