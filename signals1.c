/* Need to turn off synchronous error signals (SIGPIPE, SIGSYS). */
#include "scsh_aux.h"
#include "temp_constants.h"
/* Make sure our exports match up w/the implementation: */
#include "signals1.h"

/* This table converts Unix signal numbers to S48/scsh interrupt numbers.
** If the signal doesn't have an interrupt number, the entry is -1.
** (Only asynchronous signals have interrupt numbers.)
**
** Note that we bake into this table the integer values of the signals --
** i.e., we assume that SIGHUP=1, SIGALRM=15, etc. So this definition is
** very system-dependent.
*/
const int sig2int[] = {
  -1,      /* 0 is not a signal */
  SIGNR_1,
  SIGNR_2,
  SIGNR_3,
  SIGNR_4,
  SIGNR_5,
  SIGNR_6,
  SIGNR_7,
  SIGNR_8,
  SIGNR_9,
  SIGNR_10,
  SIGNR_11,
  SIGNR_12,
  SIGNR_13,
  SIGNR_14,
  SIGNR_15,
  SIGNR_16,
  SIGNR_17,
  SIGNR_18,
  SIGNR_19,
  SIGNR_20,
  SIGNR_21,
  SIGNR_22,
  SIGNR_23,
  SIGNR_24,
  SIGNR_25,
  SIGNR_26,
  SIGNR_27,
  SIGNR_28,
  SIGNR_29,
  SIGNR_30,
  SIGNR_31
};

const int max_sig = 31; /* SIGUNUSED */
