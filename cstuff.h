#include "scheme48.h"
#define Alloc(type)     ((type *) malloc(sizeof(type)))
#define Malloc(type,n)  ((type *) malloc(sizeof(type)*(n)))
#define Free(p)         (free((char *)(p)))
#define Realloc(type,p,n) ((type *) realloc(p, (n)*sizeof(type)))

/*
 * Macros for retrying interrupted system calls.
 */

#define RETRY_NULL(STATUS, CALL)                                \
do {                                                            \
  STATUS = (CALL);                                              \
} while ((STATUS == NULL) && (errno == EINTR))

#define RETRY_NEG(STATUS, CALL)                                 \
do {                                                            \
  STATUS = (CALL);                                              \
} while ((STATUS < 0) && (errno == EINTR))

/* These assume that there's a call type name call around.
 */
#define RETRY_OR_RAISE_NULL(STATUS, CALL, WHO)                  \
do {                                                            \
  while (1) {                                                   \
    STATUS = (CALL);                                            \
    if (STATUS != NULL)                                         \
      break;                                                    \
    else if (errno != EINTR)                                    \
      s48_os_error_2(call, WHO, errno, 0); }                    \
 } while (0)

#define RETRY_OR_RAISE_NEG(STATUS, CALL, WHO)                   \
do {                                                            \
  while (1) {                                                   \
    STATUS = (CALL);                                            \
    if (STATUS >= 0)                                            \
      break;                                                    \
    else if (errno != EINTR)                                    \
      s48_os_error_2(call, WHO, errno, 0); }                    \
 } while (0)


/* String equality predicate. */
#define streq(a,b) (!strcmp((a),(b)))

s48_ref_t char_pp_2_string_list(s48_call_t call, char **);
