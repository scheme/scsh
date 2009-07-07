/* String equality predicate. */
#define streq(a,b) (!strcmp((a),(b)))

#define Alloc(type) 	((type *) malloc(sizeof(type)))
#define Malloc(type,n)	((type *) malloc(sizeof(type)*(n)))
#define Free(p)		(free((char *)(p)))
#define Realloc(type,p,n) ((type *) realloc(p, (n)*sizeof(type)))


/* These are the interrupt numbers used by the S48/scsh VM.
** The first two are S48 interrupts. The rest were added for
** scsh to support Unix signals. Note that not all Unixes support
** all these signals.
**
** !!! The numbers have to match the enumeration low-interrupt !!!
*/
#define scshint_alarm (0)		/* S48 Unix SIGALRM signal */
#define scshint_keyboard (1)		/* S48 Unix SIGINT signal */
/* left out post-gc and i/o-completion */
#define scshint_chld (4)		/* Interrupts from here down are    */
#define scshint_cont (5)		/* Unix signals. The last ten are   */
#define scshint_hup (6)			/* non-Posix, hence not necessarily */
#define scshint_quit (7)		/* found on all Unixes.             */
#define scshint_term (8)
#define scshint_tstp (9)
#define scshint_usr1 (10)
#define scshint_usr2 (11)
#define scshint_info (12)		/* BSD        */
#define scshint_io (13)			/* BSD + SVR4 */
#define scshint_poll (14)		/*       SVR4 */
#define scshint_prof (15)		/* BSD + SVR4 */
#define scshint_pwr (16)		/*       SVR4 */
#define scshint_urg (17)		/* BSD + SVR4 */
#define scshint_vtalrm (18)		/* BSD + SVR4 */
#define scshint_winch (19)		/* BSD + SVR4 */
#define scshint_xcpu (20)		/* BSD + SVR4 */
#define scshint_xfsz (21)		/* BSD + SVR4 */
