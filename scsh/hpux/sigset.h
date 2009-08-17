/* Convert between a lo24/hi integer-pair bitset and a sigset_t value.
** These macros are OS-dependent, and must be defined per-OS.
*/

#define make_sigset(maskp, hi, lo) ((maskp)->sigset[0]=((hi)<<24)|(lo))

/* Not a procedure: */
#define split_sigset(mask, hip, lop) \
	((*(hip)=((mask).sigset[0]>>24)&0xff), \
	 (*(lop)=((mask).sigset[0]&0xffffff)))
