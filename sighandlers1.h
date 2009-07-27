/* Exports from sighandlers1.c */

s48_ref_t sig2interrupt(s48_call_t call, s48_ref_t signal);

s48_ref_t do_default_sigaction(s48_call_t call, s48_ref_t signal);

void s48_on_load(void);
