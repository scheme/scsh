#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef VEOF
s48_define_exported_binding("VEOF", s48_enter_fixnum(VEOF));
#else
s48_define_exported_binding("VEOF", S48_FALSE);
#endif
#ifdef VEOL
s48_define_exported_binding("VEOL", s48_enter_fixnum(VEOL));
#else
s48_define_exported_binding("VEOL", S48_FALSE);
#endif
#ifdef VERASE
s48_define_exported_binding("VERASE", s48_enter_fixnum(VERASE));
#else
s48_define_exported_binding("VERASE", S48_FALSE);
#endif
#ifdef VKILL
s48_define_exported_binding("VKILL", s48_enter_fixnum(VKILL));
#else
s48_define_exported_binding("VKILL", S48_FALSE);
#endif
#ifdef VINTR
s48_define_exported_binding("VINTR", s48_enter_fixnum(VINTR));
#else
s48_define_exported_binding("VINTR", S48_FALSE);
#endif
#ifdef VQUIT
s48_define_exported_binding("VQUIT", s48_enter_fixnum(VQUIT));
#else
s48_define_exported_binding("VQUIT", S48_FALSE);
#endif
#ifdef VSUSP
s48_define_exported_binding("VSUSP", s48_enter_fixnum(VSUSP));
#else
s48_define_exported_binding("VSUSP", S48_FALSE);
#endif
#ifdef VSTART
s48_define_exported_binding("VSTART", s48_enter_fixnum(VSTART));
#else
s48_define_exported_binding("VSTART", S48_FALSE);
#endif
#ifdef VSTOP
s48_define_exported_binding("VSTOP", s48_enter_fixnum(VSTOP));
#else
s48_define_exported_binding("VSTOP", S48_FALSE);
#endif
#ifdef VMIN
s48_define_exported_binding("VMIN", s48_enter_fixnum(VMIN));
#else
s48_define_exported_binding("VMIN", S48_FALSE);
#endif
#ifdef VTIME
s48_define_exported_binding("VTIME", s48_enter_fixnum(VTIME));
#else
s48_define_exported_binding("VTIME", S48_FALSE);
#endif
#ifdef VWERASE
s48_define_exported_binding("VWERASE", s48_enter_fixnum(VWERASE));
#else
s48_define_exported_binding("VWERASE", S48_FALSE);
#endif
#ifdef VREPRINT
s48_define_exported_binding("VREPRINT", s48_enter_fixnum(VREPRINT));
#else
s48_define_exported_binding("VREPRINT", S48_FALSE);
#endif
#ifdef VLNEXT
s48_define_exported_binding("VLNEXT", s48_enter_fixnum(VLNEXT));
#else
s48_define_exported_binding("VLNEXT", S48_FALSE);
#endif
#ifdef VDISCARD
s48_define_exported_binding("VDISCARD", s48_enter_fixnum(VDISCARD));
#else
s48_define_exported_binding("VDISCARD", S48_FALSE);
#endif
#ifdef VDSUSP
s48_define_exported_binding("VDSUSP", s48_enter_fixnum(VDSUSP));
#else
s48_define_exported_binding("VDSUSP", S48_FALSE);
#endif
#ifdef VEOL2
s48_define_exported_binding("VEOL2", s48_enter_fixnum(VEOL2));
#else
s48_define_exported_binding("VEOL2", S48_FALSE);
#endif
#ifdef VSTATUS
s48_define_exported_binding("VSTATUS", s48_enter_fixnum(VSTATUS));
#else
s48_define_exported_binding("VSTATUS", S48_FALSE);
#endif
#ifdef NCCS
s48_define_exported_binding("NCCS", s48_enter_fixnum(NCCS));
#else
s48_define_exported_binding("NCCS", S48_FALSE);
#endif
}
