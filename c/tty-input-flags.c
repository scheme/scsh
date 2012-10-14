#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef IGNBRK
s48_define_exported_binding("IGNBRK", s48_enter_fixnum(IGNBRK));
#else
s48_define_exported_binding("IGNBRK", S48_FALSE);
#endif
#ifdef BRKINT
s48_define_exported_binding("BRKINT", s48_enter_fixnum(BRKINT));
#else
s48_define_exported_binding("BRKINT", S48_FALSE);
#endif
#ifdef IGNPAR
s48_define_exported_binding("IGNPAR", s48_enter_fixnum(IGNPAR));
#else
s48_define_exported_binding("IGNPAR", S48_FALSE);
#endif
#ifdef PARMRK
s48_define_exported_binding("PARMRK", s48_enter_fixnum(PARMRK));
#else
s48_define_exported_binding("PARMRK", S48_FALSE);
#endif
#ifdef INPCK
s48_define_exported_binding("INPCK", s48_enter_fixnum(INPCK));
#else
s48_define_exported_binding("INPCK", S48_FALSE);
#endif
#ifdef ISTRIP
s48_define_exported_binding("ISTRIP", s48_enter_fixnum(ISTRIP));
#else
s48_define_exported_binding("ISTRIP", S48_FALSE);
#endif
#ifdef INLCR
s48_define_exported_binding("INLCR", s48_enter_fixnum(INLCR));
#else
s48_define_exported_binding("INLCR", S48_FALSE);
#endif
#ifdef IGNCR
s48_define_exported_binding("IGNCR", s48_enter_fixnum(IGNCR));
#else
s48_define_exported_binding("IGNCR", S48_FALSE);
#endif
#ifdef ICRNL
s48_define_exported_binding("ICRNL", s48_enter_fixnum(ICRNL));
#else
s48_define_exported_binding("ICRNL", S48_FALSE);
#endif
#ifdef IXON
s48_define_exported_binding("IXON", s48_enter_fixnum(IXON));
#else
s48_define_exported_binding("IXON", S48_FALSE);
#endif
#ifdef IXOFF
s48_define_exported_binding("IXOFF", s48_enter_fixnum(IXOFF));
#else
s48_define_exported_binding("IXOFF", S48_FALSE);
#endif
#ifdef IXANY
s48_define_exported_binding("IXANY", s48_enter_fixnum(IXANY));
#else
s48_define_exported_binding("IXANY", S48_FALSE);
#endif
#ifdef IMAXBEL
s48_define_exported_binding("IMAXBEL", s48_enter_fixnum(IMAXBEL));
#else
s48_define_exported_binding("IMAXBEL", S48_FALSE);
#endif
#ifdef IUCLC
s48_define_exported_binding("IUCLC", s48_enter_fixnum(IUCLC));
#else
s48_define_exported_binding("IUCLC", S48_FALSE);
#endif
}
