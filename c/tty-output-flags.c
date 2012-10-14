#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef OPOST
s48_define_exported_binding("OPOST", s48_enter_fixnum(OPOST));
#else
s48_define_exported_binding("OPOST", S48_FALSE);
#endif
#ifdef OPOST
s48_define_exported_binding("OPOST", s48_enter_fixnum(OPOST));
#else
s48_define_exported_binding("OPOST", S48_FALSE);
#endif
#ifdef ONOEOT
s48_define_exported_binding("ONOEOT", s48_enter_fixnum(ONOEOT));
#else
s48_define_exported_binding("ONOEOT", S48_FALSE);
#endif
#ifdef OXTABS
s48_define_exported_binding("OXTABS", s48_enter_fixnum(OXTABS));
#else
s48_define_exported_binding("OXTABS", S48_FALSE);
#endif
#ifdef OCRNL
s48_define_exported_binding("OCRNL", s48_enter_fixnum(OCRNL));
#else
s48_define_exported_binding("OCRNL", S48_FALSE);
#endif
#ifdef OFDEL
s48_define_exported_binding("OFDEL", s48_enter_fixnum(OFDEL));
#else
s48_define_exported_binding("OFDEL", S48_FALSE);
#endif
#ifdef OFILL
s48_define_exported_binding("OFILL", s48_enter_fixnum(OFILL));
#else
s48_define_exported_binding("OFILL", S48_FALSE);
#endif
#ifdef OLCUC
s48_define_exported_binding("OLCUC", s48_enter_fixnum(OLCUC));
#else
s48_define_exported_binding("OLCUC", S48_FALSE);
#endif
#ifdef ONLRET
s48_define_exported_binding("ONLRET", s48_enter_fixnum(ONLRET));
#else
s48_define_exported_binding("ONLRET", S48_FALSE);
#endif
#ifdef ONOCR
s48_define_exported_binding("ONOCR", s48_enter_fixnum(ONOCR));
#else
s48_define_exported_binding("ONOCR", S48_FALSE);
#endif
#ifdef NLDLY
s48_define_exported_binding("NLDLY", s48_enter_fixnum(NLDLY));
#else
s48_define_exported_binding("NLDLY", S48_FALSE);
#endif
#ifdef NL0
s48_define_exported_binding("NL0", s48_enter_fixnum(NL0));
#else
s48_define_exported_binding("NL0", S48_FALSE);
#endif
#ifdef NL1
s48_define_exported_binding("NL1", s48_enter_fixnum(NL1));
#else
s48_define_exported_binding("NL1", S48_FALSE);
#endif
#ifdef TABDLY
s48_define_exported_binding("TABDLY", s48_enter_fixnum(TABDLY));
#else
s48_define_exported_binding("TABDLY", S48_FALSE);
#endif
#ifdef TAB0
s48_define_exported_binding("TAB0", s48_enter_fixnum(TAB0));
#else
s48_define_exported_binding("TAB0", S48_FALSE);
#endif
#ifdef TAB1
s48_define_exported_binding("TAB1", s48_enter_fixnum(TAB1));
#else
s48_define_exported_binding("TAB1", S48_FALSE);
#endif
#ifdef TAB2
s48_define_exported_binding("TAB2", s48_enter_fixnum(TAB2));
#else
s48_define_exported_binding("TAB2", S48_FALSE);
#endif
#ifdef TAB3
s48_define_exported_binding("TAB3", s48_enter_fixnum(TAB3));
#else
s48_define_exported_binding("TAB3", S48_FALSE);
#endif
#ifdef CRDLY
s48_define_exported_binding("CRDLY", s48_enter_fixnum(CRDLY));
#else
s48_define_exported_binding("CRDLY", S48_FALSE);
#endif
#ifdef CR0
s48_define_exported_binding("CR0", s48_enter_fixnum(CR0));
#else
s48_define_exported_binding("CR0", S48_FALSE);
#endif
#ifdef CR1
s48_define_exported_binding("CR1", s48_enter_fixnum(CR1));
#else
s48_define_exported_binding("CR1", S48_FALSE);
#endif
#ifdef CR2
s48_define_exported_binding("CR2", s48_enter_fixnum(CR2));
#else
s48_define_exported_binding("CR2", S48_FALSE);
#endif
#ifdef CR3
s48_define_exported_binding("CR3", s48_enter_fixnum(CR3));
#else
s48_define_exported_binding("CR3", S48_FALSE);
#endif
#ifdef VTDLY
s48_define_exported_binding("VTDLY", s48_enter_fixnum(VTDLY));
#else
s48_define_exported_binding("VTDLY", S48_FALSE);
#endif
#ifdef VT0
s48_define_exported_binding("VT0", s48_enter_fixnum(VT0));
#else
s48_define_exported_binding("VT0", S48_FALSE);
#endif
#ifdef VT1
s48_define_exported_binding("VT1", s48_enter_fixnum(VT1));
#else
s48_define_exported_binding("VT1", S48_FALSE);
#endif
#ifdef BSDLY
s48_define_exported_binding("BSDLY", s48_enter_fixnum(BSDLY));
#else
s48_define_exported_binding("BSDLY", S48_FALSE);
#endif
#ifdef BS0
s48_define_exported_binding("BS0", s48_enter_fixnum(BS0));
#else
s48_define_exported_binding("BS0", S48_FALSE);
#endif
#ifdef BS1
s48_define_exported_binding("BS1", s48_enter_fixnum(BS1));
#else
s48_define_exported_binding("BS1", S48_FALSE);
#endif
#ifdef FFDLY
s48_define_exported_binding("FFDLY", s48_enter_fixnum(FFDLY));
#else
s48_define_exported_binding("FFDLY", S48_FALSE);
#endif
#ifdef FF0
s48_define_exported_binding("FF0", s48_enter_fixnum(FF0));
#else
s48_define_exported_binding("FF0", S48_FALSE);
#endif
#ifdef FF1
s48_define_exported_binding("FF1", s48_enter_fixnum(FF1));
#else
s48_define_exported_binding("FF1", S48_FALSE);
#endif
}
