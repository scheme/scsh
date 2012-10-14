#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef B0
s48_define_exported_binding("B0", s48_enter_fixnum(B0));
#else
s48_define_exported_binding("B0", S48_FALSE);
#endif
#ifdef B50
s48_define_exported_binding("B50", s48_enter_fixnum(B50));
#else
s48_define_exported_binding("B50", S48_FALSE);
#endif
#ifdef B75
s48_define_exported_binding("B75", s48_enter_fixnum(B75));
#else
s48_define_exported_binding("B75", S48_FALSE);
#endif
#ifdef B110
s48_define_exported_binding("B110", s48_enter_fixnum(B110));
#else
s48_define_exported_binding("B110", S48_FALSE);
#endif
#ifdef B134
s48_define_exported_binding("B134", s48_enter_fixnum(B134));
#else
s48_define_exported_binding("B134", S48_FALSE);
#endif
#ifdef B150
s48_define_exported_binding("B150", s48_enter_fixnum(B150));
#else
s48_define_exported_binding("B150", S48_FALSE);
#endif
#ifdef B200
s48_define_exported_binding("B200", s48_enter_fixnum(B200));
#else
s48_define_exported_binding("B200", S48_FALSE);
#endif
#ifdef B300
s48_define_exported_binding("B300", s48_enter_fixnum(B300));
#else
s48_define_exported_binding("B300", S48_FALSE);
#endif
#ifdef B600
s48_define_exported_binding("B600", s48_enter_fixnum(B600));
#else
s48_define_exported_binding("B600", S48_FALSE);
#endif
#ifdef B1200
s48_define_exported_binding("B1200", s48_enter_fixnum(B1200));
#else
s48_define_exported_binding("B1200", S48_FALSE);
#endif
#ifdef B1800
s48_define_exported_binding("B1800", s48_enter_fixnum(B1800));
#else
s48_define_exported_binding("B1800", S48_FALSE);
#endif
#ifdef B2400
s48_define_exported_binding("B2400", s48_enter_fixnum(B2400));
#else
s48_define_exported_binding("B2400", S48_FALSE);
#endif
#ifdef B4800
s48_define_exported_binding("B4800", s48_enter_fixnum(B4800));
#else
s48_define_exported_binding("B4800", S48_FALSE);
#endif
#ifdef B9600
s48_define_exported_binding("B9600", s48_enter_fixnum(B9600));
#else
s48_define_exported_binding("B9600", S48_FALSE);
#endif
#ifdef B19200
s48_define_exported_binding("B19200", s48_enter_fixnum(B19200));
#else
s48_define_exported_binding("B19200", S48_FALSE);
#endif
#ifdef B38400
s48_define_exported_binding("B38400", s48_enter_fixnum(B38400));
#else
s48_define_exported_binding("B38400", S48_FALSE);
#endif
#ifdef EXTA
s48_define_exported_binding("EXTA", s48_enter_fixnum(EXTA));
#else
s48_define_exported_binding("EXTA", S48_FALSE);
#endif
#ifdef EXTB
s48_define_exported_binding("EXTB", s48_enter_fixnum(EXTB));
#else
s48_define_exported_binding("EXTB", S48_FALSE);
#endif
}
