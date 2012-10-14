#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef CSIZE
s48_define_exported_binding("CSIZE", s48_enter_fixnum(CSIZE));
#else
s48_define_exported_binding("CSIZE", S48_FALSE);
#endif
#ifdef CS5
s48_define_exported_binding("CS5", s48_enter_fixnum(CS5));
#else
s48_define_exported_binding("CS5", S48_FALSE);
#endif
#ifdef CS6
s48_define_exported_binding("CS6", s48_enter_fixnum(CS6));
#else
s48_define_exported_binding("CS6", S48_FALSE);
#endif
#ifdef CS7
s48_define_exported_binding("CS7", s48_enter_fixnum(CS7));
#else
s48_define_exported_binding("CS7", S48_FALSE);
#endif
#ifdef CS8
s48_define_exported_binding("CS8", s48_enter_fixnum(CS8));
#else
s48_define_exported_binding("CS8", S48_FALSE);
#endif
#ifdef CSTOPB
s48_define_exported_binding("CSTOPB", s48_enter_fixnum(CSTOPB));
#else
s48_define_exported_binding("CSTOPB", S48_FALSE);
#endif
#ifdef CREAD
s48_define_exported_binding("CREAD", s48_enter_fixnum(CREAD));
#else
s48_define_exported_binding("CREAD", S48_FALSE);
#endif
#ifdef PARENB
s48_define_exported_binding("PARENB", s48_enter_fixnum(PARENB));
#else
s48_define_exported_binding("PARENB", S48_FALSE);
#endif
#ifdef PARODD
s48_define_exported_binding("PARODD", s48_enter_fixnum(PARODD));
#else
s48_define_exported_binding("PARODD", S48_FALSE);
#endif
#ifdef HUPCL
s48_define_exported_binding("HUPCL", s48_enter_fixnum(HUPCL));
#else
s48_define_exported_binding("HUPCL", S48_FALSE);
#endif
#ifdef CLOCAL
s48_define_exported_binding("CLOCAL", s48_enter_fixnum(CLOCAL));
#else
s48_define_exported_binding("CLOCAL", S48_FALSE);
#endif
#ifdef CIGNORE
s48_define_exported_binding("CIGNORE", s48_enter_fixnum(CIGNORE));
#else
s48_define_exported_binding("CIGNORE", S48_FALSE);
#endif
#ifdef CCTS_OFLOW
s48_define_exported_binding("CCTS_OFLOW", s48_enter_fixnum(CCTS_OFLOW));
#else
s48_define_exported_binding("CCTS_OFLOW", S48_FALSE);
#endif
#ifdef CRTS_IFLOW
s48_define_exported_binding("CRTS_IFLOW", s48_enter_fixnum(CRTS_IFLOW));
#else
s48_define_exported_binding("CRTS_IFLOW", S48_FALSE);
#endif
#ifdef MDMBUF
s48_define_exported_binding("MDMBUF", s48_enter_fixnum(MDMBUF));
#else
s48_define_exported_binding("MDMBUF", S48_FALSE);
#endif
}
