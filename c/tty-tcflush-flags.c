#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef TCIFLUSH
s48_define_exported_binding("TCIFLUSH", s48_enter_fixnum(TCIFLUSH));
#else
s48_define_exported_binding("TCIFLUSH", S48_FALSE);
#endif
#ifdef TCOFLUSH
s48_define_exported_binding("TCOFLUSH", s48_enter_fixnum(TCOFLUSH));
#else
s48_define_exported_binding("TCOFLUSH", S48_FALSE);
#endif
#ifdef TCIOFLUSH
s48_define_exported_binding("TCIOFLUSH", s48_enter_fixnum(TCIOFLUSH));
#else
s48_define_exported_binding("TCIOFLUSH", S48_FALSE);
#endif
}
