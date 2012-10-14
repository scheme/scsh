#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef TCSANOW
s48_define_exported_binding("TCSANOW", s48_enter_fixnum(TCSANOW));
#else
s48_define_exported_binding("TCSANOW", S48_FALSE);
#endif
#ifdef TCSADRIAN
s48_define_exported_binding("TCSADRIAN", s48_enter_fixnum(TCSADRIAN));
#else
s48_define_exported_binding("TCSADRIAN", S48_FALSE);
#endif
#ifdef TCSAFLUSH
s48_define_exported_binding("TCSAFLUSH", s48_enter_fixnum(TCSAFLUSH));
#else
s48_define_exported_binding("TCSAFLUSH", S48_FALSE);
#endif
}
