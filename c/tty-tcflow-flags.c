#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef TCOON
s48_define_exported_binding("TCOON", s48_enter_fixnum(TCOON));
#else
s48_define_exported_binding("TCOON", S48_FALSE);
#endif
#ifdef TCOOFF
s48_define_exported_binding("TCOOFF", s48_enter_fixnum(TCOOFF));
#else
s48_define_exported_binding("TCOOFF", S48_FALSE);
#endif
#ifdef TCION
s48_define_exported_binding("TCION", s48_enter_fixnum(TCION));
#else
s48_define_exported_binding("TCION", S48_FALSE);
#endif
#ifdef TCIOFF
s48_define_exported_binding("TCIOFF", s48_enter_fixnum(TCIOFF));
#else
s48_define_exported_binding("TCIOFF", S48_FALSE);
#endif
}
