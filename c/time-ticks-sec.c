#include <scheme48.h>
#include <time.h>

void s48_on_load(void) {
#ifdef CLOCKS_PER_SEC
s48_define_exported_binding("CLOCKS_PER_SEC", s48_enter_fixnum(CLOCKS_PER_SEC));
#else
s48_define_exported_binding("CLOCKS_PER_SEC", S48_FALSE);
#endif
}
