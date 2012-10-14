#include <scheme48.h>
#include <termios.h>

void s48_on_load(void) {
#ifdef ECHOE
s48_define_exported_binding("ECHOE", s48_enter_fixnum(ECHOE));
#else
s48_define_exported_binding("ECHOE", S48_FALSE);
#endif
#ifdef ECHOK
s48_define_exported_binding("ECHOK", s48_enter_fixnum(ECHOK));
#else
s48_define_exported_binding("ECHOK", S48_FALSE);
#endif
#ifdef ECHO
s48_define_exported_binding("ECHO", s48_enter_fixnum(ECHO));
#else
s48_define_exported_binding("ECHO", S48_FALSE);
#endif
#ifdef ECHONL
s48_define_exported_binding("ECHONL", s48_enter_fixnum(ECHONL));
#else
s48_define_exported_binding("ECHONL", S48_FALSE);
#endif
#ifdef ICANON
s48_define_exported_binding("ICANON", s48_enter_fixnum(ICANON));
#else
s48_define_exported_binding("ICANON", S48_FALSE);
#endif
#ifdef ISIG
s48_define_exported_binding("ISIG", s48_enter_fixnum(ISIG));
#else
s48_define_exported_binding("ISIG", S48_FALSE);
#endif
#ifdef IEXTEN
s48_define_exported_binding("IEXTEN", s48_enter_fixnum(IEXTEN));
#else
s48_define_exported_binding("IEXTEN", S48_FALSE);
#endif
#ifdef TOSTOP
s48_define_exported_binding("TOSTOP", s48_enter_fixnum(TOSTOP));
#else
s48_define_exported_binding("TOSTOP", S48_FALSE);
#endif
#ifdef NOFLSH
s48_define_exported_binding("NOFLSH", s48_enter_fixnum(NOFLSH));
#else
s48_define_exported_binding("NOFLSH", S48_FALSE);
#endif
#ifdef ECHOKE
s48_define_exported_binding("ECHOKE", s48_enter_fixnum(ECHOKE));
#else
s48_define_exported_binding("ECHOKE", S48_FALSE);
#endif
#ifdef ECHOPRT
s48_define_exported_binding("ECHOPRT", s48_enter_fixnum(ECHOPRT));
#else
s48_define_exported_binding("ECHOPRT", S48_FALSE);
#endif
#ifdef ECHOCLT
s48_define_exported_binding("ECHOCLT", s48_enter_fixnum(ECHOCLT));
#else
s48_define_exported_binding("ECHOCLT", S48_FALSE);
#endif
#ifdef FLUSHO
s48_define_exported_binding("FLUSHO", s48_enter_fixnum(FLUSHO));
#else
s48_define_exported_binding("FLUSHO", S48_FALSE);
#endif
#ifdef PENDIN
s48_define_exported_binding("PENDIN", s48_enter_fixnum(PENDIN));
#else
s48_define_exported_binding("PENDIN", S48_FALSE);
#endif
#ifdef ALTWERASE
s48_define_exported_binding("ALTWERASE", s48_enter_fixnum(ALTWERASE));
#else
s48_define_exported_binding("ALTWERASE", S48_FALSE);
#endif
#ifdef NOKERNINFO
s48_define_exported_binding("NOKERNINFO", s48_enter_fixnum(NOKERNINFO));
#else
s48_define_exported_binding("NOKERNINFO", S48_FALSE);
#endif
#ifdef XCASE
s48_define_exported_binding("XCASE", s48_enter_fixnum(XCASE));
#else
s48_define_exported_binding("XCASE", S48_FALSE);
#endif
}
