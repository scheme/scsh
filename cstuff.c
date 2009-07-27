/* build a list from  a null-terminated char* vector. */
#include "cstuff.h"

s48_ref_t char_pp_2_string_list(s48_call_t call, char **vec) {
  char ** ptr = vec;
  s48_ref_t list = s48_null_2(call);

  while (ptr && *(ptr)){
    list = s48_cons_2(call, s48_enter_string_latin_1_2(call, *ptr), list);
    ptr++;
  }

  return list;
}
