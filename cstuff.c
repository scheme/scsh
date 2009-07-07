/* build a list from  a null-terminated char* vector. */
#include "cstuff.h"
s48_value char_pp_2_string_list(char **vec){
    char ** ptr = vec;
    s48_value list = S48_NULL;

    S48_DECLARE_GC_PROTECT(1);
    S48_GC_PROTECT_1(list);
    
    while (ptr && *(ptr)){
      list = s48_cons (s48_enter_string (*ptr), list);
      ptr++;
    }
    S48_GC_UNPROTECT();
    return list;
}
