/* Exports from userinfo1.c. */
#include "scheme48.h"
s48_value my_username(void);

s48_value user_info_uid(s48_value scheme_uid, s48_value user_info_record);


s48_value user_info_name(s48_value scheme_name, s48_value user_info_record);

s48_value group_info_gid (s48_value gid, s48_value group_info_record);

s48_value group_info_name (s48_value name, s48_value group_info_record);
