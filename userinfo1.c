/* Scheme48/scsh Unix system interface.
** User and group db access routines
** Copyright (c) 1993, 1994 by Olin Shivers.
*/

/*
** The code in this file is separated out from the code in syscalls1.c
** because we must compile them with different compiler flags (non-posix)
** to get around a NeXTSTEP bug. Once they get their act together, we can
** stick it back into syscalls1.c.
*/

#include <sys/types.h>
#include <grp.h>
#include <pwd.h>

/* Make sure our exports match up w/the implementation: */
#include "userinfo1.h"

#include <unistd.h>
#include "scheme48.h"
#include "cstuff.h"

/* User db access routines
*******************************************************************************
*/
    
/* Return a user name for ourself. 
** Uses our *real* uid, not our effective one.
*/

s48_value my_username(void)
{
    char *s = getlogin();
    if( s ) return s48_enter_string (s);
    else {
	struct passwd *pwd = getpwuid(getuid());
	return pwd ? s48_enter_string(pwd->pw_name) : S48_FALSE;
	}
    }

s48_value user_info_uid(s48_value scheme_uid, s48_value user_info_record)
{
    struct passwd *pwd = getpwuid(s48_extract_fixnum(scheme_uid));
    S48_DECLARE_GC_PROTECT(1);

    S48_GC_PROTECT_1(user_info_record);

    if( !pwd ) {
      S48_GC_UNPROTECT();
      return S48_FALSE;
    }

    S48_RECORD_SET(user_info_record, 0, s48_enter_string (pwd->pw_name));
    S48_RECORD_SET(user_info_record, 2, s48_enter_fixnum (pwd->pw_gid));   
    S48_RECORD_SET(user_info_record, 3, s48_enter_string (pwd->pw_dir));
    S48_RECORD_SET(user_info_record, 4, s48_enter_string (pwd->pw_shell));
    S48_GC_UNPROTECT();
    return S48_TRUE;
}

s48_value user_info_name(s48_value scheme_name, s48_value user_info_record)
{
    struct passwd *pwd = getpwnam(s48_extract_string (scheme_name));
    S48_DECLARE_GC_PROTECT(1);
    
    S48_GC_PROTECT_1(user_info_record);

    if( !pwd ) {
      S48_GC_UNPROTECT();
      return S48_FALSE;
    }

    S48_RECORD_SET(user_info_record, 1, s48_enter_fixnum (pwd->pw_uid));
    S48_RECORD_SET(user_info_record, 2, s48_enter_fixnum (pwd->pw_gid));   
    S48_RECORD_SET(user_info_record, 3, s48_enter_string (pwd->pw_dir));
    S48_RECORD_SET(user_info_record, 4, s48_enter_string (pwd->pw_shell));
    S48_GC_UNPROTECT();
    return S48_TRUE;
}



s48_value group_info_gid (s48_value scheme_gid, s48_value group_info_record)
{
    struct group *grp = getgrgid(s48_extract_fixnum(scheme_gid));
    s48_value member_list;
    S48_DECLARE_GC_PROTECT(1);
    
    S48_GC_PROTECT_1(group_info_record);

    if( !grp ) {
      S48_GC_UNPROTECT();
      return S48_FALSE;
    }
    S48_RECORD_SET(group_info_record, 0, s48_enter_string (grp->gr_name));
    
    member_list = char_pp_2_string_list (grp->gr_mem);
    if( !member_list ) {
      S48_GC_UNPROTECT();
      return S48_FALSE;
    }
    S48_RECORD_SET(group_info_record, 2, member_list);
    
    S48_GC_UNPROTECT();
    return S48_TRUE;
}

s48_value group_info_name (s48_value scheme_name, s48_value group_info_record)
{
    struct group *grp = getgrnam(s48_extract_string(scheme_name));
    s48_value member_list;
    S48_DECLARE_GC_PROTECT(1);
    
    S48_GC_PROTECT_1(group_info_record);

    if( !grp ) {
      S48_GC_UNPROTECT();
      return S48_FALSE;
    }
    S48_RECORD_SET(group_info_record, 1, s48_enter_fixnum (grp->gr_gid));
    
    member_list = char_pp_2_string_list (grp->gr_mem);
     if( !member_list ) {
      S48_GC_UNPROTECT();
      return S48_FALSE;
    }
    S48_RECORD_SET(group_info_record, 2, member_list);
    
    S48_GC_UNPROTECT();
    return S48_TRUE;
}


void s48_init_userinfo(void){
    S48_EXPORT_FUNCTION(user_info_uid);
    S48_EXPORT_FUNCTION(user_info_name);
    S48_EXPORT_FUNCTION(my_username);
    S48_EXPORT_FUNCTION(group_info_gid); 
    S48_EXPORT_FUNCTION(group_info_name); 
}
