#include  <sys/types.h>
#include <time.h>

s48_ref_t scheme_time(s48_call_t call);

s48_ref_t time_plus_ticks(s48_call_t call);

s48_ref_t time2date(s48_call_t call, s48_ref_t t, s48_ref_t zone);

s48_ref_t date2time(s48_call_t call, s48_ref_t sec, s48_ref_t min, s48_ref_t hour,
                    s48_ref_t mday, s48_ref_t month, s48_ref_t year,
                    s48_ref_t tz_name, s48_ref_t tz_secs,
                    s48_ref_t summer);

s48_ref_t format_date(s48_call_t call, s48_ref_t fmt, s48_ref_t sch_sec,
                      s48_ref_t sch_min, s48_ref_t sch_hour,
                      s48_ref_t sch_mday, s48_ref_t sch_month,
                      s48_ref_t sch_year,
                      s48_ref_t tz, s48_ref_t sch_summer,
                      s48_ref_t sch_week_day, s48_ref_t sch_year_day);
