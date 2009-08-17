#include  <sys/types.h>
#include <time.h>

s48_value scheme_time();

s48_value time_plus_ticks();

extern s48_value time2date(s48_value t, s48_value zone);

s48_value date2time(s48_value sec, s48_value min, s48_value hour,
		    s48_value mday, s48_value month, s48_value year,
		    s48_value tz_name, s48_value tz_secs,
		    s48_value summer);
		    

extern s48_value format_date(s48_value fmt, s48_value sch_sec, 
			     s48_value sch_min, s48_value sch_hour,
			     s48_value sch_mday, s48_value sch_month, 
			     s48_value sch_year,
			     s48_value tz, s48_value sch_summer,
			     s48_value sch_week_day, s48_value sch_year_day);
