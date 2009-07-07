/* Exports from tty1.c. */

s48_value scheme_tcgetattr(s48_value sch_fd,s48_value sch_control_chars);

s48_value scheme_tcsetattr(s48_value fd, s48_value option,
		     s48_value control_chars,
		     s48_value iflag,
		     s48_value oflag,
		     s48_value cflag,
		     s48_value lflag,
		     s48_value ispeed, s48_value ospeed,
		     s48_value min, s48_value time);
s48_value sch_tcsendbreak (s48_value sch_fd, s48_value sch_duration);
s48_value sch_tcdrain (s48_value sch_fd);
s48_value sch_tcflush (s48_value sch_fd, s48_value sch_action);
s48_value sch_tcflow (s48_value sch_fd, s48_value sch_action);
s48_value sch_tcsetpgrp (s48_value sch_fd, s48_value sch_pid);
s48_value sch_tcgetpgrp (s48_value sch_fd);

s48_value open_ctty(s48_value sch_ttyname, s48_value sch_flags);

s48_value sch_isatty (s48_value sch_fd);

s48_value sch_ttyname (s48_value sch_fd);

s48_value scm_ctermid();


