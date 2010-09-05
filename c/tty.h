/* Exports from tty1.c. */

s48_ref_t scheme_tcgetattr(s48_call_t call, s48_ref_t sch_fd,s48_ref_t sch_control_chars);

s48_ref_t scheme_tcsetattr(s48_call_t call, s48_ref_t fd, s48_ref_t option,
                           s48_ref_t control_chars,
                           s48_ref_t iflag,
                           s48_ref_t oflag,
                           s48_ref_t cflag,
                           s48_ref_t lflag,
                           s48_ref_t ispeed, s48_ref_t ospeed,
                           s48_ref_t min, s48_ref_t time);
s48_ref_t sch_tcsendbreak (s48_call_t call, s48_ref_t sch_fd, s48_ref_t sch_duration);
s48_ref_t sch_tcdrain (s48_call_t call, s48_ref_t sch_fd);
s48_ref_t sch_tcflush (s48_call_t call, s48_ref_t sch_fd, s48_ref_t sch_action);
s48_ref_t sch_tcflow (s48_call_t call, s48_ref_t sch_fd, s48_ref_t sch_action);
s48_ref_t sch_tcsetpgrp (s48_call_t call, s48_ref_t sch_fd, s48_ref_t sch_pid);
s48_ref_t sch_tcgetpgrp (s48_call_t call, s48_ref_t sch_fd);

s48_ref_t open_ctty(s48_call_t call, s48_ref_t sch_ttyname, s48_ref_t sch_flags);

s48_ref_t sch_isatty (s48_call_t call, s48_ref_t sch_fd);

s48_ref_t sch_ttyname (s48_call_t call, s48_ref_t sch_fd);

s48_ref_t scm_ctermid(s48_call_t call);

s48_ref_t allocate_pty(s48_call_t call);
