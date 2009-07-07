/* Exports from syscalls1.c. */

s48_value wait_pid(s48_value pid, s48_value flags);

s48_value scheme_exec(s48_value prog, s48_value argv, s48_value env);

s48_value scsh_exit (s48_value status);

s48_value scsh__exit (s48_value status);

s48_value scsh_fork ();

s48_value scheme_pipe();

s48_value scsh_kill (s48_value pid, s48_value signal);

s48_value scsh_readlink(s48_value path);

s48_value scsh_rename(s48_value sch_from, s48_value sch_to);

s48_value scsh_rmdir(s48_value sch_path);

s48_value scsh_symlink(s48_value sch_name1, s48_value name2);

s48_value scsh_truncate(s48_value sch_path, s48_value sch_length);

s48_value scsh_ftruncate(s48_value sch_fdes, s48_value sch_length);

s48_value scsh_unlink(s48_value sch_path);

s48_value scsh_fsync(s48_value sch_fdes);

s48_value scsh_sync();

s48_value scsh_close(s48_value sch_fdes);

s48_value scsh_dup(s48_value sch_fdes);

s48_value scsh_dup2(s48_value sch_oldd,  s48_value sch_newd);

s48_value scsh_lseek(s48_value sch_fdes, s48_value sch_offset, 
		     s48_value sch_whence);

s48_value scsh_open(s48_value sch_path, s48_value sch_flags, s48_value sch_mode);

s48_value scm_utime(s48_value path, s48_value ac, s48_value mod);

s48_value scm_utime_now(s48_value path);

s48_value set_cloexec(s48_value _fd, s48_value _val);

s48_value scsh_chdir(s48_value directory);

s48_value scheme_cwd();

s48_value process_times();

s48_value cpu_clock_ticks_per_sec();

s48_value scsh_chmod(s48_value sch_path, s48_value sch_mode);

s48_value scsh_fchmod(s48_value sch_fd, s48_value sch_mode);

s48_value scsh_chown(s48_value sch_path, s48_value sch_uid, s48_value sch_gid);

s48_value scsh_fchown(s48_value sch_fd, s48_value sch_uid, s48_value sch_gid);

s48_value scsh_access(s48_value sch_path, s48_value sch_mode);

s48_value scsh_link(s48_value sch_name1, s48_value name2);

s48_value scsh_mkfifo(s48_value sch_path, s48_value sch_mode);

s48_value scsh_mkdir(s48_value sch_path, s48_value sch_mode);

s48_value char_ready_fdes(s48_value sch_fd);

s48_value read_fdes_char(int fd);

int write_fdes_char(char c, int fd);

ssize_t read_fdes_substring(s48_value buf, size_t start, size_t end, int fd);

int read_stream_substring(s48_value buf, int start, int end, FILE *f);

ssize_t write_fdes_substring(s48_value buf, size_t start, size_t end, int fd);

int write_stream_substring(s48_value buf, int start, int end, FILE *f);

s48_value scheme_stat(s48_value path, s48_value vec, s48_value chase_p);

s48_value scheme_fstat(s48_value fd, s48_value vec);

s48_value scsh_getgid();

s48_value scsh_getegid();

s48_value scsh_setgid(s48_value gid);

s48_value scsh_setegid(s48_value gid);

s48_value get_groups();

s48_value scsh_getuid();

s48_value scsh_geteuid();

s48_value scsh_setuid(s48_value uid);

s48_value scsh_seteuid(s48_value uid);

s48_value scsh_getpid();

s48_value scsh_getppid();

s48_value scsh_getpgrp();

s48_value scsh_setpgid(s48_value sch_pid, s48_value sch_pgrp);

s48_value scsh_setsid();

s48_value scsh_umask(s48_value sch_mask);

s48_value align_env(s48_value pointer_to_struct);

s48_value free_envvec (s48_value pointer_to_struct);

s48_value scm_envvec(void);

s48_value create_env(s48_value vec);

s48_value scm_gethostname(void);

s48_value errno_msg(s48_value sch_i);

s48_value fcntl_read(s48_value fd, s48_value command);

s48_value fcntl_write(s48_value fd, s48_value command, s48_value value);

s48_value scm_crypt(s48_value key, s48_value salt);

s48_value scm_openlog (s48_value _ident, s48_value _option, s48_value _facility);

s48_value scm_syslog (s48_value _facility, s48_value _level, s48_value _message);

s48_value scm_closelog();

s48_value sleep_until();
