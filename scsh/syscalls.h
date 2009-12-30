/* Exports from syscalls.c */

s48_ref_t wait_pid(s48_call_t call, s48_ref_t pid, s48_ref_t flags);

s48_ref_t scheme_exec(s48_call_t call, s48_ref_t prog, s48_ref_t argv, s48_ref_t env);

s48_ref_t scsh_exit (s48_call_t call, s48_ref_t status);

s48_ref_t scsh__exit (s48_call_t call, s48_ref_t status);

s48_ref_t scsh_fork (s48_call_t call);

s48_ref_t scheme_pipe(s48_call_t call);

s48_ref_t scsh_kill (s48_call_t call, s48_ref_t pid, s48_ref_t signal);

s48_ref_t scsh_readlink(s48_call_t call, s48_ref_t path);

s48_ref_t scsh_rename(s48_call_t call, s48_ref_t sch_from, s48_ref_t sch_to);

s48_ref_t scsh_rmdir(s48_call_t call, s48_ref_t sch_path);

s48_ref_t scsh_truncate(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_length);

s48_ref_t scsh_ftruncate(s48_call_t call, s48_ref_t sch_fdes, s48_ref_t sch_length);

s48_ref_t scsh_unlink(s48_call_t call, s48_ref_t sch_path);

s48_ref_t scsh_fsync(s48_call_t call, s48_ref_t sch_fdes);

s48_ref_t scsh_sync(s48_call_t call);

s48_ref_t scsh_close(s48_call_t call, s48_ref_t sch_fdes);

s48_ref_t scsh_dup(s48_call_t call, s48_ref_t sch_fdes);

s48_ref_t scsh_dup2(s48_call_t call, s48_ref_t sch_oldd,  s48_ref_t sch_newd);

s48_ref_t scsh_lseek(s48_call_t call, s48_ref_t sch_fdes, s48_ref_t sch_offset,
		     s48_ref_t sch_whence);

s48_ref_t scsh_open(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_flags, s48_ref_t sch_mode);

s48_ref_t scm_utime(s48_call_t call, s48_ref_t path, s48_ref_t ac, s48_ref_t mod);

s48_ref_t scm_utime_now(s48_call_t call, s48_ref_t path);

s48_ref_t set_cloexec(s48_call_t call, s48_ref_t _fd, s48_ref_t _val);

s48_ref_t scsh_chdir(s48_call_t call, s48_ref_t directory);

s48_ref_t scheme_cwd(s48_call_t call);

s48_ref_t process_times(s48_call_t call);

s48_ref_t cpu_clock_ticks_per_sec(s48_call_t call);

s48_ref_t scsh_chmod(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_mode);

s48_ref_t scsh_fchmod(s48_call_t call, s48_ref_t sch_fd, s48_ref_t sch_mode);

s48_ref_t scsh_chown(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_uid, s48_ref_t sch_gid);

s48_ref_t scsh_fchown(s48_call_t call, s48_ref_t sch_fd, s48_ref_t sch_uid, s48_ref_t sch_gid);

s48_ref_t scsh_access(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_mode);

s48_ref_t scsh_link(s48_call_t call, s48_ref_t sch_name1, s48_ref_t name2);

s48_ref_t scsh_mkfifo(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_mode);

s48_ref_t scsh_mkdir(s48_call_t call, s48_ref_t sch_path, s48_ref_t sch_mode);

s48_ref_t char_ready_fdes(s48_call_t call, s48_ref_t sch_fd);

s48_ref_t read_fdes_char(s48_call_t call, int fd);

int write_fdes_char(s48_call_t call, char c, int fd);

ssize_t read_fdes_substring(s48_call_t call, s48_ref_t buf, size_t start, size_t end, int fd);

int read_stream_substring(s48_call_t call, s48_ref_t buf, int start, int end, FILE *f);

ssize_t write_fdes_substring(s48_call_t call, s48_ref_t buf, size_t start, size_t end, int fd);

int write_stream_substring(s48_call_t call, s48_ref_t buf, int start, int end, FILE *f);

s48_ref_t scheme_stat(s48_call_t call, s48_ref_t path, s48_ref_t vec, s48_ref_t chase_p);

s48_ref_t scheme_fstat(s48_call_t call, s48_ref_t fd, s48_ref_t vec);

s48_ref_t scsh_getgid(s48_call_t call);

s48_ref_t scsh_getegid(s48_call_t call);

s48_ref_t scsh_setgid(s48_call_t call, s48_ref_t gid);

s48_ref_t scsh_setegid(s48_call_t call, s48_ref_t gid);

s48_ref_t get_groups(s48_call_t call);

s48_ref_t scsh_getuid(s48_call_t call);

s48_ref_t scsh_geteuid(s48_call_t call);

s48_ref_t scsh_setuid(s48_call_t call, s48_ref_t uid);

s48_ref_t scsh_seteuid(s48_call_t call, s48_ref_t uid);

s48_ref_t scsh_getpid(s48_call_t call);

s48_ref_t scsh_getppid(s48_call_t call);

s48_ref_t scsh_getpgrp(s48_call_t call);

s48_ref_t scsh_setpgid(s48_call_t call, s48_ref_t sch_pid, s48_ref_t sch_pgrp);

s48_ref_t scsh_setsid(s48_call_t call);

s48_ref_t scsh_umask(s48_call_t call, s48_ref_t sch_mask);

s48_ref_t align_env(s48_call_t call, s48_ref_t pointer_to_struct);

s48_ref_t free_envvec (s48_call_t call, s48_ref_t pointer_to_struct);

s48_ref_t scm_envvec(s48_call_t call);

s48_ref_t create_env(s48_call_t call, s48_ref_t vec);

s48_ref_t scm_gethostname(s48_call_t call);

s48_ref_t fcntl_read(s48_call_t call, s48_ref_t fd, s48_ref_t command);

s48_ref_t fcntl_write(s48_call_t call, s48_ref_t fd,
                      s48_ref_t command, s48_ref_t value);

s48_ref_t sleep_until(s48_call_t call, s48_ref_t scm_when);

extern void scsh_init_syscalls(void);
