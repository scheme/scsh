extern void scsh_init_syscalls(void);
extern void scsh_init_sighandlers(void);

void s48_on_load(void)
{
    scsh_init_syscalls();
    scsh_init_sighandlers();
}
