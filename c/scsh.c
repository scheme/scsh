extern void scsh_init_syscalls(void);
extern void scsh_init_tty(void);

void s48_on_load(void)
{
    scsh_init_tty();
    scsh_init_syscalls();
}
