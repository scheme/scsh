extern void scsh_init_syscalls(void);

void s48_on_load(void)
{
    scsh_init_syscalls();
}
