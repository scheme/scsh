/* Losing reimplimentation of Posix calls for NextSTEP. */

#include <stdio.h>
#include <sys/types.h>
#include <sgtty.h>

int tcsetpgrp1(int fd, int pid)
{
    int i;
    return ioctl(0, TIOCSPGRP, &i);
    }

int tcgetpgrp1(int fd)
{
    int i, j;
    return (ioctl(0, TIOCGPGRP, &i) == -1) ? -1 : i;
    }
