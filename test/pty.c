#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>

int main(){
  int pty, pty_d, tty, flags;
  char buf[80];
  pty = open("/dev/ptyq2", O_RDWR);
  tty = open("/dev/ttyq2", O_RDWR);
  pty_d = dup(pty);
  flags = fcntl(pty_d, F_GETFD);
  if (flags == -1){
    fprintf(stderr, "F_GETFD failed");
  }
  if (fcntl(pty_d, F_SETFD, flags & FD_CLOEXEC) == -1){
    fprintf(stderr, "F_SETFD failed");
  }
  write(pty, "23", 2);
  read(tty, &buf, 2);
  //close(pty_d);
  exit(0);
  return 0;
}
