#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <sys/wait.h>


main()
{
  FILE *fp;
  int pid, pipefds[2];
  char strpipefds[40];
  int i;
  
  if (pipe(pipefds) < 0) {
    perror("pipe");
    exit(1);
  }

  if ((pid=fork()) < 0) {
    perror("fork");
    exit(1);
  }

  /*
   *  CHILD (CLIENT) SIDE
   */

  if (pid==0) {
    
    /* close write side */
    close(pipefds[1]);
    sprintf(strpipefds, "%d", pipefds[0]);
    execl("./hw-client-pipe","hw-client-pipe",
          (char *)strpipefds, NULL);
    perror("exec");
    exit(1);

  }

  /*
   *  PARENT (SERVER) SIDE
   */

  close(pipefds[0]);

  fp = fdopen(pipefds[1], "w");

  for (i=0; i<10; i++) {

    fprintf(fp, "%f %f", 
            ((float)i)/10.0, sin(((float)i)/10.0));

  }

  fprintf(fp, "-1. 0\n"); 
  fclose(fp);

  while (wait((int*)0) != pid);

  return (0);
}
