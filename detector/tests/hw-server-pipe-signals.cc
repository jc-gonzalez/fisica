#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <math.h>
#include <sys/wait.h>


main()
{
  FILE *fp;
  int pid, pipefds[2];
  char strpipefds[40];
  int i;
  float x;
  
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
    execl("./hw-client-pipe-signals","hw-client-pipe-signals",
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
    
    scanf("%f", &x);

    fprintf(fp, "%f %f\n", x, sin(x));

    printf("Sending signal SIGUSR1 to process %d\n", pid);
    printf("{%d}\n",kill(pid, SIGUSR1));

  }

  fclose(fp);

  /*while (wait((int*)0) != pid);*/

  return (0);
}
