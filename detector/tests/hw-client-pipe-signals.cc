#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <math.h>

FILE *fp;

void getit(int i)
{
  float x, y;
  char line[40];

  puts("Reading from the client");
  if (feof(fp))
    puts("Ooops! EOF!");
  else
    puts("OK, no EOF.");

  fscanf(fp,"%f %f", &x, &y);
  puts("OK.");
  sprintf(line,"sin(%f) = %f\n", x, y);
  puts(line);
  /* write(1,line,strlen(line)); */

  printf("[%d]\n",signal(SIGUSR1, getit));
}


main(int argc, char **argv)
{
  int pipefd;
  int i;

  pipefd = atoi(argv[1]);
  
  printf("Opening pipe with fd=%d\n", pipefd);
  fp = fdopen(pipefd, "r");
  
  puts("Setting signal SIGUSR1");

  printf("[%d]\n",signal(SIGUSR1, getit));

  pause();

  puts("Bye!");

}
