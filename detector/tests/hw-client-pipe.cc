#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>


main(int argc, char **argv)
{
  FILE *fp;
  int pipefd;
  int i;
  float x, y;

  pipefd = atoi(argv[1]);
  
  fp = fdopen(pipefd, "r");

  while (x!=-1.) {
   
    if (!feof(fp)) {
      fscanf(fp,"%f %f", &x, &y);
      if (x!=-1.)
        printf("sin(%f) = %f\n", x, y);
    } else {
      clearerr(fp);
    }

  } 

  fclose(fp);

  return (0);
}
