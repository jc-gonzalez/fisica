#include <stdlib.h>
#include <iostream.hxx>

void main(int argc, char **argv)
{
  int nnums = 1000;

  if (argc>1)
	nnums = atoi( argv[1] );

  if (argc>2)
	srand48( atol( argv[2] ) );

  int i;

  for (i=0; i<nnums; ++i)
	cout << drand48() << "\n";

}

	
