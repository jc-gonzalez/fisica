#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <sys/types.h>
#include <dirent.h>

#include "jcmacros.h"
#include "jcdebug.h"

/*void attenu_ ( float *w, float *h, float *t, float *tr);*/



void main (int argc, char **argv)
{

  float w;
  float h;
  float t;
  
  float Tr;

  t = atof(argv[1]) * M_PI / 180.0;
  
  for (w=290.; w<601.; w+=4.) {

	for (h=1200000.; h>1000.; h-=5000.) {

	  attenu_( &w, &h, &t, &Tr );

	  printf("%e %e %e\n", w, h, Tr);

	}
  
  }

}

	
