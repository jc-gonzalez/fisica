#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "jcmacros.h"
#include "jcdebug.h"

const char * const names[]={"seno", "coseno", "tangente"};

float seno(float x);
float coseno(float x); 
float tangente(float x); 

void main(int argc, char **argv)
{
  float (*f)(float);
  int i;
  float x;

  i = atoi( argv[1] );
  x = atof( argv[2] );
  
  switch (i) {
	
  case 0:	
	f = seno;
	break;

  case 1:	
	f = coseno;
	break;

  case 2:	
	f = tangente;
	break;

  }

  cout << names[i] << "(" << x << ") = " << f(x) << endl;

}

float seno(float x) 
{ 
  return( (float)sin((double)x) );
}

float coseno(float x) 
{ 
  return( (float)cos((double)x) );
}

float tangente(float x) 
{ 
  return( (float)tan((double)x) );
}

