#include <iostream.h>
#include <iomanip.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <sys/types.h>
#include <dirent.h>

#include "jcmacros.h"
#include "jcdebug.h"

#define fromf fromf_

extern "C" {

  void fromf ( float *, float * );
		   
}

int main (int argc, char **argv)
{
  
  float a;
  float b;
  
  for (a=-10.; a<10.; a+=.7) {

	  fromf( &a, &b );
	  
	  cout << a << ' ' << b << endl;

  }

  return ( 0 );

}

	
