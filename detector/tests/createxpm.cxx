#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

void main(int argc, char **argv)
{
  
  float nx, ny;
  int ncol;
  int i, j;

  nx = atof(argv[1]);
  ny = atof(argv[2]);
  ncol = atoi(argv[3]);

  cout << "/* XPM */\n"
	   << "static char *xpm[] = {\n"
	   << "/* width height num_colors chars_per_pixel */\n"
	   << "\"   400   300  " << ncol << "  1\"\n"
	   << "/* colors */\n";

  for ( i=0; i<ncol; ++i ) {
	
	cout << 
	

/* colors */

  

}
