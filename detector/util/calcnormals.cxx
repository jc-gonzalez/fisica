#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include "jcmacros.h"

int main(int argc, char **argv)
{
  char filein[40];
  char fileout[40];
  int type=0;
  ifstream fin;
  ofstream fout;

  double i, a, b, c, x, y, z, t, p, xn, yn, zn, rho, henf, f;

  char line[200];

  strcpy( filein, argv[1] );
  strcpy( fileout, argv[2] );

  type = atoi( argv[3] );

  henf = 1.0e6;  // altitude where the telescope is pointing to

  fin.open( filein );
  fout.open( fileout );

  while ( 1 ) {
    
    fin.getline( line, 200 );

    fout << line << endl;

    if ( strstr( line, "define_mirrors" ) != NULL )
      break;

  }

  while ( 1 ) {
    
    fin.getline( line, 200 );

    if ( strstr( line, "EOF" ) != NULL )
      break;

    sscanf( line, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf ", 
            &i, &a, &b, &c,
            &x, &y, &z, 
            &t, &p, 
            &xn, &yn, &zn );

    f = ( type == 0 ) ? 488.0 : 1700.0;  // focal lengths
    cout << f;

    rho = sqrt(SQR(x)+SQR(y));
    t = ( atan2(rho,henf-z) + atan2(rho,f-z) ) * 0.5;
    p = atan2(y,x);
    p += (p<0.0) ? 2.0*M_PI : 0.0;
    cout << "  x=" << x << "  y=" << y 
         << "  t=" << t*180.0/M_PI 
         << "  p=" << p*180.0/M_PI 
         << endl << flush;
    xn = -sin(t) * cos(p);
    yn = -sin(t) * sin(p);
    zn = cos(t);

    sprintf(line, "%3d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
           (int)i,a,b,c,x,y,z,t,p,xn,yn,zn);

    fout << line;
    cout << '[' << (int)i << ']' << flush;
  }

  fout << "#EOF\n" << flush;

  fin.close();
  fout.close();
}



