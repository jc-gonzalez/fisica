#include <iostream.h>
#include <fstream.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>

int main(int argc, char **argv)
{

  double u[3], v[3], x[3], y[3];
    
  double det;
  double a, b, c, d, e, f;
  double cx, cy, cu, cv;
  double c1, c2, c3;
  double f1, f2, f3;

  int i;
  int xlog, ylog;
  int ncurve, npoints;

  char curve_name[200];
  char file_prefix[100];
  char file_name[200];
  
  xlog = atoi(argv[1]);
  ylog = atoi(argv[2]);
  strcpy(file_prefix, argv[3]);

  ifstream faxes;
  faxes.open("tmpaxes.dat");
  
  for (i=0; i<3; i++) {
    faxes >> u[i] >> v[i] >> x[i] >> y[i];

    if (xlog > 0) u[i] = log10(u[i]);
    if (ylog > 0) v[i] = log10(v[i]);
  }    

  faxes.close();
  
  det = (x[1]-x[0])*(y[2]-y[0]) - (x[2]-x[0])*(y[1]-y[0]);
  
  a = ((y[2]-y[0])*(u[1]-u[0]) + (y[0]-y[1])*(u[2]-u[0])) / det;
  b = ((x[0]-x[2])*(u[1]-u[0]) + (x[1]-x[0])*(u[2]-u[0])) / det;
  c = u[0] - a*x[0] - b*y[0];

  d = ((y[2]-y[0])*(v[1]-v[0]) + (y[0]-y[1])*(v[2]-v[0])) / det;
  e = ((x[0]-x[2])*(v[1]-v[0]) + (x[1]-x[0])*(v[2]-v[0])) / det;
  f = v[0] - d*x[0] - e*y[0];

  printf("\nImage scaling equation:\n\n");
  printf("  [ u ]   [ %12.6g  %12.6g ] [ x ]    [ %12.6g ]\n", 
         a, b, c);
  printf("  [   ] = [ %12s  %12s ] [   ]  + [ %12s ]\n", 
         " ", " ", " ");
  printf("  [ v ]   [ %12.6g  %12.6g ] [ y ]    [ %12.6g ]\n\n", 
         d, e, f);

  c1 = u[0] - a*x[0] - b*y[0];
  c2 = u[1] - a*x[1] - b*y[1];
  c3 = u[2] - a*x[2] - b*y[2];

  f1 = v[0] - d*x[0] - e*y[0];
  f2 = v[1] - d*x[1] - e*y[1];
  f3 = v[2] - d*x[2] - e*y[2];

  printf("CHECK: %f=%f=%f ; %f=%f=%f\n\n", c1, c2, c3, f1, f2, f3);

  printf("%16.8g %16.8g %16.8g %16.8g %16.8g %16.8g\n", a, b, c, d, e, f);

  ifstream fcurves;
  fcurves.open("tmpcurves.dat");

  while (1) {

    fcurves >> ncurve;

    if (ncurve < 0) break;

    sprintf(file_name, "%s_%d.dat", file_prefix, ncurve);

    ofstream fdat;
    fdat.open(file_name);
    
    fcurves.read(curve_name, 1);
    fcurves.getline(curve_name, 200);

    fdat << "# " << curve_name << endl;
    
    fcurves >> npoints;

    cout << "Curve #" << ncurve
         << ", named '" << curve_name
         << "', has " << npoints << " points." << endl;
    
    for (i=0; i<npoints; i++) {
      fcurves >> cx >> cy;

      cu = a * cx + b * cy + c;
      cv = d * cx + e * cy + f;

      if (xlog > 0) cu = pow(10.0, cu);
      if (ylog > 0) cv = pow(10.0, cv);

      fdat << cu << ' ' << cv << endl;
    }

    fdat.close();
    
  }

  fcurves.close();
  
  return 0;
}


