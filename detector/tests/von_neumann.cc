
#include <iostream.h>
#include <math.h>

extern "C" { 
#include "ranlib.h"
}

#define RandomNumber ranf()

static float
random_von_neumann( float *x, float *y, int n,
                    float maxy, float a, float b );

int main()
{

  const int n = 100;
  float x[n];
  float y[n];

  int i;

  for (i=0; i<n; i++) {
    x[i] = i*0.5;
    y[i] = cos(x[i]/50.) + sin(x[i]/30.) + 1.2;
    cout << x[i] << ' ' << y[i] << endl;
  }
  
  for (i=0; i<100000; i++)
    cerr << random_von_neumann( x, y, n, 3.0, 0.0, 50.0 ) << endl;

  return 0;
}

static float
random_von_neumann( float *x, float *y, int n,
                    float maxy, float a, float b )
{
  static float rx;
  static float ry;
  static int   ri;
  static float rbin;

  rbin = (b-a)/n;

  // simplified algorithm (check!!)
  do {
    // generate random values
    rx = RandomNumber * (b-a) + a;   // random value for x
    ry = RandomNumber * maxy;        // random value for y
    ri = int(rx/rbin);
  } while (ry > y[ri]);

  cout << ri << ' ' << rx << ' ' << ry << endl;
  
  return rx;
}
