#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <assert.h>

#define SIN60   0.866025403784439
#define COS60   0.500000000000000
#define SIN30   COS60
#define COS30   SIN60

static const float ct_PixelWidth = 1.0;
static const float ct_PixelWidth_corner_2_corner = ct_PixelWidth / COS30; 
static const float ct_PixelWidth_corner_2_corner_half =
  ct_PixelWidth_corner_2_corner / 2; 
static const float ct_Apot = ct_PixelWidth / 2;          
static const float ct_2Apot = 2 * ct_Apot;         

template<class T>
T sqr(T x) { return (x*x); }

template<class T>
inline T round(float x) { return static_cast<T>(floor(x + 0.5)); }

inline int SumNumPixelInRings(int r) { return ( 3*r*(r+1)+1 ); }
inline int FirstInRing(int r) { return ( SumNumPixelInRings(r-1) ); }
inline int NumPixelsInRing(int r) { return ( 6*r ); }

static void hexcoord (float cx, float cy, int &ki, int &kj, int &kk);

// @endcode

// @subsection Implementation.

// @code
int main(int argc, char **argv)
{
  int npix = 1;
  int iring;
  int i, j, k;

  int ki, kj, kk;
  
  float x, y;
  float dx, dy;
  float maxx, maxy;
  int nhalf;
  
  
  dx = ct_PixelWidth / 2.0;
  dy = ct_PixelWidth_corner_2_corner_half * 1.5;

  for ( int iring=1; iring<4; iring++ ) {

    assert( npix == FirstInRing(iring) );

    nhalf = NumPixelsInRing(iring)/2;
    
    x = iring * ct_PixelWidth;
    maxx = iring * dx;
    float *px = new float [ NumPixelsInRing(iring) ];

    for (k=0; k<nhalf; k++) {
      px[ k ] = x;
      px[ k + nhalf ] = -x;
      x -= dx;
    }

    y = 0.;
    maxy = (iring-1) * dy;
    float *py = new float [ NumPixelsInRing(iring) ];

    for (k=0; k<nhalf; k++) {
      py[ k ] = y;
      py[ k + nhalf ] = -y;
      y += dy;
      if ( y > maxy ) {
        for (j=1; j<iring; j++) {
          k++;
          py[ k ] = py[ k - 1 ];
          py[ k + nhalf ] = py[ k + nhalf - 1];
        }
        dy = -dy;
      }
    }

    npix = FirstInRing(iring);
  
    for (k=0; k<NumPixelsInRing(iring); k++) {
      
      x = px[ k ];
      y = py[ k ];
      hexcoord (x, y, ki, kj, kk);
      
      printf("%4d  %3d %3d %3d  %10g %10g\n", npix+1, ki, kj, kk, x, y);

      npix++;
    }

    delete [] px;
    delete [] py;

  }
  
  return 0;
}

static void hexcoord (float cx, float cy, int &ki, int &kj, int &kk)
{
  float x, y;                 // intermediate variables
  float dx, dy, dz;
  float rx, ry, rz;
  float ax, ay, az;
  int   s;
  static const float unit = 1.5*ct_PixelWidth_corner_2_corner_half;

  x = (  cx*COS30 + cy*SIN30);
  y = (- cx*SIN30 + cy*COS30);
  
  dx = x / unit;
  dy = (- x*SIN30 + y*COS30) / unit;
  dz = (- x*SIN30 - y*COS30) / unit;
  
  rx = round<float>(dx);
  ry = round<float>(dy);
  rz = round<float>(dz);
  
  ki = int(rx);
  kj = int(ry);
  kk = int(rz);
  
  s = ki + kj + kk;
  if (s) {
    ax = fabs(rx-dx);
    ay = fabs(ry-dy);
    az = fabs(rz-dz);
    
    if ((ax >= ay) && (ax >= az)) { ki -= s; }
    else if ((ay >= ax) && (ay >= az)) { kj -= s; }
    else { kk -= s; }
  }
  
  return;
}
  
