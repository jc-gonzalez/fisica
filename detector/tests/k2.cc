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

static void hex2coord (int ki, int kj, int kk, float &x, float &y);
static void coord2hex (float cx, float cy, int &ki, int &kj, int &kk);

// @endcode

// @subsection Implementation.

// @code
int main(int argc, char **argv)
{
  int npix = 1;
  int iring;
  int i, j, k;

  int ki, kj, kk, kstatic;
  
  float x, y;
  float dx, dy;
  float maxx, maxy;
  int ntot, nhalf;
  
  if ( argc<2 ) {
    cout << "usage :: " << argv[0] << " <number-of-rings> \n" << flush;
    exit(1);
  } 

  int nrings = atoi(argv[1]);

  printf("%4d %3d %3d %3d %10g %10g\n", 1, 0, 0, 0, 0., 0.);
  
  dx = ct_PixelWidth / 2.0;
  dy = ct_PixelWidth_corner_2_corner_half * 1.5;

  for ( int iring=1; iring<nrings; iring++ ) {

    assert( npix == FirstInRing(iring) );

    ntot  = NumPixelsInRing(iring);
    nhalf = ntot/2;

    ki = iring;
    kj = -ki;
    kk = 0;
    kstatic = 0;

    int * iki = new int [ ntot ];
    int * ikj = new int [ ntot ];
    int * ikk = new int [ ntot ];

    for (k=0; k<nhalf; k++) {

      if ( kstatic > iring ) ki--;

      kstatic++;
      
      iki[k] = ki;
      
      iki[k+nhalf]  = -iki[k];
    }
      
    for (k=1; k<ntot; k++) {
      ikj[ntot-k] = -iki[k];
    }
    ikj[0] = -iki[0];

    for (k=0; k<ntot; k++) {
      ikk[k] = -(iki[k]+ikj[k]);
    }

    for (k=0; k<ntot; k++) {
      ki = iki[k];
      kj = ikj[k];
      kk = ikk[k];

      hex2coord(ki, kj, kk, x, y);
      printf("%4d %3d %3d %3d %10g %10g\n", npix+1, ki, kj, kk, x, y);
      
      npix++;
    }

    delete [] iki;
    delete [] ikj;
    delete [] ikk;

  }
  
  return 0;
}

static void coord2hex (float cx, float cy, int &ki, int &kj, int &kk)
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
  
static void hex2coord (int ki, int kj, int kk, float &x, float &y)
{
  static const float unit = ct_PixelWidth_corner_2_corner_half;

  x = (ki*COS30 - kj*COS30) * unit;
  y = ((ki+kj)*SIN30 - kk) * unit;

  return;
}
  
