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
inline int FirstInRingSector(int r, int s) { 
  return ( SumNumPixelInRings(r-1)+s*r );
}

static void hex2coord (int ki, int kj, int kk, float &x, float &y);
static void coord2hex (float cx, float cy, int &ki, int &kj, int &kk);
static void xy2ijk (float cx, float cy, float &di, float &dj, float &dk);
static void ijk2xy (float ki, float kj, float kk, float &x, float &y);

#define Make_Pixel(n, x, y, i, j, k) \
{ pki[n] = i, pkj[n] = j, pkk[n] = k, px[n] = x, py[n] = y; }

int main(int argc, char **argv)
{
  int i, j, k;

  int ki, kj, kk, kstatic;
  
  float x, y;
  float dx, dy;
  int ntot, nhalf;
  
  if ( argc<3 ) {
    cout << "usage :: " << argv[0]
         << " <num-small-rings> <num-big-rings>\n" << flush;
    exit(1);
  } 

  int nsrings = atoi(argv[1]);
  int nbrings = atoi(argv[2]);

  // total number of central pixels
  int ncentral_pixels  = SumNumPixelInRings( nsrings );
  
  // total number of gap pixels
  int ngap_pixels  = (nbrings-1)*6;

  // number of big pixels in one segment in first big pixels ring
  int nbig1 = (nsrings + 1) / 2;

  // total number of gap pixels
  int nbig_pixels = (nbig1 + (nbig1 + nbrings - 1)) * nbrings / 2 * 6;

  // total number of gap pixels
  int ntotal_pixels = ncentral_pixels + ngap_pixels + nbig_pixels;
  
  float * pki = new float [ ntotal_pixels ];
  float * pkj = new float [ ntotal_pixels ];
  float * pkk = new float [ ntotal_pixels ];
  float * px  = new float [ ntotal_pixels ];
  float * py  = new float [ ntotal_pixels ];
  
  // generate coordinates of small, central pixels

  int n=0;
  
  Make_Pixel(0, 0., 0., 0, 0, 0);
    
  printf("%4d %3d %3d %3d %10g %10g\n", 1, 0, 0, 0, 0., 0.);
  
  dx = ct_PixelWidth / 2.0;
  dy = ct_PixelWidth_corner_2_corner_half * 1.5;

  n++;
  for ( int iring=1; iring<nsrings+1; iring++ ) {

    assert( n == FirstInRing(iring) );

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

      printf("%4d %3d %3d %3d %10g %10g\n", n+1, ki, kj, kk, x, y);
      Make_Pixel( n, x, y, ki, kj, kk);
      
      n++;
    }

    delete [] iki;
    delete [] ikj;
    delete [] ikk;

  }

  // generate coordinates for gap pixels

  // loop on number of gap pixels each corner
  for (i=1; i<nbrings; i++) {

    int inc = i*2;

    k = FirstInRingSector(nsrings,0);
    pki[n] = pki[k] + inc;
    pkj[n] = pkj[k] - inc;
    pkk[n] = pkk[k];
    
    n++;
    
    k = FirstInRingSector(nsrings,1);
    pki[n] = pki[k] + inc;
    pkj[n] = pkj[k];
    pkk[n] = pkk[k] - inc;

    n++;

    k = FirstInRingSector(nsrings,2);
    pki[n] = pki[k];
    pkj[n] = pkj[k] + inc;
    pkk[n] = pkk[k] - inc;

    n++;
    
    for (j=0; j<3; j++) {
      pki[n] = -pki[n-3];
      pkj[n] = -pkj[n-3];
      pkk[n] = -pkk[n-3];
      n++;
    }
    
  }

  for (k=0; k<ngap_pixels; k++) {
    
    ki = int( pki[ncentral_pixels + k] );
    kj = int( pkj[ncentral_pixels + k] );
    kk = int( pkk[ncentral_pixels + k] );
    
    hex2coord(ki, kj, kk, x, y);

    px[ncentral_pixels + k] = x;
    py[ncentral_pixels + k] = y;
    
    printf("%4d %3d %3d %3d %10g %10g\n",
           ncentral_pixels+k+1, ki, kj, kk, x, y);
    
  }
  
  // generate big pixels coordinates

  float fi, fj, fk;
  float nx, ny;
  fi = fj = fk = 0.;
  
  k = FirstInRingSector(nsrings,0);

  // loop on number of gap pixels each corner
  for (i=0; i<nbrings; i++) { 

    x = px[k] + 2 * (i+1) * ct_PixelWidth;
    y = py[k] - ct_PixelWidth_corner_2_corner_half;
    
    for (j=0; j<(nbig1+i); j++) {
      
      x -= ct_PixelWidth;
      y += 3. * ct_PixelWidth_corner_2_corner_half;

      xy2ijk( x, y, fi, fj, fk);
      Make_Pixel( n,              x,  y, fi, fj, fk );
      
      ijk2xy(-fk,-fi,-fj, nx, ny);
      Make_Pixel( n+  (nbig1+i), nx, ny,-fk,-fi,-fj );
                        
      ijk2xy( fk, fi, fj, nx, ny);
      Make_Pixel( n+2*(nbig1+i), nx, ny, fk, fi, fj );
                        
      ijk2xy(-fi,-fj,-fk, nx, ny);
      Make_Pixel( n+3*(nbig1+i), nx, ny,-fi,-fj,-fk );
                        
      ijk2xy( fj, fk, fi, nx, ny);
      Make_Pixel( n+4*(nbig1+i), nx, ny, fj, fk, fi );
                        
      ijk2xy(-fj,-fk,-fi, nx, ny);
      Make_Pixel( n+5*(nbig1+i), nx, ny,-fj,-fk,-fi );

      n++;

    }

    n+=5*(nbig1+i);
    
  }
  
  k=0;
  
  for (i=0; i<nbrings; i++) { 
    
    for (j=0; j<6*(nbig1+i); j++) {
      
      fi = pki[ncentral_pixels + ngap_pixels + k];
      fj = pkj[ncentral_pixels + ngap_pixels + k];
      fk = pkk[ncentral_pixels + ngap_pixels + k];
      
      x = px[ncentral_pixels + ngap_pixels + k];
      y = py[ncentral_pixels + ngap_pixels + k];
      
      printf("%4d %10g %10g %10g %10g %10g\n",
             ncentral_pixels+ngap_pixels+k+1, fi, fj, fk, x, y);
      
      k++;
    
    }

  }
  
  return 0;
}



static void coord2hex (float cx, float cy, int &ki, int &kj, int &kk)
{
  float dx, dy, dz;
  float rx, ry, rz;
  float ax, ay, az;
  int   s;

  xy2ijk (cx, cy, dx, dy, dz);
  
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

static void ijk2xy (float ki, float kj, float kk, float &x, float &y)
{
  static const float unit = ct_PixelWidth_corner_2_corner_half;

  x = (ki*COS30 - kj*COS30) * unit;
  y = ((ki+kj)*SIN30 - kk) * unit;

  return;
}

static void xy2ijk (float cx, float cy, float &di, float &dj, float &dk)
{
  float x, y;
  static const float unit = 1.5*ct_PixelWidth_corner_2_corner_half;
  
  x = (  cx*COS30 + cy*SIN30);
  y = (- cx*SIN30 + cy*COS30);
  
  di = x / unit;
  dj = (- x*SIN30 + y*COS30) / unit;
  dk = (- x*SIN30 - y*COS30) / unit;
  
  return;
}
  
