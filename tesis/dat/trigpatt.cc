#include <iostream>
#include <string>
#include <cmath>
#include <cstdio>
#include <cstdlib>

#include "trigpatt.h"

//### Trigger patterns defined.

//{

// definitions of patterns (enum types)
enum TriggerPattern_type {
  TP_default    = 8,
    
  TP_3pix_NotNN = 1,
  TP_3pix_NN    = 2,
  TP_4pix_NotNN = 4,
  TP_4pix_NN    = 8,
  TP_5pix_NotNN = 16,
  TP_5pix_NN    = 32,
  
  TP_3pix_all   = 3,
  TP_4pix_all   = 12,
  TP_5pix_all   = 48,

  TP_3or4pix    = 15,
  TP_4or5pix    = 60
};

// names used in the parameters file
const char *const TP_NAMES[] = {
  "default",
   
  "3pix_NotNN",
  "3pix_NN",
  "4pix_NotNN",
  "4pix_NN",
  "5pix_NotNN",
  "5pix_NN",
   
  "3pix_all",
  "4pix_all",
  "5pix_all",
   
  "3or4pix",
  "4or5pix"
};

const int nTP_NAMES = sizeof( TP_NAMES ) / sizeof( char* );

//}

TriggerPattern_type patts[] = {
  TP_3pix_NotNN,
  TP_3pix_NN,
  TP_4pix_NotNN,
  TP_4pix_NN,
  TP_5pix_NotNN,
  TP_5pix_NN };

int npatts = 6;

double hcx[] = {0., 1., 0.5, -0.5, -1., -0.5, 0.5};
double hcy[] = {0., 0., 0.866025, 0.866025, 0., -0.866025, -0.866025};

double hvx[] = {0., -0.866025, -0.866025, 0., 0.866025, 0.866025, 0.};
double hvy[] = {1., 0.5, -0.5, -1., -0.5, 0.5, 1.};

const double side = 5.;

void dogroup( int p );
void dofilledhex( int k );
void doemptyhex( int k );
void dohex( int k );

int px;
int py;
int incxy = 40;

int main()
{
  TriggerPattern_type patt;

  const int *ptr;

  cout << "set XMGL 0.01; set XMGR 0.01; set YMGU 0.01;set YMGL 0.01" << endl;

  for ( int i=0; i<npatts; i++ ) {

    px = 0;
    py = 100;

    cout << "exe epson " << TP_NAMES[i+1] << ".eps" << endl;
    
    cout << "size 18 6" << endl;
    cout << "null -20 220 40 120 ab" << endl;
    
    patt = patts[i];
    
    ptr = iTP_Lists[i];
    
    for ( int j=0; j<iTP_Numbers[i]; j++ ) {

      dogroup( ptr[j] );

      px += incxy;
      if (px > 200) {
        px  = 0;
        py -= incxy;
      }
      
    }

    cout << "exe epsoff" << endl;
    
    cout << "wait" << endl;
  }

  return 0;
}

void dogroup( int p )
{
  int n;
  
  for ( int k=0; k<7; k++ ) {

    n = 1 << k;

    if ( p & n ) 
      dofilledhex( k );
    else
      doemptyhex( k );

  }
  
}

void dofilledhex( int k )
{
  cout << "SET FACI 4; SET FAIS 1" << endl;
  dohex( k );
}

void doemptyhex( int k )
{
  cout << "SET FACI 1; SET FAIS 0" << endl;
  dohex( k );
}

void dohex( int k )
{
  string sx;
  string sy;

  char line[80];
  
  double cx, cy;

  cx = hcx[k] * side * 2. / 1.12;
  cy = hcy[k] * side * 2. / 1.12;

  sx = "v/cre vx(7) r ";
  sy = "v/cre vy(7) r ";

  for ( int i=0; i<7; i++ ) {

    sprintf(line, "%f ", px + cx + hvx[i] * side);
    sx = sx + line;

    sprintf(line, "%f ", py + cy + hvy[i] * side);
    sy = sy + line;

  }

  cout << sx << endl;
  cout << sy << endl;
  cout << "farea 7 vx vy" << endl << endl;

}
