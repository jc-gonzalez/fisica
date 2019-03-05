/////////////////////////////////////////////////////////////////
//
// CORParticle
//
//  Created: Tue Apr 28 16:43:30 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for Particles/Cherenkov photons
//  Notes:   
//
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt CORParticle.hxx}

/* @text
This section shows the include file {\tt CORParticle.hxx}
@endtext */

#ifndef CORParticle_Class
#define CORParticle_Class

// @subsection Include files

// @code
#include "jctypes.h"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdlib>
#include <cmath>
// @endcode

using namespace std;

// @subsection Class {\em CORParticle}: Definition

// @code
class CORParticle {

public:
  Float_t     w;                // wavelength/type of particle [nm/-]
  Float_t     x, y;             // position [cm]
  Float_t     u, v;             // director cosines 
  Float_t     t;                // time since first interaction [ns]
  Float_t     h;                // height [cm]

public:
  CORParticle() {} // default constructor

  // overloaded constructor
  CORParticle( ifstream &is ) { CORParticle::read( is ); }

  // overloaded constructor
  CORParticle(Float_t thew, Float_t thex, Float_t they, 
          Float_t theu, Float_t thev, Float_t thet, Float_t theh) 
    {
      w = thew;
      x = thex;
      y = they;
      u = theu;
      v = thev;
      t = thet;
      h = theh;
    }

  virtual ~CORParticle() {} // default destructor

  // reads photon from binary input stream
  Int_t read ( ifstream &is ) {
    int n;
    is.read ( (char *)this, 7 * sizeof( Float_t ) );   // 
    return is.gcount();
  }

  // writes photon to binary output stream
  Int_t write ( ofstream &os ) {
    os.write ( (char *)this, 7 * sizeof( Float_t ) );
    return 0;
  }

  // fill information
  inline void fill(Float_t thew, Float_t thex, Float_t they, 
		   Float_t theu, Float_t thev, Float_t thet, Float_t theh) 
    {
      w = thew;
      x = thex;
      y = they;
      u = theu;
      v = thev;
      t = thet;
      h = theh;
    }

  // get information about the photon
  inline Float_t get_wl( void ) { 
    return ( (w>1.0) ? 
             w - 1000.*((int)floor(w/1000.)) :
             0.0 ); 
  }
  inline Float_t get_id( void ) { return ( w ); }
  inline Int_t get_particle( void ) { 
    return ( (int)floor(w/1000.) );
  }
  inline Float_t get_x( void ) { return ( x ); }
  inline Float_t get_y( void ) { return ( y ); }
  inline Float_t get_u( void ) { return ( u ); }
  inline Float_t get_v( void ) { return ( v ); }
  inline Float_t get_h( void ) { return ( h ); }
  inline Float_t get_t( void ) { return ( t ); }
                  
  inline Float_t get_r( void ) { return ( sqrt( x*x + y*y ) ); }
  inline Float_t get_w( void ) { return ( sqrt( 1.0 - u*u - v*v ) ); }
};

// @endcode 

#endif  // not defined CORParticle_Class

