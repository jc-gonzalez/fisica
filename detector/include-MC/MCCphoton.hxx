/////////////////////////////////////////////////////////////////
//
// MCCphoton
//
//  Created: Tue Apr 28 16:43:30 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for Particles/Cherenkov photons
//  Notes:   
//
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt MCCphoton.hxx}

/* @text
This section shows the include file {\tt MCCphoton.hxx}
@endtext */

#ifndef MCCphoton_Class
#define MCCphoton_Class

// @subsection Include files

// @code
#include "jctypes.h"

#include <iostream.h>
#include <iomanip.h>
#include <fstream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "jcmacros.h"

// const char variables to use in MCCphoton::isA() function

const char FLAG_START_OF_RUN[]   = "START-RUN-START-RUN-START-RUN-START-RUN-";
const char FLAG_START_OF_EVENT[] = "START-EVENT-START-EVENT-START-EVENT-STAR";
const char FLAG_END_OF_EVENT[] =   "END-EVENT-END-EVENT-END-EVENT-END-EVENT-";
const char FLAG_END_OF_RUN[] =     "END-RUN-END-RUN-END-RUN-END-RUN-END-RUN-";
const char FLAG_END_OF_FILE[] =    "END-FILE-END-FILE-END-FILE-END-FILE-END-";
const char FLAG_END_OF_STDIN[] =   "END-STDIN-END-STDIN-END-STDIN-END-STDIN-";

#define SIZE_OF_FLAGS  40

// @endcode

// @subsection Class {\em MCCphoton}: Definition

// @code

class MCCphoton {

public:
  Float_t     w;                // wavelength [nm/-] +
                                //   1000*(id_generator)
  Float_t     x, y;             // position in the camera [cm]
  Float_t     u, v;             // director cosines 
  Float_t     t;                // time since first interaction [ns]
                                // till the camera
  Float_t     h;                // height [cm]
  Float_t     phi;              // incident angle in the camera [rad]

public:
  MCCphoton() {} // default constructor

  // overloaded constructor
  MCCphoton( ifstream &is ) { MCCphoton::read( is ); }

  virtual ~MCCphoton() {} // default destructor

  // reads photon from binary input stream
  Int_t read ( ifstream &is ) {
    is.read (  (char *)this, mysize() );
    return is.gcount();
  }

  // writes photon to binary output stream
  Int_t write ( ofstream &os ) {
    os.write ( (char *)this, mysize() );
    return 0;
  }

  // get information about the photon
  inline Float_t get_wl( void ) { return ( w - 1000.*((int)floor(w/1000.))); }
  inline Int_t get_particle( void ) { 
    return ( (int)floor(w/1000.) );
  }

  inline Float_t get_x( void ) { return ( x ); }
  inline Float_t get_y( void ) { return ( y ); }
  inline Float_t get_u( void ) { return ( u ); }
  inline Float_t get_v( void ) { return ( v ); }
  inline Float_t get_h( void ) { return ( h ); }
  inline Float_t get_t( void ) { return ( t ); }
  inline Float_t get_phi( void ) { return ( phi ); }
                  
  inline Float_t get_r( void ) { return ( sqrt( x*x + y*y ) ); }
  inline Float_t get_w( void ) { return ( sqrt( 1.0 - u*u - v*v ) ); }

  inline void fill(Float_t thew, 
                   Float_t thex, Float_t they,
                   Float_t theu, Float_t thev, 
                   Float_t theh, Float_t thet,
                   Float_t thephi) {
    w = thew;
    x = thex;
    y = they;
    u = theu;
    v = thev;
    h = theh;
    t = thet;
    phi = thephi;
  }

  int isA( const char * flag ) {
    return ( (strncmp((char *)this, flag, 8)==0) ? TRUE : FALSE );
  }

  //  inline int mysize(void) { return ( sizeof(*this) ); }
  inline int mysize(void) { return ( 40 ); }

};

// @endcode

#endif  // not defined MCCphoton_Class

