/////////////////////////////////////////////////////////////////
//
// MCEventHeader
//
//  Created: Tue Apr 28 16:27:14 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for EventHeader-classes
//  Notes:   
//
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt MCEventHeader.hxx}

/* @text
This section shows the include file {\tt MCEventHeader.hxx}
@endtext */

#ifndef MCEventHeader_Class
#define MCEventHeader_Class

// @subsection Include files

// @code
#include "jctypes.h"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstdlib>
#include <cmath>

using namespace std;

#include "COREventHeader.hxx"

#define SIZE_OF_MCEVENTHEADER    110   /* floats */

// @endcode

// @subsection Class {\em MCEventHeader}: Definition

// @code
class MCEventHeader {

protected:
  Float_t      EvtNumber;
  Float_t      PrimaryID;
  Float_t      Etotal;   
  Float_t      Thick0;   
  Float_t      FirstTarget;
  Float_t      zFirstInt;
  Float_t      p[3];     
  Float_t      Theta; 
  Float_t      Phi; 

  Float_t      NumRndSeq;
  Float_t      RndData[10][3];
  
  Float_t      RunNumber;
  Float_t      DateRun;

  union {
    Float_t      VersionPGM;
    Float_t      ImpactPar;
  } impact_union;
  
  Float_t      NumObsLev;
  Float_t      HeightLev[10]; 

  Float_t      SlopeSpec;
  Float_t      ELowLim;   
  Float_t      EUppLim;   

  Float_t      ThetaMin;
  Float_t      ThetaMax;
  Float_t      PhiMin;
  Float_t      PhiMax;

  Float_t      CWaveLower;       
  Float_t      CWaveUpper;       
  Float_t      CorePos[2][20];   
  Float_t      TimeFirst;
  Float_t      TimeLast;

  Float_t      deviationPhi;
  Float_t      deviationTheta;
  
  Float_t      Trigger;      // 110 floats

public:
  MCEventHeader(void) {}  // default constructor

  virtual ~MCEventHeader(void) {}  // default destructor
  
  // reads EventHeader from binary input stream
  Int_t read ( ifstream &is ) {
    is.read ( (char *)this, mysize() ); 
    return is.gcount();
  }

  // writes EventHeader to binary output stream
  Int_t write ( ofstream &os ) {
    os.write ( (char *)this, mysize() ); 
    return 0;
  }
  
  // get information about the EventHeader

  // get the primary type (GEANT code)
  inline Float_t get_primary ( void ) { return ( PrimaryID ); }

  // get the total primary energy
  inline Float_t get_energy ( void ) { return ( Etotal ); }
  
  // get the initial zenith angle
  inline Float_t get_theta ( void ) { return ( Theta ); }

  // get the initial phi angle
  inline Float_t get_phi ( void ) { return ( Phi ); }

  // get the spectrum slope
  inline Float_t get_slope ( void ) { return ( SlopeSpec ); }

  // get height of first interaction (in cm)
  inline Float_t get_height ( void ) { return ( zFirstInt ); }

  // get the energy range of this run
  inline void get_energy_range ( Float_t *elow, Float_t *eup ) { 
    *elow = ELowLim;   
    *eup = EUppLim;
  }

  // get the core position
  inline Float_t get_core ( Float_t *x, Float_t *y, Int_t ncore = 0 ) {
    *x = CorePos[0][ncore];
    *y = CorePos[1][ncore];
    return ( (Float_t) sqrt ( (*x)*(*x) + (*y)*(*y) ) );
  }

  // get the core position
  inline Float_t get_core ( Int_t ncore = 0 ) {
    return ( (Float_t) sqrt ( ((CorePos[0][ncore])*(CorePos[0][ncore]))+
                              ((CorePos[1][ncore])*(CorePos[1][ncore])) ) );
  }

  // get the impact parameter
  inline Float_t get_impact ( void ) {
    return ( impact_union.ImpactPar );
  }

  // put the impact parameter
  inline void put_impact ( Float_t x ) {
    impact_union.ImpactPar = x;
  }

  // transport from COREventHeader to MCEventHeader
  void transport ( COREventHeader *e );

  // write extreme times
  inline void put_times ( Float_t t1, Float_t t2 ) {
    TimeFirst = t1;
    TimeLast  = t2;
  }

  // get extreme times
  inline Float_t get_times ( Float_t *t1, Float_t *t2 ) {
    *t1 = TimeFirst;
    *t2 = TimeLast;
    return ( TimeLast - TimeFirst );
  }

  // get/set trigger
  inline void set_trigger( Int_t flag ) { Trigger = (Float_t)flag; }
  inline Int_t get_trigger( void ) { return( (Int_t)Trigger ); }

  inline int mysize(void) 
    { return ( sizeof( float ) * SIZE_OF_MCEVENTHEADER ); }

  // put deviations of the CT from the original shower direction
  inline void put_deviations ( Float_t t1, Float_t t2 ) {
    deviationTheta = t1;
    deviationPhi   = t2;
    //cerr << '\n' << deviationTheta << ' ' << deviationPhi << '\n';
  }

  // get deviations of the CT from the original shower direction
  inline Float_t get_deviations ( Float_t *t1, Float_t *t2 ) {

    float ct1,st1,cp1,sp1;
    float ct2,st2,cp2,sp2;
    float x1,y1,z1;
    float x2,y2,z2;

    ct1 = cos(Theta);
    st1 = sin(Theta);
    cp1 = cos(Phi);
    sp1 = sin(Phi);

    ct2 = cos(Theta+deviationTheta);
    st2 = sin(Theta+deviationTheta);
    cp2 = cos(Phi+deviationPhi);
    sp2 = sin(Phi+deviationPhi);

    x1 = st1*cp1; y1 = st1*sp1; z1 = ct1;
    x2 = st2*cp2; y2 = st2*sp2; z2 = ct2;

    *t1 = deviationTheta;
    *t2 = deviationPhi;

    return (  acos(x1*x2 + y1*y2 + z1*z2) );
  }

  inline void print ( void ) {
    float *ptr = (float *)this;
    for(int i=0; i<SIZE_OF_MCEVENTHEADER; ++i,++ptr)
      cerr << i << ':' << *ptr << '\n';
  }

};
// @endcode

#endif  // not defined MCEventHeader_Class
