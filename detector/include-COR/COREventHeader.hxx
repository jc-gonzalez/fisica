/////////////////////////////////////////////////////////////////
//
// COREventHeader
//
//  Created: Tue Apr 28 16:27:14 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Base class for EventHeader-classes
//  Notes:   
//
/////////////////////////////////////////////////////////////////

// @T \newpage

// @section Source code of {\tt COREventHeader.hxx}

/* @text
This section shows the include file {\tt COREventHeader.hxx}
@endtext */

#ifndef COREventHeader_Class
#define COREventHeader_Class

// @subsection Include files

// @code
#include "jctypes.h"

#include <iostream.h>
#include <iomanip.h>
#include <fstream.h>
#include <stdlib.h>
#include <math.h>
// @endcode

// @subsection Class {\em COREventHeader}: Definition

// @code
class COREventHeader {

public:
  char         EVTH[4];
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
  Float_t      VersionPGM;

  Float_t      NumObsLev;
  Float_t      HeightLev[10]; 

  Float_t      SlopeSpec;
  Float_t      ELowLim;   
  Float_t      EUppLim;   

  Float_t      Ecutoffh;  
  Float_t      Ecutoffm;  
  Float_t      Ecutoffe;  
  Float_t      Ecutoffg;  

  Float_t      NFLAIN;
  Float_t      NFLDIF;
  Float_t      NFLPI0;
  Float_t      NFLPIF;
  Float_t      NFLCHE;
  Float_t      NFRAGM; 
 
  Float_t      Bx;
  Float_t      By;
  
  Float_t      EGS4yn;
  Float_t      NKGyn;
  Float_t      GHEISHAyn;
  Float_t      VENUSyn;
  Float_t      CERENKOVyn;
  Float_t      NEUTRINOyn;
  Float_t      HORIZONTyn;
  Float_t      COMPUTER;

  Float_t      ThetaMin;
  Float_t      ThetaMax;
  Float_t      PhiMin;
  Float_t      PhiMax;

  Float_t      CBunchSize;
  Float_t      CDetInX,CDetInY;
  Float_t      CSpacInX,CSpacInY;
  Float_t      CLenInX,CLenInY;
  Float_t      COutput;

  Float_t      AngleNorthX;
  Float_t      MuonInfo;

  Float_t      StepLength;
  Float_t      CWaveLower;       
  Float_t      CWaveUpper;       
  Float_t      Multipl;       
  Float_t      CorePos[2][20];   

  Float_t      dmmy1; 
  Float_t      SpinTheta; 
  Float_t      SpinPhi;   
  Float_t      dmmy2[132]; 

public:
  COREventHeader(void) {}  // default constructor

  virtual ~COREventHeader(void) {}  // default destructor
  
  // reads EventHeader from binary input stream
  Int_t read ( ifstream &is ) {
    is.read ( (char *)this, sizeof( float ) * 273 ); 
    return is.gcount();
  }

  // writes EventHeader to binary output stream
  Int_t write ( ofstream &os ) {
    os.write ( (char *)this, sizeof( float ) * 273 ); 
    return 0;
  }
  
  // get information about the EventHeader

  // print-out of the EVTH
  void print(void);
  
  // get the primary type (GEANT code)
  inline Float_t get_primary ( void ) { return ( PrimaryID ); }

  // get the total primary energy
  inline Float_t get_energy ( void ) { return ( Etotal ); }
  
  // get the initial zenith angle
  inline Float_t get_theta ( void ) { return ( Theta ); }

  // get the initial phi angle
  inline Float_t get_phi ( void ) { return ( Phi ); }

  // get the particle xi angle
  inline Float_t get_xi ( void ) { return ( SpinTheta ); }

  // get the particle spin-theta angle
  inline Float_t get_stheta ( void ) { return ( SpinTheta ); }

  // get the particle spin-phi angle
  inline Float_t get_sphi ( void ) { return ( SpinPhi ); }

  // get the particle real angles of the particle
  void get_real_direct ( Float_t *theta, Float_t *phi ) { 
    *theta = acos(cos(Theta)*cos(SpinTheta)+
                  sin(Theta)*sin(SpinTheta)*cos(fabs(SpinPhi)));
    *phi = asin(sin(SpinTheta)*sin(fabs(SpinPhi))/sin(*theta));
    if (SpinPhi<0.0)
      *phi = Phi - *phi;
    else
      *phi = Phi + *phi;
  }

  // get the spectrum slope
  inline Float_t get_slope ( void ) { return ( SlopeSpec ); }

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

  // show the seeds in the console
  inline void print_seeds ( void ) {
    Int_t i;
    cout << "Seeds status at the beginning of this shower:" 
         << setiosflags(ios::left) << setw(10) << endl;
    for (i=0; i<NumRndSeq; ++i) {
      cout << "SEED       " 
           << RndData[i][0]
           << RndData[i][1]
           << RndData[i][2] << endl;
    }
    cout << resetiosflags(ios::left);
  }

  // fill essential information
  inline void fill ( Float_t theEvtNumber,
             Float_t thePrimaryID,
             Float_t theEtotal,   
             Float_t theFirstTarget,
             Float_t thezFirstInt,
             Float_t thepx, 
             Float_t thepy, 
             Float_t thepz, 
             Float_t theTheta, 
             Float_t thePhi,
             Float_t theX, 
             Float_t theY) {
             
    strcpy(EVTH,"EVTH");
    EvtNumber = theEvtNumber; 
    PrimaryID = thePrimaryID;
    Etotal    = theEtotal;
    FirstTarget = theFirstTarget;
    zFirstInt = thezFirstInt;
    p[0] = thepx;      
    p[1] = thepy;      
    p[2] = thepz;      
    Theta = theTheta;   
    Phi = thePhi;      
    Thick0 = 0.0;        
    CorePos[0][0] = theX;
    CorePos[1][0] = theY;

    return; 
  }

};
// @endcode

#endif  // not defined COREventHeader_Class
