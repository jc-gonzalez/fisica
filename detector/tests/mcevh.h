#ifndef __MCEVH_H__
#define __MCEVH_H__

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "jcmacros.h" 

typedef float  Float_t;

typedef struct _MCEventHeader {

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

} MCEventHeader;

#define SIZE_OF_MCEVTH    (110 * sizeof(float))

int jc_evt_read( MCEventHeader *p, FILE *f );
float jc_evt_core( MCEventHeader *p, int ncore );


#endif





