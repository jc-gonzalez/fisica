#ifndef MCEventHeader_Class
#define MCEventHeader_Class

#include <stdlib.h>
#include <math.h>

typedef struct _MCEventHeader MCEventHeader;

struct _MCEventHeader {

  gfloat      EvtNumber;
  gfloat      PrimaryID;
  gfloat      Etotal;   
  gfloat      Thick0;   
  gfloat      FirstTarget;
  gfloat      zFirstInt;
  gfloat      p[3];     
  gfloat      Theta; 
  gfloat      Phi; 

  gfloat      NumRndSeq;
  gfloat      RndData[10][3];
  
  gfloat      RunNumber;
  gfloat      DateRun;
  gfloat      VersionPGM;

  gfloat      NumObsLev;
  gfloat      HeightLev[10]; 

  gfloat      SlopeSpec;
  gfloat      ELowLim;   
  gfloat      EUppLim;   

  gfloat      ThetaMin;
  gfloat      ThetaMax;
  gfloat      PhiMin;
  gfloat      PhiMax;

  gfloat      CWaveLower;       
  gfloat      CWaveUpper;       
  gfloat      CorePos[2][20];   
  gfloat      TimeFirst;
  gfloat      TimeLast;
};

gint
jc_evt_read( MCEventHeader *p, FILE *f )
{
  return fread( p, sizeof(MCEventHeader), 1, f);
}

gfloat
jc_evt_core( MCEventHeader *p, gint ncore )
{
  return ( (gfloat) sqrt ( SQR(p->CorePos[0][ncore]) + 
                           SQR(p->CorePos[0][ncore]) ) );
}

#endif

