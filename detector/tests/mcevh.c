#include "mcevh.h"

int
jc_evt_read( MCEventHeader *p, FILE *f )
{
  return (fread( p, SIZE_OF_MCEVTH, 1, f));
}

float
jc_evt_core( MCEventHeader *p, int ncore )
{
  return ( (float) sqrt ( SQR(p->CorePos[0][ncore]) + 
                          SQR(p->CorePos[1][ncore]) ) );
}

