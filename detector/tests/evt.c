/*
 * evt.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "evt.h"

static int fileIsOpen=FALSE;
static char fileName[1000];
static float phe[1000];
static FILE *fileHandle;
static fpos_t *filePositions = NULL;
static int numShowers;
static int numPixels = 919;
static MCEventHeader MCevth;
static int Read_All_Phe=FALSE;

int
pre_read_file( char *fname )
{
  char sign[11];
  int n, shw;
  MCEventHeader mcevth;
  fpos_t *p1, *p2, p;
  register int i;
  int max, min, number;
  
  printf("%d\n", sizeof(mcevth));
  if (filePositions!=NULL)
    free(filePositions);

  if ( (fileHandle = fopen(fname, "rb")) == NULL ) {
    perror( "pre_read_file" );
    exit(1);
  }

  strcpy( fileName, fname );

  numShowers = 100;
  p1 = (fpos_t *) calloc( numShowers, sizeof(fpos_t));

  fread( sign, 11, 1, fileHandle);

  printf("SIGNATURE = [%s]\n", sign);

  n=0; 
  shw=0; 
  while ( !feof( fileHandle ) ) {

    fgetpos(fileHandle, &p);
    fread( &mcevth, sizeof(MCEventHeader), 1, fileHandle);

    ++shw;

    /*
    if ((shw % 10) == 0) 
      printf("\rReading shower %6d (E = %10.2f GeV)\n",
             shw, mcevth.Etotal);
    */

    /* has it pixels information? */
    if ((mcevth.Trigger < 0.5) && (!Read_All_Phe))
      continue;

    p1[n] = p;
    fread( phe, sizeof(float) * numPixels, 1, fileHandle);

    max=-1;min=9999;
    for (i=0; i<numPixels; ++i) {
      number = (int)phe[i];
      if (number>max) max = number;
      if (number<min) min = number;
    }

    ++n;
    
    if (n == numShowers) {
      numShowers +=100;
      p2 = realloc( p1, numShowers * sizeof(fpos_t));
      free(p1);
      p1 = p2;
    }
    
  }


  numShowers = n;
  filePositions = realloc( p1, numShowers * sizeof(fpos_t));
  free(p1);

  printf("There are %d showers in %s\n", shw, fname);
  printf("There are %d triggered showers.\n", numShowers);

  return( numShowers );
}

void 
file_locate( float value )
{
  int nshw;

  nshw = (int)value;
  if (nshw<1)
    nshw=1;
  if (nshw>numShowers)
    nshw=numShowers;
  --nshw;

  fsetpos( fileHandle, &filePositions[nshw]);
}

void
file_get_data( float *nphe )
{
  fread( &MCevth, sizeof(MCEventHeader), 1, fileHandle);
  /*  printf("ENERGY: %g\tCORE: (%g,%g)\tWL: (%g,%g)\tHeightLev: %g\n",
         MCevth.Etotal,
         MCevth.CorePos[0][0], 
         MCevth.CorePos[1][0],
         MCevth.CWaveLower,
         MCevth.CWaveUpper,
         MCevth.HeightLev[0]);
  */
  fread( nphe, numPixels * sizeof(float), 1, fileHandle);
}

MCEventHeader *
get_mcevth(void)
{
  return( &MCevth );
}

void set_read_all_phe(int flag)
{
  Read_All_Phe = flag;
}
