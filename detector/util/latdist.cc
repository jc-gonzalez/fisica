/*----------------------------------------------------------------------
   Lee fichero de estadisticas de CORSIKA 5.20 modificado
----------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define __MAIN__

#include "jchisto.h"

#define MAXBUF 273	/* 39*7 */
#define NSUBBL 21

#define boolean                 short

#define TRUE                    1
#define FALSE                   0
#define YES                     1.
#define NO                      0.

#define SQR(x)                  ((x)*(x))
#define ABS(x)                  (((x)<0)? -(x) : (x))
#define SGN(x)                  (((x)>0.)? 1. : -1.)
#define MAX(a,b)                (((a)>(b))?(a):(b))
#define MIN(a,b)                (((a)<(b))?(a):(b))
#define DEG2RAD                 (M_PI/180.)

static int nshow = 0;
static float timefirst = 0.;
static float timelast = 0.;

FILE *cetape;
FILE *patape;
FILE *sttape;
FILE *runtape;
FILE *endtape;
char tpl[100];
char certpl[100];
char dattpl[100];
char statpl[100];
char cerfile[100];
char datfile[100];
char stafile[100];
char runfile[100];
char endfile[100];

void
main (int argc, char **argv)
{
  char path[100];
  int i, k;
  float evth[MAXBUF], evte[MAXBUF];
  float plat[9][250];
  float aplat[9][250], splat[9][250];
  int nshw, nfirst, nlast;
  struct {
    float data, x, y, u, v, t, h;
  } cph;
  float wl, nct, nshift;
  float r;

  int hid, hid0;
  char fname[60]; 

  strcpy (path, argv[1]);
  nfirst = atoi (argv[2]);
  nlast = atoi (argv[3]);

/*   HLimit (HBOOKW); */
/*   HROpen (25, "LATDIST", "latdist.hbook", "N"); */
/*   HBook1 (100000, "Density of Photons", 50, 0., 250.); */

  hid0 = jc_hcreate("Lateral Distribution Acumulada", 0., 250., 50);

  for (nshow = nfirst; nshow <= nlast; nshow++) {

    sprintf (cerfile, "%s/cer%06d", path, nshow);
    if ((cetape = fopen (cerfile, "rb")) == NULL) {
      printf ("Cannot open CER file %s. Exiting.\n", cerfile);
      exit (1);
    }

    printf ("Leyendo %s . . .\n", cerfile);

    hid = jc_hcreate("Lateral Distribution", 0., 250., 50);

    fread (evth, sizeof (float) * MAXBUF, 1, cetape);

    cph.data = 999.;
    while ((cph.data > 0.) &&
	   (fread (&cph, sizeof (float) * 7, 1, cetape) == 1)) {

      nct = floor (cph.data / 100000.);
      nshift = floor (cph.data / 1000.) - nct * 100000.;
      wl = cph.data - nct * 100000. - nshift * 1000.;

/*      printf ("%f %d %d ", wl, (int) nct, (int) nshift); */
/*      printf ("%f %f %f %f %f %f\n",
	      cph.x, cph.y, cph.u, cph.v, cph.t, cph.h);
*/
      r = (float)sqrt(SQR(cph.x)+SQR(cph.y))*.01;
      jc_hfill(hid, r, 1.); 

    }

    fclose (cetape);

    sprintf( fname, "lat%06d", hid);
    jc_hcalcstat(hid);
    jc_hcum(hid,hid0); 
    printf ("We have already %d histograms accumulated.\n", 
	    jc_hcum_getn(hid0));
    jc_hfill_normarea(hid); 
    jc_hsave(hid, fname);

  }

  jc_havgcalcstat(hid0);
  jc_hfill_normarea(hid0); 
  jc_hsave(hid0, "lat000000");
/*  HClose ("LATDIST"); */
  puts ("done\n\n");

}
