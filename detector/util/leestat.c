/*----------------------------------------------------------------------
   Lee fichero de estadisticas de CORSIKA 5.20 modificado
----------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAXBUF 273	/* 39*7 */
#define NSUBBL 21
#define NPLONG 1041

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
main (short argc, char **argv)
{
  char path[100];
  int i, k;

  float evth[MAXBUF], evte[MAXBUF];
  int nproto[10];
  int nprotb[10];
  int nneutr[10];
  int nneutb[10];
  int nphoto[10];
  int nelect[10];
  int nposit[10];
  int nnu[10];
  int nmum[10];
  int nmup[10];
  int npi0[10];
  int npim[10];
  int npip[10];
  int nk0l[10];
  int nk0s[10];
  int nkmi[10];
  int nkpl[10];
  int nhyp[10];
  int ndeut[10];
  int ntrit[10];
  int nalpha[10];
  int nother[10];
  int ifinnu, ifinpi, ifinet, ifinka, ifinhy, cerele, cerhad;
  int lpct1, nstep;
  float thstep, plong[9][NPLONG];
  float aplong[9][NPLONG], splong[9][NPLONG];
  int nshw, nfirst, nlast;
  float bunch;

  strcpy (path, argv[1]);
  nfirst = atoi (argv[2]);
  nlast = atoi (argv[3]);
  bunch = atof (argv[4]);

  for (k = 0; k < 9; k++)
    for (i = 0; i < nstep; i++)
      aplong[k][i] = splong[k][i] = 0.0;

  for (nshow = nfirst; nshow <= nlast; nshow++) {

    sprintf (stafile, "%s/sta%06d", path, nshow);
    if ((sttape = fopen (stafile, "rb")) == NULL) {
      printf ("JCIO:: Cannot open STA file %s. Exiting.\n", stafile);
      exit (1);
    }

    printf ("Leyendo %s . . .\n", stafile);

    fread (evth, sizeof (float) * MAXBUF, 1, sttape);
    fread (evte, sizeof (float) * MAXBUF, 1, sttape);

    fread (&timefirst, sizeof (float), 1, sttape);
    fread (&timelast, sizeof (float), 1, sttape);

    printf ("Time spread: %f ns to %f ns = %f ns\n",
	    timefirst, timelast, timelast - timefirst);

    for (i = 0; i < 10; i++) {
      fread ((nproto + i), sizeof (int), 1, sttape);
      fread ((nprotb + i), sizeof (int), 1, sttape);
      fread ((nneutr + i), sizeof (int), 1, sttape);
      fread ((nneutb + i), sizeof (int), 1, sttape);
      fread ((nphoto + i), sizeof (int), 1, sttape);
      fread ((nelect + i), sizeof (int), 1, sttape);
      fread ((nposit + i), sizeof (int), 1, sttape);
      fread ((nnu + i), sizeof (int), 1, sttape);
      fread ((nmum + i), sizeof (int), 1, sttape);
      fread ((nmup + i), sizeof (int), 1, sttape);
      fread ((npi0 + i), sizeof (int), 1, sttape);
      fread ((npim + i), sizeof (int), 1, sttape);
      fread ((npip + i), sizeof (int), 1, sttape);
      fread ((nk0l + i), sizeof (int), 1, sttape);
      fread ((nk0s + i), sizeof (int), 1, sttape);
      fread ((nkmi + i), sizeof (int), 1, sttape);
      fread ((nkpl + i), sizeof (int), 1, sttape);
      fread ((nhyp + i), sizeof (int), 1, sttape);
      fread ((ndeut + i), sizeof (int), 1, sttape);
      fread ((ntrit + i), sizeof (int), 1, sttape);
      fread ((nalpha + i), sizeof (int), 1, sttape);
      fread ((nother + i), sizeof (int), 1, sttape);

      /*      
         printf ("%i %i %i\n", *(nproto + i), *(nphoto + i), *(nelect + i));
       */
    }

    fread (&ifinnu, sizeof (int), 1, sttape);
    fread (&ifinpi, sizeof (int), 1, sttape);
    fread (&ifinet, sizeof (int), 1, sttape);
    fread (&ifinka, sizeof (int), 1, sttape);
    fread (&ifinhy, sizeof (int), 1, sttape);
    fread (&cerele, sizeof (int), 1, sttape);
    fread (&cerhad, sizeof (int), 1, sttape);
    /*
       printf ("%i %i %i %i %i\n%i %i\n\n",
       ifinnu, ifinpi, ifinet, ifinka, ifinhy, cerele, cerhad);
     */

    fread (&lpct1, sizeof (int), 1, sttape);
    fread (&nstep, sizeof (int), 1, sttape);
    fread (&thstep, sizeof (float), 1, sttape);

    /*
       printf ("%i %i %f\n\n",
       lpct1, nstep, thstep);
     */

    for (k = 0; k < 9; k++)
      for (i = 0; i < nstep; i++)
	fread (&plong[k][i], sizeof (float), 1, sttape);

    for (k = 0; k < 9; k++)
      for (i = 0; i < nstep; i++) {
	aplong[k][i] += plong[k][i] * bunch;
	splong[k][i] += plong[k][i] * plong[k][i] * bunch * bunch;
      }

    fclose (sttape);
  }

  nshw = (nlast - nfirst + 1);
  for (k = 0; k < 9; k++)
    for (i = 0; i < nstep; i++) {
      if (nshw > 1)
	splong[k][i] = sqrt ((splong[k][i] -
			      aplong[k][i] * aplong[k][i] / nshw)
			     / (nshw - 1));
      else
	splong[k][i] = sqrt (aplong[k][i]);
      aplong[k][i] /= nshw;
    }

  for (i = 0; i < nstep; i++) {
    printf ("%8.2f ", thstep * (float) i);
    for (k = 0; k < 9; k++)
      printf ("%6i +- %4i ", (int) aplong[k][i], (int) splong[k][i]);
    puts ("");
  }

}
