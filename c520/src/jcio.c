/*----------------------------------------------------------------------
  jcio.c : 

Rutinas para la gestion de ficheros en CORSIKA 5.20 Las posibilidades
que contempla son: 

- Creacion de un fichero de inicio de run: run###### 
- Creacion de un fichero de fin de run: end###### 
- Creacion de fichero separados para cada cascada, tanto para
  particulas como para Cherenkov. Ademas, para la salida Cherenkov se
  genera en ficheros separados en caso de que se reutilice la cascada.
  Los fichero son por tanto: cer######.# y dat######, donde el indice
  del fichero cer va de 0 a 9.
- La grabacion se realiza en C, por lo que no existen marcas de inicio
  ni final de bloque en los ficheros.
----------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>

#define MAXBUF 273  /*- 39*7 -*/
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

/*--------------------------------------------------
  jcinitio: 
  graba un bloque al fichero de particulas 
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcinitio_ (char *path, int *runnum)
#else /* JC_NO_UNDERSCORES */
void
jcinitio (char *path, int *runnum)
#endif /* JC_UNDERSCORES */
{
  register int i = 0;

  while (*(path + (++i)) != ' ');
  strncpy (tpl, path, i);
  if (*(path + i - 1) != '/')
    strcat (tpl, "/");

  strcpy (certpl, tpl);
  strcat (certpl, "cer%06d");
  strcpy (dattpl, tpl);
  strcat (dattpl, "dat%06d");
  strcpy (statpl, tpl);
  strcat (statpl, "sta%06d");

  sprintf (runfile, "%srun%06d", tpl, *runnum);
  sprintf (endfile, "%send%06d", tpl, *runnum);
}

/*--------------------------------------------------
  jcdatsave: 
  graba un bloque al fichero de particulas 
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcdatsave_ (float *outbuf)
#else /* JC_NO_UNDERSCORES */
void
jcdatsave (float *outbuf)
#endif /* JC_UNDERSCORES */
{
  /* puts("JCIO:: saving particle buffer..."); */
  fwrite (outbuf, sizeof (float) * MAXBUF * NSUBBL, 1, patape);
}

/*--------------------------------------------------
  jccersave: 
  graba un bloque al fichero de particulas 
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jccersave_ (float *outbuf)
#else /* JC_NO_UNDERSCORES */
void
jccersave (float *outbuf)
#endif /* JC_UNDERSCORES */
{
  /* puts("JCIO:: saving cerenkov buffer..."); */
  fwrite (outbuf, sizeof (float) * MAXBUF * NSUBBL, 1, cetape);
}

/*--------------------------------------------------
  jcstartrun:
  abre el fichero run###### y graba el contenido
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcstartrun_ (float *runh)
#else /* JC_NO_UNDERSCORES */
void
jcstartrun (float *runh)
#endif /* JC_UNDERSCORES */
{
  if ((runtape = fopen (runfile, "wb")) == NULL) {
    printf ("JCIO:: Cannot open RUN file %s. Exiting.\n", runfile);
    exit (1);
  }
  /* puts("JCIO:: saving runheader buffer..."); */
  fwrite (runh, sizeof (float) * MAXBUF, 1, runtape);
}

/*--------------------------------------------------
  jcendrun:
  abre el fichero run###### y graba el contenido
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcendrun_ (float *rune)
#else /* JC_NO_UNDERSCORES */
void
jcendrun (float *rune)
#endif /* JC_UNDERSCORES */
{
  fwrite (rune, sizeof (float) * MAXBUF, 1, runtape);
  fclose (runtape);
}

/*--------------------------------------------------
  jcnewcerfile:
  abre un nuevo fichero Cherenkov
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcnewcerfile_ ()
#else /* JC_NO_UNDERSCORES */
void
jcnewcerfile ()
#endif /* JC_UNDERSCORES */
{
  sprintf (cerfile, certpl, nshow);
  if ((cetape = fopen (cerfile, "wb")) == NULL) {
    printf ("JCIO:: Cannot open CER file %s. Exiting.\n", cerfile);
    exit (1);
  }
  timefirst = 9.0e10;
  timelast = -9.0e10;
}

/*--------------------------------------------------
  jcnewdatfile:
  abre un nuevo fichero de particulas
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcnewdatfile_ (void)
#else /* JC_NO_UNDERSCORES */
void
jcnewdatfile (void)
#endif /* JC_UNDERSCORES */
{
  sprintf (datfile, dattpl, nshow);
  if ((patape = fopen (datfile, "wb")) == NULL) {
    printf ("JCIO:: Cannot open DAT file %s. Exiting.\n", datfile);
    exit (1);
  }
}

/*--------------------------------------------------
  jcnewstafile:
  abre un nuevo fichero de estadisticas
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcnewstafile_ (void)
#else /* JC_NO_UNDERSCORES */
void
jcnewstafile (void)
#endif /* JC_UNDERSCORES */
{
  sprintf (stafile, statpl, nshow);
  if ((sttape = fopen (stafile, "wb")) == NULL) {
    printf ("JCIO:: Cannot open STA file %s. Exiting.\n", stafile);
    exit (1);
  }
}

/*--------------------------------------------------
  jcnewshower:
  abre nuevos ficheros para la nueva cascada
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcnewshower_ (void)
#else /* JC_NO_UNDERSCORES */
void
jcnewshower (void)
#endif /* JC_UNDERSCORES */
{
  if (nshow > 0) {
    fclose (patape);
    fclose (cetape);
    fclose (sttape);
  }

  nshow++;
#ifdef JC_UNDERSCORES
  jcnewdatfile_ ();
  jcnewcerfile_ ();
  jcnewstafile_ ();
#else /* JC_NO_UNDERSCORES */
  jcnewdatfile ();
  jcnewcerfile ();
  jcnewstafile ();
#endif /* JC_UNDERSCORES */
}

/*--------------------------------------------------
  jcenddata:
  abre el fichero run###### y graba el contenido
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcenddata_ (float *runh, float *rune)
#else /* JC_NO_UNDERSCORES */
void
jcenddata (float *runh, float *rune)
#endif /* JC_UNDERSCORES */
{
  if ((endtape = fopen (endfile, "wb")) == NULL) {
    printf ("JCIO:: Cannot open END file %s. Exiting.\n", endfile);
    exit (1);
  }
  fwrite (runh, sizeof (float) * MAXBUF, 1, endtape);
  fwrite (rune, sizeof (float) * MAXBUF, 1, endtape);
  fclose (endtape);
}

/*--------------------------------------------------
  jctime:
  va echando cuentas del tiempo de los fotones
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jctime_ (float *cartim)
#else /* JC_NO_UNDERSCORES */
void
jctime (float *cartim)
#endif /* JC_UNDERSCORES */
{
  if (*cartim>timelast) timelast = *cartim;
  if (*cartim<timefirst) timefirst = *cartim;
}

/*--------------------------------------------------
  jcstadata:
  graba las estadisticas de la cascada
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcstadata_ (float *evth, float *evte,
	    double *nproto, double *nprotb, double *nneutr,
	    double *nneutb, double *nphoto, double *nelect,
	    double *nposit, double *nnu, double *nmum,
	    double *nmup, double *npi0, double *npim, double *npip,
	    double *nk0l, double *nk0s, double *nkmi, double *nkpl,
	    double *nhyp, double *ndeut, double *ntrit, double *nalpha,
	    double *nother, int *ifinnu, int *ifinpi,
	    int *ifinet, int *ifinka, int *ifinhy,
	    double *cerele, double *cerhad,
	    double *plong, int *lpct1, int *nstep, double *thstep)
#else /* JC_NO_UNDERSCORES */
void
jcstadata (float *evth, float *evte,
	    double *nproto, double *nprotb, double *nneutr,
	    double *nneutb, double *nphoto, double *nelect,
	    double *nposit, double *nnu, double *nmum,
	    double *nmup, double *npi0, double *npim, double *npip,
	    double *nk0l, double *nk0s, double *nkmi, double *nkpl,
	    double *nhyp, double *ndeut, double *ntrit, double *nalpha,
	    double *nother, int *ifinnu, int *ifinpi,
	    int *ifinet, int *ifinka, int *ifinhy,
	    double *cerele, double *cerhad,
	    double *plong, int *lpct1, int *nstep, double *thstep)
#endif /* JC_UNDERSCORES */ 
{
  register int i, ii, k;
  int np[22];
  float f;

  fwrite (evth, sizeof (float) * MAXBUF, 1, sttape);
  fwrite (evte, sizeof (float) * MAXBUF, 1, sttape);

  fwrite (&timefirst, sizeof (float), 1, sttape);
  fwrite (&timelast, sizeof (float), 1, sttape);

  for (i = 0; i < 10; i++) {
    np[0] = (int) (*(nproto + i));
    np[1] = (int) (*(nprotb + i));
    np[2] = (int) (*(nneutr + i));
    np[3] = (int) (*(nneutb + i));
    np[4] = (int) (*(nphoto + i));
    np[5] = (int) (*(nelect + i));
    np[6] = (int) (*(nposit + i));
    np[7] = (int) (*(nnu + i));
    np[8] = (int) (*(nmum + i));
    np[9] = (int) (*(nmup + i));
    np[10] = (int) (*(npi0 + i));
    np[11] = (int) (*(npim + i));
    np[12] = (int) (*(npip + i));
    np[13] = (int) (*(nk0l + i));
    np[14] = (int) (*(nk0s + i));
    np[15] = (int) (*(nkmi + i));
    np[16] = (int) (*(nkpl + i));
    np[17] = (int) (*(nhyp + i));
    np[18] = (int) (*(ndeut + i));
    np[19] = (int) (*(ntrit + i));
    np[20] = (int) (*(nalpha + i));
    np[21] = (int) (*(nother + i));
    fwrite (np, sizeof (int) * 22, 1, sttape);
  }

  np[0] = (int) (*ifinnu);
  np[1] = (int) (*ifinpi);
  np[2] = (int) (*ifinet);
  np[3] = (int) (*ifinka);
  np[4] = (int) (*ifinhy);
  np[5] = (int) (*cerele);
  np[6] = (int) (*cerhad);
  fwrite (np, sizeof (int) * 7, 1, sttape);

  fwrite (lpct1, sizeof (int), 1, sttape);
  fwrite (nstep, sizeof (int), 1, sttape);

  f = (float) (*thstep);
  fwrite (&f, sizeof (float), 1, sttape);

  for (k=0;k<9;k++) 
    for (i = 0; i < *nstep; i++) {
      f = (float)(*(plong + i + k*NPLONG));
      fwrite (&f, sizeof (float), 1, sttape);
    }


}
