/********************************************************************** 
 ********************************************************************** 
 * jchisto                                                               
 *                                                                      
 *   Created: Fri Dec 12 16:07:49 1997
 *   Author.: Jose Carlos Gonzalez
 *   Notes..: Generates histograms
 *
 *   WARNING: THIS FUNCTIONS ARE STILL EXPERIMENTAL
 *            THEY MAY CHANGE QUITE A LOT!!!
 ********************************************************************** 
 **********************************************************************/

/* System Header files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

#include "jchisto.h"
#include "jcmacros.h"

/***
 * jc_hcreate
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Create structure of a histogram
 *
 ***/
#ifdef JC_UNDERSCORES
int 
jc_hcreate_ ( char *name, float xmin, float xmax, int nbins )
#else // JC_NO_UNDERSCORES
int 
jc_hcreate ( char *name, float xmin, float xmax, int nbins )
#endif // JC_UNDERSCORES
{
  Histo_str *h;
  int i;

  h = (Histo_str*)malloc(sizeof(Histo_str));
  h->id = nhistos;
  h->name = name;
  h->xmin = xmin;
  h->xmax = xmax;
  h->nbins = nbins;
  h->nh = 0;
  h->ix = (xmax-xmin)/nbins;
  h->ix_2 = 0.5*h->ix;
  h->h = (float*)calloc(nbins, sizeof(float));
  h->w2 = (float*)calloc(nbins, sizeof(float));
  h->s = (float*)calloc(nbins, sizeof(float));
  h->n = (int*)calloc(nbins, sizeof(int));
  h->flags = 0;
  
  for ( i=0; i<nbins; i++) {
    h->h[i] = h->w2[i] = h->s[i] = 0.0;
    h->n[i] = 0;
  }
  
  histos[nhistos] = h;
  nhistos++;

  return (nhistos - 1);
}

/***
 * jc_hfill
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Fill in the histogram 'id'
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hfill_ ( int id, float x, float weight )
#else // JC_NO_UNDERSCORES
void 
jc_hfill ( int id, float x, float weight )
#endif // JC_UNDERSCORES
{
  int i;

  if ( (x < histos[id]->xmin) || (x > histos[id]->xmax) )
    return;
  i = (( x - histos[id]->xmin ) / histos[id]->ix);
  histos[id]->h[i] += weight;
  histos[id]->w2[i] += weight*weight;
  histos[id]->n[i]++;
}

/***
 * jc_hfill_normarea
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Normalize by the area a given histogram
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hfill_normarea_ ( int id )
#else // JC_NO_UNDERSCORES
void 
jc_hfill_normarea ( int id )
#endif // JC_UNDERSCORES
{
  int i;
  float x, w;

  for (i=0, x=histos[id]->xmin; 
       i<histos[id]->nbins; 
       i++, x+=histos[id]->ix ) 
    {    
      w = (M_PI * ( SQR(x + histos[id]->ix) - SQR(x) ));
      histos[id]->h[i] /= w;
      histos[id]->w2[i] /= w;
      histos[id]->s[i] /= w;
    }
}

/***
 * jc_hcum
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Accumulate one histogram into another
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hcum_ ( int id, int idavg )
#else // JC_NO_UNDERSCORES
void 
jc_hcum ( int id, int idavg )
#endif // JC_UNDERSCORES
{
  int i;

  for (i=0; i<histos[id]->nbins; i++) {
    histos[idavg]->h[i] += histos[id]->h[i];
    histos[idavg]->w2[i] += histos[id]->h[i]*histos[id]->h[i];
    histos[idavg]->n[i] += histos[id]->n[i];
  }
  histos[idavg]->nh += histos[id]->nh; 
  histos[idavg]->flags++;
}

/***
 * jc_hcum_getn
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Get number of accumulated histograms in a given one
 *
 ***/
#ifdef JC_UNDERSCORES
int 
jc_hcum_getn_ ( int id )
#else // JC_NO_UNDERSCORES
int 
jc_hcum_getn ( int id )
#endif // JC_UNDERSCORES
{
  return ( histos[id]->flags );
}

/***
 * jc_hcalcstat
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Calculate statistics of histograms
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hcalcstat_ ( int id )
#else // JC_NO_UNDERSCORES
void 
jc_hcalcstat ( int id )
#endif // JC_UNDERSCORES
{
  int i;
  float q;

  for (i=0; i<histos[id]->nbins; i++) {
    q = ( (float)histos[id]->n[i] / (float)(histos[id]->n[i] - 1) );
    histos[id]->s[i] = (float)sqrt(q *
				   (histos[id]->w2[i]/histos[id]->n[i] - 
				    SQR(histos[id]->h[i]/histos[id]->n[i]))); 
  }
}

/***
 * jc_hgetstruct
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Returns the pointer to the structure of a given histogram
 *
 ***/
#ifdef JC_UNDERSCORES
Histo_str *
jc_hgetstruct_ ( int id )
#else // JC_NO_UNDERSCORES
Histo_str *
jc_hgetstruct ( int id )
#endif
{
  return( histos[id] );
}

/***
 * jc_hsave
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Save the contents of the histogram in file 'file'
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hsave_ ( int id, char *file )
#else // JC_NO_UNDERSCORES
void 
jc_hsave ( int id, char *file )
#endif // JC_UNDERSCORES
{
  int i;
  FILE *fout;
  float x;

  fout = fopen(file, "wt");
  
  fprintf (fout, "# Histogram#: %d\n", id);
  fprintf (fout, "#       Name: %s\n", histos[id]->name);
  fprintf (fout, "#       Xmin: %g\n", histos[id]->xmin);
  fprintf (fout, "#       Xmax: %g\n", histos[id]->xmax);
  fprintf (fout, "#      Nbins: %d\n#\n", histos[id]->nbins);

  for (i=0, x=histos[id]->xmin+histos[id]->ix_2; 
       i<histos[id]->nbins; 
       i++, x+=histos[id]->ix ) {
    fprintf (fout, "%g %g %g %g\n", 
	     x, histos[id]->h[i], histos[id]->ix_2, histos[id]->s[i]);
  }

  fclose(fout);
}

/***
 * jc_hsave_all
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Save the contents of ALL the histograms in different files
 *            given by 'tpl' and with its identifer
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hsave_all_ ( char *tpl )
#else // JC_NO_UNDERSCORES
void 
jc_hsave_all ( char *tpl )
#endif // JC_UNDERSCORES
{
  int i, id;
  FILE *fout;
  char file[40];
  float x;

  for (id=0; id<nhistos; id++) {
    
    sprintf(file, tpl, id);
    fout = fopen(file, "wt");
  
    fprintf (fout, "# Histogram#: %d\n", id);
    fprintf (fout, "#       Name: %s\n", histos[id]->name);
    fprintf (fout, "#       Xmin: %g\n", histos[id]->xmin);
    fprintf (fout, "#       Xmax: %g\n", histos[id]->xmax);
    fprintf (fout, "#      Nbins: %d\n#\n", histos[id]->nbins);

    for (i=0, x=histos[id]->xmin; 
	 i<histos[id]->nbins; 
	 i++, x+=histos[id]->ix ) {
      fprintf (fout, "%g %g %g %g\n", 
	       x, histos[id]->h[i], histos[id]->ix_2, histos[id]->s[i]);
    }

    fclose(fout);
  }

}

/***
 * jc_hsavebin
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Save the contents of 'id' in one binary file
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hsavebin_ ( int id, char *file )
#else // JC_NO_UNDERSCORES
void 
jc_hsavebin ( int id, char *file )
#endif // JC_UNDERSCORES
{
  FILE *fout;
  Histo_str *htosave;
  char jch[10]="JCH 0.1  ";

  fout = fopen(file, "wb");
  fwrite (jch,10,1,fout);

  htosave = histos[id];
  fwrite (htosave, sizeof( Histo_str ), 1, fout);
  fwrite (htosave->h, sizeof(float), htosave->nbins, fout); 
  fwrite (htosave->w2, sizeof(float), htosave->nbins, fout); 
  fwrite (htosave->s, sizeof(float), htosave->nbins, fout); 
  fwrite (htosave->n, sizeof(int), htosave->nbins, fout); 

  fclose(fout);

}

/***
 * jc_hsavebin_all
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Save the contents of ALL the histograms in one binary file
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hsavebin_all_ ( char *file )
#else // JC_NO_UNDERSCORES
void 
jc_hsavebin_all ( char *file )
#endif // JC_UNDERSCORES
{
  int id;
  FILE *fout;
  Histo_str *htosave;
  char jch[10]="JCH 0.1  ";

  fout = fopen(file, "wb");
  fwrite (jch,10,1,fout);

  for (id=0; id<nhistos; id++) {
    
    htosave = histos[id];
    fwrite (htosave, sizeof( Histo_str ), 1, fout);
    fwrite (htosave->h, sizeof(float), htosave->nbins, fout); 
    fwrite (htosave->w2, sizeof(float), htosave->nbins, fout); 
    fwrite (htosave->s, sizeof(float), htosave->nbins, fout); 
    fwrite (htosave->n, sizeof(int), htosave->nbins, fout); 
  
  }
  fclose(fout);

}

/***
 * jc_hsaveoc_all
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Save the contents of ALL the histograms in one octave file
 *
 ***/
#ifdef JC_UNDERSCORES
void 
jc_hsaveoc_all_ ( char *file )
#else // JC_NO_UNDERSCORES
void 
jc_hsaveoc_all ( char *file )
#endif // JC_UNDERSCORES
{
  int id, i;
  FILE *fout;
  Histo_str *htosave;
  float x;
  char jch[10]="JCH 0.1  ";

  fout = fopen(file, "wt");

  for (id=0; id<nhistos; id++) {
    
    htosave = histos[id];
    fprintf (fout, "#name: %s\n", htosave->name);
    fprintf (fout, "#type: matrix\n");
    fprintf (fout, "#rows: %d\n", htosave->nbins);
    fprintf (fout, "#columns: 4\n");
    for (i=0, x=histos[id]->xmin; 
	 i<histos[id]->nbins; 
	 i++, x+=histos[id]->ix) 
      fprintf (fout, "%g %g %g %g\n", 
	       x, histos[id]->h[i], histos[id]->ix_2, histos[id]->s[i]);

  }
  fclose(fout);

}

/***
 * jc_hreadbin
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Read the contents of a file, in binary form
 *
 ***/
#ifdef JC_UNDERSCORES
int  
jc_hreadbin_ ( char *file )
#else // JC_NO_UNDERSCORES
int 
jc_hreadbin ( char *file )
#endif // JC_UNDERSCORES
{
  int i=0;
  FILE *fout;
  short space;
  char jch[10];
  Histo_str *htoread;

  fout = fopen(file, "rb");
  fread (jch,10,1,fout);

  htoread = (Histo_str*)malloc( sizeof( Histo_str ) );

  while (fread(htoread, sizeof(Histo_str), 1, fout) != NULL) {

    htoread->h = (float*)calloc(htoread->nbins, sizeof(float));
    htoread->w2 = (float*)calloc(htoread->nbins, sizeof(float));
    htoread->s = (float*)calloc(htoread->nbins, sizeof(float));
    htoread->n = (int*)calloc(htoread->nbins, sizeof(int));
    fread (htoread->h, sizeof(float), htoread->nbins, fout); 
    fread (htoread->w2, sizeof(float), htoread->nbins, fout); 
    fread (htoread->s, sizeof(float), htoread->nbins, fout); 
    fread (htoread->n, sizeof(int), htoread->nbins, fout); 
    histos[nhistos] = htoread;
    nhistos++;
    i++;

    htoread = (Histo_str*)malloc( sizeof( Histo_str ) );

  }
  free(htoread);
  
  fclose(fout);
  return ( i );
}

/***
 * jc_hremove
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Returns the pointer to the structure of a given histogram
 *
 ***/
#ifdef JC_UNDERSCORES
void
jc_hremove_ ( int id )
#else // JC_NO_UNDERSCORES
void
jc_hremove ( int id )
#endif
{
  free( histos[id] );
  nhistos--;
}

/***
 * jc_hshow
 *
 *   Created: Fri Dec 12 16:36:59 1997
 *   Author.: Jose Carlos Gonzales
 *   Purpose: Shows in the stdout the histogram (more or less)
 *
 ***/
#ifdef JC_UNDERSCORES
void
jc_hshow_ ( int id )
#else // JC_NO_UNDERSCORES
void
jc_hshow ( int id )
#endif
{
  int i;
  float max=-1.0e20, min=1.0e20, x;
  char l[60];

  for (i=0; i<histos[id]->nbins; i++) {
    max = (histos[id]->h[i] > max) ? histos[id]->h[i] : max;
    min = (histos[id]->h[i] < min) ? histos[id]->h[i] : min;
  }

  printf("%20s%10.3e%40s%10.3e\n", " ", min, " ", max);

  for (i=0, x=histos[id]->xmin; 
       i<histos[id]->nbins; 
       i++, x+=histos[id]->ix ) {
    printf ("%10.3e-%10.3e | ", 
	    x, x+histos[id]->ix);
    memset(l,0,60);
    memset(l,'#',(int)(40*(histos[id]->h[i]-min)/(max-min)));
    puts(l);
  }

}

