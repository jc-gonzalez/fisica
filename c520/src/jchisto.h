/********************************************************************** 
 ********************************************************************** 
 * jchisto                                                               
 *                                                                      
 *   Created: Fri Dec 12 16:07:49 1997
 *   Author.: Jose Carlos Gonzales
 *   Notes..: Generates histograms
 *                                                                      
 ********************************************************************** 
 **********************************************************************/

#ifndef __JCHISTO__
#define __JCHISTO__

typedef struct {
  char *name;
  int id;
  float xmin, xmax, ix, ix_2;
  float *h, *w2, *s;
  int nbins, *n, nh;
  int flags;
} Histo_str;

Histo_str *histos[100];
static int nhistos = 0;

#ifdef JC_UNDERSCORES

int jc_hcreate_ ( char *name, float xmin, float xmax, int nbins );
void jc_hfill_ ( int id, float x, float weight );
void jc_hfill_normarea_ ( int id );
void jc_hcum_ ( int id, int idacum );
int jc_hcum_getn_ ( int id );
void jc_hcalcstat_ ( int id );
void jc_hsave_ ( int id, char *file );
void jc_hsave_all_ ( char *tpl );
void jc_hsavebin_ ( int id, char *file );
void jc_hsavebin_all_ ( char *file );
void jc_hsaveoc_all_ ( char *file );
int jc_hreadbin_ ( char *file );
Histo_str * jc_hgetstruct_ ( int id );
void jc_hremove_ ( int id );
void jc_hshow_ ( int id );

#else // JC_NO_UNDERSCORES

int jc_hcreate ( char *name, float xmin, float xmax, int nbins );
void jc_hfill ( int id, float x, float weight );
void jc_hfill_normarea ( int id );
void jc_hcum ( int id, int idacum );
int jc_hcum_getn ( int id );
void jc_hcalcstat ( int id );
void jc_hsave ( int id, char *file );
void jc_hsave_all ( char *tpl );
void jc_hsavebin ( int id, char *file );
void jc_hsavebin_all ( char *file );
void jc_hsaveoc_all ( char *file );
int jc_hreadbin ( char *file );
Histo_str * jc_hgetstruct ( int id );
void jc_hremove ( int id );
void jc_hshow ( int id );

#endif // JC_UNDERSCORES

#endif // __JCHISTO__
