/* prevent multiple includes
 */
#ifndef CTSHBOOK_H
#define CTSHBOOK_H 1


#ifndef __cplusplus

/* we're not dealing with C++
 * if true was not defined before define boolean stuff
 */
#ifndef true
typedef int bool;
#define true 1
#define false 0
#endif

#endif


/* create symbolic links onto hbook-files; this link is in lower case
 * letters only and is used instead of the hbook-file name
 * (this is done because hbook cannot handle file names with
 *  upper case letters in)
 */
#ifdef USE_LINKS

/* template used to create link names;
 * the first link starts with number iLINK_NR + 1
 */
#define cLINKNAME      "ctshbook_link%d.tmp"
#define iLINK_START_NR 0
#endif



/*
 *            STRUCTURES
 *
 */


/* contains access data of hbook file
 * (some variables are only used for ntuples)
 */
typedef struct
{
  char *pname;             /* name of hbook-file              */
  char *pvarnam;           /* array containing variable names */
  char copt;               /* access mode                     */
  int ilrec;               /* record length of hbook-file     */
  int ilun;                /* lun number of hbook-file        */
  int iid;                 /* id number of the Ntuple         */
  int invar;               /* number of variables for ntuples */
                           /* number of histograms otherwise  */
  int inr_events;          /* number of events                */
  float *pevtdat;          /* array containing event data     */
  float *prhigh;           /* array containing max values     */
  float *prlow;            /* array containing min values     */
} HBOOK_FILE;


/* structure to ease histogram booking
 */
typedef struct
{
  char *phname;
  int  iid;
  int  ixbin;
  int  iybin;
  float fxlow;
  float fxhigh;
  float fylow;
  float fyhigh;
  float fvmx;
} HISTO;


/*
 * declaration of some fortran functions
 *
 */

/* hbook
 */
extern void cts_hrout (int, int, char *);
extern void cts_hcdir (char *, char *);
extern void cts_hfill (int, float, float, float);
extern void cts_hrend (HBOOK_FILE *);
extern void cts_hgive (HISTO *);
extern void cts_hreopen (HBOOK_FILE *);
extern void cts_hgnf (HBOOK_FILE *, int);
extern void cts_hfn (HBOOK_FILE *);
extern void cts_hpak (int, float *);
extern void cts_hpakad (int, float *);
extern void cts_hpake (int, float *);
extern void cts_hunpak (int, float *, char *, int);
extern void cts_hunpke (int, float *, char *, int);
extern void cts_hfpak1 (int, int, float *, int);
extern void cts_hopera (int, char *, int, int, float, float);
extern void cts_hreset (int, char *);
extern void cts_hdelet (int);
extern void cts_hcopy (int, int, char *);
extern void cts_htitle (char *);

extern float cts_hxy (int, float, float);
extern float cts_hstati (int, int, char *, int);


/* others
 */
extern void cts_vinit_hbook ();
extern void cts_vopen_hbook_file (HBOOK_FILE *);
extern void cts_vbook_hist (HISTO *);
extern char *cts_pget_varnam_str (char *[], int);
extern float *cts_pset_nam (HBOOK_FILE *, char *);
extern float cts_fget_max  (HBOOK_FILE *, char *);
extern float cts_fget_min  (HBOOK_FILE *, char *);
extern void cts_vkuopen (int *, char *, char *);
extern void cts_vkuclos (int , char *);


/*
 * defines
 *
 */
/* default size of pawc common block
 */
#define iNWPAWC  10000000

/* max number of histograms (including ntuples) in memory
 */
#define iMAX_HIST_NR 1000

/* max length of an Ntuple/histogram name -- according to hbook examples
*/
#define iTITLELEN 80

/* max name length for variables; 8 in HBOOK
 * never change it to 0; it is used as Divisor
 */
#define iVARNAMELEN 8

/* default record-length for ntuples
 */
#define iNTUPLE_DEF_RL 8192

/* default record-length for histograms
 */
#define iHISTO_DEF_RL 1024


/*
 * declaration of global variables
 *
 */
/* Common Block size for hbook routines; must be global
 */
extern int pawc_[];


#endif /* CTSHBOOK_H */
