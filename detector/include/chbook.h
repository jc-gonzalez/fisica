/* prevent multiple includes
 */
#ifndef CHBOOK_H
#define CHBOOK_H 1

#include "mydefs.h"


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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* template used to create link names;
 * the first link starts with number iLINK_NR + 1
 */
#define cLINKNAME      "chbook_link%d.tmp"
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
extern void hrout (int, char *);
extern void hcdir (char *, char *);
extern void hfill (int, float, float, float);
extern void hrend (HBOOK_FILE *);
extern void hgive (HISTO *);
extern void hreopen (HBOOK_FILE *);
extern void hgnf (HBOOK_FILE *, int);
extern void hfn (HBOOK_FILE *);
extern void hpak (int, float *);
extern void hpakad (int, float *);
extern void hpake (int, float *);
extern void hunpak (int, float *, char *, int);
extern void hunpke (int, float *, char *, int);
extern void hfpak1 (int, int, float *, int);
extern void hopera (int, char *, int, int, float, float);
extern void hreset (int, char *);
extern void hdelet (int);
extern void hcopy (int, int, char *);
extern void htitle (char *);

extern float hxy (int, float, float);


/* others
 *
 */
/* in ISO C external identifiers must be unique within the first 8 characters
 * (there's no distinction between upper and lower case!) */

#define vinit_hbook vinhbook
extern void vinit_hbook ();

#define vopen_hbook_file ophbofil
extern void vopen_hbook_file (HBOOK_FILE *);

#define vbook_hist vbkhi
extern void vbook_hist (HISTO *);

#define get_varnam_str getv_str
extern char *get_varnam_str (char *[], int);

extern float *pset_nam (HBOOK_FILE *, char *);
extern float fget_max  (HBOOK_FILE *, char *);
extern float fget_min  (HBOOK_FILE *, char *);
extern void kuopen (int , char *, char *);
extern void kuclos (int , char *);


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

/* max length of LUN name
 * max number of open luns (1...iMAX_LUN_NR)
*/
#define iLUNLEN 8
#define iMAX_LUN_NR 100

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


#endif /* !CHBOOK_H */
