/* prevent multiple includes
 */
#ifndef CHPLOT_H
#define CHPLOT_H 1

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

/* number of characters for opt-strings
 */
#define iCOPT_LEN 4

extern void igend ();
extern void igsg (int);
extern void igsa (int);
extern void iopks (int);
extern void iclwk (int);
extern void iacwk (int);
extern void idawk (int);
extern void iginit (int);
extern void iselnt (int);
extern void iuwk (int, int);
extern void igsse (int, int);
extern void iclrwk (int, int);
extern void igmeta (int, int);
extern void iopwk (int, int, int);
extern void igrng (float, float);
extern void iswn (int, float, float, float, float);
extern void isvp (int, float, float, float, float);
extern void iswkwn (int, float, float, float, float);
extern void iswkvp (int, float, float, float, float);


/*
 * basic output primitives
 */
extern void ipl (int, float *, float *);
extern void iml (int, float *, float *);
extern void ipm (int, float *, float *);
extern void ifa (int, float *, float *);
extern void itx (float, float, char *);

/*
 * basic output attributes
 */
extern void isclip (int);
extern void iscr (int, int, float, float, float);
extern void isplci (int);
extern void ispmci (int);
extern void isfaci (int);
extern void istxci (int);
extern void isfais (int);
extern void isfasi (int);
extern void isln (int);
extern void islwsc (float);
extern void ismk (int);
extern void ismksc (float);
extern void istxal (int, int);
extern void ischh (float);
extern void ischup (float, float);
extern void istxfp (int, int);
extern void igset (char *, float );
extern void igqwk (int, char *, void *);
extern void igaxis (float, float, float, float, float, float, int, char *);
extern void igtext (float, float, char *, float, float, char *);
extern void igzset (char *);


/*
 * HPLOT CALLS
 *
 */
extern void hplabl (int, int, char *);
extern void hplaer (float *, float *, float *, float *, float *, float *, int, char,
		    int, float);
extern void hplarc (float, float, float, float, float);
extern void hplax (char *, char *);
extern void hplbox (float, float, float, float, char);
extern void hplcap (int);
extern void hplcom (float, float, char *);
extern void hplcon (int, int, int);
extern void hpldo (int);
extern void hplego (int, float, float);
extern void hplend ();
extern void hplerr (float *, float *, float *, float *, int, char, int, float);
extern void hplfra (float, float, float, float, char *);
extern void hplfun (float *, float *, int, char);
extern void hplgiv (float, float, float, float);
extern void hpline (float *, float *, int, char);
extern void hplint ();
extern void hplkey (float, float, int, char *);
extern void hplnt (int, int, float (*) (), int, int, int, int);
extern void hplnul ();
extern void hploc (int, int, float, float, int, int, int, int);
extern void hplopt (char [][iCOPT_LEN], int);
extern void hplot (int, char *, char *, int);
extern void hplpro (int, char *, char *);
extern void hplpto (char *, char *);
extern void hplset (char *, float);
extern void hplsiz (float, float, char);
extern void hplsof (float, float, char *, float, float, float, int);
extern void hplsur (int, float, float, int);
extern void hplsym (float *, float *, int, int, float, char);
extern void hpltab (int, int, float *, char);
extern void hpltit (char *);
extern void hplwir (char, float, float, char *);
extern void hplzom (int, char *, int, int);
extern void hplzon (int, int, int, char);


/* hplot related functions
 */
#define vinit_hplot vinhplot
extern void vinit_hplot (int);

extern void vset_def ();

#endif /* !CHPLOT_H */
