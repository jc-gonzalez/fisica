/* prevent multiple includes
 */
#ifndef CTSHPLOT_H
#define CTSHPLOT_H 1


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

extern void cts_igend ();
extern void cts_igsg (int);
extern void cts_igsa (int);
extern void cts_iopks (int);
extern void cts_iclwk (int);
extern void cts_iacwk (int);
extern void cts_idawk (int);
extern void cts_iginit (int);
extern void cts_iselnt (int);
extern void cts_iuwk (int, int);
extern void cts_igsse (int, int);
extern void cts_iclrwk (int, int);
extern void cts_igmeta (int, int);
extern void cts_iopwk (int, int, int);
extern void cts_igrng (float, float);
extern void cts_iswn (int, float, float, float, float);
extern void cts_isvp (int, float, float, float, float);
extern void cts_iswkwn (int, float, float, float, float);
extern void cts_iswkvp (int, float, float, float, float);


/*
 * basic output primitives
 */
extern void cts_ipl (int, float *, float *);
extern void cts_iml (int, float *, float *);
extern void cts_ipm (int, float *, float *);
extern void cts_ifa (int, float *, float *);
extern void cts_itx (float, float, char *);

/*
 * basic output attributes
 */
extern void cts_isclip (int);
extern void cts_iscr (int, int, float, float, float);
extern void cts_isplci (int);
extern void cts_ispmci (int);
extern void cts_isfaci (int);
extern void cts_istxci (int);
extern void cts_isfais (int);
extern void cts_isfasi (int);
extern void cts_isln (int);
extern void cts_islwsc (float);
extern void cts_ismk (int);
extern void cts_ismksc (float);
extern void cts_istxal (int, int);
extern void cts_ischh (float);
extern void cts_ischup (float, float);
extern void cts_istxfp (int, int);
extern void cts_igset (char *, float );
extern void cts_igqwk (int, char *, void *);
extern void cts_igraph (int, float *, float *, char *);
extern void cts_igaxis (float, float, float, float, float, float, int, char *);
extern void cts_igtext (float, float, char *, float, float, char *);
extern void cts_igzset (char *);


/*
 * HPLOT CALLS
 *
 */
extern void cts_hplabl (int, int, char *);
extern void cts_hplaer (float *, float *, float *, float *, float *, float *,
			int, char, int, float);
extern void cts_hplarc (float, float, float, float, float);
extern void cts_hplax (char *, char *);
extern void cts_hplbox (float, float, float, float, char);
extern void cts_hplcap (int);
extern void cts_hplcom (float, float, char *);
extern void cts_hplcon (int, int, int);
extern void cts_hpldo (int);
extern void cts_hplego (int, float, float);
extern void cts_hplend ();
extern void cts_hplerr (float *, float *, float *, float *, int, char, int,
			float);
extern void cts_hplfra (float, float, float, float, char *);
extern void cts_hplfun (float *, float *, int, char);
extern void cts_hplgiv (float, float, float, float);
extern void cts_hpline (float *, float *, int, char);
extern void cts_hplint ();
extern void cts_hplkey (float, float, int, char *);
extern void cts_hplnt (int, int, float (*) (), int, int, int, int);
extern void cts_hplnul ();
extern void cts_hploc (int, int, float, float, int, int, int, int);
extern void cts_hplopt (char [][iCOPT_LEN], int);
extern void cts_hplot (int, char *, char *, int);
extern void cts_hplpro (int, char *, char *);
extern void cts_hplpto (char *, char *);
extern void cts_hplset (char *, float);
extern void cts_hplsiz (float, float, char);
extern void cts_hplsof (float, float, char *, float, float, float, int);
extern void cts_hplsur (int, float, float, int);
extern void cts_hplsym (float *, float *, int, int, float, char);
extern void cts_hpltab (int, int, float *, char);
extern void cts_hpltit (char *);
extern void cts_hplwir (char, float, float, char *);
extern void cts_hplzom (int, char *, int, int);
extern void cts_hplzon (int, int, int, char);


/* hplot related functions
 */
extern void cts_vinit_hplot (int);

extern void cts_vset_def ();

#endif  /* CTSHPLOT_H */
