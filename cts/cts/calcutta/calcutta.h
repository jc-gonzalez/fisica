/* prevent multiple includes
 */
#ifndef CALCUTTA_H
#define CALCUTTA_H 1


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



/*
 *             STRUCTURES and related
 *
 */
/* the first enum must have '0', the spacing must be '1' and the order
 * (CUTNAME) must match the definition of cut_nam in calcutta.c
 */
typedef enum {LENGTH = 0, WIDTH, DIST, DENSITY, ALPHA, MAXCUT} CUTNAME;

typedef enum {UPPER = 0, LOWER, MAXBOUND} CUT_BOUND;

typedef enum {FIXED = 0, VARIABLE, MAXPAR} PARTYPE;


/* number of cut variables and number of minuit variables per cut
 * (the additional variable contains the cut-id); if iCUT_PARS is
 * changed one has to adopt mncut below
 */
#define iCUT_PARS 8
#define iMIN_CUT_PARS 9


/* structure containing all cut information
 */
typedef struct cut_data
{
  CUTNAME ecutnam;
  CUT_BOUND cbound;
  PARTYPE epartype;
  double dcut_pars[iCUT_PARS];
  double dcut_perr[iCUT_PARS];
  struct cut_data *p2next;
} cut_data_list;

typedef struct cut_data CUT;


typedef struct result
{
  double dsignal;
  double dnr_on;
  double dnr_off;
  double dex_err;
  double dnr_off2;
} RESULT;



/*
 *             DEFINES
 *
 */
/*
 *  calcutta.c and get_param.c
 *
 */
/* keywords used for reading the cut parameter file
 * do not change the order of these keywords
 */
#define iKEYWORDARGS 1
#define cKEYWORD01   " icutnr %d"
#define cKEYWORD02   " cbound %d"
#define cKEYWORD03   " partype %d"
#define cKEYWORD04   " bdefstartvals %d"

#define cRES_FILE_NAME "calcutta.results.txt"


/*
 *  get_cuts.c
 *
 */
/* defines for histograms
 */
#define iHA_ON_ID       101
#define iHA_OFF_ID      102
#define iHA_NOF_ID      103
#define iHA_ON_ZAB_ID   201
#define iHA_OFF_ZAB_ID  202
#define iHA_NOF_ZAB_ID  203

#define fHA_XLO   0.f
#define fHA_XHI   90.f
#define iHA_BINX  18

#define sHA_ON_TIT  "ON; ALPHA \"M# deg. \"N#; No. of events"
#define sHA_OFF_TIT "OFF; ALPHA \"M# deg. \"N#; No. of events"
#define sHA_NOF_TIT "OFF norm.; ALPHA \"M# deg. \"N#; No. of events"


/*
 *        MACROS
 *
 */
/* these macros are used to set and read the cut-identify variables
 * (passed as minuit variable)
 * a - enum 'CUTNAME'
 * b - enum 'CUT_BOUND'
 * c - cut id (even if upper, odd if lower cut)
 */
#define mset_cut_id(a, b, c)                                               \
 if ((a) < 0 || (a) >= MAXCUT || ((b) != UPPER && (b) != LOWER))           \
   cts_merror ("%s: variables out of range\n", "mset_cut_id");             \
 else                                                                      \
   c = (double) (a) * 10. + (double) (b);

#define mget_cut_typ(a, b, c)                                              \
 do {a = (CUTNAME) (0.5 + (c) / 10.);                                      \
     b = (CUT_BOUND) (0.5 + fmod (c, 2.));                                 \
    } while (0)


/* define cut-function (this macro is similar to mncut, defined in cuts.h,
 * but the image pars are passed via the DLIST_EL here)
 * a: array of cut parameters
 * b: DLIST_EL struct for one event
 */
#define mncut_cal(a, b) \
 (*a + *(a+1) * b->ddist_sq + *(a+2) * b->dmcza + b->dmls * (*(a+3)  \
  + *(a+4) * b->ddist_sq + *(a+5) * b->dmcza) + b->dmls_sq * (*(a+6) \
  + *(a+7) * b->ddist))


/*
 *   GLOBAL FUNCTIONS
 */
/* returns pointer to first element in a linked list; one element contains
 * information for a single cut;  defined in get_param.c
 */
extern CUT *pget_parfile (char *);

/* calculates the significance of a set of cuts;
 * defined in get_cuts.c
 */
extern double dcut_eff (double *, int, DLIST_EL *, DLIST_EL *, ZAB_LIST_EL *,
			RESULT *, bool);


#endif  /* CALCUTTA_H */
