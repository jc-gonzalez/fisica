/* prevent multiple includes
 */
#ifndef CUTS_H
#define CUTS_H 1


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
 *  Defines
 *
 */
#define iNTUPLE_EVT_ID 1
#define iNTUPLE_RUN_ID 2
#define iNTUPLE_AC_ID  3


/* names of events Ntuple variables (at least those needed for the
 * present analysis progrs.)
 *
 * (strings must match definitions in imager - one should use a
 *  single header file together with imager for this!)
 */
#define cDIST     "DIST"
#define cSIZE     "SIZE"
#define cCONC     "CONC"
#define cMDIST    "MDIST"
#define cWIDTH    "WIDTH"
#define cALPHA    "ALPHA"
#define cLENGTH   "LENGTH"
#define cFILTER   "filter"
#define cALTDEG   "ALTdeg"
#define cRUNTYPE  "runtype"
#define cNOONMJD  "noonMJD"
#define cNOONSECS "noonsecs"
#define cFRA200NS "fra200ns"
#define cCTRUNNUM "CTrunnum"
#define cMAXPIX12 "maxpix12"
#define cNUMPIXCB "numpixCB"
#define cXBAR_DEG "xbar_deg"
#define cYBAR_DEG "ybar_deg"
#define cIMPACTRP "impactRP"

/* defines used to identify and access foreign ntuples - e.g. CT-System
 * or AIROBICC resp. MUCHACHOS ntuples (presently only used within coinc)
 */
#define cMUCHACHOS_SECS  "Secund  "
#define cMUCHACHOS_200ns "FracSecs"
#define cMUCHACHOS_MJD   "MJD     "
#define cMUCHACHOS_RUN   "RunNum  "

/* number of 200ns units in one day
 * number of 200ns units in one second
 * difference between MJD and noonMJD in seconds 
 */
#define dDAY_IN_200ns    4.32e11
#define dSECS_IN_200ns   5000000.
#define dMJD_DIFF_SECS   43200.
#define dSECS_PER_HOUR   3600.

/*
 * dynamical scuts related stuff
 *
 */
/* the cuts are developed as a taylor series around dNOMLOGSIZE
 * and dNOMCOSZA
 */
#define dNOMLOGSIZE 1.8 
#define dNOMCOSZA   1.

/* define cut-function
 * a - l: cut-parameters
 * ls:    log(SIZE) - dNOMLOGSIZE
 * ls2:   ls^2
 * ct:    Cos(ZA.) - dNOMCOSZA
 * dd:    MDIST
 * dd2:   dd^2
 */
#define mcut(a, b, c, d, e, f, g, h, ls, ct, dd, ls2, dd2) \
  (a + b*dd2 + c*ct + ls*(d + e*dd2 + f*ct) + ls2*(g + h*dd))


/* define cut-function for orig. dscuts
 * a - n: cut-parameters
 * ls:    log(SIZE)
 * ad:    ALTdeg
 */
#define mcut_o(ls, ad, a, b, c, d, e, f, g, h, i, j, k, n)                   \
  (a + b*ls + c*ls*ls + d*ls*ls*ls + e*ad + f*ls*ad + g*ls*ls*ad             \
   + h*ls*ls*ls*ad + i*ad*ad + j*ls*ad*ad + k*ls*ls*ad*ad + n*ls*ls*ls*ad*ad)



/*
 *  enums
 *
 */
/* RUN_TYPE  - enum for all run-types
 * CUT_TYPE  - enum for all applied cuts (must match array 'cut_names'
 *             defined in cuts.c)
 * SORT_TYPE - enum for all sort routines
 * DS_TYPE   - enum for all types of event data structures
 *
 * all enums are used to acces array elements, so think twice
 * before changing anything! You'll have to change the arrays
 * 'fruntype[]', 'cut_funcs[]' and 'sort_funcs[][]' in cut.c as well.
 */
typedef enum {MC = 0, OFF, ON, MAX_RUN_TYPE} RUN_TYPE;

typedef enum {NOCUT = 0, FILTER, SCUTS, DSCUT_ORIG, DSCUT, DSCUT2, HZA_CUT,
	      FS_CUT, MAX_CUT_TYPE} CUT_TYPE;

#define iMAX_CUT_NAME_LEN 10
extern const char cut_names [MAX_CUT_TYPE] [iMAX_CUT_NAME_LEN];

typedef enum {NOSORT = 0, SORT_TIME, SORT_ZA, SORT_DAY_ZA,
	      MAX_SORT_TYPE} SORT_TYPE;

typedef enum {DS_DATA = 0, DS_SIGNAL, DS_TIME, DS_ENERGY,
	      MAX_DS_TYPE} DS_TYPE;


/* enum to handle different representations of the event time
 */
typedef enum {IMAGER, MUCHACHOS} TIME_REP;


/*
 *  structures
 *
 */
/* calcutta-type event structure - contains all event data needed for
 * deriving optimized cuts
 */
typedef struct data_list
{
  double dza;
  double dlength;
  double dwidth;
  double ddens;
  double ddist;
  double dmdist;
  double dalpha;
  double dmls;               /* log (size) - dNOMLOGSIZE */
  double dmcza;              /* cos (ZA) - dNOMCOSZA */
  double dmls_sq;            /* dmls^2 */
  double dmdist_sq;          /* dmdist^2 */
  double dtheta_sq;          /* squared distance: virtual source pos. - camera centre */
  struct data_list *p2prev;
  struct data_list *p2next;
} DLIST;

typedef struct data_list DLIST_EL;


/* signal-type event structure - contains all event data needed for
 * deriving source signals
 */
typedef struct sig_data_list
{
  int inoonmjd;
  int irunnum;
  double dxbar;                 /* x-position of event c.o.m. */
  double dybar;                 /* y-position of event c.o.m. */
  double devt_time;             /* time of event in 200 ns fractions */
  double dza;                   /* zenith angle */
  double dpsi;                  /* angle virtual source position - x axis */
  double dtheta;                /* angular distance v.s.p. - camera centre */
  double dalpha;
  struct sig_data_list *p2prev;
  struct sig_data_list *p2next;
} SLIST;

typedef struct sig_data_list SLIST_EL;


/* coinc-type event structure - contains all event data needed for
 * finding coincident events
 */
typedef struct time_data_list
{
  double devt_time;              /* time of event in 200 ns fractions */
  int inoonmjd;
  int ievent_nr;                 /* event number in Ntuple */
  int *pcoinc_evts;              /* array of evt. numbers for coincident evts. */
  struct time_data_list *p2prev;
  struct time_data_list *p2next;
} TLIST;

typedef struct time_data_list TLIST_EL;


/* calei-type event structure - contains all event data needed for
 * deriving the energy and impakt-parameter functions
 */
typedef struct energy_data_list
{
  int ievt_num;
  double dza;                   /* zenith angle */
  double dlogsize;              /* log (size) */
  double ddist;
  double dwidth;
  double dlength;
  double ddens;                 /* size / width / length */
  double de_mc;                 /* energy of MC event */
  double de_est;                /* estimated energy of MC event */
  double di_mc;                 /* impakt parameter of MC event */
  double di_est;                /* estimated impakt parameter of MC event */
  struct energy_data_list *p2prev;
  struct energy_data_list *p2next;
} ELIST;

typedef struct energy_data_list ELIST_EL;


/* struct containing information for one zenith angle bin
 */
typedef struct za_bin_list
{
  int ievt_on;                  /* number of ON events in za-bin */
  int ievt_off;                 /* number of OFF events in za-bin */
  double dza_min_on;            /* minimum za for ON data bin */
  double dza_max_on;            /* maximum za for ON data bin */
  double dza_min_off;           /* minimum za for OFF data bin */
  double dza_max_off;           /* maximum za for OFF data bin */
  struct za_bin_list *p2prev;
  struct za_bin_list *p2next;
} ZABLIST;

typedef struct za_bin_list ZAB_LIST_EL;


/*
 * function declarations
 *
 */
extern bool bcuts (CUT_TYPE, HBOOK_FILE *, RUN_TYPE, bool (*) (HBOOK_FILE *));

#define pget_evtdat getevtd
extern void *pget_evtdat (HBOOK_FILE *, SORT_TYPE, DS_TYPE, CUT_TYPE, RUN_TYPE,
			  bool (*) (HBOOK_FILE *), bool, char *, int *);

#define get_za_bins get_zab
extern ZAB_LIST_EL *get_za_bins (SLIST_EL *, SLIST_EL **, int,
				 bool (*) (SLIST_EL *));

#define get_cut_type gcut_typ
extern CUT_TYPE get_cut_type (char *);


/*
 * macros
 *
 */
/* macro to get the virtual source position for each single event
 * (the routine to calculate mdisp is derived from MC data;
 *  mvars are input data or local vars, rvars are output data)
 * ma - signed alpha, mz - ZA, md - dist, ms - size, ml - length,
 * mw - width, mp - phi (angle c.o.m. - x axis)
 * rpsi   - angle virtual source pos. (v.s.p.) - x axis
 * rtheta - angular distance: v.s.p. - x axis
 * mdisp - distance c.o.m. - v.s.p.
 * mchi  - angle c.o.m. - v.s.p.
 */
#define mget_source_pos(ma, mz, md, ms, ml, mw, mp, rpsi, rtheta)              \
 do {double malpha, mcosa, mcosza, mdisp, mtheta, mchi, mtmp;                  \
                                                                               \
     malpha = dPIDIV180 * ma;                                                  \
     mcosa  = fabs (cos (malpha));                                             \
     mcosza = cos (mz * dPIDIV180);                                            \
                                                                               \
     mdisp = 1.231 - 62.49 * ml * mw / (1. + log (ms));                        \
                                                                               \
     mtheta = sqrt (m2 (mdisp) + m2 (md) - 2 * mdisp * md * mcosa);            \
                                                                               \
     mtmp = asin ((mtheta != 0.) ? mdisp * sin (malpha) / mtheta : 0.);        \
     mchi = (mdisp * mcosa > md) ? M_PI_2 - fabs (mtmp) : fabs (mtmp);         \
                                                                               \
     rpsi = mp + msign (malpha) * mchi;                                        \
     rtheta = mtheta;                                                          \
                                                                               \
    } while (0)

#endif  /* CUTS_H */
