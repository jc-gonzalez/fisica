/* prevent multiple includes
 */
#ifndef JACUZZI_H
#define JACUZZI_H 1


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
 *  defines
 *
 */
#define iHID_FAC      1000
#define dFLUX_UNIT    1e-11
#define dMIN_ZA_DIFF  0.3
#define iMAX_NUM_ZAB  30
#define iMIN_DAYS_BIN 20


/* some cut values
 */
#define fALPHA_CUT    10.f
#define fALPHA_N_LO   20.f
#define fALPHA_N_HI   80.f


/* id's of histograms
 * (the ZAB histogram ids must differ by iMAX_NUM_ZAB)
 */
#define iTMP_ID               50
#define iHTS_ON_ID           100
#define iHTS_OFF_ID          110
#define iHTS_ID              120
#define iHTS_ZAB_ON_ID       100
#define iHTS_ZAB_OFF_ID      130
#define iHTS_ZAB_ID          160
#define iHTS_DAY_ON_ID       300
#define iHTS_DAY_OFF_ID      310
#define iHTS_DAY_ID          320

#define iHALPHA_ON_ID        200
#define iHALPHA_OFF_ID       210
#define iHALPHA_ID           220
#define iHALPHA_ZAB_ON_ID    200
#define iHALPHA_ZAB_OFF_ID   230
#define iHALPHA_ZAB_ID       260
#define iHALPHA_DAY_ON_ID    400
#define iHALPHA_DAY_OFF_ID   410
#define iHALPHA_DAY_ID       420

#define fHTS_XLO     0.f
#define fHTS_XHI     1.f
#define iHTS_BINX    50
#define fHALPHA_XLO  0.f
#define fHALPHA_XHI  90.f
#define iHALPHA_BINX 18

#define iSIG_BINX    20


/* histogram titles
 */
#define sHTS_TIT        "ON - OFF; [Q]^2! \"M# deg. ^2! \"N#; No. of events"
#define sHTS_ON_TIT     "ON; [Q]^2! \"M# deg. ^2! \"N#; No. of events"
#define sHTS_OFF_TIT    "OFF; [Q]^2! \"M# deg. ^2! \"N#; No. of events"

#define sHALPHA_TIT     "ON - OFF; ALPHA \"M# deg. \"N#; No. of events"
#define sHALPHA_ON_TIT  "ON; ALPHA \"M# deg. \"N#; No. of events"
#define sHALPHA_OFF_TIT "OFF; ALPHA \"M# deg. \"N#; No. of events"


/* struct to determine observation time for a single run
 */
typedef struct run_time_list
{
  int irunnum;
  double dtime_min;            /* start time of run */
  double dtime_max;            /* time of end of run */
  double drun_time;
  struct run_time_list *p2prev;
  struct run_time_list *p2next;
} RUNTLIST;

typedef struct run_time_list RT_LIST_EL;


/* struct which combines the zab-struct (cuts.h) together with
 * some additional information
 */
typedef struct zabp_list
{
  int inr_evts_af;     /* number of evts after filter cuts */
  double davcosza;     /* average cosine za (after filter cuts) */
  double dobs_time;
  double dmean_time;   /* mean evtent time (used to get mjd of the zab) */
  ZAB_LIST_EL *pzab;
  struct zabp_list *p2prev;
  struct zabp_list *p2next;
} ZABPLIST;

typedef struct zabp_list ZABP_LIST_EL;


/* struct to handle all run information for one day
 */
typedef struct drd_list
{
  int inr_zab;
  int inoonmjd;
  double dobs_time;
  SLIST_EL *pevt_on;
  SLIST_EL *pevt_off;
  RT_LIST_EL **pprt_on;
  ZABP_LIST_EL *pzabp;
  struct drd_list *p2next;
} DRDLIST;

typedef struct drd_list DRD_LIST_EL;


/* struct to collect zab-results
 */
typedef struct zabr_list
{
  int inoonmjd;
  int ievts_on_s;
  int ievts_on_n;
  int ievts_off_s;
  int ievts_off_n;
  double dsig;
  double dex_err;
  double dex_evts;
  double dza_min;
  double dza_max;
  double davcosza;
  double dobs_time;
  double dmjd;
  double dflux;
  double dflux_err;
  double drate;
  double drate_err;
  double drate_off;
  double drate_off_err;
  struct zabr_list *p2next;
} ZABRLIST;

typedef struct zabr_list ZABR_LIST_EL;



/*
 * macros
 */
/* changes noonMJD to MJD
 * a - noonMJD, b - obs. time [s]
 */
#define mget_mjd(a, b) ((double)(a) + 0.5 * (1. + (b) / dMJD_DIFF_SECS))

/* error of normalized off events
 * a - Noff (signal), b - Moff (norm.), c - ratio (Mon / Moff)
 */
#define moff_err(a, b, c) (sqrt ((a) * (m2 (c) + mdiv (a, b) * (c + m2 (c)))))

/* errors of normalized off histo
 * a - array containing off histo event numbers (not normalized)
 * b - zabr-struct
 * result is stored in 'a'
 */
#define mhoffn_err(a, b)                                                     \
 do { int i; double dratio;                                                  \
      for (i = 0; i < iHALPHA_BINX; i++)                                     \
        {                                                                    \
          dratio = mdiv (b->ievts_on_n, b->ievts_off_n);                     \
          a[i] = moff_err (a[i], b->ievts_off_n, dratio);                    \
        }                                                                    \
    }                                                                        \
 while (0)


/*
 * functions
 */
extern void vget_obs_times (DRD_LIST_EL *, HBOOK_FILE *, RUN_TYPE,
			    bool (*pcut)());

extern void valpha_plot (ZABR_LIST_EL *, int, int, int, char *, char *);

extern void vtheta_plot (ZABR_LIST_EL *, int, int, int, char *, char *);

extern void vplot_lc (ZABR_LIST_EL *, ZABR_LIST_EL *, int, int, char *);


#endif /* JACUZZI_H */
