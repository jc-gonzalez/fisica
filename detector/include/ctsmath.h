/* prevent multiple includes
 */
#ifndef CTSMATH
#define CTSMATH 1


/*
 * defines
 *
 */
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

/* structure used by bnrb_root
 */
typedef struct nrb_struct
{
  double x;
  double f;
  double df;
} NRB_STRUCT;

#define iMAX_ITERATIONS 1000


/* math macros
 */
#define max(a, b)  ((a) > (b) ? (a) : (b))

#define min(a, b)  ((a) < (b) ? (a) : (b))

/* new compilers take care of 'a' being a complex expression, so there's
 * no need (I think) to use a temp. variable here
 */
#define m2(a) ((a)*(a))

#define m3(a) ((a)*(a)*(a))

#define m4(a) ((a)*(a)*(a)*(a))

#define msign(a) ((a) > 0. ? 1. : ((a) < 0. ? -1. : 0.))

#define mdiv(a, b) \
  ((fabs ((double)(b)) > dEPSILON) ? (double) (a) / (double) (b) : 0.)

/* returns true if c lies within a and b
 */
#define mc_in_ab(a, b, c)  (((a) < (c)) && ((c) < (b)))



/*
 * RF_FUNCS   - enum for all defined root-finding functions
 *
 * all enums are used to acces array elements, so think twice
 * before changing anything! You'll have to change the array
 * 'find_root_funcs[]' in ctsmath.c as well.
 */
typedef enum {NRB = 0, MAX_RF_FUNCS} RF_FUNCS;

extern bool cts_bfind_root (RF_FUNCS , void (NRB_STRUCT *), double, double,
			    double, double *);


/* maps angle onto {-PI, PI} or {-180, 180} range depending on whether cunit
 * is r or d
 */
extern double cts_dmap_angle (double , char);


/* complementary error function
 */
extern double cts_derfc (double );


/* returns mean and sample variance of data sample x[]
 */
extern void cts_vget_svar (double [], int, double *, double *);


/* get number of independent test frequencies and smallest frequency
 * (determines size of ifs)
 */
extern void cts_vgetfreq (double [], int, int, double *, double *, int *);


/* calculates Lomb-Scargle periodogram
 */
extern void cts_ls_power (double [], double [], double [], int, int,
			  double, bool *, double **, double **, int *);


/*
 * defines and macros to access sin and cos values on a grid
 */
/* defines number of grid points on which sines and cosines are calculated
 */
#define iSC_GRID_POINTS 1000000

/* macro to access sine and cosine values on the grid
 * it takes the angle 'c' and returns the index of the grid 'a' and the sign
 * of the angle 'b'
 */
#define cts_mget_index(a, b, c)                                               \
 do { b = msign (c);                                                          \
      a = (int) (fmod (fabs (c), d2PI) * (double) iSC_GRID_POINTS / d2PI);    \
    }                                                                         \
 while (0)

/* macro to translate an index into the appropriate grid angle
 * (it's the reverse function of mget_index_sign)
 */
#define cts_mput_angle(a) (d2PI * (a) / (double) iSC_GRID_POINTS)


/* some constants
 */
#define d2PI         6.28318530717958647692
#define dPIDIV180    0.017453292519943295769
#define dSQRT_PIDIV2 1.2533141373155002512
#define d180DIVPI    57.295779513082320877
#define dEPSILON     1e-16


#endif /* CTSMATH */
