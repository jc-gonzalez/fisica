/* prevent multiple includes
 */
#ifndef MYMATH
#define MYMATH 1


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

/*
 * RF_FUNCS   - enum for all defined root-finding functions
 *
 * all enums are used to acces array elements, so think twice
 * before changing anything! You'll have to change the array
 * 'find_root_funcs[]' in mymath.c as well.
 */
typedef enum {NRB = 0, MAX_RF_FUNCS} RF_FUNCS;

#define bfind_root bf_root
extern bool bf_root (RF_FUNCS , void (NRB_STRUCT *), double,
		     double, double, double *);


/* maps angle onto {-PI, PI} or {-180, 180} range depending on whether cunit
 * is r or d
 */
#define dmap_angle dmap_ang
extern double dmap_ang (double , char);


/* complementary error function
 */
extern double derfc (double );

/* returns mean and sample variance of data sample x[]
 */
#define vget_mean_svar vmeanvar
extern void vget_mean_svar (double [], int, double *, double *);

/* get number of independent test frequencies and smallest frequency
 * (determines size of ifs)
 */
extern void vgetfreq (double [], int, int, double, double *, int *);

/* calculates Lomb-Scargle periodogram
 */
extern void ls_power (double [], double [], double [], int, int,
		      double, bool *, double **, double **, int *);

/* defines number of grid points on which sines and cosines are calculated
 */
#define iMAX_NUM_SIN 1000000


/* macro to access sine and cosine values on the grid
 * it takes the angle 'c' and returns the index of the grid 'a' and the sign
 * of the angle 'b'
 */
#define mget_index_sign(a, b, c)                                              \
 do { b = msign (c);                                                          \
      a = (int) (fmod (fabs (c), d2PI) * (double) iMAX_NUM_SIN / d2PI);       \
    }                                                                         \
 while (0)

/* macro to translate an index into the appropriate grid angle
 * (it's the reverse function of mget_index_sign)
 */
#define mput_angle(a) (d2PI * (a) / (double) iMAX_NUM_SIN)


#endif /* !MYMATH */
