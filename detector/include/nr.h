/* prevent multiple includes
 */
#ifndef NR_H
#define NR_H 1

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
 * Defines
 *
 */
#define iNR_END 1
#define iNMAX 5000
#define iTINY 1.0e-10
#define FREE_ARG char*


/*
 * Macros
 *
 */
#define GET_PSUM                                   \
 do {for (j = 1; j <= ndim; j++)                   \
      {                                            \
       for (sum = 0.0, i = 1; i <= mpts; i++)      \
         sum += p[i][j];                           \
       psum[j]=sum;                                \
      }                                            \
    } while (0)

#define SWAP(a,b) do {swap=(a);(a)=(b);(b)=swap;} while (0)

#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))


/* declaration of global functions
 */
extern int *ivector (long, long);

extern double brent (double, double, double, double (*)(double), double,
		     double *);
extern double gamma (double);
extern double gasdev(long *idum);
extern double ran1(long *idum);
extern double ran2(long *idum);

extern double *vector (long, long);
extern double **matrix (long, long, long, long);

extern void amoeba (double **, double [], int, double, double (*)(double []),
		    int *);

extern void frprmn(double [], int, double, int *, double *, double (*)(double []),
		   void (*)(double [], double []));

extern void mnbrak(double *, double *, double *, double *, double *,
		   double *, double (*)(double));

extern void gaussj (double **, int, double **, int);
extern void covsrt (double **, int, int [], int);
extern void lfit (double [], double [], double [], int, double [], int [], int,
		  double **, double *, void (*) ());

#endif  /* NR_H */
