/* prevent multiple includes
 */
#ifndef CTSMINUIT_H
#define CTSMINUIT_H 1


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
 * declaration of minuit functions
 */
extern void cts_mnexcm (void (), char *, double *, int);
extern void cts_mnparm (int, char *, double, double);
extern void cts_mnpout (int, char *, double *, double *, int *);
extern void cts_mnerrs (int, double *, double *, double *, double *);
extern void cts_mninit (int, int, int);


/*
 * defines
 *
 */
/* max name length for parameters in MINUIT
 */
#define iPARNAMELEN 10


#endif /* CTSMINUIT_H */
