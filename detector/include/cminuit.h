/* prevent multiple includes
 */
#ifndef CMINUIT_H
#define CMINUIT_H 1

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


/*
 * declaration of some fortran functions
 *
 */

/* minuit
 */
extern void mnexcm(void (), char *, double *, int);
extern void mnparm(int, char *, double, double);
extern void mnpout(int, char *, double *, double *, int *);
extern void mnerrs(int, double *, double *, double *, double *);
extern void mninit(int, int, int);


/*
 * defines
 *
 */
/* max name length for parameters in MINUIT
 */
#define iPARNAMELEN 10


#endif /* !CMINUIT_H */
