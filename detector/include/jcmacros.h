/********************************************************************** 
 ********************************************************************** 
 * jcmacros                                                               
 *                                                                      
 *   Created: Mon Jan 12 10:14:05 1998
 *   Author.: Jose Carlos Gonzales
 *   Notes..: 
 *                                                                      
 ********************************************************************** 
 **********************************************************************/

// @T \newpage

// @section Source code of {\tt jcmacros.h}

// @code

/* Begin */

#ifndef __JC_MACROS__
#define __JC_MACROS__

#define TRUE            1
#define FALSE           0
#define YES             1.
#define NO              0.
                        
#define SQR(x)          ((x)*(x))
#define CUB(x)          ((x)*(x)*(x))
#define ABS(x)          (((x)<0)? -(x) : (x))
#define SGN(x)          (((x)>0.)? 1. : -1.)
#define MAX(a,b)        (((a)>(b))?(a):(b))
#define MIN(a,b)        (((a)<(b))?(a):(b))
#define DEG2RAD         (M_PI/180.)
#define NORM(x)       (sqrt(SQR(x[0])+SQR(x[1])+SQR(x[2]))) /* Norm(vector) */
#define PROD(x,y)     ((x[0]*y[0])+(x[1]*y[1])+(x[2]*y[2])) /* Direct.Prod. */

#define DEG2RAD       (M_PI/180.)   /* Degrees to Radians conversion factor */
#define DEG360        (2.*M_PI)                                 /* 2 Pi     */
#define DEG180        (M_PI)                                    /*   Pi     */
#define DEG120        (2.*M_PI/3.)                              /* 2 Pi / 3 */
#define DEG90         (M_PI/2.)                                 /*   Pi / 2 */
#define DEG60         (M_PI/3.)                                 /*   Pi / 3 */
#define DEG30         (M_PI/6.)                                 /*   Pi / 6 */
#define RAD(x)        ((x)*0.0174532925199433)
#define DEG(x)        ((x)*57.2957795130823)


#define RANDOM        ((float)drand48())           /* Uniform random number */

                                       /* Define Null pointer (in fact,     */ 
#define NIL           (void *)0        /* there is a NULL=(void*)0 defined) */

#endif // __JC_MACROS__

// @endcode

/* End */
