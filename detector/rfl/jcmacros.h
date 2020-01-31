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

/* Begin */

#ifndef __JC_MACROS__
#define __JC_MACROS__

#define TRUE                    1
#define FALSE                   0
#define YES                     1.
#define NO                      0.

#define SQR(x)                  ((x)*(x))
#define ABS(x)                  (((x)<0)? -(x) : (x))
#define SGN(x)                  (((x)>0.)? 1. : -1.)
#define MAX(a,b)                (((a)>(b))?(a):(b))
#define MIN(a,b)                (((a)<(b))?(a):(b))
#define DEG2RAD                 (M_PI/180.)

#endif // __JC_MACROS__

/* End */
