/********************************************************************** 
 ********************************************************************** 
 * jcdebug                                                               
 *                                                                      
 *   Created: Mon Jan 12 12:36:19 1998
 *   Author.: Jose Carlos Gonzales
 *   Notes..: 
 *                                                                      
 ********************************************************************** 
 **********************************************************************/

// @T \newpage

// @section Source code of {\tt jcdebug.h}

/* Begin */

#ifndef __JC_DEBUG__
#define __JC_DEBUG__

// @code

#ifdef __DEBUG__

int DBGi;

#define DBGvec(a,n,m)    {puts("DBG> vector " #a);\
               for(DBGi=n;DBGi<=m;DBGi++)\
                 printf("     " #a "[%d] = %e \012",\
                    DBGi, (a[DBGi]));\
                 puts("");}

#define DBG(a)           { printf("DBG> " #a " = %g\n", (a)); }
#define DBGi(a)          { printf("DBG> " #a " = %d\n", (a)); }
#define DBGmsg(m)        { puts("DBG> " #m); }

#else // not __DEBUG__

#define DBGvec(a,n,m)    
#define DBG(a)           
#define DBGi(a)           
#define DBGmsg(m)       

#endif // not __DEBUG__


#endif // __JC_DEBUG__

// @endcode

/* End */


