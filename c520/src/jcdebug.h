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

/* Begin */

#ifndef __JC_DEBUG__
#define __JC_DEBUG__

int DBGi;

#ifdef DBG_ON

#define DBGvec(a,n,m)    {puts("DBG> vector " #a);\
			   for(DBGi=n;DBGi<=m;DBGi++)\
			     printf("     " #a "[%d] = %e \012",\
				    DBGi, (a[DBGi]));\
			     puts("");}

#define DBG(a)           { printf("DBG> " #a " = %g\n", (a)); }
#define DBGmsg(m)        { puts("DBG> " #m); }

#else // DBG_ON

#define DBGvec(a,n,m)    
#define DBG(a)           
#define DBGmsg(m)       

#endif // DBG_ON


#endif // __JC_DEBUG__

/* End */


