/*----------------------------------------------------------------------
  jcdate.c : 

  Function to get the hour/date of the system
  modified 13-10-98, D. Petry

----------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static int s0=0;

/*--------------------------------------------------
  jcdate: 
--------------------------------------------------*/
#ifdef JC_UNDERSCORES
void
jcdate_ (int *id, int *it, int *isl1, int *isl2, int *isl3, 
                           int *isl4, int *isl5, int *isl6 )
#else /* JC_NO_UNDERSCORES */
void
jcdate (int *id, int *it, int *isl1, int *isl2, int *isl3, 
                          int *isl4, int *isl5, int *isl6 )
#endif /* JC_UNDERSCORES */
{
  time_t timesecs;
  struct tm *t;

  s0 = (int) time( &timesecs );
  t = gmtime( &timesecs );
  *isl1 = (int) t->tm_year;
  *isl2 = (int) t->tm_mon+1;
  *isl3 = (int) t->tm_mday;
  *isl4 = (int) t->tm_hour;
  *isl5 = (int) t->tm_min;
  *isl6 = (int) t->tm_sec;

  *id = *isl1 * 10000 + *isl2 * 100 + *isl3;
  *it = *isl4 * 10000 + *isl5 * 100 + *isl6;

  return;
}

