#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "ctscuts.h"
#include "jacuzzi.h"


#define CFUNCNAME "vget_obs_times"

void vget_obs_times (DRD_LIST_EL *pdrdlist, HBOOK_FILE *pntuple_on,
		     RUN_TYPE rtyp, bool (*pcut)())

  /* this function is used to derive the time spent in each run for each
   * za-bin
   * input:
   * pdrdlist   - struct-list containing information for all days
   * pntuple_on - ON-ntuple needed to get observation times
   * rtyp       - data type of events (MC, ON, OFF ...)
   * pcut       - pointer to cut function
   */
{
  int i, k, inr_days, irunnum, inoonmjd, inr_zab, izab, inr_evts_af;
  int irnumold = 0;

  double dza, dobs_time_zab, dobs_time_day, dt_levt;

  bool bfinished, bdone;

  SLIST_EL **pevtlist, *pevt;

  RT_LIST_EL *prt_root, *prt_el;

  DRD_LIST_EL *pdrd_el;

  ZAB_LIST_EL *pzab;
  ZABP_LIST_EL *pzabp;

  FILE_TYPE dat_file;


  /* open ntuple and get event data into linked list
   */
  cts_hreopen (pntuple_on);
  pevtlist = cts_pevtdat (pntuple_on, SORT_DAY_TIME, DS_SIGNAL, FILTER, rtyp,
			  pcut, &inr_days);

  cts_hrend (pntuple_on);


  /*
   * loop over all days
   */
  pdrd_el = pdrdlist;

  for (i = 0; i < inr_days; i++)
    {
      /* check whether data for that day exist
       */
      if (*pevtlist != NULL)
	{
	  /* look for drd_list entry with matching date
	   */
	  bfinished = false;

	  while (!bfinished)
	    {
	      if (pdrd_el == NULL || pdrd_el->inoonmjd == (*pevtlist)->inoonmjd)
		bfinished = true;

	      else
		pdrd_el = pdrd_el->p2next;
	    }

	  /* skip rest, if noonmjd is not in the list
	   */
	  if (pdrd_el == NULL)
	    {
	      fprintf (stderr, "Warning, unrecognized noonmjd %i!\n", inoonmjd);
	      continue;
	    }
	}

      else
	{
	  pevtlist++;
	  continue;
	}


      /*
       * loop over all za-bins, get runnumber and fill rt-struct
       */
      bdone = false;
      pevt = *pevtlist;
      inr_zab = pdrd_el->inr_zab;

      while (!bdone)
	{
	  dza = pevt->dza;

	  /* look for matching za-bin
	   */
	  pzabp = pdrd_el->pzabp;

	  for (k = 0; k < inr_zab; k++)
	    {
	      izab = k;
	      pzab = pzabp->pzab;
	      prt_root = *(pdrd_el->pprt_on+k);


	      /* since we use the filter-cut here there might be some
	       * events outside the za-boundaries; take them too (otherwise
	       * the obs. time depends on the applied cut)
	       */
	      if ((k == 0 && dza < pzab->dza_min_on)
		  || (k == inr_zab - 1 && dza > pzab->dza_max_on)
		  || (dza >= pzab->dza_min_on && dza <= pzab->dza_max_on))

		k = inr_zab;

	      else
		pzabp = pzabp->p2next;
	    }


	  /* za-bin is fixed, now look for the runnumber
	   */
	  bfinished = false;
	  prt_el = prt_root;
	  irunnum = pevt->irunnum;

	  while (!bfinished)
	    {
	      if (prt_el == NULL)
		{
		  /* allocate memory for first rt-list element
		   * (and store it in drd-struct)
		   */
		  prt_el = cts_mmalloc (RT_LIST_EL, 1, CFUNCNAME);
		  *(pdrd_el->pprt_on+izab) = prt_el;


		  /* init (part of) prt_el
		   */
		  prt_el->irunnum = irunnum;
		  prt_el->drun_time = 0.;
		  prt_el->p2prev = NULL;
		  prt_el->p2next = NULL;

		  bfinished = true;
		}

	      else if (prt_el->irunnum == irunnum)
		{
		  /* matching rt-struct found
		   */
		  bfinished = true;
		}

	      else if (prt_el->p2next == NULL)
		{
		  /* last run-struct found - create a new one and fill it
		   */
		  prt_el->p2next = cts_mmalloc (RT_LIST_EL, 1, CFUNCNAME);

		  prt_el->p2next->p2prev = prt_el;
		  prt_el = prt_el->p2next;

		  prt_el->irunnum = pevt->irunnum;
		  prt_el->drun_time = 0.;
		  prt_el->p2next = NULL;

		  bfinished = true;
		}

	      else
		prt_el = prt_el->p2next;

	    }        /* END of while (!bfinished) */



	  /* loop over all events for that day, za-bin and run number and
	   * calculate runtime and mean values
	   */
	  bfinished = false;

	  /* take time of last event as start time if runnumber didn't change
	   * else take the time of the current event
	   */
	  prt_el->dtime_min = (irnumold == irunnum) ? dt_levt : pevt->devt_time;
	  irnumold = irunnum;

	  while (!bfinished)
	    {
	      dza = pevt->dza;

	      if (pevt->irunnum == prt_el->irunnum
		  && ((izab == 0 && dza < pzab->dza_min_on)
		      || (izab == inr_zab - 1 && dza > pzab->dza_max_on)
		      || (dza >= pzab->dza_min_on && dza <= pzab->dza_max_on)))
		{
		  prt_el->dtime_max = pevt->devt_time;

		  /* update also part of the zabp-struct
		   */
		  pzabp->davcosza += cos (dza * dPIDIV180);
		  pzabp->dmean_time += pevt->devt_time;
		  pzabp->inr_evts_af += 1;

		  pevt = pevt->p2next;
		  bfinished = (pevt == NULL) ? true : false;
		}

	      else
		bfinished = true;
	    }


	  /* remember time of last event for next za-bin
	   */
	  dt_levt = prt_el->dtime_max;


	  /* update total obs. time for that run
	   */
	  prt_el->drun_time += (prt_el->dtime_max - prt_el->dtime_min)
	    / dSECS_IN_200ns;


	  /* last event for that day?
	   */
	  bdone = (pevt == NULL) ? true : false;

	}   /* End while (!bdone) */

      pevtlist++;

    }   /* End for (i = 0; ... ) */



  /* open file to store results
   */
  dat_file.pname = "jacuzzi_rt.out";
  dat_file.pfile = NULL;
  dat_file.acm = "w";

  cts_vopen_file (&dat_file);

  fprintf (dat_file.pfile, "# %6s %8s", "noonMJD:", "za-bin:");
  fprintf (dat_file.pfile, "%10s %13s\n#\n", "run-nr:", "run- time:");


  /* up to now we calculated the amount of time each run spent in each
   * single za-bin (if > 0). Now we calculate the overall time spent
   * in each za-bin for each day
   */
  pdrd_el = pdrdlist;

  while (pdrd_el != NULL)
    {
      /* check all za-bins for that day
       */
      dobs_time_day = 0.;
      pzabp = pdrd_el->pzabp;
      inr_zab = pdrd_el->inr_zab;

      for (k = 0; k < inr_zab; k++)
	{
	  dobs_time_zab = 0.;
	  prt_el = *(pdrd_el->pprt_on+k);

	  /* check all runs for that given za-bin
	   */
	  while (prt_el != NULL)
	    {
	      /* save overall run-time, and update obs. time for that za-bin
	       */
	      dobs_time_zab += prt_el->drun_time;

	      fprintf (dat_file.pfile, "%8i", pdrd_el->inoonmjd);
	      fprintf (dat_file.pfile, "%8i %11i", k+1, prt_el->irunnum);
	      fprintf (dat_file.pfile, "%16f\n", prt_el->drun_time);

	      prt_el = prt_el->p2next;

	    }   /* END while (prt_el != NULL) */


	  /* finish calculation of pzbp-values
	   */
	  inr_evts_af = pzabp->inr_evts_af;

	  if (inr_evts_af == 0)
	    cts_merror("%s: no events after filter left!\n", CFUNCNAME);

	  pzabp->davcosza /= (double) inr_evts_af;
	  pzabp->dmean_time /= ((double) inr_evts_af * dSECS_IN_200ns);
	  pzabp->dobs_time = dobs_time_zab;
	  dobs_time_day += dobs_time_zab;

	  fprintf (dat_file.pfile, "# obs. time zab: %8f\n", dobs_time_zab);
	  pzabp = pzabp->p2next;

	}   /* END for (k = inr_zab ... ) */

      pdrd_el->dobs_time = dobs_time_day;

      fprintf (dat_file.pfile, "# obs. time day: %8f\n#\n", dobs_time_day);
      pdrd_el = pdrd_el->p2next;

    }  /* END while (pdrd_el != NULL) */
}

#undef CFUNCNAME
