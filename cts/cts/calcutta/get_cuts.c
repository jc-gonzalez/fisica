/*
 * Atmospheric Cherenkov Telescope Data Analysis Software
 * MPI f"ur Physik, M"unchen
 *
 */

/*
 * This File belongs to the program
 *
 *         C A L C U T T A
 *
 * Purpose: calculates best cuts for a given on- and off-data Ntuple
 *
 * Author: D. Kranich
 *
 */
#include <math.h>
#include <stdio.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "ctscuts.h"
#include "calcutta.h"


double dcut_eff (double *xval, int inc, DLIST_EL *pon_list, DLIST_EL *poff_list,
		 ZAB_LIST_EL *pzab_list, RESULT *presults, bool bfinished)

  /* this function calculates (and returns) the significance of a given
   * set of cuts;
   * xval      - array containing the cut-parameters
   * inc       - number of cuts (fixed or variable)
   * pon_list  - pointer to list of on-data
   * poff_list - pointer to list of off-data
   * pzab      - list of all za-bins
   * presults  - struct containing the final results
   * bfinished - true if minuit converged
   */
{
  int i, k, ibin, ievt_on, ievt_off;
  int ievts_on_s, ievts_on_n, ievts_off_s, ievts_off_n;
  int ievts_on_s_fin, ievts_on_n_fin, ievts_off_s_fin, ievts_off_n_fin;

  double dalpha, dcutval, dcutval_alpha, diff, dsign;
  double dratio, doff_evts, dex_evts, dex_err, dsig, dpurity;

  double dcutvars[MAXCUT];
  double *pcpar;

  bool bcut;

  CUTNAME cnam;
  CUT_BOUND cbound;

  ZAB_LIST_EL *pzab;
  DLIST_EL *pevt_on, *pevt_off;

  HBOOK_FILE hfile;

  HISTO ha_zab_on  = {sHA_ON_TIT, 0, iHA_BINX, 0.f, fHA_XLO, fHA_XHI, 0.f,
		      0.f, 0.f};

  HISTO ha_zab_off = {sHA_OFF_TIT, 0, iHA_BINX, 0.f, fHA_XLO, fHA_XHI, 0.f,
		      0.f, 0.f};

  HISTO ha_zab_nof = {sHA_NOF_TIT, 0, iHA_BINX, 0.f, fHA_XLO, fHA_XHI, 0.f,
		      0.f, 0.f};

  HISTO ha_on  = {sHA_ON_TIT, iHA_ON_ID, iHA_BINX, 0.f, fHA_XLO, fHA_XHI, 0.f,
		  0.f, 0.f};
	   
  HISTO ha_off = {sHA_OFF_TIT, iHA_OFF_ID, iHA_BINX, 0.f, fHA_XLO, fHA_XHI, 0.f,
		  0.f, 0.f};
	   
  HISTO ha_nof = {sHA_NOF_TIT, iHA_NOF_ID, iHA_BINX, 0.f, fHA_XLO, fHA_XHI, 0.f,
		  0.f, 0.f};


  /* open hbook file for results if finished
   */
  if (bfinished)
    {
        hfile.pname = "calcutta.hbook";
	hfile.copt = 'N';
	hfile.ilrec = 1024;
	hfile.iid = -1;

	cts_vopen_hbook_file (&hfile);


	/* book histos for whole data sample
	 */
	cts_vbook_hist (&ha_on);
	cts_vbook_hist (&ha_off);
	cts_vbook_hist (&ha_nof);
    }


  /*
   * loop over all za-bins and apply cuts to ON-events
   */
  ibin = 0;
  ievts_on_s_fin = ievts_on_n_fin = ievts_off_s_fin = ievts_off_n_fin = 0;

  dcutval_alpha = dALPHA_CUT;

  pzab = pzab_list;
  pevt_on  = pon_list;
  pevt_off = poff_list;


  while (pzab != NULL)
    {
      ibin++;
      ievts_on_s = ievts_on_n = ievts_off_s = ievts_off_n = 0;


      /* book histos when finished
       */
      if (bfinished)
	{
	  ha_zab_on.iid  = iHA_ON_ZAB_ID + ibin * 10;
	  ha_zab_off.iid = iHA_OFF_ZAB_ID + ibin * 10;
	  ha_zab_nof.iid = iHA_NOF_ZAB_ID + ibin * 10;

	  cts_vbook_hist (&ha_zab_on);
	  cts_vbook_hist (&ha_zab_off);
	  cts_vbook_hist (&ha_zab_nof);
	}


      /* ON data bin
       */
      for (ievt_on = 0; ievt_on < pzab->ievt_on; ievt_on++)
	{
	  /*
	   * use cuts to derive a modified alpha which is used in final
	   * malpha cut
	   */
	  dcutvars[DIST]    = pevt_on->ddist;
	  dcutvars[WIDTH]   = pevt_on->dwidth;
	  dcutvars[LENGTH]  = pevt_on->dlength;
	  dcutvars[DENSITY] = pevt_on->ddens;


	  /* loop over all defined cuts
	   */
	  bcut = false;

	  for (i = 0; i < inc; i++)
	    {
	      k = i * iMIN_CUT_PARS;

	      /* get value of cut (pcpar points to array of cut parameters)
	       */
	      pcpar = &xval[k+1];
	      dcutval = mncut_cal (pcpar, pevt_on);

	      /* get cut type and derive scaling factor (if event is cut,
	       * the scaling factor is the relative distance:
	       * event variable - cut value else 0.)
	       */
	      mget_cut_typ (cnam, cbound, xval[k]);


	      if (cnam != ALPHA)
		{
		  /* dsign flips sign depending on UPPER or LOWER cut;
		   * increase relative distance by factor 10
		   */
		  dsign = (double) (1 - 2 * (int) cbound);
		  diff = dsign * (dcutval - dcutvars[cnam]);
		  bcut |= (diff > 0) ? false : true;
		}

	      else
		dcutval_alpha = dcutval;
	    }



	  /* now apply alpha cut
	   */
	  if (!bcut)
	    {
	      dalpha = fabs (pevt_on->dalpha);

	      if (dalpha < dcutval_alpha)
		ievts_on_s++;

	      else if (dALPHA_N_LO < dalpha && dalpha < dALPHA_N_HI)
		ievts_on_n++;


	      /* fill histogram when finished
	       */
	      if (bfinished)
		cts_hfill (ha_zab_on.iid, dalpha, 0.f, 1.f);
	    }


	  pevt_on = pevt_on->p2next;

	}    /* END of  for (ievt_on = 0; ... */



      /* OFF data bin
       */
      for (ievt_off = 0; ievt_off < pzab->ievt_off; ievt_off++)
	{
	  /*
	   * use cuts to derive a modified alpha which is used in final
	   * malpha cut
	   */
	  dcutvars[DIST]    = pevt_off->ddist;
	  dcutvars[WIDTH]   = pevt_off->dwidth;
	  dcutvars[ALPHA]   = pevt_off->dalpha;
	  dcutvars[LENGTH]  = pevt_off->dlength;
	  dcutvars[DENSITY] = pevt_off->ddens;


	  /* loop over all defined cuts and derive scale factor for
	   * alpha parameter
	   */
	  bcut = false;

	  for (i = 0; i < inc; i++)
	    {
	      k = i * iMIN_CUT_PARS;

	      /* get value of cut (pcpar points to array of cut parameters)
	       */
	      pcpar = &xval[k+1];
	      dcutval = mncut_cal (pcpar, pevt_off);

	      /* get cut type and derive scaling factor (if event is cut,
	       * the scaling factor is the relative distance:
	       * event variable - cut value else 0.)
	       */
	      mget_cut_typ (cnam, cbound, xval[k]);


	      if (cnam != ALPHA)
		{
		  /* dsign flips sign depending on UPPER or LOWER cut;
		   * increase relative distance by factor 10
		   */
		  dsign = (double) (1 - 2 * (int) cbound);
		  diff = dsign * (dcutval - dcutvars[cnam]);
		  bcut |= (diff > 0) ? false : true;
		}

	      else
		dcutval_alpha = dcutval;
	    }


	  /* now apply alpha cut
	   */
	  if (!bcut)
	    {
	      dalpha = fabs (pevt_off->dalpha);

	      if (dalpha < dcutval_alpha)
		ievts_off_s++;

	      else if (dALPHA_N_LO < dalpha && dalpha < dALPHA_N_HI)
		ievts_off_n++;


	      /* fill histogram when finished
	       */
	      if (bfinished)
		cts_hfill (ha_zab_off.iid, dalpha, 0.f, 1.f);
	    }


	  pevt_off = pevt_off->p2next;

	}    /* END of  for (ievt_off = 0; ... */



      /* calculate results for this za-bin
       */
      dratio = mdiv (ievts_on_n, ievts_off_n);
      doff_evts = dratio * (double) ievts_off_s;
      dex_evts = (double) ievts_on_s - doff_evts;
      dex_err = cts_mex_err (ievts_on_s, ievts_off_s, dratio);
      dsig = (dex_err > dEPSILON ) ? dex_evts / dex_err : -1.;

      ievts_on_s_fin += ievts_on_s;
      ievts_on_n_fin += ievts_on_n;
      ievts_off_s_fin += ievts_off_s;
      ievts_off_n_fin += ievts_off_n;


      /* fill normalized off histo and update sample histos, when finished
       */
      if (bfinished)
	{
	  cts_hopera (ha_zab_off.iid, "+", ha_zab_off.iid, ha_zab_nof.iid,
		      (float) dratio, 0.f);

	  cts_hopera (ha_on.iid, "+", ha_zab_on.iid, ha_on.iid, 1.f, 1.f);
	  cts_hopera (ha_off.iid, "+", ha_zab_off.iid, ha_off.iid, 1.f, 1.f);
	  cts_hopera (ha_nof.iid, "+", ha_zab_nof.iid, ha_nof.iid, 1.f, 1.f);
	}

      pzab = pzab->p2next;

    }   /* END of while (pzab != NULL) */



  /* calculate final significance
   */
  dratio = mdiv (ievts_on_n_fin, ievts_off_n_fin);
  doff_evts = dratio * (double) ievts_off_s_fin;
  dex_evts = (double) ievts_on_s_fin - doff_evts;
  dex_err = cts_mex_err (ievts_on_s_fin, ievts_off_s_fin, dratio);
  dsig = (dex_err > dEPSILON ) ? dex_evts / dex_err : -1.;

/*   dpurity = mdiv (dex_evts, ievts_on_s_fin); */
/*   dsig /= ((dpurity > 0.45) ? 1. : 1. + 20. * (0.45 - dpurity)); */


  /* fill results struct
   */
  presults->dsignal = dsig;
  presults->dnr_on = (double) ievts_on_s_fin;
  presults->dnr_off = dratio * (double) ievts_off_s_fin;
  presults->dex_err = dex_err;


  /* save histos and close hbook file when finished
   */
  if (bfinished)
    {
      cts_hrout (0, hfile.ilun, &hfile.copt);
      cts_hrend (&hfile);
    }

  return (-dsig);
}
