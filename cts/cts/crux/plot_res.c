#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "ctshplot.h"
#include "ctscuts.h"
#include "crux.h"

/*
 * declaration of local functions
 */
static float fget_weight (float *, float *, float);
static void vplot_ehisto (HISTO, int, float, float, bool);
static void vplot_ihisto (HISTO, int, float, float, bool);
static void vplot_eres (float [], float [], float [], float [], float [],
			float [], bool, char *);
static void vplot_ires (float [], float [], float [], float [], float [],
			float [], bool, char *);

/*
 * functions
 */
static float fget_weight (float *pebound, float *peweight, float femc)

  /* this function returns the weight for a given energy
   * input:
   * pebound  - boundaries of the energy bins
   * peweight - corresponding weights
   * femc     - mc energy for which weight is returned
   */
{
  int k;
  float fweight;

  for (k = 0; k < iNIEBINS; k++)
    {
      if (femc > pebound[k] && femc < pebound[k+1])
	{
	  fweight = peweight[k];

	  k = iNIEBINS;
	}
    }

  return fweight;
}


static void vplot_ehisto (HISTO histo, int inr, float femin, float femax,
			  bool btake_mc)

  /* this function plots a given histogram and adds some text
   * (used for energy dependent histos)
   * input:
   * histo    - histogram struct
   * inr      - histogram entries
   * femin    - minimum energy (either mc or estimated)
   * femax    - maximum energy (either mc or estimated)
   * btake_mc - flag to indicate energy type for femin and femax
   */

{
  char *pstr;
  char chopt[3][iCOPT_LEN] = {{"LINX"}, {"LINY"}, {"HTIT"}};

  static bool binit = true;

  float fmean, frms;
  static float fdy = 0.45f;


  if (binit)
    {
      cts_hplsiz (20., 22., ' ');
      cts_hplzon (1, 2, 1, ' ');
      cts_hplopt (chopt, 3);
      cts_hplset ("ASIZ", 0.4);
      cts_hplset ("CSIZ", 0.1);

      binit = false;
    }


  /* fdy is used to plot text onto lower or upper picture
   */
  fdy = (fdy > 0.1f) ? 0.f : 0.45f;


  cts_hplot (histo.iid, "EP", "", 0);

  cts_iselnt (0);
  pstr = cts_pint_to_str (inr, "entries: %d");
  cts_itx (0.66f, 0.85f - fdy, pstr);

  fmean = cts_hstati (histo.iid, 1, "", 0);
  pstr = cts_preal_to_str (fmean, 3, "mean: %6.3f");
  cts_itx (0.66f, 0.82f - fdy, pstr);

  frms = cts_hstati (histo.iid, 2, "", 0);
  pstr = cts_preal_to_str (frms, 3, "rms: %6.3f");
  cts_itx (0.66f, 0.79f - fdy, pstr);

  pstr = (btake_mc) ? cSUBTITEMC : cSUBTITER;
  pstr = cts_preal_to_str (femax, 2, pstr);
  pstr = cts_preal_to_str (femin, 2, pstr);
  cts_itx (0.2f, 0.85f - fdy, pstr);
}


static void vplot_ihisto (HISTO histo, int inr, float fimin, float fimax,
			  bool btake_mc)

  /* this function plots a given histogram and adds some text
   * (used for energy dependent histos)
   * input:
   * histo    - histogram struct
   * inr      - histogram entries
   * fimin    - minimum impact parameter (either mc or estimated)
   * fimax    - maximum impact parameter (either mc or estimated)
   * btake_mc - flag to indicate impact parameter type for fimin and fimax
   */

{
  char *pstr;
  char chopt[3][iCOPT_LEN] = {{"LINX"}, {"LINY"}, {"HTIT"}};

  static bool binit = true;

  float fmean, frms;
  static float fdy = 0.45f;


  if (binit)
    {
      cts_hplsiz (20., 22., ' ');
      cts_hplzon (1, 2, 1, ' ');
      cts_hplopt (chopt, 3);
      cts_hplset ("ASIZ", 0.4);
      cts_hplset ("CSIZ", 0.1);

      binit = false;
    }


  /* fdy is used to plot text onto lower or upper picture
   */
  fdy = (fdy > 0.1f) ? 0.f : 0.45f;


  cts_hplot (histo.iid, "EP", "", 0);

  cts_iselnt (0);
  pstr = cts_pint_to_str (inr, "entries: %d");
  cts_itx (0.66f, 0.85f - fdy, pstr);

  fmean = cts_hstati (histo.iid, 1, "", 0);
  pstr = cts_preal_to_str (fmean, 3, "mean: %6.3f");
  cts_itx (0.66f, 0.82f - fdy, pstr);

  frms = cts_hstati (histo.iid, 2, "", 0);
  pstr = cts_preal_to_str (frms, 3, "rms: %6.3f");
  cts_itx (0.66f, 0.79f - fdy, pstr);

  pstr = (btake_mc) ? cSUBTITIMC : cSUBTITIR;
  pstr = cts_preal_to_str (fimax, 2, pstr);
  pstr = cts_preal_to_str (fimin, 2, pstr);
  cts_itx (0.2f, 0.85f - fdy, pstr);
}


static void vplot_eres (float febound[], float febins[], float fmean[],
			float fmean_err[], float frms[], float frms_err[],
			bool btake_mc, char *patit)
{
  char chopt[2][iCOPT_LEN] = {{"LOGX"}, {"LINY"}};
  char *pstr;

  static bool binit = true;

  float fnull[iNIEBINS] = {0.f};


  if (binit)
    {
      cts_hplopt (chopt, 2);
      cts_hplzon (1, 1, 1, ' ');
      cts_hplsiz (20., 15., ' ');
      cts_hplset ("KSIZ", 0.3);
      cts_hplset ("CSIZ", 0.3);

      binit = false;
    }

  cts_iselnt (0);
  cts_hplfra (febound[0], febound[iNIEBINS], -0.2f, 0.5f, " ");
  cts_hplerr (febins, frms, fnull, frms_err, iNIEBINS, '0', 22, 0.28f);
  cts_hplerr (febins, fmean, fnull, fmean_err, iNIEBINS, '0', 24, 0.28f);

  pstr = (btake_mc) ? ";E?mc! \"M#TeV\"N#" : ";E?r! \"M#TeV\"N#";
  pstr = cts_pstrcat (2, pstr, patit);
  cts_hpltit (pstr);

  cts_hplkey (13.f, 12.f, 22, "CT1  0^o!, RMS");
  cts_hplkey (13.f, 11.f, 24, "CT1  0^o!, MEAN");
}


static void vplot_ires (float febound[], float febins[], float fmean[],
			float fmean_err[], float frms[], float frms_err[],
			bool btake_mc, char *patit)
{
  char chopt[2][iCOPT_LEN] = {{"LINX"}, {"LINY"}};
  char *pstr;

  static bool binit = true;

  float fnull[iNIEBINS] = {0.f};


  if (binit)
    {
      cts_hplopt (chopt, 2);
      cts_hplzon (1, 1, 1, ' ');
      cts_hplsiz (20., 15., ' ');
      cts_hplset ("KSIZ", 0.3);
      cts_hplset ("CSIZ", 0.3);

      binit = false;
    }

  cts_iselnt (0);
  cts_hplfra (febound[0], febound[iNIEBINS], -0.2f, 0.5f, " ");
  cts_hplerr (febins, frms, fnull, frms_err, iNIEBINS, '0', 22, 0.28f);
  cts_hplerr (febins, fmean, fnull, fmean_err, iNIEBINS, '0', 24, 0.28f);

  pstr = (btake_mc) ? ";I?mc! \"M#m\"N#" : ";I?r! \"M#m\"N#";
  pstr = cts_pstrcat (2, pstr, patit);
  cts_hpltit (pstr);

  cts_hplkey (13.f, 12.f, 22, "CT1  0^o!, RMS");
  cts_hplkey (13.f, 11.f, 24, "CT1  0^o!, MEAN");
}


#define CFUNCNAME "vplot_eires_e"

void vplot_eires_e (HBOOK_FILE *pntuple, bool btake_mc)

  /* this function plots results from energy and impact parameter fit
   */
{
  char *pfnam = "eires_e.ps";

  int i, j, k, n, ilun, inp, imax, inoent;
  int *pnr;

  float fmde, fsde, fmdi, fsdi, fnorm;
  float ebound[iNIEBINS+1] = {0.8, 1.0, 1.5, 2.3, 3.0, 5.5, 8.4, 12., 19.6, 30};
  float ebins[iNIEBINS] = {0.9, 1.25, 1.90, 2.65, 4.25, 6.95, 10.6, 16.2, 24.8};
  float eweight24[iNIEBINS] = {2.25013, 2.54687, 1.0, 0.427619, 0.943111,
			       0.630672, 0.347570, 0.388712, 0.213921};

  float mde[iNIEBINS] = {0.f}, mde_err[iNIEBINS] = {0.f};
  float sde[iNIEBINS] = {0.f}, sde_err[iNIEBINS] = {0.f};
  float mdi[iNIEBINS] = {0.f}, mdi_err[iNIEBINS] = {0.f};
  float sdi[iNIEBINS] = {0.f}, sdi_err[iNIEBINS] = {0.f};

  float *pza, *pemc, *peest, *pimc, *piest, *pxe;

  IE_LIST_EL **ppie;
  IE_LIST_EL *pie_root, *pie_el;

  HISTO hde     = {cEHISTO_TIT, 100, 100, 0, -2.f, 2.f, 0.f, 0.f, 0.f};
  HISTO hdi     = {cIHISTO_TIT, 200, 100, 0, -2.f, 2.f, 0.f, 0.f, 0.f};
  HISTO hde_bin = {cEHISTO_TIT, 110, 50, 0, -2.f, 2.f, 0.f, 0.f, 0.f};
  HISTO hdi_bin = {cIHISTO_TIT, 210, 50, 0, -2.f, 2.f, 0.f, 0.f, 0.f};



  /* get location of variable name in ntuple
   */
  pza   = cts_pset_nam (pntuple, "ZA");
  pemc  = cts_pset_nam (pntuple, "E_mc");
  peest = cts_pset_nam (pntuple, "E_est");
  pimc  = cts_pset_nam (pntuple, "I_mc");
  piest = cts_pset_nam (pntuple, "I_est");

  /* set pointer to x-value (either MC-values for energy or estimated one)
   */
  pxe = (btake_mc) ? pemc : peest;


  /* allocate memory for arrays (reset ppie)
   */
  inp = pntuple->inr_events;
  pnr = cts_mmalloc (int, iNIEBINS, CFUNCNAME);
  ppie = cts_mmalloc (IE_LIST_EL *, iNIEBINS, CFUNCNAME);
  pie_root = cts_mmalloc (IE_LIST_EL, inp, CFUNCNAME);

  for (k = 0; k < iNIEBINS; k++)
    ppie[k] = NULL;


  /* fill arrays
   */
  pie_el = pie_root;
  cts_vreset_iarray (pnr, iNIEBINS);

  for (i = 1; i <= inp; i++)
    {
      cts_hgnf (pntuple, i);

      pie_el->fza  = *pza;
      pie_el->fde  = (*pemc - *peest) / *pemc;
      pie_el->fdi  = (*pimc - *piest) / *pimc;
      pie_el->fweight = fget_weight (ebound, eweight24, *pemc);

      /* put event into list and increase number for matching energy bin
       */
      for (k = 0; k < iNIEBINS; k++)
	{
	  if (*pxe > ebound[k] && *pxe < ebound[k+1])
	    {
	      /* check for first entry in hash table
	       */
	      pie_el->p2next = (pnr[k] == 0) ? NULL : ppie[k];
	      ppie[k] = pie_el++;

	      pnr[k] += 1;

	      k = iNIEBINS;
	    }
	}
    }



  /*
   * derive energy and impact parameter resolution using bootstrap method
   */
  srand (32789);

  /* loop over all energy bins
   */
  for (i = 0; i < iNIEBINS; i++)
    {
      /* perform bootstrap - n/2 times using n/2 of the available events
       * for this energy bin (randomly chosen) and calculate mean and sdev
       * values
       */
      imax = (int) (0.5 * (double) pnr[i]);

      for (k = 0; k < imax; k++)
	{
	  fmde = fsde = fnorm = 0;

	  for (n = 0; n < imax; n++)
	  {
	    /* random number between 1 and pnr[i]
	     */
	    j = 1 + (int) (pnr[i] * (double) rand() / (RAND_MAX + 1.0));

	    /* get event number j and use it to update mean and sdev values
	     */
	    pie_el = ppie[i];
	    mget_el (pie_el, j);

	    fnorm += pie_el->fweight;

	    fmde += pie_el->fde * pie_el->fweight;
	    fsde += m2 (pie_el->fde) * pie_el->fweight;

	    fmdi += pie_el->fdi * pie_el->fweight;
	    fsdi += m2 (pie_el->fdi) * pie_el->fweight;
	  }


	  fmde /= fnorm;
	  fsde = sqrt (fsde / fnorm - m2 (fmde));
	  fmdi /= fnorm;
	  fsdi = sqrt (fsdi / fnorm - m2 (fmdi));

	  mde[i] += fmde;
	  sde[i] += fsde;
	  mde_err[i] += m2 (fmde);
	  sde_err[i] += m2 (fsde);

	  mdi[i] += fmdi;
	  sdi[i] += fsdi;
	  mdi_err[i] += m2 (fmdi);
	  sdi_err[i] += m2 (fsdi);
	}


      mde[i] /= (float) imax;
      sde[i] /= (float) imax;
      mde_err[i] = sqrt (mde_err[i] / (float) imax - m2 (mde[i]));
      sde_err[i] = sqrt (sde_err[i] / (float) imax - m2 (sde[i]));

      mdi[i] /= (float) imax;
      sdi[i] /= (float) imax;
      mdi_err[i] = sqrt (mdi_err[i] / (float) imax - m2 (mdi[i]));
      sdi_err[i] = sqrt (sdi_err[i] / (float) imax - m2 (sdi[i]));
    }



  /*
   * produce plots
   */
  /* open ps-file
   */
  cts_vkuopen (&ilun, pfnam, "UNKNOWN");
  cts_igmeta (-ilun, -111);


  /* book histos
   */
  cts_vbook_hist (&hde);
  cts_vbook_hist (&hdi);
  cts_vbook_hist (&hde_bin);
  cts_vbook_hist (&hdi_bin);


  /* loop over all energy bins, fill histos and plot'em
   */
  inoent = 0;

  for (i = 0; i < iNIEBINS; i++)
    {
      /* count number of entries
       */
      inoent += pnr[i];

      /* read in all structs for that energy bin
       */
      pie_el = ppie[i];

      while (pie_el != NULL)
	{
	  cts_hfill (hde.iid , pie_el->fde, 0.f, pie_el->fweight);
	  cts_hfill (hdi.iid , pie_el->fdi, 0.f, pie_el->fweight);
	  cts_hfill (hde_bin.iid , pie_el->fde, 0.f, pie_el->fweight);
	  cts_hfill (hdi_bin.iid , pie_el->fdi, 0.f, pie_el->fweight);

	  pie_el = pie_el->p2next;
	}


      /* plot energy-bin histos and text
       */
      vplot_ehisto (hde_bin, pnr[i], ebound[i], ebound[i+1], btake_mc);
      vplot_ehisto (hdi_bin, pnr[i], ebound[i], ebound[i+1], btake_mc);


      /* reset bin-histos
       */
      cts_hreset (hde_bin.iid, "");
      cts_hreset (hdi_bin.iid, "");
    }


  /* plot histos for whole energy range
   */
  vplot_ehisto (hde, inoent, ebound[0], ebound[iNIEBINS], btake_mc);
  vplot_ehisto (hdi, inoent, ebound[0], ebound[iNIEBINS], btake_mc);


  /* plot energy dependent energy and impact parameter resolution
   */
  vplot_eres (ebound, ebins, mde, mde_err, sde, sde_err, btake_mc, cERES_TIT);
  vplot_eres (ebound, ebins, mdi, mdi_err, sdi, sdi_err, btake_mc, cIRES_TIT);


  /* close everything
   */
  cts_igmeta (999, 0);
  cts_hplend ();
  cts_vkuclos (ilun, " ");
}

#undef CFUNCNAME


#define CFUNCNAME "vplot_eires_e"

void vplot_eires_i (HBOOK_FILE *pntuple, bool btake_mc)

  /* this function plots results from energy and impact parameter fit
   */
{
  char *pfnam = "eires_i.ps";

  int i, j, k, n, ilun, inp, imax, inoent;
  int *pnr;

  float fmde, fsde, fmdi, fsdi, fnorm;
  float ebound[iNIEBINS+1] = {0.8, 1.0, 1.5, 2.3, 3.0, 5.5, 8.4, 12., 19.6, 30};
  float ibound[iNIEBINS+1] = {5., 35., 65., 85., 105., 115., 125., 135., 165.,
			      275.};
  float ibins[iNIEBINS] = {20., 50., 75., 95., 110., 120., 130., 150., 215.};
  float eweight24[iNIEBINS] = {2.25013, 2.54687, 1.0, 0.427619, 0.943111,
			       0.630672, 0.347570, 0.388712, 0.213921};

  float mde[iNIEBINS] = {0.f}, mde_err[iNIEBINS] = {0.f};
  float sde[iNIEBINS] = {0.f}, sde_err[iNIEBINS] = {0.f};
  float mdi[iNIEBINS] = {0.f}, mdi_err[iNIEBINS] = {0.f};
  float sdi[iNIEBINS] = {0.f}, sdi_err[iNIEBINS] = {0.f};

  float *pza, *pemc, *peest, *pimc, *piest, *pxe;

  IE_LIST_EL **ppie;
  IE_LIST_EL *pie_root, *pie_el;

  HISTO hde     = {cEHISTO_TIT, 300, 100, 0, -2.f, 2.f, 0.f, 0.f, 0.f};
  HISTO hdi     = {cIHISTO_TIT, 400, 100, 0, -2.f, 2.f, 0.f, 0.f, 0.f};
  HISTO hde_bin = {cEHISTO_TIT, 310, 50, 0, -2.f, 2.f, 0.f, 0.f, 0.f};
  HISTO hdi_bin = {cIHISTO_TIT, 410, 50, 0, -2.f, 2.f, 0.f, 0.f, 0.f};



  /* get location of variable name in ntuple
   */
  pza   = cts_pset_nam (pntuple, "ZA");
  pemc  = cts_pset_nam (pntuple, "E_mc");
  peest = cts_pset_nam (pntuple, "E_est");
  pimc  = cts_pset_nam (pntuple, "I_mc");
  piest = cts_pset_nam (pntuple, "I_est");

  /* set pointer to x-value (either MC-values for energy or estimated one)
   */
  pxe = (btake_mc) ? pimc : piest;


  /* allocate memory for arrays (reset ppie)
   */
  inp = pntuple->inr_events;
  pnr = cts_mmalloc (int, iNIEBINS, CFUNCNAME);
  ppie = cts_mmalloc (IE_LIST_EL *, iNIEBINS, CFUNCNAME);
  pie_root = cts_mmalloc (IE_LIST_EL, inp, CFUNCNAME);

  for (k = 0; k < iNIEBINS; k++)
    ppie[k] = NULL;


  /* fill arrays
   */
  pie_el = pie_root;
  cts_vreset_iarray (pnr, iNIEBINS);

  for (i = 1; i <= inp; i++)
    {
      cts_hgnf (pntuple, i);

      pie_el->fza  = *pza;
      pie_el->fde  = (*pemc - *peest) / *pemc;
      pie_el->fdi  = (*pimc - *piest) / *pimc;
      pie_el->fweight = fget_weight (ebound, eweight24, *pemc);

      /* put event into list and increase number for matching impact bin
       */
      for (k = 0; k < iNIEBINS; k++)
	{
	  if (*pxe > ibound[k] && *pxe < ibound[k+1])
	    {
	      /* check for first entry in hash table
	       */
	      pie_el->p2next = (pnr[k] == 0) ? NULL : ppie[k];
	      ppie[k] = pie_el++;

	      pnr[k] += 1;

	      k = iNIEBINS;
	    }
	}
    }



  /*
   * derive energy and impact parameter resolution using bootstrap method
   */
  srand (32789);

  /* loop over all impact bins
   */
  for (i = 0; i < iNIEBINS; i++)
    {
      /* perform bootstrap - n/2 times using n/2 of the available events
       * for this impact bin (randomly chosen) and calculate mean and sdev
       * values
       */
      imax = (int) (0.5 * (double) pnr[i]);

      for (k = 0; k < imax; k++)
	{
	  fmde = fsde = fnorm = 0;

	  for (n = 0; n < imax; n++)
	  {
	    /* random number between 1 and pnr[i]
	     */
	    j = 1 + (int) (pnr[i] * (double) rand() / (RAND_MAX + 1.0));

	    /* get event number j and use it to update mean and sdev values
	     */
	    pie_el = ppie[i];
	    mget_el (pie_el, j);

	    fnorm += pie_el->fweight;

	    fmde += pie_el->fde * pie_el->fweight;
	    fsde += m2 (pie_el->fde) * pie_el->fweight;

	    fmdi += pie_el->fdi * pie_el->fweight;
	    fsdi += m2 (pie_el->fdi) * pie_el->fweight;
	  }


	  fmde /= fnorm;
	  fsde = sqrt (fsde / fnorm - m2 (fmde));
	  fmdi /= fnorm;
	  fsdi = sqrt (fsdi / fnorm - m2 (fmdi));

	  mde[i] += fmde;
	  sde[i] += fsde;
	  mde_err[i] += m2 (fmde);
	  sde_err[i] += m2 (fsde);

	  mdi[i] += fmdi;
	  sdi[i] += fsdi;
	  mdi_err[i] += m2 (fmdi);
	  sdi_err[i] += m2 (fsdi);
	}


      mde[i] /= (float) imax;
      sde[i] /= (float) imax;
      mde_err[i] = sqrt (mde_err[i] / (float) imax - m2 (mde[i]));
      sde_err[i] = sqrt (sde_err[i] / (float) imax - m2 (sde[i]));

      mdi[i] /= (float) imax;
      sdi[i] /= (float) imax;
      mdi_err[i] = sqrt (mdi_err[i] / (float) imax - m2 (mdi[i]));
      sdi_err[i] = sqrt (sdi_err[i] / (float) imax - m2 (sdi[i]));
    }



  /*
   * produce plots
   */
  cts_vset_def ();

  /* open ps-file
   */
  cts_vkuopen (&ilun, pfnam, "UNKNOWN");
  cts_igmeta (-ilun, -111);


  /* book histos
   */
  cts_vbook_hist (&hde);
  cts_vbook_hist (&hdi);
  cts_vbook_hist (&hde_bin);
  cts_vbook_hist (&hdi_bin);


  /* loop over all impact bins, fill histos and plot'em
   */
  inoent = 0;

  for (i = 0; i < iNIEBINS; i++)
    {
      /* count number of entries
       */
      inoent += pnr[i];

      /* read in all structs for that energy bin
       */
      pie_el = ppie[i];

      while (pie_el != NULL)
	{
	  cts_hfill (hde.iid , pie_el->fde, 0.f, pie_el->fweight);
	  cts_hfill (hdi.iid , pie_el->fdi, 0.f, pie_el->fweight);
	  cts_hfill (hde_bin.iid , pie_el->fde, 0.f, pie_el->fweight);
	  cts_hfill (hdi_bin.iid , pie_el->fdi, 0.f, pie_el->fweight);

	  pie_el = pie_el->p2next;
	}


      /* plot impact-bin histos and text
       */
      vplot_ihisto (hde_bin, pnr[i], ibound[i], ibound[i+1], btake_mc);
      vplot_ihisto (hdi_bin, pnr[i], ibound[i], ibound[i+1], btake_mc);


      /* reset bin-histos
       */
      cts_hreset (hde_bin.iid, "");
      cts_hreset (hdi_bin.iid, "");
    }


  /* plot histos for whole impact range
   */
  vplot_ihisto (hde, inoent, ibound[0], ibound[iNIEBINS], btake_mc);
  vplot_ihisto (hdi, inoent, ibound[0], ibound[iNIEBINS], btake_mc);


  /* plot impact dependent energy and impact parameter resolution
   */
  vplot_ires (ibound, ibins, mde, mde_err, sde, sde_err, btake_mc, cERES_TIT);
  vplot_ires (ibound, ibins, mdi, mdi_err, sdi, sdi_err, btake_mc, cIRES_TIT);


  /* close everything
   */
  cts_igmeta (999, 0);
  cts_hplend ();
  cts_vkuclos (ilun, " ");
}

#undef CFUNCNAME
