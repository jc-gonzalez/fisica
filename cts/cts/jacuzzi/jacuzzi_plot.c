#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "ctshplot.h"
#include "ctscuts.h"
#include "jacuzzi.h"


/*
 * declaration of local functions
 */
static void vplot_tit (char *, int, char *, double, double, double);


#define CFUNCNAME "vplot_tit"

static void vplot_tit (char *psrc_nam, int itype, char *pmjdstr, double dza_min,
		       double dza_max, double dobs_time)

  /* puts source name and additional text on top of plots
   */
{
  float fchhe_tit, fchhe_txt;
  float fza_x, fza_y, ftit_x, ftit_y, fmjd_x, fmjd_y, ftime_x, ftime_y;

  char *pstr;


  /* individual settings according to 'itype'
   */
  switch (itype)
    {
    case 0:

      fza_x = 0.35f;
      fza_y = 0.86f;
      ftit_x = 0.45f;
      ftit_y = 0.97f;
      fmjd_x = 0.35f;
      fmjd_y = 0.90f;
      ftime_x = 0.35f;
      ftime_y = 0.82f;
      fchhe_tit = 0.03f;
      fchhe_txt = 0.02f;
      break;

    case 1:

      fza_x = 0.25f;
      fza_y = 0.915f;
      ftit_x = 0.45f;
      ftit_y = 0.98f;
      fmjd_x = 0.25f;
      fmjd_y = 0.94f;
      ftime_x = 0.25f;
      ftime_y = 0.89f;
      fchhe_tit = 0.02f;
      fchhe_txt = 0.015f;
      break;

    default:

      cts_merror ("%s: unknown title settings.", CFUNCNAME);
    }


  /* plot text
   */
  cts_iselnt (0);
  cts_igset ("CHHE", fchhe_tit);
  cts_itx (ftit_x, ftit_y, psrc_nam);

  cts_igset ("CHHE", fchhe_txt);
  cts_itx (fmjd_x, fmjd_y, pmjdstr);

  pstr = cts_preal_to_str (dza_min, 1, "za range:     %.1f - %%.1f \"M#deg\"N#");
  pstr = cts_preal_to_str (dza_max, 1, pstr);
  cts_itx (fza_x, fza_y, pstr);

  pstr = cts_preal_to_str (dobs_time, 1, "obs. time:    %.1f \"M#h\"N#");
  cts_itx (ftime_x, ftime_y, pstr);
}

#undef CFUNCNAME


#define CFUNCNAME "valpha_plot"

void valpha_plot (ZABR_LIST_EL *pzabr, int ihid_on, int ihid_off,
		  int ihid_ex, char *psource_nam, char *pmjdstr)

  /* creates ALPHA plot for each individual zenith angle bin
   */
{
  char *pstr;

  float fratio;
  static float *phist_cont;

  static bool binit = true;


  if (binit)
    {
      /* memory for histogram content
       */
      phist_cont = cts_mmalloc (float, iHALPHA_BINX, CFUNCNAME);
      binit = false;
    }


  /*
   * plot on, off and (in 2nd picture) on-off
   * (do not plot histogram titles, use new labels)
   */
  cts_igset ("MTYP", 21);
  cts_hplset ("YGTI", -0.5);
  cts_hplot (ihid_on, "EP", "", 0);
  cts_hpltit (";ALPHA \"M# deg. \"N#; N?ON!, N?b!");

  /* calculate normalized off-histo; the errors are derived from the
   * bin contents of the orig. off histogram
   * (hoff = dratio * (hoff_old + 0. * hoff_old))
   */
  fratio = (float) mdiv (pzabr->ievts_on_n, pzabr->ievts_off_n);
  cts_hopera (ihid_off, "+", ihid_off, iTMP_ID, fratio, 0.f);
  cts_hunpak (ihid_off, phist_cont, "", 0);
  mhoffn_err (phist_cont, pzabr);
  cts_hpake (iTMP_ID, phist_cont);

  cts_igset ("MTYP", 25);
  cts_hplset ("DMOD", 1.);
  cts_hplot (iTMP_ID, "SEP", "", 0);
  cts_hplkey (5.5, 14.9, 21, "= ON data (N?ON!)");
  cts_hplkey (5.5, 14.1, 25, "= background (N?b!)");

  /*
   * on-off (create additional histo for plotting shaded signal
   * region - first 2 bins)
   */
  cts_igset ("MTYP", 0);
  cts_hplot (ihid_ex, "E", "", 0);
  cts_hpltit (";ALPHA \"M# deg. \"N#; N?ON! - N?b!");

  /* build up 'shaded' histo
   */
  cts_hunpak (ihid_ex, phist_cont, "", 0);
  cts_vreset_farray (phist_cont+2, iHALPHA_BINX-2);
  cts_hpak (iTMP_ID, phist_cont);
  cts_hplset ("DMOD", 2.);
  cts_hplset ("HTYP", 244);
  cts_hplot (iTMP_ID, "SHIST", "", 0);
  cts_hplset ("DMOD", 1.);
  cts_hplset ("HTYP", 0);
  cts_hdelet (iTMP_ID);


  /* plot text on ALPHA plot
   * (use passed mjd-string if not NULL)
   */
  pstr = cts_pint_to_str (pzabr->inoonmjd, "noonMJD:   %d");
  pstr = (pmjdstr != NULL) ? pmjdstr : pstr;

  vplot_tit (psource_nam, 0, pstr, pzabr->dza_min, pzabr->dza_max,
	     pzabr->dobs_time / dSECS_PER_HOUR);

  cts_igset ("TXAL", 20.f);
  cts_igset ("CHHE", 0.015f);
  pstr = cts_preal_to_str (pzabr->dsig, 1, "%.1f [s]");

  cts_itx (0.75f, 0.67f, "Significance for");
  cts_itx (0.75f, 0.63f,"ALPHA \"W# 10^o!");
  cts_itx (0.75f, 0.60f, pstr);
  cts_igset ("TXAL", 0.f);
}

#undef CFUNCNAME


#define CFUNCNAME "vtheta_plot"

void vtheta_plot (ZABR_LIST_EL *pzabr, int ihid_on, int ihid_off,
		  int ihid_ex, char *psource_nam, char *pmjdstr)
{
  char *pstr;

  float fratio, fx, fy;


  /* plot on, off and (in 2nd picture) on-off
   * (do not plot histogram titles, use new labels)
   */
  cts_hplot (ihid_on, "", "", 0);
  cts_hpltit ("; [Q]^2! \"M# deg. ^2! \"N#; N?ON!, N?b!");

  fx = 50.f;
  fy = 0.75f * (float) pzabr->ievts_on_s;
  cts_itx (fx, fy, "theta^2!");


  /* calculate normalized off-histo
   * (hoff = 0.5 * dratio * (hoff_old + hoff_old))
   */
  cts_igset ("LTYP", 2);
  fratio = 0.5f * (float) mdiv (pzabr->ievts_on_n, pzabr->ievts_off_n);
  cts_hopera (ihid_off, "+", ihid_off, iTMP_ID, fratio, fratio);
  cts_hplot (iTMP_ID, "S", "", 0);
  cts_hdelet (iTMP_ID);

  cts_hplot (ihid_ex, "", "", 0);
  cts_hpltit ("; [Q]^2! \"M# deg. ^2! \"N#; N?ON! - N?b!");

  /* plot text on theta^2 plot
   * (use passed mjd-string if not NULL)
   */
  pstr = cts_pint_to_str (pzabr->inoonmjd, "noonMJD:   %d");
  pstr = (pmjdstr != NULL) ? pmjdstr : pstr;

  vplot_tit (psource_nam, 0, pstr, pzabr->dza_min, pzabr->dza_max,
	     pzabr->dobs_time / dSECS_PER_HOUR);
}

#undef CFUNCNAME


#define CFUNCNAME "vplot_lc"

void vplot_lc (ZABR_LIST_EL *pzabrlist, ZABR_LIST_EL *pzabr_fin, int inr_zab,
	       int idays_bin, char *psource_nam)
{
  int i, imjd_bins, ilun, inid;

  char *pfnam = "jacuzzi_lc.ps";
  char *pstr, *pmjdstr;

  float fx_min, fx_max, fy_min, fy_max, fdmjd;
  float fmjd_min, fmjd_max, frate_min, frate_max, fflux_min, fflux_max;
  float fsig_min, fsig_max, frate_off_min, frate_off_max, fmavcosza;
  float favcosza_min, favcosza_max, fobs_time_min, fobs_time_max;

  float *fmjd, *frate, *fflux, *fno_err, *frate_err, *fflux_err;
  float *frate_off, *frate_off_err, *favcosza, *fobs_time, *fsig;

  float xline[2], yline[2];

  double dobs_time;

  ZABR_LIST_EL *pzabr;

  HISTO hsig;


  /* open ps-file
   */
  cts_vkuopen (&ilun, pfnam, "UNKNOWN");

  cts_igmeta (-ilun, -111);


  /* set picture size in x and y (in cm) and split picture
   * into three zone
   */
  cts_hplset ("YMGU", 3.2);
  cts_hplset ("XMGL", 2.0);
  cts_hplset ("XMGR", 2.3);
  cts_hplset ("XLAB", 1.2);
  cts_hplsiz (19., 25., ' ');
  cts_hplzon (1, 3, 1, ' ');


  /* allocate memory for hplerr-arrays
   */
  fmjd = cts_mmalloc (float, inr_zab, CFUNCNAME);
  fsig = cts_mmalloc (float, inr_zab, CFUNCNAME);
  frate = cts_mmalloc (float, inr_zab, CFUNCNAME);
  fflux = cts_mmalloc (float, inr_zab, CFUNCNAME);
  favcosza = cts_mmalloc (float, inr_zab, CFUNCNAME);
  frate_off = cts_mmalloc (float, inr_zab, CFUNCNAME);
  fobs_time = cts_mmalloc (float, inr_zab, CFUNCNAME);

  fno_err = cts_mmalloc (float, inr_zab, CFUNCNAME);
  frate_err = cts_mmalloc (float, inr_zab, CFUNCNAME);
  fflux_err = cts_mmalloc (float, inr_zab, CFUNCNAME);
  frate_off_err = cts_mmalloc (float, inr_zab, CFUNCNAME);


  /* loop over results-struct to fill hplerr arrays
   */
  fsig_min = frate_off_min = 1e10f;
  fsig_max = frate_off_max = fmavcosza = 0.f;
  fmjd_max = frate_max = fflux_max = fobs_time_max = favcosza_max = 0.f;
  fmjd_min = frate_min = fflux_min = fobs_time_min = favcosza_min = 1e10f;

  pzabr = pzabrlist;

  for (i = 0; i < inr_zab; i++)
    {
      fmjd[i] = (float) pzabr->dmjd;
      fsig[i] = (float) pzabr->dsig;
      frate[i] = (float) pzabr->drate;
      fflux[i] = (float) pzabr->dflux;
      favcosza[i] = (float) pzabr->davcosza;
      frate_off[i] = (float) pzabr->drate_off;
      fobs_time[i] = (float) (pzabr->dobs_time / dSECS_PER_HOUR);

      fno_err[i] = 0.f;
      frate_err[i] = (float) pzabr->drate_err;
      fflux_err[i] = (float) pzabr->dflux_err;
      frate_off_err[i] = (float) pzabr->drate_off_err;


      /* remember minima and maxima of hplerr-arrays
       * (and calculate some additional values)
       */
      fmjd_min = min (fmjd_min, fmjd[i]);
      fmjd_max = max (fmjd_max, fmjd[i]);

      fsig_min = min (fsig_min, fsig[i]);
      fsig_max = max (fsig_max, fsig[i]);

      frate_min = min (frate_min, frate[i]);
      frate_max = max (frate_max, frate[i]);

      frate_off_min = min (frate_off_min, frate_off[i]);
      frate_off_max = max (frate_off_max, frate_off[i]);

      fflux_min = min (fflux_min, fflux[i]);
      fflux_max = max (fflux_max, fflux[i]);

      fmavcosza += favcosza[i];
      favcosza_min = min (favcosza_min, favcosza[i]);
      favcosza_max = max (favcosza_max, favcosza[i]);

      fobs_time_min = min (fobs_time_min, fobs_time[i]);
      fobs_time_max = max (fobs_time_max, fobs_time[i]);

      pzabr = pzabr->p2next;
    }

  fmavcosza /= (float) inr_zab;


  /* derive number of mjd-parts and plot lightcurves for
   * each of them
   */
  pmjdstr = NULL;
  imjd_bins = 1 + (int) ((fmjd_max - fmjd_min) / (float) idays_bin);
  fdmjd = (fmjd_max - fmjd_min) / (float) imjd_bins;

  dobs_time = pzabr_fin->dobs_time / dSECS_PER_HOUR;

  for (i = 0; i < imjd_bins; i++)
    {
     /* flux first
       */
      fx_min = fmjd_min + (float) i * fdmjd - 0.5f;
      fx_max = fx_min + fdmjd + 1.f;
      fy_min = fflux_min - 1.f;
      fy_max = fflux_max + 1.f;

      xline[0] = fx_min;
      xline[1] = fx_max;
      yline[0] = yline[1] = (float) pzabr_fin->dflux;

      cts_hplfra (fx_min, fx_max, fy_min, fy_max, " ");
      cts_hplerr (fmjd, fflux, fno_err, fflux_err, inr_zab, '1', 20, 0.2f);
      cts_hplax ("MJD","Flux (E \"G# 1.5 TeV) \"M#10^-11! cm^-2!s^-1!\"N#");
      cts_igset ("LTYP", 2);
      cts_hpline (xline, yline, 2, ' ');
      cts_igset ("LTYP", 1);


      /* rate
       */
      fy_min = min (frate_min, frate_off_min) - 5.f;
      fy_max = 1.2f * max (frate_max, frate_off_max);
      yline[0] = yline[1] = (float) pzabr_fin->drate;

      cts_hplfra (fx_min, fx_max, fy_min, fy_max, " ");
      cts_hplerr (fmjd, frate, fno_err, frate_err, inr_zab, '1', 20, 0.2f);
      cts_igset ("LTYP", 2);
      cts_hpline (xline, yline, 2, ' ');
      cts_igset ("LTYP", 1);
      yline[0] = yline[1] = (float) pzabr_fin->drate_off;
      cts_hplerr (fmjd, frate_off, fno_err, frate_off_err, inr_zab, '1', 24, 0.2f);
      cts_hplax ("MJD","rate \"M#h^-1!\"N#");
      cts_igset ("LTYP", 3);
      cts_hpline (xline, yline, 2, ' ');
      cts_igset ("LTYP", 1);


      /* obs. time
       */
      fy_min = fobs_time_min - 1.f;
      fy_max = fobs_time_max + 1.f;
      yline[0] = yline[1] = (float) dobs_time / (float) inr_zab;

      cts_hplfra (fx_min, fx_max, fy_min, fy_max, " ");
      cts_hplerr (fmjd, fobs_time, fno_err, fno_err, inr_zab, ' ', 24, 0.2f);
      cts_hplax ("MJD","~m# = obs. time \"M#h\"N#");
      cts_igset ("LTYP", 2);
      cts_hpline (xline, yline, 2, ' ');
      cts_igset ("LTYP", 1);


      /* next plot on top of 'obs-time-plot
       */
      fy_min = 0.98f * favcosza_min;
      fy_max = 1.02f * favcosza_max;
      yline[0] = yline[1] = fmavcosza;

      cts_igset ("LASI", 0.06f * (fy_max - fy_min));
      cts_igset ("LAOF", 0.5);
      cts_hplset ("XLAB",-16.4);
      cts_hplfra (fx_min, fx_max, fy_min, fy_max, "SA");
      cts_igaxis (fx_max, fx_max, fy_min, fy_max, fy_min, fy_max, 510, "+LHD");
      cts_hplerr (fmjd, favcosza, fno_err, fno_err, inr_zab, ' ', 21, 0.2f);
      cts_hplax (" ","~n# = average cos ([q])");
      cts_hplset ("XLAB", 1.2);
      cts_igset ("LTYP", 3);
      cts_hpline (xline, yline, 2, ' ');
      cts_igset ("LTYP", 1);

      /* plot title for lightcurve
       */
      pstr = cts_preal_to_str ((double) fmjd_min, 2, "MJD:            %.2f - %%.2f");
      pmjdstr = cts_preal_to_str ((double) fmjd_max, 2, pstr);

      vplot_tit (psource_nam, 1, pmjdstr, pzabr_fin->dza_min,
		 pzabr_fin->dza_max, dobs_time);


      /* significances one new page
       */
      fy_min = fsig_min - 1.f;
      fy_max = fsig_max + 1.f;

      cts_hplfra (fx_min, fx_max, fy_min, fy_max, " ");
      cts_hplerr (fmjd, fsig, fno_err, fno_err, inr_zab, ' ', 21, 0.2f);
      cts_hplax ("MJD","significance \"M#[s]\"N#");

      vplot_tit (psource_nam, 1, pmjdstr, pzabr_fin->dza_min,
		 pzabr_fin->dza_max, dobs_time);

      cts_hplzon (1, 3, 1, ' ');
    }


  /* plot final ALPHA and theta^2 distribution
   */
  cts_hplset ("YMGU", 5.0);
  cts_hplset ("XMGL", 2.0);
  cts_hplset ("XMGR", 1.5);
  cts_hplsiz (20., 22., ' ');
  cts_hplzon (2, 1, 1, ' ');

  valpha_plot (pzabr_fin, iHALPHA_ON_ID, iHALPHA_OFF_ID, iHALPHA_ID,
	       psource_nam, pmjdstr);

  vtheta_plot (pzabr_fin, iHTS_ON_ID, iHTS_OFF_ID, iHTS_ID, psource_nam,
	       pmjdstr);


  /* plot distribution of significances
   */
  hsig.phname = "";
  hsig.iid = iTMP_ID;
  hsig.ixbin = iSIG_BINX;
  hsig.iybin = 0;
  hsig.fxlow = fsig_min - 2.f;
  hsig.fxhigh = fsig_max + 2.f;
  hsig.fylow = 0.f;
  hsig.fyhigh = 0.f;
  hsig.fvmx = 0.f;

  cts_vbook_hist (&hsig);

  inid = 0;
  cts_hfpak1 (hsig.iid, inid, fsig, inr_zab);

  cts_hplzon (1, 2, 1, ' ');
  cts_hplot (hsig.iid, "", "", 0);
  cts_hpltit (";significance \"M#[s]\"N#;nr. of points");

  vplot_tit (psource_nam, 0, pmjdstr, pzabr_fin->dza_min, pzabr_fin->dza_max,
	     dobs_time);

  cts_hdelet (iTMP_ID);


  /* close higz window
   */
  cts_igmeta (999, 0);
  cts_hplend ();
  cts_vkuclos (ilun, " ");
}

#undef CFUNCNAME
