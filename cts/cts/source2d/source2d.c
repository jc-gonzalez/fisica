/*
 * Atmospheric Cherenkov Telescope Data Analysis Software
 * MPI f"ur Physik, M"unchen
 *
 */

/*
 * This File belongs to the program
 *
 *         S O U R C E 2 D
 *
 * Purpose: calculate and plot 2D distr. of arriving gamma events
 *
 * Author: D. Kranich
 *
 */
#include <errno.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "source2d.h"
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "ctscuts.h"


/*
 * file scope variables
 */
/* used within bcut_za(), set by main()
 */
static float fmin_alt = 0.f, fmax_alt = 180.f;
static float fmin_nmjd = 0.f, fmax_nmjd = 1e10f;


/*
 * declaration of local functions
 */
static void vgive_description_and_exit ();
static void vget_nt_ac (HBOOK_FILE *, char *, CUT_TYPE, RUN_TYPE, ZAB_LIST_EL *,
			bool (*)());
static bool bcheck_fs (double, double, SLIST_EL *, double *);
static bool bcut (HBOOK_FILE *);


static void vgive_description_and_exit ()

  /* give a description how to call this program
   * and exit
   */
{
  int i;

  fprintf (stdout, "Usage: source2d hbook-file-on hbook-file-off cut ");
  fprintf (stdout, "[options]\n\n");
  fprintf (stdout, "supported cuts:\t");
  for (i = 0; i < MAX_CUT_TYPE; i++)
    fprintf (stdout, "%s%s", &cut_names [i][0], (i != MAX_CUT_TYPE - 1) ? ", ": "\n\n");

  fprintf (stdout, "options:\n");
  fprintf (stdout, "-hon <name>         name of ON ntuple");
  fprintf (stdout, " (if not 1st argument)\n");
  fprintf (stdout, "-hoff <name>        name of OFF ntuple");
  fprintf (stdout, " (if not 2nd argument)\n");
  fprintf (stdout, "-cut <name>         name of cut");
  fprintf (stdout, " (if not 3rd argument)\n");
  fprintf (stdout, "-ron <integer>      runtype of 'hbook-file-on'");
  fprintf (stdout, " (default: '2')\n");
  fprintf (stdout, "-roff <integer>     runtype of 'hbook-file-off'");
  fprintf (stdout, " (default: '1')\n");
  fprintf (stdout, "-zma <real>         maximum zenith angle of the events");
  fprintf (stdout, " (default: '180.')\n");
  fprintf (stdout, "-zmi <real>         minimum zenith angle of the events");
  fprintf (stdout, " (default: '0.')\n");
  fprintf (stdout, "-neb <integer>      minimum nr. of events per zenith");
  fprintf (stdout, " angle bin (default: '200')\n");

  exit (iERRCODE);
}


static bool bcheck_fs (double dx, double dy, SLIST_EL *pevt_dat,
		       double *palpha_fs)

  /* this function calculates the new ALPHA and DIST values for a
   * given evnet for a given camera coordinate centre
   * input:
   * dx, dy   - coordinates of new camera centre
   * pevt_dat - data of event
   * ouptu:
   * palha_fs - new ALPHA value
   * return:
   * true if event survives new DIST cut
   */
{
  /* dodo   - distance camera centre - new camera centre
   * dndist - dist related to new camera centre
   * dcospsi - cosine of angle between fdist and fndist line
   * dxbar, dybar - c.o.m. of event
   * dalpha, ddist - HILLAS parameters
   * dphi1 - angle x-axis - line: camera centre - c.o.m.
   * dphi2 - angle x-axis - line: camera centre - new camera centre
   * dsign - determines zone of new camera centre (see below)
   */
  static double dodo, dndist, dcospsi, dxbar, dybar, dalpha;
  static double ddist, dphi1, dphi2, dsign;

  static bool bkeep;

  /* set used variables
   */
  dxbar  = pevt_dat->dxbar;
  dybar  = pevt_dat->dybar;
  dalpha = pevt_dat->dalpha;
  ddist  = sqrt (m2 (dxbar) + m2 (dybar));


  /* derive new ALPHA and DIST with respect to new camera
   * centre (dx, dy)
   */
  dndist = sqrt (m2 (dxbar - dx) + m2 (dybar - dy));
  dodo   = sqrt (m2 (dx) + m2 (dy));
  dcospsi = (m2 (dodo) - m2 (ddist) - m2 (dndist))
    / (-2. * ddist * dndist);


  /* determine zone of new camera centre:  if you put a coordinate system on top
   * of the event (origin at c.o.m.) with its y-axis pointing towards the old
   * camera centre then zone I corresponds to positive x-values and zone II to
   * negative x-values;
   * then:   NEW_ALPHA = ALPHA - psi  (zone I)
   *         NEW_ALPHA = ALPHA + psi  (zone II)
   *
   * dphi1 and dphi2 are used to determine zone
   */
  dphi1 = atan2 (dybar, dxbar);
  dphi2 = atan2 (dy, dx);

  if (dphi1 > 0.)
    dsign = mc_in_ab (dphi1 - M_PI, dphi1, dphi2) ? 1. : -1.;

  else
    dsign = mc_in_ab (dphi1, dphi1 + M_PI, dphi2) ? -1. : 1.;


  /* calculate new alpha and map it onto {-180., 180.} range
   */
  dalpha = (dalpha + d180DIVPI * dsign * acos (dcospsi));

  *palpha_fs = cts_dmap_angle (dalpha, 'D');


  bkeep = (0.5 < dndist && dndist < 1.1) ? true : false;

  return (bkeep);
}


static bool bcut (HBOOK_FILE *pntuple)

  /* this function is used to apply an additional za and noonmjd cut on the
   * events read from the ntuple. the noonmjd cut is only applied to ON-data
   * (function uses file scope variables fmax_alt, fmin_alt, fmax_nmjd and
   *  fmin_nmjd)
   */
{
  static char *pname = "";

  static float *paltdeg, *pnoonmjd, *pruntype;

  bool bkeep = false;


  if (strcmp (pname, pntuple->pname))
    {
      /* get location of variable name in ntuple
       */
      paltdeg  = cts_pset_nam (pntuple, cALTDEG);
      pnoonmjd = cts_pset_nam (pntuple, cNOONMJD);
      pruntype = cts_pset_nam (pntuple, cRUNTYPE);

      pname = pntuple->pname;
    }


  /* keep event, if za-range is ok and if event is off or on with
   * matching noonmjd-range
   */
  if (*paltdeg > fmin_alt && *paltdeg < fmax_alt)
    if ((*pruntype == ON && *pnoonmjd > fmin_nmjd && *pnoonmjd < fmax_nmjd)
	|| *pruntype == OFF)

      bkeep = true;


  return (bkeep);
}


static void vget_nt_ac (HBOOK_FILE *pntuple, char *pntnam, CUT_TYPE ctyp,
			RUN_TYPE rtyp, ZAB_LIST_EL *pzab, bool (*pcut)())
{
  int i, imax;

  float *paltdeg;
  double dza, dza_min, dza_max;

  bool bsig;
  HBOOK_FILE ntuple_ac;


  /* open ntuple and get pointers to evt. data
   */
  cts_hreopen (pntuple);
  paltdeg = cts_pset_nam (pntuple, cALTDEG);


  /* open new ntuple to store events after cuts
   */
  ntuple_ac.pvarnam = pntuple->pvarnam;
  ntuple_ac.invar = pntuple->invar;
  ntuple_ac.pname = pntnam;
  ntuple_ac.ilrec = 8192;
  ntuple_ac.copt = 'N';
  ntuple_ac.iid = 3;

  cts_vopen_hbook_file (&ntuple_ac);


  /* derive minimum and maximum za
   */
  dza_min = (rtyp == ON) ? pzab->dza_min_on : pzab->dza_min_off;

  while (pzab->p2next != NULL)
    pzab = pzab->p2next;

  dza_max = (rtyp == ON) ? pzab->dza_max_on : pzab->dza_max_off;


  /* read in events apply cuts and store surviving events to
   * disk (this is done by just resetting the pevtdat pointer)
   */
  imax = pntuple->inr_events;
  ntuple_ac.pevtdat = pntuple->pevtdat;

  for (i = 1; i <= imax; i++)
    {
      cts_hgnf (pntuple, i);
      dza = 90. - (double) *paltdeg;

      if (cts_bcuts (ctyp, pntuple, rtyp, pcut, &bsig) && dza < dza_max
	  && dza > dza_min)

	cts_hfn (&ntuple_ac);
    }


  /* write ntuple to file
   */
  cts_hrout (ntuple_ac.iid, ntuple_ac.ilun, &ntuple_ac.copt);


  /* close ntuples
   */
  cts_hrend (&ntuple_ac);
  cts_hrend (pntuple);
}


#define CFUNCNAME "source2d"

int main (int argc, char **argv)

{
  /* ibin      - bin number
   * inr_on_s  - nr. of evts in signal region ON
   * inr_on_n  - nr. of evts in normalization region ON
   * inr_off_s - nr. of evts in signal region OFF
   * inr_off_n - nr. of evts in normalization region OFF
   * ievt_on   - counter for nr. of ON evts read in the present bin
   * ievt_off  - counter for nr. of OFF evts read in the present bin
   * ihts      - size of hash table (not used here)
   */
  int i, j, k, ibin, inr_on_s, inr_on_n, inr_off_s, inr_off_n;
  int ievt_on, ievt_off, ihts;
  int imin_evts_bin = iMIN_EVTS_BIN;


  /* number of evts in different regions (see previous definitions)
   * for each false source position
   */
  int inr_on_fs_s [i2D_BINX][i2D_BINY] = {{0,0}};
  int inr_on_fs_n [i2D_BINX][i2D_BINY] = {{0,0}};
  int inr_off_fs_s [i2D_BINX][i2D_BINY] = {{0,0}};
  int inr_off_fs_n [i2D_BINX][i2D_BINY] = {{0,0}};


  /* fx        - x value of vsp. or bin (false source method)
   * fy        - y value of vsp. or bin (false source method)
   */
  float fx, fy;

  /* position of bins for 2dim histogram
   * (used for false source method)
   */
  float fbinx [i2D_BINX][i2D_BINY], fbiny [i2D_BINX][i2D_BINY];

  /* bin-sizes according to definition of 2d histos below
   * fsig, fweight - used to access histogram contents
   */
  float fbin_size_x = (f2D_XHI - f2D_XLO) / (float) i2D_BINX;
  float fbin_size_y = (f2D_YHI - f2D_YLO) / (float) i2D_BINY;
  float fsig, fweight;

  /* dalpha    - ALPHA (absolut value)
   * dalpha_fs - ALPHA (absolut value) for a given false source pos.
   * dtheta    - angular distance virtual source position (vsp) - camera center
   */
  double dalpha, dalpha_fs, dtheta;

  /* dnr_on  - nr. of all ON evts in signal region
   * dnr_off - nr. of all OFF evts in signal region
   * dnr_off_norm - normalized nr. of OFF evts in signal region
   * dratio  - ratio ON/OFF evts. in normalization region
   */
  double dratio, dnr_off, dnr_on, dnr_off_norm;


  /* flags and cut-function
   */
  bool bkeep, bfalse_src = false;
  bool (*pcut) () = NULL;

  /* string used to read in command line pars.
   */
  char *popt_str = "-hon-hof-cut-zmi-zma-ron-rof-neb-nmi-nma";

  /* ctyp    - cut passed over command line
   */
  CUT_TYPE ctyp = MAX_CUT_TYPE;


  /* type of events (ON, OFF, MC, ...) which are read from
   * ntuples;
   * may be passed via command line
   */
  RUN_TYPE rtyp_on = ON, rtyp_off = OFF;


  /* pointers to ON- and OFF events (linked lists)
   * to get the normalized OFF distribution after the ratio faktor is
   * known
   */
  SLIST_EL *pevt_on, *pevt_off;


  /* 2-dim histograms containing source-plot of ON, OFF and normalized
   * OFF events
   * 1-dim histograms containing squared theta-value (distance camera
   * center - virtual source position) and ALPHA value
   */
  HISTO hsig_on    = {sHSIG_ON_TIT, iHSIG_ON_ID, i2D_BINX, i2D_BINY,
	              f2D_XLO, f2D_XHI, f2D_YLO, f2D_YHI, 0.f};

  HISTO hsig_off   = {sHSIG_OFF_TIT, iHSIG_OFF_ID, i2D_BINX, i2D_BINY,
		      f2D_XLO, f2D_XHI, f2D_YLO, f2D_YHI, 0.f};

  HISTO hsig_off_n = {sHSIG_OFF_N_TIT, iHSIG_OFF_N_ID, i2D_BINX, i2D_BINY,
	             f2D_XLO, f2D_XHI, f2D_YLO, f2D_YHI, 0.f};

  HISTO hsig       = {sHSIG_TIT, iHSIG_ID, i2D_BINX, i2D_BINY,
		      f2D_XLO, f2D_XHI, f2D_YLO, f2D_YHI, 0.f};

  HISTO hsig_zoom  = {sHSIG_ZOOM_TIT, iHSIG_ZOOM_ID, i2D_Z_BINX, i2D_Z_BINY,
	              f2D_Z_XLO, f2D_Z_XHI, f2D_Z_YLO, f2D_Z_YHI, 0.f};

  HISTO hts        = {sHTS_TIT, iHTS_ID, i1D_TS_BINX, 0,
	              f1D_TS_XLO, f1D_TS_XHI, 0.f, 0.f, 0.f};

  HISTO hts_on     = {sHTS_ON_TIT, iHTS_ON_ID, i1D_TS_BINX, 0,
	              f1D_TS_XLO, f1D_TS_XHI, 0.f, 0.f, 0.f};

  HISTO hts_off    = {sHTS_OFF_TIT, iHTS_OFF_ID, i1D_TS_BINX, 0,
		      f1D_TS_XLO, f1D_TS_XHI, 0.f, 0.f, 0.f};

  HISTO hts_off_n  = {sHTS_OFF_N_TIT, iHTS_OFF_N_ID, i1D_TS_BINX, 0,
		      f1D_TS_XLO, f1D_TS_XHI, 0.f, 0.f, 0.f};

  HISTO ha_on      = {sHA_ON_TIT, iHALPHA_ON_ID, i1D_A_BINX, 0,
	              f1D_A_XLO, f1D_A_XHI, 0.f, 0.f, 0.f};

  HISTO ha_on2     = {sHA_ON2_TIT, iHALPHA_ON2_ID, i1D_A2_BINX, 0,
	              f1D_A2_XLO, f1D_A2_XHI, 0.f, 0.f, 0.f};

  HISTO ha_off     = {sHA_OFF_TIT, iHALPHA_OFF_ID, i1D_A_BINX, 0,
	              f1D_A_XLO, f1D_A_XHI, 0.f, 0.f, 0.f};

  HISTO ha_off2    = {sHA_OFF2_TIT, iHALPHA_OFF2_ID, i1D_A2_BINX, 0,
	              f1D_A2_XLO, f1D_A2_XHI, 0.f, 0.f, 0.f};

  HISTO ha_off_n   = {sHA_OFF_N_TIT, iHALPHA_OFF_N_ID, i1D_A_BINX, 0,
		      f1D_A_XLO, f1D_A_XHI, 0.f, 0.f, 0.f};


  /* temporary histograms 
   * (used to fill normalized histograms; they're not written to disk)
   * ...bin  - histograms which store OFF-data for one ZA-bin
   * ...zoom - histo for zoomed 2d plot (normalized OFF)
   */
  HISTO hsig_off_bin    = {"", iHSIG_OFF_BIN_ID, i2D_BINX, i2D_BINY,
			   f2D_XLO, f2D_XHI, f2D_YLO, f2D_YHI, 0.f};

  HISTO hts_off_bin     = {"", iHTS_OFF_BIN_ID, i1D_TS_BINX, 0,
			   f1D_TS_XLO, f1D_TS_XHI, 0.f, 0.f, 0.f};

  HISTO ha_off_bin      = {"", iHALPHA_OFF_BIN_ID, i1D_A_BINX, 0,
			   f1D_A_XLO, f1D_A_XHI, 0.f, 0.f, 0.f};

  HISTO hsig_off_n_zoom = {"", iHSIG_OFF_N_ZOOM_ID, i2D_Z_BINX, i2D_Z_BINY,
			   f2D_Z_XLO, f2D_Z_XHI, f2D_Z_YLO, f2D_Z_YHI, 0.f};


  HBOOK_FILE ntuple_on, ntuple_off, hfile;


  ZAB_LIST_EL *pzab;



  /* read in and check command line arguments
   */
  if (argc < 4)
    vgive_description_and_exit ();

  for (i = 1; i < argc; i++)
    {
      k = cts_icheck_flag (argv[i], popt_str, 4);

      switch (k)
	{
	case -1:

	  /* ntuple names and cut can be passed without the flag
	   * (only fixed order)
	   */
	  switch (i)
	    {
	    case 1:

	      ntuple_on.pname = argv[i];
	      break;

	    case 2:

	      ntuple_off.pname = argv[i];
	      break;

	    case 3:

	      ctyp = cts_cut_type (argv[i]);

	      /* check for unknown cut
	       */
	      if (ctyp == MAX_CUT_TYPE)
		{
		  fprintf (stdout, "%s: unsupported cut.\n\n", argv[i]);
		  vgive_description_and_exit ();
		}
	      break;

	    default:

	      fprintf (stdout, "\n %s: unknown flag.\n\n", argv[i]);
	      vgive_description_and_exit ();
	    }

	  break;

	case 0:

	  ntuple_on.pname = argv[++i];
	  break;

	case 4:

	  ntuple_off.pname = argv[++i];
	  break;

	case 8:

	  /* read in cut name and check for unknown cut
	   */
	  ctyp = cts_cut_type (argv[++i]);

	  if (ctyp == MAX_CUT_TYPE)
	    {
	      fprintf (stdout, "%s: unsupported cut.\n\n", argv[i]);
	      vgive_description_and_exit ();
	    }
	  break;

	case 12:

	  /* read minimum zenith angle and put it into file
	   * scope variable (set function pointer too)
	   */
	  fmax_alt = 90.f - (float) cts_dstrtod (argv[++i]);
	  pcut = bcut;

	  break;

	case 16:

	  /* read maximum zenith angle and put it into file
	   * scope variableb (set function pointer too)
	   */
	  fmin_alt = 90.f - (float) cts_dstrtod (argv[++i]);
	  pcut = bcut;

	  break;

	case 20:

	  /* set runtype for ON ntuple
	   */
	  rtyp_on = (RUN_TYPE) cts_lstrtol (argv[++i], 10);

	  if (0 > rtyp_on || rtyp_on > MAX_RUN_TYPE)
	    cts_merror ("%s: invalid runtype!\n", CFUNCNAME);

	  break;

	case 24:

	  /* set runtype for OFF ntuple
	   */
	  rtyp_off = (RUN_TYPE) cts_lstrtol (argv[++i], 10);

	  if (0 > rtyp_off || rtyp_off > MAX_RUN_TYPE)
	    cts_merror ("%s: invalid runtype!\n", CFUNCNAME);

	  break;

	case 28:

	  /* read number (min.) of events per za-bin
	   */
	  imin_evts_bin = (int) cts_lstrtol (argv[++i], 10);
	  break;

	case 32:

	  /* read minimum noonMJD and put it into file
	   * scope variableb (set function pointer too)
	   */
	  fmin_nmjd = (float) cts_dstrtod (argv[++i]);
	  pcut = bcut;

	  break;

	case 36:

	  /* read maximum noonMJD and put it into file
	   * scope variableb (set function pointer too)
	   */
	  fmax_nmjd = (float) cts_dstrtod (argv[++i]);
	  pcut = bcut;

	  break;

	default:

	  cts_merror ("%s: default reached!\n", CFUNCNAME);

	} /* END switch (k) */

    }   /* END   for (i = 1; ... ) */



  /* initialize hbook
   */
  cts_vinit_hbook ();


  /* book histograms
   */
  cts_vbook_hist (&hsig);
  cts_vbook_hist (&hsig_zoom);
  cts_vbook_hist (&hsig_off_n_zoom);
  cts_vbook_hist (&hts);
  cts_vbook_hist (&hts_on);
  cts_vbook_hist (&hts_off);
  cts_vbook_hist (&hts_off_n);
  cts_vbook_hist (&ha_on);
  cts_vbook_hist (&ha_on2);
  cts_vbook_hist (&ha_off);
  cts_vbook_hist (&ha_off2);
  cts_vbook_hist (&ha_off_n);
  cts_vbook_hist (&hsig_on);
  cts_vbook_hist (&hsig_off);
  cts_vbook_hist (&hsig_off_n);
  cts_vbook_hist (&ha_off_bin);
  cts_vbook_hist (&hts_off_bin);
  cts_vbook_hist (&hsig_off_bin);


  /* get information on ON ntuple
   * (ntuple name is set above)
   */
  ntuple_on.copt = ' ';

  cts_vopen_hbook_file (&ntuple_on);


  /* get event data into linked list and break if list is empty
   * (ctyp is passed via command line)
   */
  pevt_on = cts_pevtdat (&ntuple_on, SORT_ZA, DS_SIGNAL, ctyp, rtyp_on, pcut,
			 &ihts);

  if (pevt_on == NULL)
    cts_merror ("%s: empty ON data list.\n", CFUNCNAME);


  /* close ntuple
   */
  cts_hrend (&ntuple_on);



  /* get information on OFF ntuple
   * (ntuple name is set above)
   */
  ntuple_off.copt = ' ';

  cts_vopen_hbook_file (&ntuple_off);


  /* get event data into linked list and break if list is empty
   * (ctyp is passed via command line)
   */
  fmax_alt = (pcut == NULL) ? 90.f - (float) pevt_on->dza : fmax_alt;
  pevt_off = cts_pevtdat (&ntuple_off, SORT_ZA, DS_SIGNAL, ctyp, rtyp_off, pcut,
			  &ihts);

  if (pevt_off == NULL)
    cts_merror ("%s: empty OFF data list.\n", CFUNCNAME);

  /* close ntuple
   */
  cts_hrend (&ntuple_off);


  /* get distribution of zenith angle bins as linked list;
   * 'pevt_off' is set onto the first off event within the on-data
   * za-range
   */
  pzab = cts_pza_bins (pevt_on, (void **) &pevt_off, iMIN_EVTS_BIN, DS_SIGNAL);

  /* produce ntuples after cuts
   */
  vget_nt_ac (&ntuple_on, "s2d_ac_on.hbook", ctyp, rtyp_on, pzab, pcut);
  vget_nt_ac (&ntuple_off, "s2d_ac_off.hbook", ctyp, rtyp_off, pzab, pcut);


  /*
   * init variables and arrays
   */
  ibin = 0;
  inr_on_s = inr_on_n = 0;
  inr_off_s = inr_off_n = 0;

  dnr_off_norm = dnr_off = dnr_on = 0.;

  /* set x- and y-value of bins for 2dim histograms
   */
  for (i = 0; i < i2D_BINX; i++)
    for (j = 0; j < i2D_BINY; j++)
      {
	fbinx [i][j] = f2D_XLO + fbin_size_x * ((float) i + 0.5f);
	fbiny [i][j] = f2D_YLO + fbin_size_y * ((float) j + 0.5f);
      }


  /*
   * loop over all bins, read events and fill histograms
   */
  while (pzab != NULL)
    {
      ibin++;

      /* ON data bin
       */
      for (ievt_on = 0; ievt_on < pzab->ievt_on; ievt_on++)

	{
	  /* set counter for ON events in signal and normalization region
	   */
	  dalpha = fabs (pevt_on->dalpha);

	  if (pevt_on->bsig)
	    inr_on_s++;

	  else if (dALPHA_N_LO < dalpha && dalpha < dALPHA_N_HI)
	    inr_on_n++;


	  /* fill 1d histograms (OFF data is subtracted below):
	   * iHTS_ON_ID     - theta squared ON
	   * iHALPHA_ON_ID  - unsigned ALPHA ON
	   * iHALPHA_ON2_ID - signed ALPHA ON
	   */
	  dtheta = pevt_on->dtheta;
	  cts_hfill (iHTS_ON_ID, (float) m2 (dtheta), 0.f, 1.f);
	  cts_hfill (iHALPHA_ON_ID, (float) dalpha, 0.f, 1.f);
	  cts_hfill (iHALPHA_ON2_ID, (float) pevt_on->dalpha, 0.f, 1.f);



	  /* there are two different approaches for producing 2d plots:
	   * false source method   - shift camera centre and recalculate
	   *                         number of excess events -> content of
	   *                         one bin in 2d histo
	   * virtual source method - derive vs. position  for each single
	   *                         event and put it into 2d histo
	   * either one are used here
	   */
	  if (bfalse_src)

	    /* false source method
	     */
	    {
	      /* loop over all false source positions
	       */
	      for (i = 0; i < i2D_BINX; i++)
		for (j = 0; j < i2D_BINY; j++)
		  {
		    fx = fbinx [i][j];
		    fy = fbiny [i][j];

		    /* apply cuts and calculate alpha for new
		     * source position
		     */
		    bkeep = bcheck_fs ((double) fx, (double) fy, pevt_on,
				       &dalpha_fs);


		    if (bkeep)
		      {
			/* set counter for ON events in signal and
			 * normalization regions
			 */
			if (fabs (dalpha_fs) < dALPHA_CUT_FS)
			  {
			    inr_on_fs_s [i][j] += 1;

			    /* fill 2d histograms (OFF data is subtracted below):
			     * iHSIG_ID      - false source method ON - OFF
			     * iHSIG_ZOOM_ID - fsm. ON - OFF (zoomed)
			     * iHSIG_ON_ID   - fsm. ON
			     */
			    cts_hfill (iHSIG_ID, fx, fy, 1.f);
			    cts_hfill (iHSIG_ZOOM_ID, fx, fy, 1.f);
			    cts_hfill (iHSIG_ON_ID, fx, fy, 1.f);
			  }

			else if (dALPHA_N_LO < fabs (dalpha_fs)
				 && fabs (dalpha_fs) < dALPHA_N_HI)
			  inr_on_fs_n [i][j] += 1;
		      }

		  } /* END for (i = 0; ... */

	    } /* END false source method */

	  else

	    /* virtual source method
	     */
	    {
	      /* fill 2d histograms (OFF data is subtracted below):
	       * iHSIG_ID      - virtual source positions ON - OFF
	       * iHSIG_ZOOM_ID - vsp's ON - OFF (zoomed)
	       * iHSIG_ON_ID   - vsp's ON
	       */
	      dtheta = pevt_on->dtheta;
	      fx = (float) (dtheta * cos (pevt_on->dpsi));
	      fy = (float) (dtheta * sin (pevt_on->dpsi));
	      cts_hfill (iHSIG_ID, fx, fy, 1.f);
	      cts_hfill (iHSIG_ZOOM_ID, fx, fy, 1.f);
	      cts_hfill (iHSIG_ON_ID, fx, fy, 1.f);
	    }

	  pevt_on = pevt_on->p2next;

	}    /* END of  for (ievt_on = 0; ... */



      /* OFF data bin
       */
      for (ievt_off = 0; ievt_off < pzab->ievt_off; ievt_off++)

	{
	  /* set counter for ON events in signal and normalization region
	   */
	  dalpha =  fabs (pevt_off->dalpha);
	  if (pevt_off->bsig)
	    inr_off_s++;

	  else if (dALPHA_N_LO < dalpha && dalpha < dALPHA_N_HI)
	    inr_off_n++;


	  /* fill 1d histograms
	   * iHTS_OFF_BIN_ID    - theta squared OFF (one ZA-bin)
	   * iHALPHA_OFF_BIN_ID - unsigned ALPHA OFF  (one ZA-bin)
	   * iHALPHA_OFF2_ID    - signed ALPHA OFF
	   */
	  dtheta = pevt_off->dtheta;
	  cts_hfill (iHTS_OFF_BIN_ID, (float) m2 (dtheta), 0.f, 1.f);
	  cts_hfill (iHALPHA_OFF_BIN_ID, (float) dalpha, 0.f, 1.f);
	  cts_hfill (iHALPHA_OFF2_ID, (float) pevt_off->dalpha, 0.f, 1.f);



	  /* there are two different approaches for producing 2d plots:
	   * false source method   - shift camera centre and recalculate
	   *                         number of excess events -> content of
	   *                         one bin in 2d histo
	   * virtual source method - derive vs. position  for each single
	   *                         event and put it into 2d histo
	   * either one are used here
	   */
	  if (bfalse_src)

	    /* false source method
	     */
	    {
	      /* loop over all false source positions
	       */
	      for (i = 0; i < i2D_BINX; i++)
		for (j = 0; j < i2D_BINY; j++)
		  {
		    fx = fbinx [i][j];
		    fy = fbiny [i][j];

		    /* apply cuts and calculate alpha for new
		     * source position
		     */
		    bkeep = bcheck_fs ((double) fx, (double) fy, pevt_off,
				       &dalpha_fs);

		    if (bkeep)
		      {
			/* set counter for ON events in signal and
			 * normalization regions
			 */
			if (fabs (dalpha_fs) < dALPHA_CUT_FS)
			  {
			    inr_off_fs_s [i][j] += 1;

			    /* fill 2d histogram
			     * iHSIG_OFF_BIN_ID    - false source method OFF
			     * (contains data for one ZA bin)
			     */
			    cts_hfill (iHSIG_OFF_BIN_ID, fx, fy, 1.f);
			  }

			else if (dALPHA_N_LO < fabs (dalpha_fs)
				 && fabs (dalpha_fs) < dALPHA_N_HI)
			  inr_off_fs_n [i][j] += 1;

		      }

		  } /* END for (i = 0; ... */

	    } /* END false source method */

	  else

	    /* virtual source method
	     */
	    {
	      /* fill 2d histogram
	       * iHSIG_OFF_BIN_ID    - false source method OFF
	       * (contains data for one ZA bin)
	       */
	      dtheta = pevt_off->dtheta;
	      fx = (float) (dtheta * cos (pevt_off->dpsi));
	      fy = (float) (dtheta * sin (pevt_off->dpsi));
	      cts_hfill (iHSIG_OFF_BIN_ID, fx, fy, 1.f);
	    }

	  pevt_off = pevt_off->p2next;

	}    /* END of  for (ievt_on = 0; ... */



      /*
       * fill normalized OFF histo and reset counters
       */
      dratio = mdiv (inr_on_n, inr_off_n);
      dnr_off_norm += dratio * (double) inr_off_s;
      dnr_on += (double) inr_on_s;
      dnr_off += (double) inr_off_s;


      /* fill OFF-data into histograms
       * (iHSIG_OFF_BIN_ID, iHTS_OFF_BIN_ID and iHALPHA_OFF_BIN_ID
       *  contain data for one ZA bin)
       * iHTS_OFF_N_ID    - 1dim plot: theta squared OFF (normalized)
       * iHALPHA_OFF_N_ID - 1dim plot: unsigned ALPHA OFF (normalized)
       * iHSIG_OFF_N_ID   - 2dim plot: vsm. or fsm. OFF (normalized)
       */
      cts_hopera (iHTS_OFF_ID, "+", iHTS_OFF_BIN_ID, iHTS_OFF_ID, 1.f, 1.f);
      cts_hopera (iHTS_OFF_N_ID, "+", iHTS_OFF_BIN_ID, iHTS_OFF_N_ID, 1.f,
		  (float) dratio);
      cts_hopera (iHTS_ON_ID, "-", iHTS_OFF_N_ID, iHTS_ID, 1.f, 1.f);


      cts_hopera (iHALPHA_OFF_ID, "+", iHALPHA_OFF_BIN_ID, iHALPHA_OFF_ID, 
		  1.f, 1.f);
      cts_hopera (iHALPHA_OFF_N_ID, "+", iHALPHA_OFF_BIN_ID, iHALPHA_OFF_N_ID,
		  1.f, (float) dratio);


      /* 2dim histograms: loop over all bins, read in content and
       * multiply by ratio; the ratio is bin dependent for false
       * source method, but not for the virtual source method
       */
      for (j = 0; j < i2D_BINY; j++)
	for (i = 0; i < i2D_BINX; i++)
	  {
	    if (bfalse_src )
	      dratio = mdiv (inr_on_fs_n [i][j], inr_off_fs_n [i][j]);

	    fx = fbinx [i][j];
	    fy = fbiny [i][j];

	    fsig    = cts_hxy (iHSIG_OFF_BIN_ID, fx, fy);
	    fweight = fsig * (float) dratio;

	    cts_hfill (iHSIG_OFF_ID, fx, fy, fsig);
	    cts_hfill (iHSIG_OFF_N_ID, fx, fy, fweight);
	    cts_hfill (iHSIG_OFF_N_ZOOM_ID, fx, fy, fweight);
	  }


      /* report resuts for this bin and increase bin number
       */
      fprintf (stdout, "ZA bin %d:\n", ibin);
      fprintf (stdout, "max. ZA ON: %f\t", pzab->dza_max_on);
      fprintf (stdout, " OFF:%f\n", pzab->dza_max_off);
      fprintf (stdout, "sig evt. ON: %d\t", inr_on_s);
      fprintf (stdout, " sig evt. OFF: %d\n", inr_off_s);
      fprintf (stdout, "evt. ratio: %f\n", dratio);
      fprintf (stdout, "bin evt. ON: %d\t", ievt_on);
      fprintf (stdout, " bin evt. OFF: %d\n\n", ievt_off);

      /* reset counters for next bin
       */
      inr_on_s = inr_on_n = 0;
      inr_off_s = inr_off_n = 0;


      for (i = 0; i < i2D_BINX; i++)
	for (j = 0; j < i2D_BINY; j++)
	  {
	    inr_on_fs_s  [i][j] = 0;
	    inr_on_fs_n  [i][j] = 0;
	    inr_off_fs_s [i][j] = 0;
	    inr_off_fs_n [i][j] = 0;
	  }


      /* reset ZA-bin histograms
       */
      cts_hreset (iHSIG_OFF_BIN_ID, "");
      cts_hreset (iHTS_OFF_BIN_ID, "");
      cts_hreset (iHALPHA_OFF_BIN_ID, "");


      /* reset pointer to next zab-struct
       */
      pzab = pzab->p2next;

    }   /* END while (pzab != NULL) */



  /* fill 2dim histos
   * iHSIG_ID      - ON - OFF
   * iHSIG_ZOOM_ID - ON - OFF (zoomed)
   */
  cts_hopera (iHSIG_ID, "-", iHSIG_OFF_N_ID, iHSIG_ID, 1.f, 1.f);
  cts_hopera (iHSIG_ZOOM_ID, "-", iHSIG_OFF_N_ZOOM_ID, iHSIG_ZOOM_ID, 1.f, 1.f);


  /* report final result
   */
  fprintf (stdout, "final results:\n");
  fprintf (stdout, "ON evts: %f\t OFF evts: %f\n", dnr_on, dnr_off);
  fprintf (stdout, "OFF evts normalized: %f\n\n", dnr_off_norm);


  /* create hbook-file for saving histograms
   */
  hfile.pname = "source2d.hbook";
  hfile.copt = 'N';
  hfile.ilrec = 1024;
  hfile.iid = -1;

  cts_vopen_hbook_file (&hfile);


  /* write histograms to hbook-file and close files
   */
  cts_hrout (0, hfile.ilun, &hfile.copt);
  cts_hrend (&hfile);

  exit(0);
}

#undef CFUNCNAME
