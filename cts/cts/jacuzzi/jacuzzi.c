/*
 * Atmospheric Cherenkov Telescope Data Analysis Software
 * MPI f"ur Physik, M"unchen
 *
 */

/*
 * This File belongs to the program
 *
 *         J A C U Z Z I
 *
 * Purpose: calculate and plot lightcurves
 *
 * Author: D. Kranich (28.10.99)
 *
 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "ctshplot.h"
#include "ctscuts.h"
#include "jacuzzi.h"


/*
 * file scope variables
 */
/* used within bcut(), set by main()
 */
static float fmin_alt = 0.f, fmax_alt = 180.f;
static float fmin_nmjd = 0.f, fmax_nmjd = 1e10f;


/*
 * declaration of local functions
 */
static void vgive_description_and_exit ();
static void vcalc_flux (double *, double *, double, double, double);
static void vfin_off (HISTO, HISTO, SLIST_EL *, double, double, int *, int *);
static bool bcut (HBOOK_FILE *);


/*
 * functions
 */
static void vgive_description_and_exit ()

  /* give a description how to call this program
   * and exit
   */
{
  int i;

  fprintf (stdout, "Usage: jacuzzi hbook-file-on hbook-file-off cut ");
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
  fprintf (stdout, "-nam <name>         source name to be used for plots");
  fprintf (stdout, " (default: \"\")\n");
  fprintf (stdout, "-neb <integer>      minimum nr. of events per zenith");
  fprintf (stdout, " angle bin (default: '200')\n");
  fprintf (stdout, "-ndb <integer>      number of days per MJD bin (plots)");
  fprintf (stdout, " (default: '20')\n");
  fprintf (stdout, "-nmi <real>         minimum noonMJD (default: '0.')\n");
  fprintf (stdout, "-nma <real>         maximum noonMJD (default: '1e10')\n");

  exit (iERRCODE);
}


static void vcalc_flux (double *pflux, double *pfluxe, double drate,
			double drate_err, double davcosza)

  /* this function calculates the flux and flux errors, given the rate,
   * average cosine of the za and the error on the number of excess
   * events (this is a q&d implementation which has to be changed some day)
   */
{
  double dmag00 = 36537., dmag30 = 26784., dmag45 = 18035.;
/*   double dmag00 = 45080., dmag30 = 32091., dmag45 = 18967.; */
  double dexprat, dcos30, dcos45, da, db, dfac;

  dcos30 = cos (30. * dPIDIV180);
  dcos45 = cos (45. * dPIDIV180);

  if (davcosza >= dcos30)
    {
      da = (davcosza - dcos30) / (1. - dcos30);
      db = 1. - da;

      dexprat = da * dmag00 + db * dmag30;
    }
  else
    {
      da = (davcosza - dcos45) / (dcos30 - dcos45);
      db = 1. - da;

      dexprat = da * dmag30 + db * dmag45;
    }


  /* 1e4 - m^2 in cm^2;
   */
  dfac = dexprat * 1e4 * dSECS_PER_HOUR * dFLUX_UNIT;

  *pflux  = drate / dfac;
  *pfluxe = drate_err / dfac;
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


static void vfin_off (HISTO halpha, HISTO hts, SLIST_EL *pevt, double dza_min,
		      double dza_max, int *pevts_s, int *pevts_n)

  /* this function is used to get the off histograms and the number of off
   * events in the signal and normalization region for the overall data sample
   * input:
   * halpha  - histo struct for the OFF-ALPHA distribution
   * hts     - histo struct for the OFF-theta^2 distribution
   * pevt    - linked list of all OFF events
   * dza_min - min. ON zenith angle 
   * dza_max - max. ON zenith angle 
   * output:
   * pevts_s - number of OFF events in signal region
   * pevts_n - number of OFF events in normalization region
   */
{
  int ievts_s, ievts_n;
  double dalpha, dtheta;


  ievts_s = ievts_n = 0;

  /* skip events below min. za
   */
  while (pevt != NULL && pevt->dza < dza_min)
    pevt = pevt->p2next;


  while (pevt != NULL && pevt->dza < dza_max)
    {
      dtheta = (pevt->dtheta);
      dalpha = fabs (pevt->dalpha);

      cts_hfill (hts.iid, (float) m2 (dtheta), 0.f, 1.f);
      cts_hfill (halpha.iid , (float) dalpha, 0.f, 1.f);

      if (pevt->bsig)
	ievts_s++;

      else if (dALPHA_N_LO < dalpha && dalpha < dALPHA_N_HI)
	ievts_n++;

      pevt = pevt->p2next;
    }

  *pevts_s = ievts_s;
  *pevts_n = ievts_n;
}


#define CFUNCNAME "jacuzzi"

int main (int argc, char **argv)

{
  int i, k, ilun, imax, inr_evts_af, inoonmjd, inr_days;
  int izab, inr_zab, inr_zab_day;
  int ievts_zab_on_s, ievts_zab_off_s, ievts_zab_on_n, ievts_zab_off_n;
  int ievts_day_on_s, ievts_day_off_s, ievts_day_on_n, ievts_day_off_n;
  int ievts_on_s, ievts_off_s, ievts_on_n, ievts_off_n;
  int imin_evts_bin = iMIN_EVTS_BIN, imin_days_bin = iMIN_DAYS_BIN;

  char *psource_nam = "", *ppsfile_nam = "jacuzzi.ps";
  char *popt_str = "-hon-hof-cut-zmi-zma-ron-rof-nam-neb-ndb-nmi-nma";

  double dratio, dex_evts, dex_err, dsig, dobs_time, davcosza, doff_evts;
  double dza_min, dza_min_day, dza_max, dza_max_day, dmjd_min, dmjd_max;
  double dmrate, dmrate_err, dmrate_off, dsrate, dmflux, dmflux_err, dsflux;
  double dflux_max, dalpha, dtheta, dtmp;

  bool (*pcut) () = NULL;

  /* file handler for log file
   */
  FILE_TYPE logfile, dat_day, dat_zab, dat_res, dat_lc;

  HBOOK_FILE ntuple_on, ntuple_off, hfile;

  RT_LIST_EL **pprt_on;

  ZAB_LIST_EL *pzab_el;
  ZABP_LIST_EL *pzabplist, *pzabp_el;
  ZABR_LIST_EL *pzabrlist, *pzabr_el, zabr_fin;

  /* pointers to ON- and OFF events (linked lists)
   */
  SLIST_EL **pevtlist_on, *pevt_on, *pevt_off, *pevt_off_orig;

  DRD_LIST_EL *pdrdlist, *pdrd_el;

  /* ctyp    - cut passed over command line
   */
  CUT_TYPE ctyp = MAX_CUT_TYPE;

  /* type of events (ON, OFF, MC, ...) which are read from
   * ntuples;
   * may be passed via command line
   */
  RUN_TYPE rtyp_on = ON, rtyp_off = OFF;


  HISTO hts     = {sHTS_TIT, iHTS_ID, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		   0.f, 0.f, 0.f};

  HISTO hts_on  = {sHTS_ON_TIT, iHTS_ON_ID, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		   0.f, 0.f, 0.f};

  HISTO hts_off = {sHTS_OFF_TIT, iHTS_OFF_ID, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		   0.f, 0.f, 0.f};

  HISTO hts_zab     = {sHTS_TIT, 0, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		       0.f, 0.f, 0.f};

  HISTO hts_zab_on  = {sHTS_ON_TIT, 0, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		       0.f, 0.f, 0.f};

  HISTO hts_zab_off = {sHTS_OFF_TIT, 0, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		       0.f, 0.f, 0.f};

  HISTO hts_day     = {sHTS_TIT, 0, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		       0.f, 0.f, 0.f};

  HISTO hts_day_on  = {sHTS_ON_TIT, 0, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		       0.f, 0.f, 0.f};

  HISTO hts_day_off = {sHTS_OFF_TIT, 0, iHTS_BINX, 0.f, fHTS_XLO, fHTS_XHI,
		       0.f, 0.f, 0.f};

  HISTO halpha     = {sHALPHA_TIT, iHALPHA_ID, iHALPHA_BINX, 0.f, fHALPHA_XLO,
		      fHALPHA_XHI, 0.f, 0.f, 0.f};

  HISTO halpha_on  = {sHALPHA_ON_TIT, iHALPHA_ON_ID, iHALPHA_BINX, 0.f,
		      fHALPHA_XLO, fHALPHA_XHI, 0.f, 0.f, 0.f};

  HISTO halpha_off = {sHALPHA_OFF_TIT, iHALPHA_OFF_ID, iHALPHA_BINX, 0.f,
		      fHALPHA_XLO, fHALPHA_XHI, 0.f, 0.f, 0.f};

  HISTO halpha_zab     = {sHALPHA_TIT, 0, iHALPHA_BINX, 0.f, fHALPHA_XLO,
			  fHALPHA_XHI, 0.f, 0.f, 0.f};

  HISTO halpha_zab_on  = {sHALPHA_ON_TIT, 0, iHALPHA_BINX, 0.f, fHALPHA_XLO,
			  fHALPHA_XHI, 0.f, 0.f, 0.f};

  HISTO halpha_zab_off = {sHALPHA_OFF_TIT, 0, iHALPHA_BINX, 0.f, fHALPHA_XLO,
			  fHALPHA_XHI, 0.f, 0.f, 0.f};

  HISTO halpha_day     = {sHALPHA_TIT, 0, iHALPHA_BINX, 0.f, fHALPHA_XLO,
			  fHALPHA_XHI, 0.f, 0.f, 0.f};

  HISTO halpha_day_on  = {sHALPHA_ON_TIT, 0, iHALPHA_BINX, 0.f, fHALPHA_XLO,
			  fHALPHA_XHI, 0.f, 0.f, 0.f};

  HISTO halpha_day_off = {sHALPHA_OFF_TIT, 0, iHALPHA_BINX, 0.f, fHALPHA_XLO,
			  fHALPHA_XHI, 0.f, 0.f, 0.f};


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

	      /* check for unknown cut (cts_cut_type returns MAX_CUT_TYPE then)
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

	  /* read global title (source name)
	   */
	  psource_nam = argv[++i];
	  break;

	case 32:

	  /* read number (min.) of events per za-bin
	   */
	  imin_evts_bin = (int) cts_lstrtol (argv[++i], 10);
	  break;

	case 36:

	  /* read number (min.) of days per mjd-bin
	   */
	  imin_days_bin = (int) cts_lstrtol (argv[++i], 10);
	  break;

	case 40:

	  /* read minimum noonMJD and put it into file
	   * scope variableb (set function pointer too)
	   */
	  fmin_nmjd = (float) cts_dstrtod (argv[++i]);
	  pcut = bcut;

	  break;

	case 44:

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



  /* assign stderr to logfile
   */
  logfile.pname = "jacuzzi.err";
  logfile.pfile = NULL;
  logfile.acm = "w+";

  cts_vset_stream (&logfile, stderr);


  /* initialize hbook and hplot
   */
  cts_vinit_hbook ();
  cts_vinit_hplot (0);


  /* get information on ON ntuple
   * (ntuple name is set above)
   */
  ntuple_on.copt = ' ';

  cts_vopen_hbook_file (&ntuple_on);


  /* get event data into linked list and break if list is empty
   * (ctyp is passed via command line; number of days of observation
   *  period is stored in inr_days)
   */
  pevtlist_on = cts_pevtdat (&ntuple_on, SORT_DAY_ZA, DS_SIGNAL, ctyp, rtyp_on,
			     pcut, &inr_days);

  if (pevtlist_on == NULL)
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
   * (ctyp is passed via command line; '&i' is a dummy here and not used)
   */
  pevt_off_orig = cts_pevtdat (&ntuple_off, SORT_ZA, DS_SIGNAL, ctyp, rtyp_off,
			       pcut, &i);

  if (pevt_off_orig == NULL)
    cts_merror ("%s: empty OFF data list.\n", CFUNCNAME);

  /* close ntuple
   */
  cts_hrend (&ntuple_off);


  /* loop over all days, get zab-distribution and fill drd-struct
   */
  inr_zab = inr_zab_day = 0;
  pdrdlist = pdrd_el = NULL;

  for (i = 0; i < inr_days; i++)
    {
      pevt_on = pevtlist_on[i];

      /* check whether there are some observations on that day
       */
      if (pevt_on != NULL)
	{
	  /* get distribution of zenith angle bins as linked list
	   * 'pevt_off' is set onto the first off event within the on-data
	   * za-range
	   */
	  pevt_off = pevt_off_orig;
	  pzab_el = cts_pza_bins (pevt_on, (void **) &pevt_off, imin_evts_bin,
				  DS_SIGNAL);


	  /* get number of zenith-angle bins for that day and create
	   * pzabp-struct list (extension to the pzab-list)
	   */
	  inr_zab_day = 0;
	  pzabplist = pzabp_el = NULL;

	  while (pzab_el != NULL)
	    {
	      if (pzabplist == NULL)
		{
		  pzabplist = cts_mmalloc (ZABP_LIST_EL, 1, CFUNCNAME);
		  pzabp_el = pzabplist;
		  pzabp_el->p2prev = NULL;
		}
	      else
		{
		  pzabp_el->p2next = cts_mmalloc (ZABP_LIST_EL, 1, CFUNCNAME);
		  pzabp_el->p2next->p2prev = pzabp_el;
		  pzabp_el = pzabp_el->p2next;
		}

	      pzabp_el->inr_evts_af = 0;
	      pzabp_el->davcosza = 0.;
	      pzabp_el->dobs_time = 0.;
	      pzabp_el->dmean_time = 0.;
	      pzabp_el->p2next = NULL;
	      pzabp_el->pzab = pzab_el;
	      pzab_el = pzab_el->p2next;

	      inr_zab_day++;
	    }


	  /* allocate memory for array of rt-list pointers
	   * (and init them to NULL)
	   */
	  pprt_on = cts_mmalloc (RT_LIST_EL *, inr_zab_day, CFUNCNAME);

	  for (k = 0; k < inr_zab_day; k++)
	    pprt_on[k] = NULL;


	  /* allocate memory for next drd-struct and fill it
	   * (check for first element in list)
	   */
	  if (pdrdlist == NULL)
	    {
	      pdrdlist = cts_mmalloc (DRD_LIST_EL, 1, CFUNCNAME);
	      pdrd_el = pdrdlist;
	    }
	  else
	    {
	      pdrd_el->p2next = cts_mmalloc (DRD_LIST_EL, 1, CFUNCNAME);
	      pdrd_el = pdrd_el->p2next;
	    }

	  pdrd_el->inr_zab  = inr_zab_day;
	  pdrd_el->inoonmjd = pevt_on->inoonmjd;
	  pdrd_el->pzabp    = pzabplist;
	  pdrd_el->pevt_on  = pevt_on;
	  pdrd_el->pevt_off = pevt_off;
	  pdrd_el->pprt_on  = pprt_on;
	  pdrd_el->p2next   = NULL;


	  /* update counter for overall number of zab-bins
	   */
	  inr_zab += inr_zab_day;

	}   /* END if (pevt_on != NULL) */

    }    /* END  for (i = 0 ... ) */


  /* derive observation times for each day and za-bin
   */
  vget_obs_times (pdrdlist, &ntuple_on, rtyp_on, pcut);


  /* open file to store results for each day
   */
  dat_day.pname = "jacuzzi_day.out";
  dat_day.pfile = NULL;
  dat_day.acm = "w+";

  cts_vopen_file (&dat_day);

  fprintf (dat_day.pfile, "# %6s %8s %8s", "noonMJD:", "za-min:", "za-max:");
  fprintf (dat_day.pfile, "%8s %8s %10s", "evt-on:", "evt-off:", "evt-on_n:");
  fprintf (dat_day.pfile, "%12s %10s\n#\n", "evt-off_n:", "obs.time:");


  /* open file to store results for each zab
   */
  dat_zab.pname = "jacuzzi_zab.out";
  dat_zab.pfile = NULL;
  dat_zab.acm = "w+";

  cts_vopen_file (&dat_zab);

  fprintf (dat_zab.pfile, "# %6s %8s %8s", "noonMJD:", "za-min:", "za-max:");
  fprintf (dat_zab.pfile, "%8s %8s %10s", "evt-on:", "evt-off:", "evt-on_n:");
  fprintf (dat_zab.pfile, "%12s %10s\n#\n", "evt-off_n:", "obs.time:");


  /* create hbook-file for saving histograms
   */
  hfile.pname = "jacuzzi.hbook";
  hfile.copt = 'N';
  hfile.ilrec = 1024;
  hfile.iid = -1;

  cts_vopen_hbook_file (&hfile);


  /* book histograms for whole data-sample (clean up before)
   */
  cts_hdelet (0);
  cts_vbook_hist (&hts);
  cts_vbook_hist (&hts_on);
  cts_vbook_hist (&hts_off);

  cts_vbook_hist (&halpha);
  cts_vbook_hist (&halpha_on);
  cts_vbook_hist (&halpha_off);


  /* open ps-file for alpha- and theta^2 plots
   * (define a larger window for text and plots)
   */
  cts_vkuopen (&ilun, ppsfile_nam, "UNKNOWN");

  cts_igmeta (-ilun, -111);

  cts_hplset ("YMGU", 5.0);
  cts_hplsiz (20., 22., ' ');
  cts_hplzon (2, 1, 1, ' ');


  /*
   * loop over all days and za-bins, produce ALPHA and theta^2 histograms
   * and lightcurves
   */
  dza_min = 180.;
  dmjd_min = 1e99;
  dmjd_max = davcosza = 0.;
  dza_max = dobs_time = 0.;
  ievts_on_s = ievts_off_s = 0;
  ievts_on_n = ievts_off_n = inr_evts_af = 0;

  pdrd_el = pdrdlist;
  pzabrlist = pzabr_el = NULL;

  while (pdrd_el != NULL)
    {
      /* init variables and pointers
       */
      izab = 0;
      dza_max_day = 0.;
      dza_min_day = 180.;
      inoonmjd = pdrd_el->inoonmjd;
      ievts_day_on_s = ievts_day_off_s = 0;
      ievts_day_on_n = ievts_day_off_n = 0;

      pzabp_el = pdrd_el->pzabp;
      pevt_on = pdrd_el->pevt_on;
      pevt_off = pdrd_el->pevt_off;


      /* book day-histograms
       */
      inoonmjd = pevt_on->inoonmjd;
      hts_day.iid     = iHID_FAC * inoonmjd + iHTS_DAY_ID;
      hts_day_on.iid  = iHID_FAC * inoonmjd + iHTS_DAY_ON_ID;
      hts_day_off.iid = iHID_FAC * inoonmjd + iHTS_DAY_OFF_ID;

      halpha_day.iid     = iHID_FAC * inoonmjd + iHALPHA_DAY_ID;
      halpha_day_on.iid  = iHID_FAC * inoonmjd + iHALPHA_DAY_ON_ID;
      halpha_day_off.iid = iHID_FAC * inoonmjd + iHALPHA_DAY_OFF_ID;

      cts_vbook_hist (&hts_day);
      cts_vbook_hist (&hts_day_on);
      cts_vbook_hist (&hts_day_off);

      cts_vbook_hist (&halpha_day);
      cts_vbook_hist (&halpha_day_on);
      cts_vbook_hist (&halpha_day_off);


      /* loop over all za-bins and read events
       */
      while (pzabp_el != NULL)
	{
	  pzab_el = pzabp_el->pzab;

	  /* get new zab-result-struct (check for first element)
	   */
	  if (pzabrlist == NULL)
	    {
	      pzabrlist = cts_mmalloc (ZABR_LIST_EL, 1, CFUNCNAME);
	      pzabr_el = pzabrlist;
	    }
	  else
	    {
	      pzabr_el->p2next = cts_mmalloc (ZABR_LIST_EL, 1, CFUNCNAME);
	      pzabr_el = pzabr_el->p2next;
	    }

	  pzabr_el->p2next = NULL;


	  /* book zab-histograms
	   */
	  hts_zab.iid     = iHID_FAC * inoonmjd + iHTS_ZAB_ID + izab;
	  hts_zab_on.iid  = iHID_FAC * inoonmjd + iHTS_ZAB_ON_ID + izab;
	  hts_zab_off.iid = iHID_FAC * inoonmjd + iHTS_ZAB_OFF_ID + izab;

	  halpha_zab.iid     = iHID_FAC * inoonmjd + iHALPHA_ZAB_ID + izab;
	  halpha_zab_on.iid  = iHID_FAC * inoonmjd + iHALPHA_ZAB_ON_ID + izab;
	  halpha_zab_off.iid = iHID_FAC * inoonmjd + iHALPHA_ZAB_OFF_ID + izab;

	  cts_vbook_hist (&hts_zab);
	  cts_vbook_hist (&hts_zab_on);
	  cts_vbook_hist (&hts_zab_off);

	  cts_vbook_hist (&halpha_zab);
	  cts_vbook_hist (&halpha_zab_on);
	  cts_vbook_hist (&halpha_zab_off);

	  /* update and check izab
	   */
	  if (++izab > iMAX_NUM_ZAB)
	    cts_merror ("%s: max. number of za-bins reached!\n", CFUNCNAME);


	  /* ON data bin
	   */
	  imax = pzab_el->ievt_on;
	  ievts_zab_on_s = ievts_zab_on_n = 0;

	  for (i = 0; i < imax; i++)
	    {
	      /* calculate number of ON events in signal and normalization
	       * region
	       */
	      dalpha = fabs (pevt_on->dalpha);

	      if (pevt_on->bsig)
		ievts_zab_on_s++;

	      else if (dALPHA_N_LO < dalpha && dalpha < dALPHA_N_HI)
		ievts_zab_on_n++;


	      /* fill histograms
	       */
	      dtheta = pevt_on->dtheta;
	      cts_hfill (hts_zab_on.iid, (float) m2 (dtheta), 0.f, 1.f);
	      cts_hfill (halpha_zab_on.iid , (float) dalpha, 0.f, 1.f);

	      pevt_on = pevt_on->p2next;

	    }    /* END of  for (ievt_on = 0; ... ) */



	  /* OFF data bin
	   */
	  imax = pzab_el->ievt_off;
	  ievts_zab_off_s = ievts_zab_off_n = 0;

	  for (i = 0; i < imax; i++)
	    {
	      /* calculate number of OFF events in signal and normalization
	       * region
	       */
	      dalpha = fabs (pevt_off->dalpha);

	      if (pevt_off->bsig)
		ievts_zab_off_s++;

	      else if (dALPHA_N_LO < dalpha && dalpha < dALPHA_N_HI)
		ievts_zab_off_n++;


	      /* fill histograms
	       */
	      dtheta = pevt_off->dtheta;
	      cts_hfill (hts_zab_off.iid, (float) m2 (dtheta), 0.f, 1.f);
	      cts_hfill (halpha_zab_off.iid , (float) dalpha, 0.f, 1.f);

	      pevt_off = pevt_off->p2next;

	    }    /* END of  for (ievt_on = 0; ... ) */


	  /* update day-histos and report results to zab-file
	   */
	  dratio =  mdiv (ievts_zab_on_n, ievts_zab_off_n);

	  cts_hopera (hts_zab_on.iid, "-", hts_zab_off.iid, hts_zab.iid, 1.f,
		      (float) dratio);

	  cts_hopera (hts_day_on.iid, "+", hts_zab_on.iid, hts_day_on.iid, 1.f,
		      1.f);

	  cts_hopera (hts_day_off.iid, "+", hts_zab_off.iid, hts_day_off.iid,
		      1.f, 1.f);


	  cts_hopera (halpha_zab_on.iid, "-", halpha_zab_off.iid,
		      halpha_zab.iid, 1.f, (float) dratio);

	  cts_hopera (halpha_day_on.iid, "+", halpha_zab_on.iid,
		      halpha_day_on.iid, 1.f, 1.f);

	  cts_hopera (halpha_day_off.iid, "+", halpha_zab_off.iid,
		      halpha_day_off.iid, 1.f, 1.f);


	  fprintf (dat_zab.pfile, "%8i %9.2f", inoonmjd, pzab_el->dza_min_on);
	  fprintf (dat_zab.pfile, "%9.2f", pzab_el->dza_max_on);
	  fprintf (dat_zab.pfile, "%6i %9i", ievts_zab_on_s, ievts_zab_off_s);
	  fprintf (dat_zab.pfile, "%10i %12i", ievts_zab_on_n, ievts_zab_off_n);
	  fprintf (dat_zab.pfile, "%13.2f\n", pzabp_el->dobs_time);


	  /* update zab_res-struct
	   */
	  doff_evts = dratio * (double) ievts_zab_off_s;
	  dex_evts = (double) ievts_zab_on_s - doff_evts;
	  dex_err  = cts_mex_err (ievts_zab_on_s, ievts_zab_off_s, dratio);
	  dsig = (dex_err > dEPSILON) ? dex_evts / dex_err : -1.;

	  pzabr_el->inoonmjd = inoonmjd;
	  pzabr_el->ievts_on_s = ievts_zab_on_s;
	  pzabr_el->ievts_on_n = ievts_zab_on_n;
	  pzabr_el->ievts_off_s = ievts_zab_off_s;
	  pzabr_el->ievts_off_n = ievts_zab_off_n;

	  pzabr_el->dsig = dsig;
	  pzabr_el->dex_err = dex_err;
	  pzabr_el->dex_evts = dex_evts;
	  pzabr_el->dza_min = pzab_el->dza_min_on;
	  pzabr_el->dza_max = pzab_el->dza_max_on;
	  pzabr_el->davcosza = pzabp_el->davcosza;
	  pzabr_el->dobs_time = pzabp_el->dobs_time;

	  pzabr_el->dmjd = mget_mjd (inoonmjd, pzabp_el->dmean_time);
	  pzabr_el->drate = dex_evts * dSECS_PER_HOUR / pzabp_el->dobs_time;
	  pzabr_el->drate_err = pzabr_el->drate / dsig;
	  pzabr_el->drate_off = doff_evts * dSECS_PER_HOUR / pzabp_el->dobs_time;

	  dtmp = moff_err (ievts_zab_off_s, ievts_zab_off_n, dratio);
	  pzabr_el->drate_off_err = dtmp * dSECS_PER_HOUR / pzabp_el->dobs_time;


	  vcalc_flux (&pzabr_el->dflux, &pzabr_el->dflux_err, pzabr_el->drate,
		      pzabr_el->drate_err, pzabp_el->davcosza);


	  /* create ALPHA plot
	   */
	  valpha_plot (pzabr_el, halpha_zab_on.iid, halpha_zab_off.iid,
		       halpha_zab.iid, psource_nam, NULL);

	  /* create theta^2 plot
	   */
	  vtheta_plot (pzabr_el, hts_zab_on.iid, hts_zab_off.iid, hts_zab.iid,
		       psource_nam, NULL);

	  /* update counters for daily results
	   */
	  ievts_day_on_s += ievts_zab_on_s;
	  ievts_day_on_n += ievts_zab_on_n;
	  ievts_day_off_s += ievts_zab_off_s;
	  ievts_day_off_n += ievts_zab_off_n;
	  dza_min_day = min (dza_min_day, pzab_el->dza_min_on);
	  dza_max_day = max (dza_max_day, pzab_el->dza_max_on);


	  /* update counters for final result
	   */
	  dmjd_min = min (dmjd_min, pzabr_el->dmjd);
	  dmjd_max = max (dmjd_max, pzabr_el->dmjd);
	  davcosza += pzabp_el->davcosza * (double) pzabp_el->inr_evts_af;
	  inr_evts_af += pzabp_el->inr_evts_af;


	  /* set pointer to next zab-struct
	   */
	  pzabp_el = pzabp_el->p2next;

	}   /* END while (pzab_el != NULL) */


      /* create excess histos for given day, update ON histos for whole
       * data-sample and report results to day-file (OFF histos are derived
       * below - it's wrong to just add the day_off histos, since some
       * OFF-events are then used several times)
       */
      dratio = mdiv (ievts_day_on_n, ievts_day_off_n);

      cts_hopera (hts_day_on.iid, "-", hts_day_off.iid, hts_day.iid, 1.f,
		  (float) dratio);

      cts_hopera (halpha_day_on.iid, "+", halpha_day_off.iid, halpha_day.iid,
		  1.f, (float) dratio);

      cts_hopera (hts_on.iid, "+", hts_day_on.iid, hts_on.iid, 1.f, 1.f);
      cts_hopera (halpha_on.iid, "+", halpha_day_on.iid, halpha_on.iid, 1.f,
		  1.f);

      fprintf (dat_day.pfile, "%8i %9.2f", inoonmjd, dza_min_day);
      fprintf (dat_day.pfile, "%9.2f %6i", dza_max_day, ievts_day_on_s);
      fprintf (dat_day.pfile, "%9i %10i", ievts_day_off_s, ievts_day_on_n);
      fprintf (dat_day.pfile, "%12i", ievts_day_off_n);
      fprintf (dat_day.pfile, "%13.2f\n", pdrd_el->dobs_time);


      /* update counters for final results
       */
      ievts_on_s += ievts_day_on_s;
      ievts_on_n += ievts_day_on_n;

      dobs_time += pdrd_el->dobs_time;
      dza_min = min (dza_min, dza_min_day);
      dza_max = max (dza_max, dza_max_day);


      pdrd_el = pdrd_el->p2next;

    }  /* END while (pdrd_el != NULL) */

  /* final calculation of mean and time
   */
  davcosza /= (double) inr_evts_af;
  dobs_time /= dSECS_PER_HOUR;

  /* close ps-files
   */
  cts_igmeta (999, 0);
  cts_vkuclos (ilun, " ");

  /* fill final off histos and derive number of off events in signal and
   * normalization regions
   */
  vfin_off (halpha_off, hts_off, pevt_off_orig, dza_min, dza_max, &ievts_off_s,
	    &ievts_off_n);


  /* create excess histo's for whole data sample
   */
  dratio = mdiv (ievts_on_n, ievts_off_n);

  cts_hopera (hts_on.iid, "-", hts_off.iid, hts.iid, 1.f, (float) dratio);

  cts_hopera (halpha_on.iid, "-", halpha_off.iid, halpha.iid, 1.f,
	      (float) dratio);



  /*
   * report final results
   */
  /* open file for lightcurve data
   */
  dat_lc.pname = "jacuzzi_res.dat";
  dat_lc.pfile = NULL;
  dat_lc.acm = "w+";

  cts_vopen_file (&dat_lc);

  fprintf (dat_lc.pfile, "# %5s %13s", "MJD:", "flux:");
  fprintf (dat_lc.pfile, "%17s %13s", "rate:", "rate-off:");
  fprintf (dat_lc.pfile, "%13s %11s\n", "<cos(za)>:", "obs.time:");
  fprintf (dat_lc.pfile, "# %8s [%.0e cm^-2s^-1]", "", dFLUX_UNIT);
  fprintf (dat_lc.pfile, "%10s %11s %11s %11s\n#\n", "[h-1]", "[h-1]", "[deg]", "[s]");


  inr_zab = 0;
  dmrate_off = dflux_max = 0.;
  dmrate = dmrate_err = dsrate = 0.;
  dmflux = dmflux_err = dsflux = 0.;
  pzabr_el = pzabrlist;

  while (pzabr_el != NULL)
    {
      inr_zab++;

      fprintf (dat_lc.pfile, "%10.4f", pzabr_el->dmjd);
      fprintf (dat_lc.pfile, "%8.2f +- ", pzabr_el->dflux);
      fprintf (dat_lc.pfile, "%3.2f", pzabr_el->dflux_err);
      fprintf (dat_lc.pfile, "%8.2f +- ", pzabr_el->drate);
      fprintf (dat_lc.pfile, "%4.2f", pzabr_el->drate_err);
      fprintf (dat_lc.pfile, "%8.2f", pzabr_el->drate_off);
      fprintf (dat_lc.pfile, "%12.3f", pzabr_el->davcosza);
      fprintf (dat_lc.pfile, "%14.2f\n", pzabr_el->dobs_time);

      dmrate += pzabr_el->drate;
      dsrate += m2 (pzabr_el->drate);
      dmrate_err += pzabr_el->drate_err;
      dmrate_off += pzabr_el->drate_off;

      dmflux += pzabr_el->dflux;
      dsflux += m2 (pzabr_el->dflux);
      dmflux_err += pzabr_el->dflux_err;
      dflux_max = max (dflux_max, pzabr_el->dflux);

      pzabr_el = pzabr_el->p2next;
    }

  dmrate /= (double) inr_zab;
  dmrate_err /= (double) inr_zab;
  dmrate_off /= (double) inr_zab;
  dmflux /= (double) inr_zab;
  dmflux_err /= (double) inr_zab;

  if (inr_zab > 1)
    {
      dsrate = sqrt ((dsrate - m2 (dmrate) * (double) inr_zab)
		     / (double) (inr_zab - 1));

      dsflux = sqrt ((dsflux - m2 (dmflux) * (double) inr_zab)
		     / (double) (inr_zab - 1));
    }

  dratio = mdiv (ievts_on_n, ievts_off_n);
  doff_evts = dratio * (double) ievts_off_s;
  dex_evts = (double) ievts_on_s - doff_evts;
  dex_err  = cts_mex_err (ievts_on_s, ievts_off_s, dratio);
  dsig = (dex_err > dEPSILON) ? dex_evts / dex_err : -1.;


  /* fill zabr-struct with overall results and append it to list
   * (structure-fields not needed are set to 0)
   */
  zabr_fin.dflux_err = zabr_fin.drate_err = zabr_fin.drate_off_err = 0.;
  zabr_fin.inoonmjd = zabr_fin.dex_err = zabr_fin.dex_evts = zabr_fin.dmjd = 0.;

  zabr_fin.dsig = dsig;
  zabr_fin.p2next = NULL;
  zabr_fin.dflux = dmflux;
  zabr_fin.drate = dmrate;
  zabr_fin.dza_min = dza_min;
  zabr_fin.dza_max = dza_max;
  zabr_fin.davcosza = davcosza;
  zabr_fin.drate_off = dmrate_off;
  zabr_fin.ievts_on_s = ievts_on_s;
  zabr_fin.ievts_on_n = ievts_on_n;
  zabr_fin.ievts_off_s = ievts_off_s;
  zabr_fin.ievts_off_n = ievts_off_n;
  zabr_fin.dobs_time = dobs_time * dSECS_PER_HOUR;

  vplot_lc (pzabrlist, &zabr_fin, inr_zab, imin_days_bin, psource_nam);


  dat_res.pname = "jacuzzi_res.out";
  dat_res.pfile = NULL;
  dat_res.acm = "w+";

  cts_vopen_file (&dat_res);

  fprintf (dat_res.pfile, "\n\t This is %s\n\n", CFUNCNAME);
  fprintf (dat_res.pfile, "date of compilation: %s\n", __DATE__);
  fprintf (dat_res.pfile, "time of compilation: %s\n\n", __TIME__);
  fprintf (dat_res.pfile, "The following results have been obtained\n");
  fprintf (dat_res.pfile, "from the source of your choice:\n\n");

  fprintf (dat_res.pfile, "Non_s: %i   Noff_s: %i  ", ievts_on_s, ievts_off_s);
  fprintf (dat_res.pfile, "Non_n: %i   Noff_n: %i\n", ievts_on_n, ievts_off_n);
  fprintf (dat_res.pfile, "Nex: %6.2f\t sigma: %6.2f\n\n", dex_evts, dsig);

  fprintf (dat_res.pfile, "mean excess rate: %6.2f", dmrate);
  fprintf (dat_res.pfile, " +- %4.2f [h^-1]\n", dmrate_err);
  fprintf (dat_res.pfile, "standard dev.: %6.2f\n", dsrate);
  fprintf (dat_res.pfile, "mean background rate: %6.2f\n\n", dmrate_off);

  fprintf (dat_res.pfile, "mean flux: %6.2f +- %4.2f", dmflux, dmflux_err);
  fprintf (dat_res.pfile, " [%.0e cm^-2 s^-1]\n", dFLUX_UNIT);
  fprintf (dat_res.pfile, "standard dev. of the flux: %6.2f\n", dsflux);
  fprintf (dat_res.pfile, "maximum flux: %6.2f\n\n", dflux_max);

  fprintf (dat_res.pfile, "Properties of the data:\n\n");
  fprintf (dat_res.pfile, "za-range: %6.2f - %5.2f [deg]\n", dza_min, dza_max);
  fprintf (dat_res.pfile, "overall obs. time: %6.2f [h]\n", dobs_time);
  fprintf (dat_res.pfile, "MJD-range: %6.2f - %6.2f\n", dmjd_min, dmjd_max);
  fprintf (dat_res.pfile, "average cos(za): %6.3f\n\n", davcosza);


  /* write histogram to hbook-file
   */
  cts_hrout (0, hfile.ilun, &hfile.copt);
  cts_hrend (&hfile);


  /* finish hplot
   */
  cts_igend ();

  exit(0);
}

#undef CFUNCNAME
