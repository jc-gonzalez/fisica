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
 * Purpose: optimizes cuts on a given on- and off-data Ntuple
 *
 * Author: D. Kranich
 *
 */

#include <string.h>
#include <stdio.h>
#include "ctsbase.h"
#include "ctshbook.h"
#include "ctscuts.h"
#include "calcutta.h"
#include "ctsminuit.h"


/*
 * file scope variables
 */
/* used within vcut (), set by main ()
 */
static DLIST_EL *pevt_on, *pevt_off;
static ZAB_LIST_EL *pzab;

/* linked list of CUT structs (each struct contains data for one cut)
 * used within vcut (), set by main ()
 */
static CUT *pclist;

/* used within bcut_za(), set by main()
 */
static float fmin_alt = 0.f, fmax_alt = 180.f;


/*
 * declaration of local functions
 */
static void vgive_description_and_exit ();
static void vwrite_datfiles (CUT *);
static void vdef_pars (CUT *);
static bool bcut_za (HBOOK_FILE *);


/*
 * functions
 */
static void vgive_description_and_exit ()

  /* give a description how to call this program
   * and exit
   */
{
  fprintf (stdout, "Usage: calcutta hbook-file-on hbook-file-off");
  fprintf (stdout, "[options]\n\n");
  fprintf (stdout, "options:\n");
  fprintf (stdout, "-hon <name>         name of ON ntuple");
  fprintf (stdout, " (if not 1st argument)\n");
  fprintf (stdout, "-hoff <name>        name of OFF ntuple");
  fprintf (stdout, " (if not 2nd argument)\n");
  fprintf (stdout, "-nam <name>         name of parameter file");
  fprintf (stdout, " (default: calcutta.par)\n");
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


static bool bcut_za (HBOOK_FILE *pntuple)

  /* this function is used to apply an additional za-cut on the events read
   * from the ntuple  (function uses file scope variables fmax_alt and fmin_alt)
   */
{
  static char *pname = "";

  static float *paltdeg;

  bool bkeep = false;


  if (strcmp (pname, pntuple->pname))
    {
      /* get location of variable name in ntuple
       */
      paltdeg = cts_pset_nam (pntuple, cALTDEG);

      pname = pntuple->pname;
    }

  if (*paltdeg > fmin_alt && *paltdeg < fmax_alt)
    bkeep = true;

  return (bkeep);
}


static void vdef_pars (CUT *pcut)

  /* defines the minuit parameters
   */
{
  int i, icut_par;
  double dcut_typ;

  icut_par = 1;

  while (pcut != NULL)
    {
      /* first variable characterizes the cut (fixed)
       */
      mset_cut_id (pcut->ecutnam, pcut->cbound, dcut_typ);
      cts_mnparm (icut_par++, "", dcut_typ, 0);
      
      for (i = 0; i < iCUT_PARS; i++)
	cts_mnparm (icut_par++, "", pcut->dcut_pars[i], pcut->dcut_perr[i]);

      pcut = pcut->p2next;
    }
}



static void vwrite_datfiles (CUT *pclist)

  /* write cut paramters to disk
   */
{
  int i;

  /* names and handler of files (to store cut pars)
   * calcutta.cut     - contains cut data usable for cuts.c
   * calcutta.par.out - contains cut data usable as input for calcutta
   */
  static FILE_TYPE par_file, cut_file;

  /* used to assign a cut name to a given cut enum when writing down results
   * (order must match definition of CUTNAME in calcutta.h)
   */
  static const char *cut_nam[MAXCUT] = {"length", "width", "dist", "density",
					"alpha"};
  /* same for upper and lower cut
   */
  static const char *cut_typ[2] = {"up", "low"};


  /* open file to store cut-data
   */
  cut_file.pname = "calcutta.cut";
  cut_file.pfile = NULL;
  cut_file.acm = "w+";

  cts_vopen_file (&cut_file);


  /* open file to store par-data
   */
  par_file.pname = "calcutta.par.out";
  par_file.pfile = NULL;
  par_file.acm = "w+";

  cts_vopen_file (&par_file);


  /* loop over all defined cuts
   */
  while (pclist != NULL)
    {
      fprintf (par_file.pfile,cKEYWORD01, pclist->ecutnam);
      fprintf (par_file.pfile,"\n");
      fprintf (par_file.pfile,cKEYWORD02, pclist->cbound);
      fprintf (par_file.pfile,"\n");
      fprintf (par_file.pfile,cKEYWORD03, pclist->epartype);
      fprintf (par_file.pfile,"\n");
      fprintf (par_file.pfile,cKEYWORD04, true);
      fprintf (par_file.pfile,"\n");

      fprintf (cut_file.pfile, "  static double %s", cut_nam[pclist->ecutnam]);
      fprintf (cut_file.pfile, "%s", cut_typ[pclist->cbound]);
      fprintf (cut_file.pfile, "[%d] = {", iCUT_PARS);

      for (i = 0; i < iCUT_PARS; i++)
	{
	  fprintf (par_file.pfile,"%f\n", pclist->dcut_pars[i]);
	  fprintf (par_file.pfile,"%f\n", pclist->dcut_perr[i]);
	  fprintf (cut_file.pfile,"%f", pclist->dcut_pars[i]);
	  fprintf (cut_file.pfile,"%s", (i+1 < iCUT_PARS) ? ", " : "};\n\n");
	}

      fprintf (par_file.pfile,"\n");

      pclist = pclist->p2next;
    }


  /* close files
   */
  cts_vclose_file (&cut_file);
  cts_vclose_file (&par_file);
}



void vcut (int *npar, double *grad, double *fcnval, double *xval, int *iflag,
	   int *dummy)

  /* this function is called by minuit and is used to get optimized cuts
   * (variables are described in the minuit manual)
   */
{
  int i, icut_par, ivarbl;

  double dpars, dperr;

  char parname[iPARNAMELEN] = {' '};
  char *pabc = cABC;
  CUT *pclist_el;

  /* struct containing results from final cuts
   */
  static RESULT results;

  /* struct used to access results file
   */
  static FILE_TYPE rfile;

  /* used to assign a cut name to a given cut enum when writing down results
   * (order must match definition of CUTNAME in calcutta.h)
   */
  static const char *cut_nam[MAXCUT] = {"LENGTH", "WIDTH", "DIST", "DENSITY",
					"ALPHA"};
  /* same for upper and lower cut
   */
  static const char *cut_typ[2] = {"up", "low"};

  /* number of applied cuts (fixed or variable)
   */
  static int inc;


  /* open output file and read input data from ntuples
   */
  if (*iflag == 1)
    {
      /* derive number of applied cuts (fixed or variable)
       */
      inc = 0;
      pclist_el = pclist;

      while (pclist_el != NULL)
	{
	  inc++;
	  pclist_el = pclist_el->p2next;
	}


      /* open file to store results
       */
      rfile.pname = cRES_FILE_NAME;
      rfile.pfile = NULL;
      rfile.acm = "w+";

      cts_vopen_file (&rfile);
      fprintf (rfile.pfile, "\n\t\t\t CALCUTTA RESULTS:\n\n");
    }


  /* calculate fval (file scope variables pevt_on, pevt_off and pzab are used)
   * (it's the negative significance of the cuts)
   */
  if (*iflag == 4)
    *fcnval = dcut_eff (xval, inc, pevt_on, pevt_off, pzab, &results, false);


  /* minuit converged (well, maybe)
   */
  if (*iflag == 3)
    {
      /* get final cut pars. and errors from minuit and write them
       * to results file (use list of defined cuts from file scope variable
       * pclist)
       */
      icut_par = 1;
      pclist_el = pclist;

      while (pclist_el != NULL)
	{
	  /* first cut parameter contains cut enum (skip it)
	   */
	  icut_par++;
	  fprintf (rfile.pfile, "\n%s ", cut_nam[pclist_el->ecutnam]);
	  fprintf (rfile.pfile, "%s:\n", cut_typ[pclist_el->cbound]);

	  for (i = 0; i < iCUT_PARS; i++)
	    {
	      cts_mnpout (icut_par++, parname, &dpars, &dperr, &ivarbl);

	      fprintf (rfile.pfile, "%c: %10.5g", pabc[i], dpars);
	      fprintf (rfile.pfile, "%10s %.4g\n", "error:", dperr);

	      pclist_el->dcut_pars[i] = dpars;
	      pclist_el->dcut_perr[i] = dperr;
	    }

	  pclist_el = pclist_el->p2next;
	}


      /* calculate & print final results
       * (file scope variables pevt_on, pevt_off and pzab are used)
       */
      *fcnval = dcut_eff (xval, inc, pevt_on, pevt_off, pzab, &results, true);

      fprintf (rfile.pfile, "\n\n\t Non: %.1f\t", results.dnr_on);
      fprintf (rfile.pfile, "Noff: %.1f", results.dnr_off);
      fprintf (rfile.pfile, "\t dex_err: %.1f\n", results.dex_err);
      fprintf (rfile.pfile, "\t Signal: %.4f \n\n", results.dsignal);


      /* save data to additional files
       */
      vwrite_datfiles (pclist);


      /* close result file
       */
      cts_vclose_file (&rfile);
    }
}


#define CFUNCNAME "calcutta"

int main (int argc, char **argv)

{
  int i, k, imin_evts_bin = 10000;

  /* variables used to pass parameters to minuit
   * (their meaning depends on the corresponding minuit command)
   */
  int iargs;
  double dval[2];

  /* command strings passed to minuit
   */
  char *psimplex   = "simplex";
  char *pcallvcut   = "call vcut";

  char *popt_str = "-hon-hof-nam-zmi-zma-ron-rof-neb";

  /* default name for parameter file
   */
  char *p2par_file = "calcutta.par";

  bool (*pcut) () = NULL;

  /* type of events (ON, OFF, MC, ...) which are read from
   * ntuples;
   * may be passed via command line
   */
  RUN_TYPE rtyp_on = ON, rtyp_off = OFF;

  /* file handler for log file
   */
  FILE_TYPE logfile;

  HBOOK_FILE ntuple_on, ntuple_off;


  /* read in and check command line arguments
   */
  if (argc < 3)
    vgive_description_and_exit ();

  for (i = 1; i < argc; i++)
    {
      k = cts_icheck_flag (argv[i], popt_str, 4);

      switch (k)
	{
	case -1:

	  /* ntuple names and par-file name can be passed without the flag
	   * (only fixed order)
	   */
	  switch (i)
	    {
	    case 1:

	      /* set on ntuple name in file scope variable
	       */
	      ntuple_on.pname = argv[i];
	      break;

	    case 2:

	      /* set off ntuple name in file scope variable
	       */
	      ntuple_off.pname = argv[i];
	      break;

	    case 3:

	      p2par_file = argv[i];
	      break;

	    default:

	      fprintf (stdout, "\n %s: unknown flag.\n\n", argv[i]);
	      vgive_description_and_exit ();
	    }

	  break;

	case 0:

	  /* set on ntuple name in file scope variable
	   */
	  ntuple_on.pname = argv[++i];
	  break;

	case 4:

	  /* set off ntuple name in file scope variable
	   */
	  ntuple_off.pname = argv[++i];
	  break;

	case 8:

	  p2par_file = argv[++i];
	  break;

	case 12:

	  /* read minimum zenith angle and put it into file
	   * scope variable (set function pointer too)
	   */
	  fmax_alt = 90.f - (float) cts_dstrtod (argv[++i]);
	  pcut = bcut_za;

	  break;

	case 16:

	  /* read maximum zenith angle and put it into file
	   * scope variableb (set function pointer too)
	   */
	  fmin_alt = 90.f - (float) cts_dstrtod (argv[++i]);
	  pcut = bcut_za;

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

	default:

	  cts_merror ("%s: default reached!\n", CFUNCNAME);

	} /* END switch (k) */

    }   /* END   for (i = 1; ... ) */



  /* assign stderr to logfile
   */
  logfile.pname = "calcutta.log";
  logfile.pfile = NULL;
  logfile.acm = "w+";

  cts_vset_stream (&logfile, stderr);


  /* write messages to log file
   * (some depend on special defines set)
   */
  fprintf (stderr, "\n\t this is calcutta\n\n");
  fprintf (stderr, "date of compilation: %s\n", __DATE__);
  fprintf (stderr, "time of compilation: %s\n\n", __TIME__);


  /* read content of cut parameter file in file scope variable
   */
  pclist = pget_parfile (p2par_file);


  /* initialize hbook
   */
  cts_vinit_hbook ();


  /* get information on ON ntuple
   * (ntuple name is set above)
   */
  ntuple_on.copt = ' ';

  cts_vopen_hbook_file (&ntuple_on);


  /* get event data into linked list (file scope variable) and break if
   * list is empty ('&i' is a dummy here and not used)
   */
  pevt_on = cts_pevtdat (&ntuple_on, SORT_ZA, DS_DATA, FILTER, rtyp_on, pcut,
			 &i);

  if (pevt_on == NULL)
    cts_merror ("%s: empty ON data list.\n", ntuple_on.pname);


  /* close ntuple
   */
  cts_hrend (&ntuple_on);


  /* get information on OFF ntuple
   * (ntuple name is set above)
   */
  ntuple_off.copt = ' ';

  cts_vopen_hbook_file (&ntuple_off);


  /* get event data into linked list (file scope variable) and break if
   * list is empty ('&i' is a dummy here and not used)
   */
  pevt_off = cts_pevtdat (&ntuple_off, SORT_ZA, DS_DATA, FILTER, rtyp_off, pcut,
			  &i);

  if (pevt_off == NULL)
    cts_merror ("%s: empty OFF data list.\n", ntuple_off.pname);


  /* close ntuple
   */
  cts_hrend (&ntuple_off);


  /* get distribution of zenith angle bins as linked list (file scope variable)
   * 'pevt_off' is set onto the first off event within the on-data
   * za-range
   */
  pzab = cts_pza_bins (pevt_on, (void **) &pevt_off, imin_evts_bin, DS_DATA);


  /* initialize minuit
   */
  cts_mninit (5, 6, 7);


  /* define parameters in minuit
   */
  vdef_pars (pclist);


  /* read in data
   */
  iargs = 1;
  *dval = 1.;
  cts_mnexcm (vcut, pcallvcut, dval, iargs);


  /* start MINUIT
   */
  iargs = 2;
  *dval = 1000.;
  *(dval+1) = 0.1;
  cts_mnexcm (vcut, psimplex, dval, iargs);


  /* calculate & write down final result
   */
  iargs = 1;
  *dval = 3.;
  cts_mnexcm (vcut, pcallvcut, dval, iargs);


  fprintf (stderr,"\n\t calcutta finished successfully!!");
  fprintf (stderr,"\n\t find results in %s", cRES_FILE_NAME);
  fprintf (stderr,"\n\t\t have a nice day!\n\n");

  exit (0);
}

#undef CFUNCNAME
