/*
 * Atmospheric Cherenkov Telescope Data Analysis Software
 * MPI f"ur Physik, M"unchen
 *
 */

/*
 * This File belongs to the program
 *
 *         C R U X
 *
 * Purpose: derive Energy and Impakt Parameter Functions from
 *          fit to MC data
 *
 * Author: D. Kranich (20.11.99)
 *
 */
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "ctshplot.h"
#include "ctscuts.h"
#include "crux.h"


/*
 * file scope variables
 */
/* used within bcut_dist(), set by main()
 */
static float fmin_dist = fMINDIST, fmax_dist = fMAXDIST;


/*
 * declaration of local functions
 */
static bool bcut_dist (HBOOK_FILE *);
static void vgive_description_and_exit ();


static bool bcut_dist (HBOOK_FILE *pntuple)

  /* this function is used to apply an additional dist-cut on the
   * events read from the ntuple
   */
{
  static char *pname = "";

  static float *pdist;

  bool bkeep = false;


  if (strcmp (pname, pntuple->pname))
    {
      /* get location of variable name in ntuple
       */
      pdist = cts_pset_nam (pntuple, cDIST);

      pname = pntuple->pname;
    }

  if (fmin_dist < *pdist && *pdist < fmax_dist)
    bkeep = true;

  return (bkeep);
}


static void vgive_description_and_exit ()

  /* give a description how to call this program
   * and exit
   */
{
  int i;

  fprintf (stdout, "Usage: crux hbook-file-list");
  fprintf (stdout, " [options]\n\n");

  fprintf (stdout, "options:\n");
  fprintf (stdout, "-cut <name>         name of cut\n");
  fprintf (stdout, "supported cuts:\n");
  for (i = 0; i < MAX_CUT_TYPE; i++)
    fprintf (stdout, "%s%s", &cut_names [i][0], (i != MAX_CUT_TYPE - 1) ? ", ": "\n\n");

  fprintf (stdout, "-out <name>         name of output ntuple");
  fprintf (stdout, " (default: \"%s\")\n", cNTUP_NAM_OUT);

  exit (iERRCODE);
}


#define CFUNCNAME "main"

int main (int argc, char **argv)

{
  int i, ip, k, idummy, inr_ntup;

  /* the order of variable names must macht the order in the output
   * ntuple below
   */
  char *cvarnam [iNTUPLE_PARS] =
  {"evt_num", "ZA", "SIZE", "DIST", "WIDTH", "LENGTH", "density", "E_mc",
   "E_est", "I_mc", "I_est", "Phi_mc", "Phi_est"};

  char *popt_str = "-cut-rmc-out-dmi-dma-gpl";

  float *pevtdat;

  double *pfpari, *pfpari_err, *pfpare, *pfpare_err;

  bool bflag, bfinished, btake_mc;

  HBOOK_FILE ntuple_mc, ntuple_out;

  FILE_TYPE dat_file;

  /* ctyp    - cut passed via command line
   */
  CUT_TYPE ctyp = NOCUT;

  /* run type - passed via command line
   */
  RUN_TYPE rtyp = MC;

  /* pointers to MC events (linked list)
   */
  ELIST_EL *pevt_el, *pevt_last, *pevt_mc = NULL;

  bool (*pcut) () = NULL;


  /* initialize hbook and hplot
   */
  cts_vinit_hbook ();
  cts_vinit_hplot (0);


  /* read in and check command line arguments
   */
  inr_ntup = 0;
  bflag = false;
  ntuple_out.pname = cNTUP_NAM_OUT;

  if (argc < 2)
    vgive_description_and_exit ();

  ip = 1;
  bfinished = false;

  while (!bfinished)
    {
      /* k is offset of string argv[ip] in string popt_str
       * (-1 if not found)
       */
      k = (ip < argc) ? cts_icheck_flag (argv[ip], popt_str, 4) : -1;

      /* if first flag is read, bflag is set to true
       * (used to mark end of ntuple names)
       */
      bflag = (k >= 0) ? true : bflag;

      switch (k)
	{
	case -1:

	  /* count number of input ntuples (ntuple names are passed
	   * without a flag and before first flag)
	   */
	  if (!bflag)
	    inr_ntup++;

	  else
	    {
	      fprintf (stdout, "\n %s: unknown flag.\n\n", argv[ip]);
	      vgive_description_and_exit ();
	    }

	  break;

	case 0:

	  /* read in cut name and check for unknown cut (cts_cut_type returns
	   * MAX_CUT_TYPE then)
	   */
	  ctyp = (++ip < argc) ? cts_cut_type (argv[ip]) : NOCUT;

	  if (ctyp == MAX_CUT_TYPE)
	    {
	      fprintf (stdout, "%s: unsupported cut.\n\n", argv[ip]);
	      vgive_description_and_exit ();
	    }
	  break;

	case 4:

	  /* set runtype for MC ntuple
	   */
	  rtyp = (++ip < argc) ? (RUN_TYPE) cts_lstrtol (argv[ip], 10) : MC;

	  if (0 > rtyp || rtyp > MAX_RUN_TYPE)
	    cts_merror ("%s: invalid runtype!\n", CFUNCNAME);

	  break;

	case 8:

	  /* read name of output ntuple
	   */
	  ntuple_out.pname = (++ip < argc) ? argv[ip] : cNTUP_NAM_OUT;
	  break;

	case 12:

	  /* read minimum dist value and put it into file
	   * scope variable (set function pointer)
	   */
	  fmin_dist = (++ip < argc) ? (float) cts_dstrtod (argv[ip]) : fMINDIST;
	  pcut = bcut_dist;

	  break;

	case 16:

	  /* read maximum dist value and put it into file
	   * scope variableb (set function pointer)
	   */
	  fmax_dist = (++ip < argc) ? (float) cts_dstrtod (argv[ip]) : fMAXDIST;
	  pcut = bcut_dist;

	  break;

	case 20:

	  /* produce plots
	   */
	  ntuple_mc.pname = (++ip < argc) ? argv[ip] : cNTUP_NAM_OUT;
	  ntuple_mc.copt = ' ';

	  btake_mc = (bool) ((++ip < argc) ? cts_lstrtol (argv[ip], 10) : 1);

	  cts_vopen_hbook_file (&ntuple_mc);
	  vplot_eires_e (&ntuple_mc, btake_mc);
	  vplot_eires_i (&ntuple_mc, btake_mc);

	  cts_hrend (&ntuple_mc);
	  exit (0);

	  break;

	default:

	  cts_merror ("%s: default reached!\n", CFUNCNAME);

	} /* END switch (k) */


      /* any argumnents left?
       */
      bfinished = (++ip < argc) ? false : true;
    }



  /* open ntuples and read in events
   */
  pevt_last = NULL;

  for (i = 1; i <= inr_ntup; i++)
    {
      ntuple_mc.pname = argv[i];
      ntuple_mc.copt = ' ';

      cts_vopen_hbook_file (&ntuple_mc);

      /* get event data into linked list and break if list is empty
       * (ctyp is passed via command line; '&idummy' is a dummy here
       *  and not used)
       */
      pevt_el = cts_pevtdat (&ntuple_mc, NOSORT, DS_ENERGY, ctyp, rtyp, pcut,
			     &idummy);

      if (pevt_el == NULL)
	cts_merror ("%s: empty ON data list.\n", CFUNCNAME);

      /* close ntuple
       */
      cts_hrend (&ntuple_mc);

      /* append list to previously read list (if existent)
       */
      if (pevt_mc == NULL)
	{
	  pevt_mc = pevt_el;
	  pevt_last = pevt_el;
	}

      else
	{
	  pevt_last->p2next = pevt_el;
	  pevt_el->p2prev = pevt_last;
	}

      /* move pevt_last to last event
       */
      while (pevt_last->p2next != NULL)
	pevt_last = pevt_last->p2next;
    }


  /* call function to determine the impact parameter
   */
  vget_ifunc (pevt_mc, &pfpari, &pfpari_err);

  /* store results
   */
  dat_file.pname = "ifunc.dat";
  dat_file.pfile = NULL;
  dat_file.acm = "w+";

  cts_vopen_file (&dat_file);

  fprintf (dat_file.pfile, "# pars: \t error\n");

  for (i = 0; i < iIFUNC_PARS; i++)
    fprintf (dat_file.pfile, "%g\t %g\n", pfpari[i], pfpari_err[i]);


  /* call function to determine the energy function
   */
  vget_efunc (pevt_mc, &pfpare, &pfpare_err);

  /* store results
   */
  dat_file.pname = "efunc.dat";
  dat_file.pfile = NULL;
  dat_file.acm = "w+";

  cts_vopen_file (&dat_file);

  fprintf (dat_file.pfile, "# pars: \t error\n");

  for (i = 0; i < iEFUNC_PARS; i++)
    fprintf (dat_file.pfile, "%g\t %g\n", pfpare[i], pfpare_err[i]);



  /* open output ntuple
   */
  ntuple_out.pvarnam = cts_pget_varnam_str (cvarnam, iNTUPLE_PARS);
  ntuple_out.invar = iNTUPLE_PARS;
  ntuple_out.ilrec = 8192;
  ntuple_out.copt = 'n';
  ntuple_out.iid = 1;

  cts_vopen_hbook_file (&ntuple_out);


  /* write all events into output ntuple
   */
  while (pevt_mc != NULL)
    {
      pevtdat = ntuple_out.pevtdat;

      *pevtdat++ = (float) pevt_mc->ievt_num;
      *pevtdat++ = (float) pevt_mc->dza;
      *pevtdat++ = (float) exp (pevt_mc->dlogsize);
      *pevtdat++ = (float) pevt_mc->ddist;
      *pevtdat++ = (float) pevt_mc->dwidth;
      *pevtdat++ = (float) pevt_mc->dlength;
      *pevtdat++ = (float) pevt_mc->ddens;
      *pevtdat++ = (float) pevt_mc->de_mc;
      *pevtdat++ = (float) pevt_mc->de_est;
      *pevtdat++ = (float) pevt_mc->di_mc;
      *pevtdat++ = (float) pevt_mc->di_est;
      *pevtdat++ = (float) pevt_mc->dphi_mc;
      *pevtdat++ = (float) pevt_mc->dphi_est;

      cts_hfn (&ntuple_out);

      pevt_mc = pevt_mc->p2next;
    }


  /* write ntuple to file & close it
   */
  cts_hrout (ntuple_out.iid, ntuple_out.ilun, &ntuple_out.copt);
  cts_hrend (&ntuple_out);


  exit(0);
}

#undef CFUNCNAME
