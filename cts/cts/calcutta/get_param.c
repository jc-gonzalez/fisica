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

#include <stdio.h>
#include "ctsbase.h"
#include "ctshbook.h"
#include "ctscuts.h"
#include "calcutta.h"

/*
 * declaration of local functions
 */
static void vcheck_cutpars (CUT *);



static void vcheck_cutpars (CUT *pcut)

  /* checks contents of parameter-file for consistency
   */
{
  CUTNAME ecutnam;


  /* check cut-data
   */
  while (pcut != NULL)
    {
      ecutnam = pcut->ecutnam;

      if (ecutnam < 0 || ecutnam >= MAXCUT)
	{
	  fprintf (stderr, "\nparameterfile: this cut is not defined: ");
 	  fprintf (stderr, "cutnr = %d.\n",ecutnam);
	  fprintf (stderr, "up to now, the following cuts are defined:\n");
	  fprintf (stderr, "%30s cut:\t  cutnr = %d\n", "LENGTH", LENGTH);
	  fprintf (stderr, "%30s cut:\t  cutnr = %d\n", "WIDTH", WIDTH);
	  fprintf (stderr, "%30s cut:\t  cutnr = %d\n", "DIST", DIST);
	  fprintf (stderr, "%30s cut:\t  cutnr = %d\n", "DENSITY", DENSITY);
	  exit (iERRCODE);
        }


      if (pcut->cbound != UPPER && pcut->cbound != LOWER)
	cts_merror ("%d: only lower '1' or upper '0' cut possible.\n", ecutnam);


      if (pcut->epartype != FIXED && pcut->epartype != VARIABLE)
	cts_merror ("%d: 'partype' must be fixed '0' or variable '1'.\n",
		    ecutnam);


      pcut = pcut->p2next;
    }
}


#define CFUNCNAME "pget_parfile"

CUT *pget_parfile (char *par_file)

  /* reads content of parameter file 'par_file_path'
   * (cuts used, pre-defined ...)
   */
{
  int i, itmp;

  /* ifs       - from fscanf: contains EOF or number of read values
   * inr_cuts - counter for number of read cuts
   * ipar      - number of pars read
   */
  int ifs, inr_cuts, ipar;

  char *pstr;

  /* used to read in parameter errors
   */
  double derr;

  /* arrays of default start values and errors
   * (arrays must match definitions in calcutta.h)
   */
  double dcpar_up[iCUT_PARS][MAXCUT] = {{0.3, 0.15, 1.1, 20000., 10.}};
  double dcpar_lo[iCUT_PARS][MAXCUT] = {{0.16, 0.07, 0.5, 0., 0.}};

  double dcerr[iCUT_PARS][MAXCUT] = {{0.1, 0.05, 0.1, 1000., 4.},
				     {0.1, 0.1, 0.0, 0.1, 0.1},
				     {0.1, 0.1, 0.1, 0.1, 0.1},
				     {0.1, 0.1, 0.1, 0.1, 0.1},
				     {0.1, 0.1, 0.0, 0.1, 0.1},
				     {0.1, 0.1, 0.1, 0.1, 0.1},
				     {0.1, 0.1, 0.1, 0.1, 0.1},
				     {0.1, 0.1, 0.0, 0.1, 0.1}};


  /* bdefstartvals - start values are given in parameter file, if true
   * bfile_end     - true, if parameter file was read completely
   */
  bool bdefstartvals, bfile_end;

  /* parfile    - struct to handle file access
   * p2cut, ... - pointers to cut data
   */
  FILE_TYPE parfile;
  CUT *p2cut, *p2first_cut, *p2prev_cut;



  /* fill struct for file access
   */
  parfile.pname = par_file;
  parfile.pfile = NULL;
  parfile.acm = "r";


  /* open parameter file
   */
  cts_vopen_file (&parfile);


  /* read in file content
   */
  bfile_end = false;
  ipar = inr_cuts = 0;
  p2first_cut = p2prev_cut = NULL;

  while (!bfile_end)
    {
      /*  create & fill one CUT struct;
       */ 
      p2cut = cts_mmalloc (CUT, 1, CFUNCNAME);

      /* read in keyword and check it
       * (using the switch statement)
       */
      ifs = fscanf (parfile.pfile, cKEYWORD01, &itmp);
      p2cut->ecutnam = (CUTNAME) itmp;
      ipar++;

      switch (ifs)
	{
	case 0:

	  /* empty line, just ignore it
	   */
	  break;

	case  1:

	  /* we got it, now read the other part of CUT struct
	   * (it's save not to put the '++' within the macro!)
	   */
	  ifs = fscanf (parfile.pfile, cKEYWORD02, &itmp);
	  p2cut->cbound = (CUT_BOUND) itmp;
	  pstr = cts_pint_to_str (++ipar, "parameter %d");
	  cts_mcheck_ifs (ifs, iKEYWORDARGS, pstr);

	  ifs = fscanf (parfile.pfile, cKEYWORD03, &itmp);
	  p2cut->epartype = (PARTYPE) itmp;
	  pstr = cts_pint_to_str (++ipar, "parameter %d");
	  cts_mcheck_ifs (ifs, iKEYWORDARGS, pstr);

	  ifs = fscanf (parfile.pfile, cKEYWORD04, &bdefstartvals);
	  pstr = cts_pint_to_str (++ipar, "parameter %d");
	  cts_mcheck_ifs (ifs, iKEYWORDARGS, pstr);


	  /* check for start values and which cuts have to be optimized;
	   * (cuts with FIXED parameters are applied, but not modified)
	   */
	  if (!bdefstartvals)
	    {
	      /* check if cut is used
	       */
	      if (p2cut->epartype != FIXED)
		{
		  /* set default cut_pars values and errors
		   */
		  for (i = 0; i < iCUT_PARS; i++)
		    {
		      if (p2cut->cbound == LOWER)
			p2cut->dcut_pars[i] = dcpar_lo[i][p2cut->ecutnam];

		      else
			p2cut->dcut_pars[i] = dcpar_up[i][p2cut->ecutnam];

		      p2cut->dcut_perr[i] = dcerr[i][p2cut->ecutnam];
		    }

		}

	      else
		cts_merror ("cut %d: fixed, but no start values given",
			    p2cut->ecutnam);
	    }

	  else
	    {
	      /* read in cut_pars values
	       */
	      for (i = 0; i < iCUT_PARS; i++)
		{
		  ifs = fscanf (parfile.pfile, "%lf", &p2cut->dcut_pars[i]);
		  pstr = cts_pint_to_str (++ipar, "parameter %d");
		  cts_mcheck_ifs (ifs, iKEYWORDARGS, pstr);

		  /* read in parameter errors (use '0' if partype is fixed)
		   */
		  ifs = fscanf (parfile.pfile, "%lf", &derr);
		  pstr = cts_pint_to_str (ipar, "parameter %d");
		  cts_mcheck_ifs (ifs, iKEYWORDARGS, pstr);

		  p2cut->dcut_perr[i] = (p2cut->epartype != FIXED) ? derr : 0.;
		}
	    }

	  /* init pointer to next cut
	   */
	  p2cut->p2next = NULL;


	  /*  put cut in list;
	   */
	  inr_cuts++;

	  if (p2first_cut != NULL)
	    p2prev_cut->p2next = p2cut;

	  else
	    p2first_cut = p2cut;

	  p2prev_cut = p2cut;

	  break;


	case  EOF:

	  /*  check for empty file;
	   */
	  if (p2first_cut == NULL)
	    cts_merror ("%s: empty parameter file.\n", parfile.pname);

	  else
	    bfile_end = true;

	  break;


	default:

	  cts_merror ("%s: default reached!\n", CFUNCNAME);

	}        /* end of switch */

    }       /* end of while */



  /* check read cuts and close par file
   */
  vcheck_cutpars (p2first_cut);
  cts_vclose_file (&parfile);


  /* print log message
   */
  fprintf (stderr, "%s contains %d CUT-structs!\n\n", parfile.pname, inr_cuts);


  return (p2first_cut);
}

#undef CFUNCNAME
