/*
 * this program is used to analyse time-series and to run MC-Simulations
 * reltated to time-series analysis
 * this program is suitable for non-equally spaced data points
 *
 * D.K. 09.07.1998
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "powspec.h"

/*
 * declaration of local functions
 */
static int iget_errflag (char **, int);
static int icompar (const void *, const void *);
static int iget_npar (int, char *, char **, int);
static void vget_exf_list (int, char **, int, int **);
static void vgive_description_and_exit ();


/*
 * functions
 */
static int iget_errflag (char **argv, int argc)

  /* scan all passed arguments for perr_str and set return value
   * according to its parameter (or to default, if not found)
   */
{
  int i, ierrflag = 0;
  char *perr_str = "-errf";

  for (i = 1; i < argc; i++)
    {
      if (cts_icheck_flag (argv[i], perr_str, 5) == 0)
	ierrflag = (++i < argc) ? (int) cts_lstrtol (argv[i], 10) : iERRF_DEF;
    }

  return (ierrflag);
}


static int iget_npar (int ipar, char *popt_str, char **argv, int argc)

  /* this function counts the number of parameters between two consecutive
   * option flags (specified by popt_str)
   * input:
   * ipar     - position of first flag in argv
   * popt_str - string of flag names
   * argv     - array of parameters and flags
   * argc     - number of elements in argv
   * return:
   * number of parameters
   */
{
  int inr_pars;
  bool bfinished;


  inr_pars = 0;
  bfinished = false;

  while (!bfinished)
    {
      ipar++;

      if (ipar < argc && cts_icheck_flag (argv[ipar], popt_str, 5) < 0)
	inr_pars++;

      else
	bfinished = true;
    }

  return (inr_pars);
}


static int icompar (const void *p1, const void *p2)

  /* function used by qsort
   */
{
  return ((int) msign (*((int *) p1) - *((int *) p2)));
}


#define CFUNCNAME "vget_exf_list"

static void vget_exf_list (int ip, char **argv, int inxf, int **pxfi)

  /* this function reads a list of parameters (the indices of all
   * frequencies excluded for the mle-fit) and puts it into an array
   * (use qsort and icompar() to sort array in ascending order)
   * input:
   * ip     - index of first parameter in argv
   * argv   - array of parameters and flags
   * inxf   - number of excluded frequencies
   * output:
   * pxfi   - array of excluded frequencies
   */
{
  int i, k;

  /* allocate memory for block of excluded frequencies and fill block
   */
  *pxfi = (inxf > 0) ? cts_mmalloc (int, inxf, CFUNCNAME) : NULL;

  for (i = 0; i < inxf; i++)
    *(*pxfi+i) = (int) cts_lstrtol (argv[ip+i], 10);


  /* sort array and remove double indices
   */
  qsort (*pxfi, (size_t) inxf, sizeof (int), icompar);

  for (i = 0; i < inxf; i++)
    {
      if (*(*pxfi+i-1) == *(*pxfi+i))
	{
	  /* remove double index from list
	   * (the left over array element is set to -1)
	   */
	  for (k = i; k < inxf-1; k++)
	    *(*pxfi+k) = *(*pxfi+k+1);

	  *(*pxfi+inxf-1) = -1;
	}
    }
}

#undef CFUNCNAME


static void vgive_description_and_exit ()

  /* give a description how to call this program
   * and exit
   */
{
  fprintf (stdout, "Usage: powspec -plots [fmax] or\n");
  fprintf (stdout, "       powspec file-name [options]\n\n");
  fprintf (stdout, "-plots:        produce plots from existing output files\n");
  fprintf (stdout, "  fmax:        max. frequency used for the power");
  fprintf (stdout, " spectrum plot\n");
  fprintf (stdout, "               (default: max. frequency from file)\n\n");

  fprintf (stdout, "file-name:     name of data file (format: date, flux, ");
  fprintf (stdout, "flux-error)\n\n");

  fprintf (stdout, "options:\n");
  fprintf (stdout, "([pars] is a list of numbers (fixed order) passed via ");
  fprintf (stdout, "command line.\n If not all numbers are given, they're set");
  fprintf (stdout, " to the default values)\n\n");

  fprintf (stdout, "-errf [par]    specify how to treat data errors\n");
  fprintf (stdout, "       par:    either 0 - normalize on sample variance or");
  fprintf (stdout, "\n             1 - normalize on individual data errors ");
  fprintf (stdout, "(default %d)\n\n", iERRF_DEF);

  fprintf (stdout, "-gpow [pars]   derive power spectrum for real data\n");
  fprintf (stdout, "       par 1:  title of plot (default: \"\")\n");
  fprintf (stdout, "       par 2:  oversampling factor");
  fprintf (stdout, " (default: %d)\n", iOFAC_DEF);
  fprintf (stdout, "       par 3:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n");
  fprintf (stdout, "       par 4:  maximum frequency used for maximum");
  fprintf (stdout, " likeliehood fit\n");
  fprintf (stdout, "               (default: value of par 2)\n");
  fprintf (stdout, "       par 5:  space separated list of frequencies ");
  fprintf (stdout, "(index), not used for the \n\t       mle-fit\n\n");

  fprintf (stdout, "-nvar [pars]:  simulate lightcurves with fixed shot noise");
  fprintf (stdout, " parameters.\n");
  fprintf (stdout, "       par 1:  number of simulations");
  fprintf (stdout, " (default: %d)\n", iSIMS_DEF);
  fprintf (stdout, "       par 2:  oversampling factor");
  fprintf (stdout, " (default: %d)\n", iOFAC_DEF);
  fprintf (stdout, "       par 3:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n");
  fprintf (stdout, "       par 4:  maximum frequency used for maximum");
  fprintf (stdout, " likeliehood fit\n");
  fprintf (stdout, "               (default: value of par 3)\n");
  fprintf (stdout, "       par 5:  shot rate [flares / day]");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_DEF);
  fprintf (stdout, "       par 6:  mean squared amplitude of the flares");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_DEF);
  fprintf (stdout, "       par 7:  standard dev. of gaussian shaped flares");
  fprintf (stdout, " (default: %4.2f)\n", dSDEV_DEF);
  fprintf (stdout, "       par 8:  space separated list of frequencies ");
  fprintf (stdout, "(index), not used for the \n\t       mle-fit\n\n");

  fprintf (stdout, "-avar [pars]:  simulate lightcurves with variable flare");
  fprintf (stdout, " amplitudes.\n");
  fprintf (stdout, "       par 1:  minimum mean squared amplitude (MSA)");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_MIN_DEF);
  fprintf (stdout, "       par 2:  maximum MSA");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_MAX_DEF);
  fprintf (stdout, "       par 3:  stepsize for MSA");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_SS_DEF);
  fprintf (stdout, "       par 4:  number of simulations for a given MSA");
  fprintf (stdout, " (default: %d)\n", iSIMS_DEF);
  fprintf (stdout, "       par 5:  oversampling factor");
  fprintf (stdout, " (default: %d)\n", iOFAC_DEF);
  fprintf (stdout, "       par 6:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n");
  fprintf (stdout, "       par 7:  maximum frequency used for maximum");
  fprintf (stdout, " likeliehood fit\n");
  fprintf (stdout, "               (default: value of par 6)\n");
  fprintf (stdout, "       par 8:  shot rate [flares / day]");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_DEF);
  fprintf (stdout, "       par 9:  standard dev. of gaussian shaped flares");
  fprintf (stdout, " (default: %4.2f)\n", dSDEV_DEF);
  fprintf (stdout, "       par 10: space separated list of frequencies ");
  fprintf (stdout, "(index), not used for the \n\t       mle-fit\n\n");

  fprintf (stdout, "-svar [pars]:  simulate lightcurves with variable flare");
  fprintf (stdout, " shapes.\n");
  fprintf (stdout, "       par 1:  minimum sigma of gaussian shaped flares");
  fprintf (stdout, " (default: %4.2f)\n", dSDEV_MIN_DEF);
  fprintf (stdout, "       par 2:  maximum sigma");
  fprintf (stdout, " (default: %4.2f)\n", dSDEV_MAX_DEF);
  fprintf (stdout, "       par 3:  stepsize for sigma");
  fprintf (stdout, " (default: %4.2f)\n", dSDEV_SS_DEF);
  fprintf (stdout, "       par 4:  number of simulations for a given sigma");
  fprintf (stdout, " (default: %d)\n", iSIMS_DEF);
  fprintf (stdout, "       par 5:  oversampling factor");
  fprintf (stdout, " (default: %d)\n", iOFAC_DEF);
  fprintf (stdout, "       par 6:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n");
  fprintf (stdout, "       par 7:  maximum frequency used for maximum");
  fprintf (stdout, " likeliehood fit\n");
  fprintf (stdout, "               (default: value of par 6)\n");
  fprintf (stdout, "       par 8:  shot rate [flares / day]");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_DEF);
  fprintf (stdout, "       par 9:  mean squared amplitude of the flares");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_DEF);
  fprintf (stdout, "       par 10: space separated list of frequencies ");
  fprintf (stdout, "(index), not used for the \n\t       mle-fit\n\n");

  fprintf (stdout, "-rvar [pars]:  simulate lightcurves with variable shot");
  fprintf (stdout, " rate.\n");
  fprintf (stdout, "       par 1:  minimum shot rate [flares / day]");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_MIN_DEF);
  fprintf (stdout, "       par 2:  maximum shot rate");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_MAX_DEF);
  fprintf (stdout, "       par 3:  stepsize for shot rate");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_SS_DEF);
  fprintf (stdout, "       par 4:  number of simulations for a given shot");
  fprintf (stdout, " rate (default: %d)\n", iSIMS_DEF);
  fprintf (stdout, "       par 5:  oversampling factor");
  fprintf (stdout, " (default: %d)\n", iOFAC_DEF);
  fprintf (stdout, "       par 6:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n");
  fprintf (stdout, "       par 7:  maximum frequency used for maximum");
  fprintf (stdout, " likeliehood fit\n");
  fprintf (stdout, "               (default: value of par 6)\n");
  fprintf (stdout, "       par 8:  mean squared amplitude of the flares");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_DEF);
  fprintf (stdout, "       par 9:  standard dev. of gaussian shaped flares");
  fprintf (stdout, " (default: %4.2f)\n", dSDEV_DEF);
  fprintf (stdout, "       par 10: space separated list of frequencies ");
  fprintf (stdout, "(index), not used for the \n\t       mle-fit\n\n");

  fprintf (stdout, "-ct_r [pars]:  calculate cross-terms with variable shot");
  fprintf (stdout, " rate.\n");
  fprintf (stdout, "       par 1:  minimum shot rate [flares / day]");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_MIN_DEF);
  fprintf (stdout, "       par 2:  maximum shot rate");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_MAX_DEF);
  fprintf (stdout, "       par 3:  stepsize for shot rate");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_SS_DEF);
  fprintf (stdout, "       par 4:  number of simulations for a given shot");
  fprintf (stdout, " rate (default: %d)\n", iSIMS_DEF);
  fprintf (stdout, "       par 5:  oversampling factor");
  fprintf (stdout, " (default: %d)\n", iOFAC_DEF);
  fprintf (stdout, "       par 6:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n");
  fprintf (stdout, "       par 7:  mean squared amplitude of the flares");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_DEF);
  fprintf (stdout, "       par 8:  standard dev. of gaussian shaped flares");
  fprintf (stdout, " (default: %4.2f)\n", dSDEV_DEF);

  fprintf (stdout, "-ct_t [pars]:  calculate cross-terms with sliding obs.");
  fprintf (stdout, " times (from orig. \n");
  fprintf (stdout, "               data to aequidistant time bins).\n");
  fprintf (stdout, "       par 1:  number of steps when moving orig. data");
  fprintf (stdout, " to aequidistant time\n");
  fprintf (stdout, "               bins (default: %d)\n", iSTEPS_DEF);
  fprintf (stdout, "       par 2:  number of simulations for a given 'obs.");
  fprintf (stdout, " time' (default: %d)\n", iSIMS_DEF);
  fprintf (stdout, "       par 3:  oversampling factor");
  fprintf (stdout, " (default: %d)\n", iOFAC_DEF);
  fprintf (stdout, "       par 4:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n");
  fprintf (stdout, "       par 5:  shot rate [flares / day]");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_DEF);
  fprintf (stdout, "       par 6:  mean squared amplitude of the flares");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_DEF);
  fprintf (stdout, "       par 7:  standard dev. of gaussian shaped flares");
  fprintf (stdout, " (default: %4.2f)\n\n", dSDEV_DEF);

  fprintf (stdout, "-gpdf [pars]:  calculate probability distribution ");
  fprintf (stdout, "function\n");
  fprintf (stdout, "       par 1:  either 0 - pdf for simulated lightcurves,");
  fprintf (stdout, "\n               1 - pdf for bootstrapped data or\n");
  fprintf (stdout, "               2 - pdf for gaussian random noise ");
  fprintf (stdout, " (default: %d)\n", iPDF_DEF);
  fprintf (stdout, "       par 2:  number of simulations");
  fprintf (stdout, " (default: %d)\n", iSIMS_DEF);
  fprintf (stdout, "       par 3:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n");
  fprintf (stdout, "       par 4:  shot rate [flares / day]");
  fprintf (stdout, " (default: %4.2f)\n", dSRATE_DEF);
  fprintf (stdout, "       par 5:  mean squared amplitude of the flares");
  fprintf (stdout, " (default: %4.2f)\n", dMSA_DEF);
  fprintf (stdout, "       par 6:  standard dev. of gaussian shaped flares");
  fprintf (stdout, " (default: %4.2f)\n\n", dSDEV_DEF);

  fprintf (stdout, "-gsin [pars]   derive power spectrum for sinusoidal ");
  fprintf (stdout, "lightcurve\n");
  fprintf (stdout, "       par 1:  period of sinusoid ");
  fprintf (stdout, "(default: %4.2f)\n", dPERIOD_DEF);
  fprintf (stdout, "       par 2:  time origin (default: date_min)\n");
  fprintf (stdout, "       par 3:  amplitude (default: %4.2f)\n", dSINAMP_DEF);
  fprintf (stdout, "       par 4:  oversampling factor");
  fprintf (stdout, " (default: %d)\n", iOFAC_DEF);
  fprintf (stdout, "       par 5:  maximum frequency used in power spectrum\n");
  fprintf (stdout, "               (default: nyquist frequency)\n\n");

  exit (iERRCODE);
}


#define CFUNCNAME "main"

int main (int argc, char **argv)

{
  int j, k, ip, inp, inf, ins, ipar, ieflag, iofac, inxf, isims, isiz;
  int *pxfi;

  double dfmax, dfmax_mle, dwmin, dsrate, dmsa, dsdev, dperiod, dstime, dsinamp;
  double dmsa_min, dmsa_max, dmsa_ss, dsdev_min, dsdev_max, dsdev_ss;
  double dsrate_min, dsrate_max, dsrate_ss;


  double *pflux, **p2al;

  /* string must be consistent with perr_str in iget_errflag()
   */
  char *popt_str = "-gpow-errf-nvar-avar-svar-rvar-ct_r-ct_t-gpdf-gsin";
  char *ptitle;

  bool bfinished;

  NHYP nhyp;


  /*
   * read in and check command line arguments
   */
  if (argc < 2)
    vgive_description_and_exit ();


  bfinished = false;
  ieflag = iget_errflag (argv, argc);

  if (cts_icheck_flag (argv[1], "-plots", 6) == 0)
    {
      /* only produce plots; look for additional parameter
       */
      dfmax = (argc > 2) ? cts_dstrtod (argv[2]) : -1.;
      vplot_all (dfmax);

      bfinished = true;
    }

  else
    {
      /* read in flux data (3 columns) and sort them (1st row);
       * set index onto first option
       */
      cts_vget_dat_file (3, argv[1], &p2al, &inp);
      cts_vsort_dat (p2al, 3, inp, 1, true);

      /* backup copy of flux values (array is overwritten by simulation
       * functions
       */
      isiz = inp * sizeof (double);
      pflux = cts_mmalloc (double, inp, CFUNCNAME);
      memcpy (pflux, p2al[1], (size_t) isiz);

      ip = 2;
    }


  while (!bfinished)
    {
      /* k is offset of string argv[i] in string popt_str
       * (-1 if not found)
       */
      k = (ip < argc) ? cts_icheck_flag (argv[ip], popt_str, 5) : -1;

      switch (k)
	{
	case -1:

	  /* unknown option
	   */
	  fprintf (stdout, "\n %s: unknown option.\n\n", argv[ip]);
	  vgive_description_and_exit ();

	  break;

	case 0:

	  /* derive power spectrum for real data - get number of passed
	   * parameters (for this option) and set input parameters
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  ptitle = (j++ < ipar) ? argv[++ip] : "";
	  iofac = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iOFAC_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dfmax_mle = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;


	  /* get list of all frequencies excluded from mle-fit (pass index
	   * to next parameter 'ip+1' and update index afterwards)
	   */
	  inxf = (ipar - j > 0) ? ipar - j : 0;
	  vget_exf_list (ip+1, argv, inxf, &pxfi);
	  ip += inxf;


	  /* finally call routine to derive power-spectrum
	   */
	  vget_powspec (p2al, inp, ieflag, iofac, dfmax, dfmax_mle, pxfi, inxf,
		        ptitle);

	  break;

	case 5:

	  /* just increase index, error flag was already read
	   */
	  ip++;

	  break;

	case 10:

	  /* simulate lightcurves with fixed shot noise parameters;
	   * get number of passed parameters (for this option) and
	   * set input parameters
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  isims = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iSIMS_DEF;
	  iofac = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iOFAC_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dfmax_mle = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dsrate = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_DEF;
	  dmsa = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_DEF;
	  dsdev = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_DEF;


	  /* get list of all frequencies excluded from mle-fit (pass index
	   * to next parameter 'ip+1' and update index afterwards)
	   */
	  inxf = (ipar - j > 0) ? ipar - j : 0;
	  vget_exf_list (ip+1, argv, inxf, &pxfi);
	  ip += inxf;


	  /* finally call simulation routine
	   */
	  vsim_no_var (p2al, inp, ieflag, isims, iofac, inf, dsrate, dmsa,
		       dsdev, dfmax, dfmax_mle, pxfi, inxf);


	  /* restore original flux values
	   */
	  memcpy (p2al[1], pflux, (size_t) isiz);

	  break;

	case 15:

	  /* simulate lightcurves with variable flare amplitudes
	   * get number of passed parameters (for this option) and
	   * set input parameters
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  dmsa_min = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_MIN_DEF;
	  dmsa_max = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_MAX_DEF;
	  dmsa_ss = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_SS_DEF;
	  isims = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iSIMS_DEF;
	  iofac = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iOFAC_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dfmax_mle = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dsrate = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_DEF;
	  dsdev = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_DEF;


	  /* get list of all frequencies excluded from mle-fit (pass index
	   * to next parameter 'ip+1' and update index afterwards)
	   */
	  inxf = (ipar - j > 0) ? ipar - j : 0;
	  vget_exf_list (ip+1, argv, inxf, &pxfi);
	  ip += inxf;


	  /* finally call simulation routine
	   */
	  vsim_a_var (p2al, inp, ieflag, isims, iofac, dsrate, dmsa_min,
		      dmsa_max, dmsa_ss, dsdev, dfmax, dfmax_mle, pxfi, inxf);


	  /* restore original flux values
	   */
	  memcpy (p2al[1], pflux, (size_t) isiz);

	  break;

	case 20:

	  /* simulate lightcurves with variable flare shapes (sdev)
	   * get number of passed parameters (for this option) and
	   * set input parameters
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  dsdev_min = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_MIN_DEF;
	  dsdev_max = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_MAX_DEF;
	  dsdev_ss = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_SS_DEF;
	  isims = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iSIMS_DEF;
	  iofac = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iOFAC_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dfmax_mle = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dsrate = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_DEF;
	  dmsa = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_DEF;


	  /* get list of all frequencies excluded from mle-fit (pass index
	   * to next parameter 'ip+1' and update index afterwards)
	   */
	  inxf = (ipar - j > 0) ? ipar - j : 0;
	  vget_exf_list (ip+1, argv, inxf, &pxfi);
	  ip += inxf;


	  /* finally call simulation routine
	   */
	  vsim_s_var (p2al, inp, ieflag, isims, iofac, dsrate, dmsa, dsdev_min,
		      dsdev_max, dsdev_ss, dfmax, dfmax_mle, pxfi, inxf);


	  /* restore original flux values
	   */
	  memcpy (p2al[1], pflux, (size_t) isiz);

	  break;

	case 25:

	  /* simulate lightcurves with variable shot rates
	   * get number of passed parameters (for this option) and
	   * set input parameters
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  dsrate_min = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_MIN_DEF;
	  dsrate_max = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_MAX_DEF;
	  dsrate_ss = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_SS_DEF;
	  isims = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iSIMS_DEF;
	  iofac = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iOFAC_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dfmax_mle = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dmsa = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_DEF;
	  dsdev = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_DEF;


	  /* get list of all frequencies excluded from mle-fit (pass index
	   * to next parameter 'ip+1' and update index afterwards)
	   */
	  inxf = (ipar - j > 0) ? ipar - j : 0;
	  vget_exf_list (ip+1, argv, inxf, &pxfi);
	  ip += inxf;


	  /* finally call simulation routine
	   */
	  vsim_r_var (p2al, inp, ieflag, isims, iofac, dsrate_min, dsrate_max,
		      dsrate_ss, dmsa, dsdev, dfmax, dfmax_mle, pxfi, inxf);


	  /* restore original flux values
	   */
	  memcpy (p2al[1], pflux, (size_t) isiz);

	  break;

	case 30:

	  /* simulate lightcurves with variable shot rates and
	   * calculate cross terms
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  dsrate_min = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_MIN_DEF;
	  dsrate_max = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_MAX_DEF;
	  dsrate_ss = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_SS_DEF;
	  isims = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iSIMS_DEF;
	  iofac = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iOFAC_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dmsa = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_DEF;
	  dsdev = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_DEF;


	  /* finally call simulation routine
	   */
	  vget_ct_r (p2al, inp, ieflag, isims, iofac, dsrate_min, dsrate_max,
		     dsrate_ss, dmsa, dsdev, dfmax);


	  /* restore original flux values
	   */
	  memcpy (p2al[1], pflux, (size_t) isiz);

	  break;

	case 35:

	  /* simulate lightcurves with variable shot rates and calculate
	   * cross terms for sliding obs. times (from aequidistant obs. points
	   * to real obs. points)
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  ins = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iSTEPS_DEF;
	  isims = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iSIMS_DEF;
	  iofac = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iOFAC_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dsrate = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_DEF;
	  dmsa = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_DEF;
	  dsdev = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_DEF;


	  /* finally call simulation routine
	   */
	  vget_ct_t (p2al, inp, ieflag, isims, iofac, ins, dsrate, dmsa, dsdev,
		     dfmax);


	  /* restore original flux values
	   */
	  memcpy (p2al[1], pflux, (size_t) isiz);

	  break;

	case 40:

	  /* calculate probability distribution function
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  nhyp = (j++ < ipar) ? (NHYP) cts_lstrtol (argv[++ip], 10) : iPDF_DEF;
	  isims = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iSIMS_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  iofac = 1;
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;
	  dsrate = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSRATE_DEF;
	  dmsa = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dMSA_DEF;
	  dsdev = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSDEV_DEF;


	  /* finally call pdf routine
	   */
	  vget_pdf (p2al, inp, ieflag, isims, dsrate, dmsa, dsdev, nhyp, dfmax,
		    inf);

	  /* restore original flux values
	   */
	  memcpy (p2al[1], pflux, (size_t) isiz);

	  break;

	case 45:

	  /* derives the power spectrum for a sinusoidal lightcurve
	   */
	  ipar = iget_npar (ip, popt_str, argv, argc);

	  j = 0;
	  dperiod = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dPERIOD_DEF;
	  dstime = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : **p2al;
	  dsinamp = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dSINAMP_DEF;
	  iofac = (j++ < ipar) ? (int) cts_lstrtol (argv[++ip], 10) : iOFAC_DEF;

	  /* derive nyquist frequency (dfmax is set to nyquist frequency if < 0
	   * when passed to cts_vgetfreq)
	   */
	  dfmax = -1.;
	  cts_vgetfreq (p2al[0], inp, iofac, &dfmax, &dwmin, &inf);

	  dfmax = (j++ < ipar) ? cts_dstrtod (argv[++ip]) : dfmax;


	  /* finally derive power spectrum
	   */
	  vsim_sin (p2al, inp, ieflag, iofac, dperiod, dstime, dsinamp, dfmax);


	  /* restore original flux values
	   */
	  memcpy (p2al[1], pflux, (size_t) isiz);

	  break;

	default:

	  cts_merror ("%s: default reached!\n", CFUNCNAME);
	}


      /* any argumnents left?
       */
      bfinished = (++ip < argc) ? false : true;
    }


  return(0);
}

#undef CFUNCNAME
