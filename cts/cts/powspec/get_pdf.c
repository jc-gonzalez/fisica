#include <math.h>
#include <stdio.h>
#include "ctsbase.h"
#include "ctshbook.h"
#include "ctshplot.h"
#include "ctsmath.h"
#include "powspec.h"



#define CFUNCNAME "vget_pdf"

void vget_pdf (double **p2al, int inp, int ieflag, int isims, double dsrate,
	       double dmsa, double dsdev, NHYP nhyp, double dfmax, int inf)

  /* this function derives the probability distribution from a set of
   * simulated lightcurves (the probability for a given frequency f0
   * and power-value p0 is the number of trials (for f == f0) with a
   * power-value pow > p0 divided by the overall number of trials)
   * input:
   * p2al   - array of 3 arrays (mjd, flux, flux error)
   * inp    - number of points in p2al arrays
   * ieflag - take sample variance if 0, else use individual data errors
   *          (used for normalizing power spectrum)
   * isims  - number of simulations
   * dsrate - mean shot rate of the flares (used for simulated lc only)
   * dmsa   - mean squared amplitude of the flares ( - " - )
   * dsdev  - stand. dev. of the gaussian type flares ( - " - )
   * nhyp   - null hypothesis (either shot noise, bootstrapped data
   *          or gaussian random data)
   * dfmax  - max. frequency (defaults to nyquist frequency, if < 0)
   * inf    - number of independent frequencies
   */
{
  int i, k;
  int ioffset, iofac = 1;

  /* pointer to memory block; contains (frequency dependent) number of
   * power-values in a givne bin
   */
  int *pnr_powbin;

  double dbinwidth, dpowmax;

  double *pf, *ppow, *dx, *dy, *dz, *dz2;

  char cfilenam [MAX_NHYP_TYPE] [iMAX_NHYP_NAME_LEN] =
  {cPDF_SIM_DAT, cPDF_BOOT_DAT, cPDF_GAUSS_DAT};

  void (*vnhypfunc [MAX_NHYP_TYPE]) () = {vsim_lc2, vsim_lc3, vsim_lc4};
  void (*pfunc) ();

  bool bnewdat;

  FILE_TYPE dat_file;


  /* check nhyp and set shot noise parameters if not used
   */
  if (nhyp < 0 || nhyp >= MAX_NHYP_TYPE)
    cts_merror ("NHYP %d: invalid enum.\n", nhyp);

  else if (nhyp != SIMLC)
    dsrate = dmsa = dsdev = -1.;


  /* set ieflag related variables
   */
  dpowmax = (ieflag == 0) ? dPOWMAX : dPOWMAX * dPOWMAXSF;
  dbinwidth = mdiv (dpowmax, iPOWBINS);


  /* init array pointers (set dz2 to NULL, if normalization
   * on sample variance is wanted, else dz = dz2)
   */
  dx = p2al[0];
  dy = p2al[1];
  dz = p2al[2];
  dz2 = (ieflag == 0) ? NULL : dz;


  /* open file to store pdf
   */
  dat_file.pname = cfilenam [nhyp];
  dat_file.pfile = NULL;
  dat_file.acm = "w";

  cts_vopen_file (&dat_file);


  /* allocate memory for pnr_powbin array and reset it to zero
   */
  pnr_powbin = cts_mmalloc (int, iPOWBINS * inf, CFUNCNAME);

  cts_vreset_iarray (pnr_powbin, iPOWBINS * inf);


  /* run simulations, use 'nhyp' to choose the correct light curve
   * simulation function (bnewdat is modified within ls_power)
   */
  bnewdat = true;
  pfunc = vnhypfunc [nhyp];

  for (i = 0; i < isims; i++)
    {
      pfunc (dx, dy, dz, inp, dsrate, dmsa, dsdev, bnewdat);
      cts_ls_power (dx, dy, dz2, inp, iofac, dfmax, &bnewdat, &pf, &ppow, &inf);


      for (k = 0; k < inf; k++)
	{
	  /* calculate pointer offset from ppow
	   * (if ppow is in the allowed range)
	   */
	  if (ppow[k] < dpowmax && ppow[k] >= 0.)
	    {
	      ioffset = k + inf * (int) (ppow[k] / dbinwidth);
	      pnr_powbin[ioffset] += 1;
	    }
	}
    }


  /* create plot
   */
  vplot_pdf (pf, pnr_powbin, dpowmax, iPOWBINS, inf, isims, nhyp, dsrate, dmsa,
	     dsdev, ieflag);


  /* store results
   */
  fprintf (dat_file.pfile, cPDF_WRITE1, dsdev, dsrate, dmsa, ieflag);
  fprintf (dat_file.pfile, cPDF_WRITE2, isims, inf, dpowmax, iPOWBINS);

  for(i = 0; i < iPOWBINS * inf; i++)
    fprintf (dat_file.pfile, cPDF_WRITE3, pf[i%inf], pnr_powbin[i]);


  cts_vclose_file (&dat_file);
}

#undef CFUNCNAME

