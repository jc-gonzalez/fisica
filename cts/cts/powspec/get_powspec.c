#include <stdio.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "powspec.h"


void vget_powspec (double **p2al, int inp, int ieflag, int iofac, double dfmax,
		   double dfmax_mle, int *pxfi, int inxf, char *ptitle)

  /* this function derives the power spectrum for a set of data points
   * input:
   * p2al      - array of 3 lists (mjd, flux, flux error)
   * inp       - number of points in p2al arrays
   * ieflag    - take sample variance if 0, else use individual data errors
   *             (used for normalizing power spectrum)
   * iofac     - oversampling factor
   * dfmax     - max. frequency used for power spectrum
   * dfmax_mle - max. frequency used for the mle-fit
   * pxfi      - integer array containing the index of all frequencies which
   *             are not used for the mle fit (in addition to dfmax_mle)
   *             the integers in pxfi are expected to be in ascending order!
   * inxf      - number of excluded frequencies (in addition to dfmax_mle)
   * ptitle    - title of plot
   */
{
  int i, inf;

  double *dx, *dy, *dz, *pf, *ppow, *pw2x, *ppowx;

  bool bnewdat;

  FILE_TYPE pow_file;

  MLE_STRUCT mle;


  /* open file to store results
   */
  pow_file.pname = cPOW_DAT;
  pow_file.pfile = NULL;
  pow_file.acm = "w";

  cts_vopen_file (&pow_file);


  /* init pointers and variables (pass flux errors only, if normalization
   * on individual data errors is wanted - pointer is used as a flag)
   */
  bnewdat = true;
  dx = p2al[0];
  dy = p2al[1];
  dz = (ieflag == 0) ? NULL : p2al[2];

  cts_ls_power (dx, dy, dz, inp, iofac, dfmax, &bnewdat, &pf, &ppow, &inf);

  fprintf (pow_file.pfile, "#  %s\n", ptitle); 
  fprintf (pow_file.pfile, cPOW_WRITE1, dfmax_mle, inf, ieflag);
  fprintf (pow_file.pfile, cPOW_HEAD1);


  mle = get_mle_g (inf, pf, ppow, dfmax_mle, pxfi, inxf, pow_file, &pw2x,
		   &ppowx);


  fprintf (pow_file.pfile, cPOW_HEAD2);

  for (i = 0; i < inf; i++)
    fprintf (pow_file.pfile, cPOW_WRITE3, pf[i], ppow[i]);

  if (inxf > 0)
    {
      fprintf (pow_file.pfile, cPOW_HEAD3);

      for (i = 0; i < inxf; i++)
	fprintf (pow_file.pfile, cPOW_WRITE4, pxfi[i], pf[i]);
    }


  /* create plot
   */
  vplot_powspec (pf, ppow, inf, mle, dfmax, dfmax_mle, ptitle, ieflag);
}
