#include <math.h>
#include <stdio.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctsminuit.h"
#include "powspec.h"


/* file scope variables; set by get_mle(), and dfunc() functions
 */
static double *pw2x = NULL, *ppowx = NULL;
static MLE_STRUCT mle;


/*
 * declaration of local functions
 */
static int ifill_fit_arr (int, int, int *, double, double *, double *, double *,
			  double *);
static void vfunc_g (int *, double *, double *, double *, int *, int *);


/*
 * functions
 */
static int ifill_fit_arr (int inf, int inxf, int *pxfi, double dfmax,
			  double *pf, double *ppow, double *w2x, double *powx)

  /* this function fills the arrays used for the mle analysis
   * input:
   * inf   - number of data points of the passed arrays pf and ppow
   * inxf  - number of excluded frequencies (in addition to dfmax)
   * pxfi  - integer array containing the index of all frequencies which
   *         are not used for the mle fit (in addition to dfmax)
   *         the integers in pxfi are expected to be in ascending order!
   * dfmax - maximum frquency used for the fit
   * pf    - array of frequencies
   * ppow  - array of power values
   *
   * output
   * w2x  - array containing the angular frequency of all selected frequencies
   * powx - array containing all selected powers
   */
{
  int i, inff;

  i = inff = 0;

  while (i < inf)
    {
      /* check the index of excluded frequencies for 'i'
       * (if inxf <= 0 one must not check *pxfi)
       */
      if (pf[i] < dfmax && (inxf <= 0 || i != *pxfi)) 
	{
	  /* take this frequency
	   */
	  w2x[inff] = m2 (d2PI * pf[i]);
	  powx[inff] = ppow[i];

	  inff++;
	}

      else
	{
	  if (pf[i] >= dfmax)
	    {
	      /* nothing to be done left
	       */
	      i = inf;
	    }

	  else
	    {
	      /* we arrived at one of the excluded frequencies
	       * so set pxfi to the next frequency (if available)
	       */
	      if (inxf > 0)
		{
		  pxfi++;
		  inxf--;
		}
	    }
	}

      i++;
    }      /* END of while (i < inf) */

#ifdef DEBUG
  for (i = 0; i < inff; i++)
    fprintf (stderr, "%f     %f\n", sqrt(w2x[i])/d2PI, powx[i]);
#endif

  return inff;
}


static void vfunc_g (int *npar, double *grad, double *fcnval,
		     double *dfitpars, int *iflag, int *dummy)

  /* this function is used to derive the Maximum Likelihood value for a
   * set of shot noise parameters (gaussian type shots)
   * input:
   * npar    - number of fit parameters
   * dfitpar - array of shot noise parameters
   * iflag   - flag which indicates status of minuit
   * dummy   - not used here
   *
   * output value:
   * grad    - array of partial derivatives (optional) 
   * fcnval  - likeliehood value (-log(L))
   *
   * this function uses the following file scope variables:
   * ppowx - array of powers                    (without excluded frequencies)
   * pw2x  - array of squared angular frequency          -   "   -
   * mle   - struct of mle values and additional info
   */
{
  int i, ivarbl, in_f;

  char parname[iPARNAMELEN];

  /* a     - fit parameter (shot amplitude)
   * s2    - fit parameter^2 (shot standard deviation)
   * c     - fit parameter (power spectrum noise level)
   * w2    - squared angular frequency
   * lval  - lval = -log (L)
   * dpowm - mean fourier power
   */
  double a, s2, c, w2, lval, dpowm;

  in_f = mle.i;


  /* calculate log likeliehood
   */
  if (*iflag == 4)
    {
      a  = fabs (dfitpars[0]);
      s2 = m2 (dfitpars[1]);
      c  = dfitpars[2];

      lval = 0.;
      for (i = 0; i < in_f; i++)
	{
	  w2 = pw2x[i];

	  dpowm = a * exp (-w2 * s2) + c * sqrt (w2);
	  dpowm = (dpowm < dEPSILON) ? dEPSILON : dpowm;

	  lval += log (dpowm) + ppowx[i] / dpowm;
	}

      *fcnval = lval;      
    }


  /* minuit converged
   */
  if (*iflag == 3)
    {
      a  = fabs (dfitpars[0]);
      s2 = m2 (dfitpars[1]);
      c  = dfitpars[2];

      lval = 0.;
      for (i = 0; i < in_f; i++)
	{
	  w2 = pw2x[i];

	  dpowm = a * exp (-w2 * s2) + c * sqrt (w2);
	  dpowm = (dpowm < dEPSILON) ? dEPSILON : dpowm;

	  lval += log (dpowm) + ppowx[i] / dpowm;
	}

      mle.l = lval;


      /* get final results from minuit
       */
      cts_mnpout (1, parname, &mle.a, &mle.ae, &ivarbl);
      cts_mnpout (2, parname, &mle.s, &mle.se, &ivarbl);
      cts_mnpout (3, parname, &mle.c, &mle.ce, &ivarbl);
    }
}


#define CFUNCNAME "get_mle_g"

MLE_STRUCT get_mle_g (int inf, double *pf, double *ppow, double dfmax,
		      int *pxfi, int inxf, FILE_TYPE mle_file, double **w2x,
		      double **powx)

  /* this function derives the maximum likeliehood estimator for the
   * parameters A, C and s. It's done by fitting the function
   * A * exp (-w^2 * s^2) + c + cf * w (derived within the shot noise
   * model of gaussian shaped flares) to the power spectrum
   * input:
   * inf      - number of data points of the passed arrays
   * pf       - array of frequencies
   * ppow     - array of power values
   * dfmax    - maximum frequency used for the fit
   * pxfi     - integer array containing the index of all frequencies which
   *            are not used for the mle fit (in addition to dfmax)
   *            the integers in pxfi are expected to be in ascending order!
   * inxf     - number of excluded frequencies (in addition to dfmax)
   * mle_file - file to store results
   *
   * output:
   * w2x  - array of squared angular frequency (without excluded frequencies)
   * powx - array of powers (without excluded frequencies)

   * return value:
   * mle-struct containing the fitted parameters and the likeliehood value
   *
   * this function sets the following file scope variables:
   * ppowx - array of powers                    (without excluded frequencies)
   * pw2x  - array of squared angular frequency          -   "   -
   * in_f  - number of independent frequencies           -   "   -
   */
{
  /* command strings passed to minuit
   */
  char *pminos   = "minos";
  char *pcallfcn = "call vfunc_g";

  /* iasiz - array size of pw2x and ppowx (see below)
   */
  static int i, iasiz = 0;

  /* variables used to pass parameters to minuit
   * (their meaning depends on the corresponding minuit command)
   */
  int iargs;
  double dval[2];

  /* dfpar  - start values for shot parameters
   * dfpare - errors of shot parameters
   */
  double dfpar[iSN_PARS_G] = {45., 0.4, 0.0};
  double dfpare[iSN_PARS_G] = {45., 0.4, 0.1};


  /* reallocate memory for p2wx and ppowx array if necessary
   */
  if (inf > iasiz)
    {
      pw2x  = cts_mrealloc (pw2x, double, inf, CFUNCNAME);
      ppowx = cts_mrealloc (ppowx, double, inf, CFUNCNAME);
      iasiz = inf;
    }


  /* fill ppowx and pw2x arrays (the excluded frequencies removed) and
   * set in_f to the new number of test frequencies
   */
  mle.i = ifill_fit_arr (inf, inxf, pxfi, dfmax, pf, ppow, pw2x, ppowx);


  /*
   * use minuit routines for minimization
   */
  /* initialize minuit
   */
  cts_mninit (5, 6, 7);


  /* define parameters in minuit
   */
  dfpar[0] = ppow[0];
  dfpare[0] = ppow[0];
  for (i = 0; i < iSN_PARS_G; i++)
    cts_mnparm (i+1, "", dfpar[i], dfpare[i]);

  /* read in data
   */
  iargs = 1;
  *dval = 1.;
  cts_mnexcm (vfunc_g, pcallfcn, dval, iargs);

  /* start MINUIT
   */
  iargs = 2;
  *dval = 2000.;
  *(dval+1) = 0.1;
  cts_mnexcm (vfunc_g, pminos, dval, iargs);


  /* calculate & write down final result
   */
  iargs = 1;
  *dval = 3.;
  cts_mnexcm (vfunc_g, pcallfcn, dval, iargs);


  /* save results to mle file
   */
  mle.a = fabs (mle.a);
  mle.s = fabs (mle.s);
  mle.c = mle.c;

  fprintf (mle_file.pfile, cMLE_WRITE, mle.a, mle.ae, mle.s, mle.se, mle.c,
	   mle.ce, mle.l);


  /* set return values
   */
  *w2x = pw2x;
  *powx = ppowx;

  return (mle);
}
