/*
 *  some numerical routines
 *
 *  Daniel Kranich
 *  12.06. 1999
 */
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "ctsbase.h"
#include "ctsmath.h"


/* 
 * declaration of static functions
 */
static void ls_pow1 (double [], double [], double [], int, int, double, bool *,
		     double **, double **, int, double);
static void ls_pow2 (double [], double [], double [], int, int, double, bool *,
		     double **, double **, int, double);

static bool bnrb_root (void (NRB_STRUCT *), double, double, double, double *);


/* array of all defined root-finding functions - must match definition of
 * RF_FUNCS in ctsmath.h
 */
static bool (*const find_root_funcs [MAX_RF_FUNCS]) ()
  = {bnrb_root};


static bool bnrb_root (void fun (NRB_STRUCT *), double b1, double b2, 
		       double fbreak, double *x0)

  /* this function searches for the root of function fun within the
   * range [x1, x2]; the algorithm used is a combination of the
   * newton-raphson and the bisection method
   *
   * input values: fun () - function which reads x and returns f = f(x)
   *                        and df = f'(x)
   *               b1, b2 - boundary of valid x-range (b1 - low, b2 - high)
   *               fbreak - break condition: |f(x0)| < fbreak
   *
   * return values: x0   - root
   *                bool - true if succeeded
   */
{
  /* structs to contain x-value, function value and derivative for a given
   * point
   * nrb[0] - from newton method on b1
   * nrb[1] - from newton method on b2
   * nrb[2] - from bisection method (b1+b2) / 2
   * pb1, pb2 - from boundary points b1 and b2
   * ppb1, ppb2 - pointer to structs of new boundaries
   * ppr        - pointer to struct of best root-estimation
   */
  NRB_STRUCT nrb[3], pb1, pb2;
  NRB_STRUCT *ppb1, *ppb2, *ppr;

  int i, iloop;
  static size_t nrb_size = sizeof (NRB_STRUCT);

  bool bfinished = false;


  /* put pb1.x, pb2.x in ascending order and fill boundary structs
   */
  if (b1 > b2)
    {
      pb1.x = b2;
      pb2.x = b1;
    }
  else
    {
      pb1.x = b1;
      pb2.x = b2;
    }



  /* calculate start values and check them;
   * function values must have opposite sign
   */
  fun (&pb1);
  fun (&pb2);

  if (pb1.f * pb2.f > 0.) return (false);
  if (fabs (pb1.f) < fbreak)
    {
      *x0 = pb1.x;
      bfinished = true;
    }
  if (fabs (pb2.f) < fbreak)
    {
      *x0 = pb2.x;
      bfinished = true;
    }


  iloop = 0;
  while (!bfinished)

    {
      iloop++;

      /* derive x- and function values from newton and bisection method
       * (use new x-values only if within [b1, b2])
       */
      nrb[0].x = (pb1.df > dEPSILON) ? pb1.x - pb1.f / pb1.df : pb1.x;
      nrb[0].x = (mc_in_ab (pb1.x, pb2.x, nrb[0].x) ? nrb[0].x : pb1.x);
      fun (&nrb[0]);

      nrb[1].x = (pb2.df > dEPSILON) ? pb2.x - pb2.f / pb2.df : pb2.x;
      nrb[1].x = (mc_in_ab (pb1.x, pb2.x, nrb[1].x) ? nrb[1].x : pb2.x);
      fun (&nrb[1]);

      nrb[2].x = 0.5 * (pb1.x + pb2.x);
      fun (&nrb[2]);


      /* search new upper/lower boundary point
       * (x must be smaller/bigger and f must have same sign as old
       *  upper/lower boundary point)
       */
      ppb1 = &pb1;
      ppb2 = &pb2;

      for (i = 0; i < 3; i++)
	{
	  if ((nrb[i].f * ppb1->f > 0.) && (nrb[i].x > ppb1->x))
	    ppb1 = &nrb[i];

	  if ((nrb[i].f * ppb2->f > 0.) && (nrb[i].x < ppb2->x))
	    ppb2 = &nrb[i];
	}


      /* update pb1 and pb2
       */
      memcpy (&pb1, ppb1, nrb_size);
      memcpy (&pb2, ppb2, nrb_size);


      /* get best root estimation and check for break conditions
       */
      ppr = (fabs (pb1.f) < fabs (pb2.f)) ? &pb1 : &pb2;

      if (fabs (ppr->f) < fbreak)
	{
	  *x0 = ppr->x;
	  bfinished = true;
	}

      else if (iloop > iMAX_ITERATIONS)

	return (false);

#ifdef DEBUG
  fprintf (stdout, "loop: %d\t x0: %f\t f: %f\n", iloop, ppr->x, ppr->f);
#endif

    } /* while () */

  return (true);
}


#define CFUNCNAME "cts_bfind_root"

bool cts_bfind_root (RF_FUNCS rf_func, void fun (NRB_STRUCT *), double b1,
		     double b2, double fbreak, double *x0)

  /* interface to all defined root-finding functions;
   */
{
  bool bsuccess = false;

  if (0 <= rf_func && rf_func < MAX_RF_FUNCS)
    bsuccess = (* find_root_funcs [rf_func]) (fun, b1, b2, fbreak, x0);

  else
    cts_merror ("%s: function!\n", CFUNCNAME);

  return (bsuccess);
}

#undef CFUNCNAME


double cts_dmap_angle (double dangle, char cunit)

  /* maps angle onto {-Pi, Pi} or {-180, 180} range
   * depending on char 'cunit'
   */
{
  int ia_div_pi;
  double dunit;
  double da = dangle;

  if (cunit == 'R' || cunit == 'r')
    dunit = M_PI;

  else if (cunit == 'D' || cunit == 'd')
    dunit = 180.;

  else
    cts_merror ("dmap_anlge: unknown unit for angle %f!\n", dangle);


  /* if  2*n * pi < |alpha| < (2*n+1) * pi, n = 0, 1, ...:
   *   alpha -= sign (alpha) * ia_div_pi * pi
   *
   * e.g. (in degree) 361 -> 361 - 2 * 180 = 1
   *
   *
   * if  (2*n+1) * pi < |alpha| < 2*n * pi, n = 0, 1, ...:
   *   alpha -= sign (alpha) * (ia_div_pi + 1) * pi
   *
   * e.g. (in degree) 181 -> 181 - 2 * 180 = -179
   *
   * => if ia_dev_pi is odd we have to add 1 else nothing
   */
  ia_div_pi = (int) (fabs (da) / dunit);

  da -= msign (da) * dunit * (ia_div_pi + ((ia_div_pi & 1) ? 1 : 0));

  return (da);
}


double cts_derfc (double x)

  /* complementary error function (based on Chebychev fit)
   */
{
  double dx, dt, res;

  dx = fabs (x);
  dt = 1.0 / (1.0 + 0.5 * dx);

  res = dt * exp (-dx * dx - 1.26551223 + dt * (1.00002368 + dt * (0.37409196
	+ dt * (0.09678418 + dt * (-0.18628806 + dt * (0.27886807
        + dt * (-1.13520398 + dt * (1.48851587 + dt * (-0.82215223
        + dt * 0.17087277)))))))));

  return (x < 0. ? 2.0 - res : res);
}


void cts_vget_svar (double x[], int inp, double *dmean, double *dsvar)

  /* this function calculates the mean and sample variance of a
   * given data sample 'x[]' consisting of 'inp' points;
   * the results are returned in 'dmean' and 'dsvar'
   */
{
  int i;

  /* expectation value of x and x^2
   */
  double dev_x, dev_x2;


  dev_x = dev_x2 = 0.;
  for (i = 0; i < inp; i++)
    {
      dev_x  += x[i];
      dev_x2 += m2 (x[i]);
    }

  *dmean = dev_x / (double) inp;
  *dsvar  = (dev_x2 - dev_x * dev_x / (double) inp) / (double) (inp - 1);
}


#define CFUNCNAME "cts_vgetfreq"

void cts_vgetfreq (double dt[], int inp, int iofac, double *pfmax,
		   double *pwmin, int *pnf)

  /* calculate number of independent test frequencies and smallest
   * frequency (determines size of ifs); exits, if the time range
   * is too small
   * input:
   * dt  - array of obs. times
   * inp - number of data points in 'dt'
   * iofac - oversampling factor
   * pfmax - maximum test frequency (take nyquist, if dfmax < 0)
   *
   * output:
   * pfmax - set to nyquist if < 0.
   * pwmin - smallest frequency
   * pnf   - number of independent frequencies
   */
{
  int i, inf;
  double tmin, tmax, wmin, wmax;

  tmin = tmax = dt[0];
  for (i = 0; i < inp; i++)
    {
      tmin = min (tmin, dt[i]);
      tmax = max (tmax, dt[i]);
    }

  if (tmax - tmin < dEPSILON)
    cts_merror ("%s: time range too small!\n", CFUNCNAME);

  iofac = (iofac > 0) ? iofac : 1;
  wmin = d2PI / (tmax - tmin) / iofac;
  wmax = d2PI * *pfmax;

  /* set wmax to nyquist frequency, if < 0
   */
  if (wmax > dEPSILON)
    inf = (int) (wmax / wmin);

  else
    {
      inf = (int) (0.5 * inp * iofac);
      wmax = wmin * (double) inf;
    }

  /* set return values
   */
  *pnf = inf;
  *pwmin = wmin;
  *pfmax = wmax / d2PI;
}

#undef CFUNCNAME


#define CFUNCNAME "ls_pow1"

static void ls_pow1 (double dt[], double df[], double dfe[], int inp, int iofac,
		     double dfmax, bool *bnewdat, double **freq, double **power,
		     int inf, double w0)

  /* function to calculate the lomb-scargle periodogram for a given dataset
   * (this function uses exact calculations of sines and cosines, but runs
   *  out of memory, if the data set is too large)
   * input:
   * dt[]    - array of obs. times
   * df[]    - array of obs. fluxes
   * dfe[]   - array of obs. flux errors (sample variance is used if NULL)
   * inp     - number of data points for dt, df and dfe
   * iofac   - oversampling factor
   * dfmax   - only frequencies below or equal 'dfmax' are tested (all
   *           frequencies up to the nyquist are tested, if 'dfmax' < 0.)
   * bnewdat - true if data set has changed; forces recalculation of all
   *           sines and cosines
   * inf     - number of test frequencies
   * w0      - smallest test frequency
   *
   * output:
   * bnewdat  - is changed to false if it was true
   * dfreq[]  - array of test frequencies
   * dpower[] - power values for all test frequencies
   */
{
  int i, j, k;
  static int inp_bak;

  double w, tau, dfe2;

  double dsinw, dcosw, dcoswtau, dsinwtau, dsumcos2w, dsumsin2w;
  double dsumcoswdt2, dsumsinwdt2, dsinsum, dcossum, dsinwdt, dcoswdt;
  static double dsvar, dmean;

  double *psinw0, *pcosw0, *psinw, *pcosw;
  static double *psinwdt, *pcoswdt, *psum_coswdt2, *psum_sinwdt2;
  static double *pfe2, *pfreq, *ppower = NULL;

  static bool bno_dsvar = false;


  if (*bnewdat)

    /* do all the following calculations only if we're dealing with a new
     * data set (new dt[], dfe[]). this will significantly speed up all the
     * simulations
     */
    {
      /* release old arrays if they exist
       */
      if (ppower != NULL)
	{
	  cts_mfree (pfe2);
	  cts_mfree (pfreq);
	  cts_mfree (ppower);
	  cts_mfree (psinwdt);
	  cts_mfree (pcoswdt);
	  cts_mfree (psum_sinwdt2);
	  cts_mfree (psum_coswdt2);
	}


      /* check whether to use individual flux errors or the sample variance;
       * in the first case, I set a flag not to use dsvar (it's then set to 1)
       * and put the squared errors in an array, in the latter case '1' is used
       * instead of all the squared errors
       */
      pfe2 = cts_mmalloc (double, inp, CFUNCNAME);

      if (dfe != NULL)
	{
	  bno_dsvar = true;
	  for (i = 0; i < inp; i++)
	    {
	      pfe2[i] = m2 (dfe[i]);

	      /* check squared flux error
	       */
	      if (pfe2[i] < dEPSILON)
		cts_merror ("%s: error of flux too small!\n", CFUNCNAME);
	    }
	}
      else
	{
	  bno_dsvar = false;
	  for (i = 0; i < inp; i++)
	    pfe2[i] = 1.;
	}


      /* allocate memory for arrays which store sin (w0*ti), cos (w0*ti),
       * sin (w*ti) and cos (w*ti)
       */
      psinw0 = cts_mmalloc (double, inp, CFUNCNAME);
      pcosw0 = cts_mmalloc (double, inp, CFUNCNAME);
      psinw  = cts_mmalloc (double, inp, CFUNCNAME);
      pcosw  = cts_mmalloc (double, inp, CFUNCNAME);

      /* allocate memory for sin (w*(ti-tau)), cos (w*(ti-tau)) and the sum
       * of these sines and cosines squared (these arrays are finally used
       * for calculating the LS-periodogram)
       */
      psum_sinwdt2 = cts_mmalloc (double, inf, CFUNCNAME);
      psum_coswdt2 = cts_mmalloc (double, inf, CFUNCNAME);
      psinwdt = cts_mmalloc (double, inp * inf, CFUNCNAME);
      pcoswdt = cts_mmalloc (double, inp * inf, CFUNCNAME);


      /* allocate memory for the frequency and power value
       * arrays
       */
      pfreq = cts_mmalloc (double, inf, CFUNCNAME);
      ppower = cts_mmalloc (double, inf, CFUNCNAME);


      /* fill psinw0 and pcosw0 arrays and copy to psinw, pcosw
       * (we start with w = w0)
       */
      for (i = 0; i < inp; i++)
	{
	  psinw0[i] = sin (w0 * dt[i]);
	  pcosw0[i] = cos (w0 * dt[i]);
	}

      memcpy (psinw, psinw0, (size_t) inp * sizeof (double));
      memcpy (pcosw, pcosw0, (size_t) inp * sizeof (double));


      /*
       * now calculate the LS-parameters
       */
      for (w = 0., j = 0; j < inf; j++)

	/* loop over all frequencies
	 */
	{
	  dsumsin2w = dsumcos2w = 0.;
	  dsumsinwdt2 = dsumcoswdt2 = 0.;

	  /* loop over all data points and calculate tau
	   */
	  for (i = 0; i < inp; i++)
	    {
	      dsumsin2w += 2. * psinw[i] * pcosw[i];
	      dsumcos2w += m2 (pcosw[i]) - m2 (psinw[i]);
	    }

	  w += w0;
	  tau = 0.5 * atan2 (dsumsin2w, dsumcos2w) / w;


	  /* fill frequency array
	   */
	  pfreq[j] = w / d2PI;


	  /* now do final calculations and store them in the
	   * arrays;
	   * in addition, update psinw and pcosw array for next
	   * loop (frequency w = w + w0)
	   */
	  dsinwtau = sin (w * tau);
	  dcoswtau = cos (w * tau);

	  for (k = j * inp, i = 0; i < inp; i++, k++)
	    {
	     dsinwdt = psinw[i] * dcoswtau - pcosw[i] * dsinwtau;
	     dcoswdt = pcosw[i] * dcoswtau + psinw[i] * dsinwtau;

	     dsinw = psinw[i] * pcosw0[i] + psinw0[i] * pcosw[i];
	     dcosw = pcosw[i] * pcosw0[i] - psinw0[i] * psinw[i];

	     psinwdt[k] = dsinwdt;
	     pcoswdt[k] = dcoswdt;

	     psinw[i] = dsinw;
	     pcosw[i] = dcosw;

	     dfe2 = pfe2[i];
	     dsumsinwdt2 += m2 (dsinwdt) / dfe2;
	     dsumcoswdt2 += m2 (dcoswdt) / dfe2;
	    }

	  psum_sinwdt2[j] = dsumsinwdt2;
	  psum_coswdt2[j] = dsumcoswdt2;

	}   /* END loop over all frequencies */


      /* release memory
       */
      cts_mfree (psinw);
      cts_mfree (pcosw);
      cts_mfree (psinw0);
      cts_mfree (pcosw0);

      /* remember number of data points and reset bnewdat
       */
      inp_bak = inp;
      *bnewdat = false;

    }  /* END if (*bnewdat) */



  /* check whether number of data points is consitent
   */
  if (inp_bak != inp)
    cts_merror ("%s: data set changed; use 'bnewdat = true' !\n", CFUNCNAME);


  /* get mean and sample variance of dataset; reset dsvar to 1 if
   * not used
   */
  cts_vget_svar (df, inp, &dmean, &dsvar);

  if (bno_dsvar) dsvar = 1.;


  /* calculate Lomb-Scargle periodogram for each frequency
   */
  for (j = 0; j < inf; j++)
    {
      dsinsum = dcossum = 0.;

      for (k = j * inp, i = 0; i < inp; k++, i++)
	{
	  dfe2 = pfe2[i];
	  dsinsum += (df [i] - dmean) * psinwdt[k] / dfe2;
	  dcossum += (df [i] - dmean) * pcoswdt[k] / dfe2;
	}

      ppower[j] = 0.5 * (m2 (dcossum) / psum_coswdt2[j]
			 + m2 (dsinsum) / psum_sinwdt2[j]) / dsvar;
    }



  /* set return values
   */
  *freq = pfreq;
  *power = ppower;
}

#undef CFUNCNAME


#define CFUNCNAME "ls_pow2"

static void ls_pow2 (double dt[], double df[], double dfe[], int inp, int iofac,
		     double dfmax, bool *bnewdat, double **freq, double **power,
		     int inf, double w0)

  /* function to calculate the lomb-scargle periodogram for a given
   * dataset (this function uses sines and cosines calculated on a grid,
   * to estimate exact values of all sines and cosines - saves memory
   * and computing time)
   * input:
   * dt[]    - array of obs. times
   * df[]    - array of obs. fluxes
   * dfe[]   - array of obs. flux errors (sample variance is used if NULL)
   * inp     - number of data points for dt, df and dfe
   * iofac   - oversampling factor
   * dfmax   - only frequencies below or equal 'dfmax' are tested (all
   *           frequencies up to the nyquist are tested, if 'dfmax' < 0.)
   * bnewdat - true if data set has changed; forces recalculation of some
   *           sines and cosines
   * inf     - number of test frequencies
   * w0      - smallest test frequency
   *
   * output:
   * bnewdat  - is changed to false if it was true
   * dfreq[]  - array of test frequencies
   * dpower[] - power values for all test frequencies
   */
{
  int i, j, k;
  static int inp_bak;

  double dsign, tau, dfe2;

  double dsinw, dcosw, dcoswtau, dsinwtau, dsumcos2w, dsumsin2w;
  double dsumcoswdt2, dsumsinwdt2, dsinsum, dcossum, dsinwdt, dcoswdt;
  static double dsvar, dmean, w;

  double *psinw0, *pcosw0, *psinw, *pcosw;
  static double *psinwdt, *pcoswdt, *psum_coswdt2, *psum_sinwdt2;
  static double *pfe2, *ptau, *pfreq, *ppower = NULL;

  static bool bno_dsvar = false;


  if (*bnewdat)

    /* do all the following calculations only if we're dealing with a new
     * data set (new dt[], dfe[]). this will significantly speed up all the
     * simulations
     */
    {
      /* release old arrays if they exist
       */
      if (ppower != NULL)
	{
	  cts_mfree (pfe2);
	  cts_mfree (pfreq);
	  cts_mfree (ppower);
	  cts_mfree (psinwdt);
	  cts_mfree (pcoswdt);
	  cts_mfree (psum_sinwdt2);
	  cts_mfree (psum_coswdt2);
	}


      /* check whether to use individual flux errors or the sample variance;
       * in the first case, I set a flag not to use dsvar (it's then set to 1)
       * and put the squared errors in an array, in the latter case '1' is used
       * instead of all the squared errors
       */
      pfe2 = cts_mmalloc (double, inp, CFUNCNAME);

      if (dfe != NULL)
	{
	  bno_dsvar = true;
	  for (i = 0; i < inp; i++)
	    {
	      pfe2[i] = m2 (dfe[i]);

	      /* check squared flux error
	       */
	      if (pfe2[i] < dEPSILON)
		cts_merror ("%s: error of flux too small!\n", CFUNCNAME);
	    }
	}
      else
	{
	  bno_dsvar = false;
	  for (i = 0; i < inp; i++)
	    pfe2[i] = 1.;
	}


      /* allocate memory for arrays which store sin (w0*ti), cos (w0*ti),
       * sin (w*ti) and cos (w*ti)
       */
      psinw0 = cts_mmalloc (double, inp, CFUNCNAME);
      pcosw0 = cts_mmalloc (double, inp, CFUNCNAME);
      psinw  = cts_mmalloc (double, inp, CFUNCNAME);
      pcosw  = cts_mmalloc (double, inp, CFUNCNAME);

      /* allocate memory for sin (w*(ti-tau)), cos (w*(ti-tau)) and the sum
       * of these sines and cosines squared (these arrays are finally used
       * for calculating the LS-periodogram)
       * psinwdt and pcoswdt are calculated on a grid to estimate the exact
       * sines and cosines
       */
      psum_sinwdt2 = cts_mmalloc (double, inf, CFUNCNAME);
      psum_coswdt2 = cts_mmalloc (double, inf, CFUNCNAME);
      psinwdt = cts_mmalloc (double, iSC_GRID_POINTS, CFUNCNAME);
      pcoswdt = cts_mmalloc (double, iSC_GRID_POINTS, CFUNCNAME);

      for (i = 0; i < iSC_GRID_POINTS; i++)
	{
	  psinwdt[i] = sin (cts_mput_angle (i));
	  pcoswdt[i] = cos (cts_mput_angle (i));
	}


      /* allocate memory for the tau, frequency and power values
       * arrays
       */
      ptau = cts_mmalloc (double, inf, CFUNCNAME);
      pfreq = cts_mmalloc (double, inf, CFUNCNAME);
      ppower = cts_mmalloc (double, inf, CFUNCNAME);


      /* fill psinw0 and pcosw0 arrays and copy to psinw, pcosw
       * (we start with w = w0)
       */
      for (i = 0; i < inp; i++)
	{
	  /* get index to access grid sines and cosines
	   */
	  cts_mget_index (k, dsign, w0 * dt[i]);

	  psinw0[i] = dsign * psinwdt[k];
	  pcosw0[i] = pcoswdt[k];
	}


      memcpy (psinw, psinw0, (size_t) inp * sizeof (double));
      memcpy (pcosw, pcosw0, (size_t) inp * sizeof (double));


      /*
       * now calculate the LS-parameters
       */
      for (w = 0., j = 0; j < inf; j++)

	/* loop over all frequencies
	 */
	{
	  dsumsin2w = dsumcos2w = 0.;
	  dsumsinwdt2 = dsumcoswdt2 = 0.;

	  /* loop over all data points and calculate tau
	   */
	  for (i = 0; i < inp; i++)
	    {
	      dsumsin2w += 2. * psinw[i] * pcosw[i];
	      dsumcos2w += m2 (pcosw[i]) - m2 (psinw[i]);
	    }

	  w += w0;
	  tau = 0.5 * atan2 (dsumsin2w, dsumcos2w) / w;

	  /* fill frequency and tau array
	   */
	  pfreq[j] = w / d2PI;
	  ptau[j] = tau;


	  /* now do final calculations and store them in the
	   * arrays;
	   * in addition, update psinw and pcosw array for next
	   * loop (frequency w = w + w0)
	   */
	  /* get index to access grid sines and cosines
	   */
	  cts_mget_index (k, dsign, w0 * dt[i]);

	  dsinwtau = dsign * psinwdt[k];
	  dcoswtau = pcoswdt[k];


	  for (i = 0; i < inp; i++)
	    {
	     dfe2 = pfe2[i];
	     dsinwdt = (psinw[i] * dcoswtau - pcosw[i] * dsinwtau);
	     dcoswdt = (pcosw[i] * dcoswtau + psinw[i] * dsinwtau);

	     dsumsinwdt2 += m2 (dsinwdt) / dfe2;
	     dsumcoswdt2 += m2 (dcoswdt) / dfe2;

	     dsinw = psinw[i] * pcosw0[i] + psinw0[i] * pcosw[i];
	     dcosw = pcosw[i] * pcosw0[i] - psinw0[i] * psinw[i];

	     psinw[i] = dsinw;
	     pcosw[i] = dcosw;
	    }

	  psum_sinwdt2[j] = dsumsinwdt2;
	  psum_coswdt2[j] = dsumcoswdt2;

	}   /* END loop over all frequencies */


      /* release memory
       */
      cts_mfree (psinw);
      cts_mfree (pcosw);
      cts_mfree (psinw0);
      cts_mfree (pcosw0);

      /* remember number of data points and reset bnewdat
       */
      inp_bak = inp;
      *bnewdat = false;

    }  /* END if (*bnewdat) */



  /* check whether number of data points is consitent
   */
  if (inp_bak != inp)
    cts_merror ("%s: data set changed; use 'bnewdat = true' !\n", CFUNCNAME);


  /* get mean and sample variance of dataset; reset dsvar to 1 if
   * not used
   */
  cts_vget_svar (df, inp, &dmean, &dsvar);

  if (bno_dsvar) dsvar = 1.;


  /* calculate Lomb-Scargle periodogram for each frequency
   */
  for (w = 0., j = 0; j < inf; j++)
    {
      w += w0;
      tau = ptau[j];
      dsinsum = dcossum = 0.;

      for (i = 0; i < inp; i++)
	{
	  /* get index to access grid sines and cosines
	   */
	  dfe2 = pfe2[i];
	  cts_mget_index (k, dsign, w * (dt[i] - tau));

	  dsinsum += (df [i] - dmean) * dsign * psinwdt[k] / dfe2;
	  dcossum += (df [i] - dmean) * pcoswdt[k] / dfe2;
	}

      ppower[j] = 0.5 * (m2 (dcossum) / psum_coswdt2[j]
			 + m2 (dsinsum) / psum_sinwdt2[j]) / dsvar;
    }


  /* set return values
   */
  *freq = pfreq;
  *power = ppower;
}


void cts_ls_power (double dt[], double df[], double dfe[], int inp, int iofac,
		   double dfmax, bool *bnewdat, double **freq, double **power,
		   int *nf)

  /* this is the driver for the ls_pow1() and ls_pow2() functions. The
   * first one calculates exact sine and cosine values, but runs out of
   * memory, if the dataset is too large. The second function uses sines
   * and cosines calculated on a grid, to estimate the real sines and
   * cosines. This driver decides which function to use.
   * All parameters passed to this function are describen in ls_pow1()
   * and ls_pow2;
   */
{
  static int inf;
  static double w0;

  static void (*vpfunc)();


  if (*bnewdat)
    {
      /* get number of independent test frequencies and smallest frequency
       * (determines size of ifs)
       */
      cts_vgetfreq (dt, inp, iofac, &dfmax, &w0, &inf);

      /* decide which power function to take (set function pointer)
       */
      vpfunc = (inp * inf < iSC_GRID_POINTS) ? ls_pow1 : ls_pow2;
    }


  /* call ls_pow function
   */
  vpfunc (dt, df, dfe, inp, iofac, dfmax, bnewdat, freq, power, inf, w0);


  /* set return value
   */
  *nf = inf;
}
