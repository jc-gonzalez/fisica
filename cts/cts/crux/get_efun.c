#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "ctsminuit.h"
#include "ctscuts.h"
#include "crux.h"


/* file scope variables
 */
/* list of event data (set in vget_efunc, used in vefunc)
 */
static ELIST_EL *pelist_mc;

/* arrays containing final fit parameters (set in vifunc)
 */
static double dpar[iEFUNC_PARS], dpar_err[iEFUNC_PARS];


/*
 * declaration of local functions
 */
static void vefunc (int *, double *, double *, double *, int *, int *);


static void vefunc (int *npar, double *grad, double *fcnval,
		    double *dfitpars, int *iflag, int *dummy)

  /* this function is used to derive the best Energy estimation function
   * for a set of MC events
   * input:
   * npar    - number of fit parameters
   * dfitpar - array of fit parameters
   * iflag   - flag which indicates status of minuit
   * dummy   - not used here
   *
   * output value:
   * grad    - array of partial derivatives (optional)
   * fcnval  - chi^2 value
   *
   * this function uses the following file scope variable:
   * pelist_mc - array of mc-event data
   * dpar, dpar_err - arrays containing final fit parameters
   */
{
  int i, ivarbl;

  char parname[iPARNAMELEN];

  double denergy, dchisq;

  ELIST_EL *pelist_el;


  /* first call to this function. nothing to be done
   */
  if (*iflag == 1);


  /* loop over all events and calculate chi^2
   */
  if (*iflag == 4)
    {
      dchisq = 0.;
      pelist_el = pelist_mc;

      while (pelist_el != NULL)
	{
	  menergy (pelist_el, dfitpars, denergy);
	  dchisq += fabs ((pelist_el->de_mc - denergy) / pelist_el->de_mc);

	  pelist_el = pelist_el->p2next;
	}

      *fcnval = dchisq;
    }


  /* minuit converged
   */
  if (*iflag == 3)
    {
      /* loop over all events and calculate final chi^2; set estimated
       * energy in evt-data list
       */
      dchisq = 0.;
      pelist_el = pelist_mc;

      while (pelist_el != NULL)
	{
	  menergy (pelist_el, dfitpars, denergy);
	  dchisq += fabs ((pelist_el->de_mc - denergy) / pelist_el->de_mc);
	  pelist_el->de_est = denergy;

	  pelist_el = pelist_el->p2next;
	}


      /* save final results
       */
      for (i = 0; i < iEFUNC_PARS; i++)
	cts_mnpout (i+1, parname, &dpar[i], &dpar_err[i], &ivarbl);
    }
}



void vget_efunc (ELIST_EL *pelist, double **pfitpar, double **pfitpar_err)

  /* this function derives the energy estimation function
   * for a set of MC events passed in 'pelist'. The estimated
   * Energy from the final function is stored in the event list
   * (this function sets the file scope variable 'pelist_mc')
   * output:
   * pfitpar, pfitpar_err - fit parameter results
   */
{
  int i;
  /* command strings passed to minuit
   */
  char *pminos   = "minos";
  char *pcallfcn   = "call vefunc";

  /* dfpar  - start values for shot parameters
   * dfpare - errors of shot parameters
   */
  double dfpar[iEFUNC_PARS];
  double dfpare[iEFUNC_PARS];

  /* variables used to pass parameters to minuit
   * (their meaning depends on the corresponding minuit command)
   */
  int iargs;
  double dval[2];



  /* set file scope variable 'pelist_mc'
   */
  pelist_mc = pelist;


  /*
   * use minuit routines for minimization
   */
  /* initialize minuit
   */
  cts_mninit (5, 6, 7);


  /* define parameters in minuit
   */
  for (i = 0; i < iEFUNC_PARS; i++)
    {
      dfpar[i] = 1.0;
      dfpare[i] = fabs (dfpar[i]);
      cts_mnparm (i+1, "", dfpar[i], dfpare[i]);
    }

  /* read in data
   */
  iargs = 1;
  *dval = 1.;
  cts_mnexcm (vefunc, pcallfcn, dval, iargs);


  /* start MINUIT
   */
  iargs = 2;
  *dval = 20000.;
  *(dval+1) = 0.1;
  cts_mnexcm (vefunc, pminos, dval, iargs);


  /* calculate & write down final result
   */
  iargs = 1;
  *dval = 3.;
  cts_mnexcm (vefunc, pcallfcn, dval, iargs);

  /* set return pointers
   */
  *pfitpar = dpar;
  *pfitpar_err = dpar_err;

  return;
}
