#include <stdio.h>
#include "ctsbase.h"
#include "ctsminuit.h"


void cts_mnparm (int pnr, char *pname, double pval, double perr)
{
  extern int  mnparm_ ();
  int err;
  double dnull = 0.;

  mnparm_ (&pnr, pname, &pval, &perr, &dnull, &dnull, &err, strlen (pname));


  if (err > 0)
    cts_merror ("%s: define parameters failed.\n", pname);
}


void cts_mnexcm (void fcn (), char *call, double *arglist, int nr_args)
{
  extern int  mnexcm_ ();
  int err;
  char *c = "";

  mnexcm_ (fcn, call, arglist, &nr_args, &err, c, strlen (call), strlen (c));
}


void cts_mnpout (int nr_par, char *pname, double *dval, double *derr, int *var)
{
  extern void mnpout_ ();
  double bnd1, bnd2;

  mnpout_ (&nr_par, pname, dval, derr, &bnd1, &bnd2, var, iPARNAMELEN);
}


void cts_mnerrs (int nr_par, double *plus, double *min, double *parab,
		 double *globcc)
{
  extern void mnerrs_ ();

  mnerrs_ (&nr_par, plus, min, parab, globcc);
}


void cts_mninit (int i, int k, int l)
{
  extern void mninit_ ();

  mninit_ (&i, &k, &l);
}
