#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctshbook.h"
#include "ctshplot.h"
#include "ctsmath.h"
#include "powspec.h"


/*
 * declaration of local functions
 */
static bool bopen_dat_file (FILE_TYPE *);
static char *pread_title (FILE_TYPE);


/*
 * functions
 */
static bool bopen_dat_file (FILE_TYPE *file)

  /* tries to open input file
   */
{
  bool bfound;

  struct stat file_stat;

  fprintf (stdout, "looking for input file %s ... ", file->pname);

  if (stat (file->pname, &file_stat) == 0)
    {
      fprintf (stdout, "found.\n Create ps-file.\n");
      bfound = true;

      cts_vopen_file (file);
    }

  else
    {
      fprintf (stdout, "not found!\n");
      bfound = false;
    }

  return (bfound);
}


#define CFUNCNAME "pread_title"

static char *pread_title (FILE_TYPE file)

  /* reads in powspec-plot title from input file
   */
{
  int i, isiz;
  char c;
  char *ptitle;


  /* first determine the size of the title (actually it's the size
   * of the first line; minimum size is 1)
   */
  isiz = 1;

  while ((c = getc (file.pfile)) != '\n' && c != EOF)
    isiz++;


  /* now read in title (reset file pos. indicator to beginning of
   * the title - 4th char in file
   */
  ptitle = cts_mmalloc (char, isiz, CFUNCNAME);
  *ptitle = '\0';


  if (fseek (file.pfile, 3, SEEK_SET))
    cts_merror ("%s: unable to access file position!\n", CFUNCNAME);

  for (i = 0; i < isiz - 3; i++)
    ptitle[i] = getc (file.pfile);

  /* read '\n' to complete line - not used
   */
  (void) getc (file.pfile);


  return (ptitle);
}

#undef CFUNCNAME


#define CFUNCNAME "vplot_pdf"

void vplot_pdf (double *pf, int *ppow_pdf, double dpowmax, int ipbins, int inf,
		int isims, NHYP nhyp, double dsrate, double dmsa, double dsdev,
		int ieflag)

  /* create ps-file for a given pdf-distribution
   * input:
   * pf       - array of frequencies
   * ppow_pdf - array containing pdf (frequency dependent)
   * dpowmax  - maximum power for ppow_pdf
   * ipbins   - number of power bins in ppow_pdf
   * inf      - number of frequencies
   * isims    - number of simulations to get ppow_pdf
   * nhyp     - underlying null hypothesis of pdf
   * dsrate   - mean shot rate of the flares (used for simulated lc only)
   * dmsa     - mean squared amplitude of the flares ( - " - )
   * dsdev    - stand. dev. of the gaussian type flares ( - " - )
   * ieflag   - take sample variance if 0, else use individual data errors
   *            (used for normalizing power spectrum)
   */
{
  char chopt[1][iCOPT_LEN] = {{"LOGY"}};
  char *pstr, *psubtit;
  char *ppsfile [MAX_NHYP_TYPE] = {cPDF_SIM_PS, cPDF_BOOT_PS, cPDF_GAUSS_PS};
  char *pfilenam [MAX_NHYP_TYPE] = {"Simulated lightcurves", "Bootstrapped data",
				    "Gaussian random noise"};
  int i, k, index, ilun;

  float fnr_pow, fdelta_pow, fnorm_lo, fnorm_hi, fpmax;
  float xline[2], yline[2];

  /* pcumdf        - cumulative distribution (freq. dependent) for ppow_pdf
   * pcumdf_lf     - sum of pcumdf (lowest frequencies)
   * pcumdf_lf_err - error of pcumdf_lf
   * pcumdf_hf     - sum of pcumdf (highest frequencies)
   * pcumdf_hf_err - error of pcumdf_hf
   * pow           - set of power values - corresponding to arrays above
   * pow_err       - error of pow (set to 0)
   */
  float *pcumdf, *pcumdf_lf, *pcumdf_hf, *pcumdf_lf_err, *pcumdf_hf_err;
  float *pow, *pow_err;

  /* init hplot and open ps-file
   */
  cts_vinit_hplot (0);
  cts_vkuopen (&ilun, ppsfile[nhyp], "UNKNOWN");

  cts_igmeta (-ilun, -111);


  /* set ieflag related variables
   */
  if (ieflag == 0)
    {
      fpmax = 50.f;
      psubtit = "(sample variance)";
    }

  else
    {
      fpmax = (float) (50. * dPOWMAXSF);
      psubtit = "(ind. data errors)";
    }


  /* allocate memory for arrays and fill them
   */
  pow = cts_mmalloc (float, ipbins, CFUNCNAME);
  pow_err = cts_mmalloc (float, ipbins, CFUNCNAME);
  pcumdf_lf = cts_mmalloc (float, ipbins, CFUNCNAME);
  pcumdf_hf = cts_mmalloc (float, ipbins, CFUNCNAME);
  pcumdf_lf_err = cts_mmalloc (float, ipbins, CFUNCNAME);
  pcumdf_hf_err = cts_mmalloc (float, ipbins, CFUNCNAME);
  pcumdf = cts_mmalloc (float, ipbins * inf, CFUNCNAME);

  cts_vreset_farray (pcumdf_lf, ipbins);
  cts_vreset_farray (pcumdf_hf, ipbins);
  cts_vreset_farray (pcumdf_lf, ipbins);
  cts_vreset_farray (pcumdf_hf, ipbins);


  for (i = 0; i < inf; i++)
    {
      fnr_pow = (float) isims;

      for (k = 0; k < ipbins; k++)
	{
	  /* fill cumulative distribution (frequency dependent)
	   */
	  index = i + k * inf;
	  fnr_pow -= (k > 0) ? (float) ppow_pdf[index-inf] : 0.f;
	  pcumdf[index] = fnr_pow;


	  if (i < iNRLOWFREQ)
	    {
	      pcumdf_lf[k] += pcumdf[index];
	      pcumdf_lf_err[k] += sqrt (pcumdf[index]);
	    }

	  else if (i > inf - iNRHIGHFREQ)
	    {
	      pcumdf_hf[k] += pcumdf[index];
	      pcumdf_hf_err[k] += sqrt (pcumdf[index]);
	    }

	  pcumdf[index] /= (float) isims;
	}
    }


  fnorm_lo = (float) (iNRLOWFREQ * isims);
  fnorm_hi = (float) (iNRHIGHFREQ * isims);
  fdelta_pow = (float) mdiv (dpowmax, ipbins);

  for (k = 0; k < ipbins; k++)
    {
      pow_err[k] = 0.f;
      pow[k] = (float) k * fdelta_pow;

      /* prevent zero array-content - they're used in a log plot
       */
      pcumdf_lf[k] /= fnorm_lo;
      pcumdf_lf_err[k] /= fnorm_lo;
      pcumdf_lf[k] += (pcumdf_lf[k] > dEPSILON) ? 0.f : dEPSILON;

      pcumdf_hf[k] /= fnorm_hi;
      pcumdf_hf_err[k] /= fnorm_hi;
      pcumdf_hf[k] += (pcumdf_hf[k] > dEPSILON) ? 0.f : dEPSILON;
    }



  /*
   * plot cumulative distribution
   */
  cts_hplopt (chopt, 1);
  cts_igset ("PMCI", 3);
  cts_igset ("TXCI", 1);
  cts_hplset ("FCOL", 4);
  cts_hplsiz (20., 15., ' ');

  cts_hplfra (0.f, fpmax, 1e-10f, 1.f, " ");
  cts_hpltit (";power; Probability");
  cts_hplerr (pow, pcumdf_lf, pow_err, pcumdf_lf_err, ipbins, '0', 25, 0.1f);

  cts_igset ("PMCI", 2);
  cts_hplerr (pow, pcumdf_hf, pow_err, pcumdf_hf_err, ipbins, '0', 25, 0.1f);

  /* plot theoretical expectation - exp(-z), a line in logy scale
   */
  xline[0] = 0.f;
  xline[1] = 50.f;
  yline[0] = 1.f;
  yline[1] = 1.93e-22f;

  cts_igset ("PLCI", 4);
  cts_hpline (xline, yline, 2, ' ');
  cts_igset ("PLCI", 0);


  /* text
   */
  cts_iselnt (0);
  cts_igset ("TXCI", 3);
  cts_igset ("CHHE", 0.02f);
  pstr = cts_preal_to_str (pf[0], 3, "(%.3f - %%.3f \"M#1/d\"N#)");
  pstr = cts_preal_to_str (pf[iNRLOWFREQ-1], 3, pstr);
  cts_itx (0.62f, 0.72f, "low frequency range");
  cts_itx (0.63f, 0.69f, pstr);

  cts_igset ("TXCI", 2);
  pstr = cts_preal_to_str (pf[inf-iNRHIGHFREQ], 3, "(%.3f - %%.3f \"M#1/d\"N#)");
  pstr = cts_preal_to_str (pf[inf-1], 3, pstr);
  cts_itx (0.62f, 0.64f, "high frequency range");
  cts_itx (0.63f, 0.61f, pstr);

  if (nhyp == SIMLC)
    {
      cts_igset ("TXCI", 1);
      pstr = cts_preal_to_str (dsdev, 2, "stand. dev.: %.2f");
      cts_itx (0.65f, 0.56f, pstr);
      pstr = cts_preal_to_str (dsrate, 2, "shot rate: %.2f");
      cts_itx (0.65f, 0.52f, pstr);
      pstr = cts_preal_to_str (dmsa, 2, "msa: %.2f");
      cts_itx (0.65f, 0.48f, pstr);
    }

  cts_igset ("TXCI", 4);
  cts_itx (0.4f, 0.8f, psubtit);
  cts_igset ("CHHE", 0.03f);
  cts_itx (0.3f, 0.85f, pfilenam[nhyp]);

  /* close higz window
   */
  cts_igmeta (999, 0);
  cts_hplend ();
  cts_vkuclos (ilun, " ");


  /* free arrays
   */
  cts_mfree (pow);
  cts_mfree (pow_err);
  cts_mfree (pcumdf_lf);
  cts_mfree (pcumdf_hf);
  cts_mfree (pcumdf_lf_err);
  cts_mfree (pcumdf_hf_err);
  cts_mfree (pcumdf);
}

#undef CFUNCNAME


#define CFUNCNAME "vplot_powspec"

void vplot_powspec (double *pf, double *ppow, int inf, MLE_STRUCT mle,
		    double dfmax, double dfmax_mle, char *ptitle, int ieflag)

  /* create ps-file for a given power spectrum
   * input:
   * pf        - array of frequencies
   * ppow      - array containing power values (frequency dependent)
   * inf       - number of frequencies
   * mle       - struct which contains results from mle-fit
   * dfmax     - maximum frequency used for the plot
   * dfmax_mle - maximum frequency used for the mle-fit
   * ptitle    - title of plot
   * ieflag    - take sample variance if 0, else use individual data errors
   *             (used for normalizing power spectrum)
   */
{
  char *pstr, *psubtit;

  int i, ilun;

  float fpowmax;
  float *pff, *pfpow, *pfmle;


  /* init hplot and open ps-file
   */
  cts_vinit_hplot (0);
  cts_vkuopen (&ilun, cPOW_PS, "UNKNOWN");

  cts_igmeta (-ilun, -111);


  /* transform double arrays into float arrays, fill array of mle-fit
   * (also derive max. power)
   */
  pff = cts_mmalloc (float, inf, CFUNCNAME);
  pfpow = cts_mmalloc (float, inf, CFUNCNAME);
  pfmle = cts_mmalloc (float, inf, CFUNCNAME);

  fpowmax = 0.f;

  for (i = 0; i < inf; i++)
    {
      pff[i] = (float) pf[i];
      pfpow[i] = (float) ppow[i];
      pfmle[i] = (float) (mle.a * exp (-m2 (d2PI * pf[i] * mle.s)) + mle.c);

      fpowmax = max (fpowmax, ppow[i]);
    }


  /*
   * plot power spectrum
   */
  cts_igset ("TXCI", 1);
  cts_igset ("PLCI", 4);
  cts_hplsiz (20., 15., ' ');
  cts_hplfra (0.f, (float) dfmax, 0.f, 1.1f * fpowmax, " ");
  cts_hpltit (";frequency; power");
  cts_igraph (inf, pff, pfpow, "L*");

  cts_igset ("PLCI", 3);
  cts_igraph (inf, pff, pfmle, "CS");


  /* text
   */
  cts_iselnt (0);
  cts_igset ("TXCI", 1);
  cts_igset ("CHHE", 0.02f);
  pstr = cts_preal_to_str (dfmax_mle, 2, "fmax?mle!\"J# %6.2f");
  cts_itx (0.605f, 0.72f, pstr);

  pstr = cts_preal_to_str (mle.a, 2, "A?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.ae, 2, pstr);
  cts_itx (0.65f, 0.68f, pstr);

  pstr = cts_preal_to_str (mle.s, 2, "S?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.se, 2, pstr);
  cts_itx (0.65f, 0.64f, pstr);

  pstr = cts_preal_to_str (mle.c, 2, "C?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.ce, 2, pstr);
  cts_itx (0.65f, 0.60f, pstr);

  pstr = cts_preal_to_str (mle.l, 2, "L?mle!\"J# %.2f");
  cts_itx (0.655f, 0.56f, pstr);

  cts_igset ("TXCI", 4);
  psubtit = (ieflag == 0) ? "(sample variance)" : "(ind. data errors)";
  cts_itx (0.4f, 0.8f, psubtit);
  cts_igset ("CHHE", 0.03f);
  cts_itx (0.35f, 0.85f, ptitle);



  /* close higz window
   */
  cts_igmeta (999, 0);
  cts_hplend ();
  cts_vkuclos (ilun, " ");


  /* free arrays
   */
  cts_mfree (pff);
  cts_mfree (pfpow);
  cts_mfree (pfmle);
}

#undef CFUNCNAME


#define CFUNCNAME "vplot_sinpow"

void vplot_sinpow (double *pf, double *ppow, int inf, double dfmax,
		   double dperiod, double dstime, double damp, int ieflag)

  /* create ps-file for a power spectrum from a sinusoidal lightcurve
   * input:
   * pf      - array of frequencies
   * ppow    - array containing power values (frequency dependent)
   * inf     - number of frequencies
   * dfmax   - maximum frequency used for the plot
   * dperiod - period of sinusoidal lightcurve
   * dstime  - start time (t0)
   * damp    - amplitude
   * ieflag  - take sample variance if 0, else use individual data errors
   *           (used for normalizing power spectrum)
   */
{
  char *pstr, *psubtit;
  char *ptitle = "Sinusoidal lightcurve";
  int i, ilun;

  float *pff, *pfpow;
  double dpowmax;


  /* init hplot and open ps-file
   */
  cts_vinit_hplot (0);
  cts_vkuopen (&ilun, cSIN_PS, "UNKNOWN");

  cts_igmeta (-ilun, -111);


  /* derive max. power and transform double arrays into float arrays
   * (pfpow is normalized to a maximum power of 1)
   */
  pff = cts_mmalloc (float, inf, CFUNCNAME);
  pfpow = cts_mmalloc (float, inf, CFUNCNAME);

  dpowmax = 0.;

  for (i = 0; i < inf; i++)
    dpowmax = max (dpowmax, ppow[i]);

  for (i = 0; i < inf; i++)
    {
      pff[i] = (float) pf[i];
      pfpow[i] = (float) mdiv (ppow[i], dpowmax);
    }


  /*
   * plot power spectrum
   */
  cts_igset ("TXCI", 1);
  cts_igset ("PLCI", 4);
  cts_hplsiz (20., 15., ' ');
  cts_hplfra (0.f, (float) dfmax, 0.f, 1.1f, " ");
  cts_hpltit (";frequency; power");
  cts_igraph (inf, pff, pfpow, "L*");

  /* text
   */
  cts_iselnt (0);
  cts_igset ("TXCI", 1);
  cts_igset ("CHHE", 0.02f);
  pstr = cts_preal_to_str (damp, 2, "Amplitude: %.2f");
  cts_itx (0.65f, 0.72f, pstr);

  pstr = cts_preal_to_str (dperiod, 2, "Period: %.2f");
  cts_itx (0.65f, 0.68f, pstr);

  pstr = cts_preal_to_str (dstime, 2, "T?0!: %.2f");
  cts_itx (0.65f, 0.64f, pstr);

  cts_igset ("TXCI", 4);
  psubtit = (ieflag == 0) ? "(sample variance)" : "(ind. data errors)";
  cts_itx (0.45f, 0.8f, psubtit);
  cts_igset ("CHHE", 0.03f);
  cts_itx (0.35f, 0.85f, ptitle);


  /* close higz window
   */
  cts_igmeta (999, 0);
  cts_hplend ();
  cts_vkuclos (ilun, " ");


  /* free arrays
   */
  cts_mfree (pff);
  cts_mfree (pfpow);
}

#undef CFUNCNAME


#define CFUNCNAME "vplot_sim"

void vplot_sim (bool bfinal, int ins, double dsdev, double dsrate, double dmsa,
		int isims, double dfmax, double dfmax_mle, double dmfs,
		double dmfvs, double dma, MLE_STRUCT mle, double *pf,
		double *ppow, int inf, int ieflag, SIMTYPE styp)

  /* plot results from simulations
   * input:
   * bfinal    - triggers final plot and finishes higz
   * ins       - number of different shot noise parameters
   * dsdev     - stand. dev. of the gaussian type flares
   * dsrate    - mean shot rate of the flares
   * dmsa      - mean squared amplitude of the flares
   * isims     - mumber of simulations
   * dfmax     - max. frequency used for power spectrum
   * dfmax_mle - max. frequency used for the mle-fit
   * dmfv      - flux variance for a given lightcurve (not used yet)
   * dmfvs     - mean flux variance for a set of dmfv values (not used yet)
   * dma       - theoretical estimation of mle-parameter 'a' (amplitude)
   * mle       - struct which contains results from mle-fit
   * pf        - array of test frequencies
   * ppow      - array of power spectrum
   * inf       - number of independent frequencies
   * ieflag    - take sample variance if 0, else use individual data errors
   *             (used for normalizing power spectrum)
   * styp      - enum to characterize simulation type
   */
{
  int i;
  static int ilun, ins_bak, inf_bak, instep = 0;

  char *pstr;
  char *pfilenam[MAX_SIM_TYPE] = {cAVAR_PS, cSVAR_PS, cRVAR_PS};
  char *pxvar[MAX_SIM_TYPE] = {"msa: %.2f", "sdev: %.2f", "srate: %.2f"};
  char *ptit[MAX_SIM_TYPE] = {"variable msa", "variable sdev",
			      "variable srate"};
  char *psubtit[MAX_SIM_TYPE] = {" sdev %.2f, srate %%.2f)",
				 " msa %.2f, srate %%.2f)",
				 " sdev %.2f, msa %%.2f)"};
  char *phpltit1[MAX_SIM_TYPE] = {";msa;A?analytical! / (A?mle! + C?mle!)",
				  ";sdev;A?analytical! / (A?mle! + C?mle!)",
				  ";srate;A?analytical! / (A?mle! + C?mle!)"};
  char *phpltit2[MAX_SIM_TYPE] = {";msa;S?analytical! / S?mle!",
				  ";sdev;S?analytical! / S?mle!",
				  ";srate;S?analytical! / S?mle!"};


  float fx, fstit1, fstit2, fpowmax, xline[2], yline[2];
  static float fx_min, fx_max, fdma_max, fdsdev_max, fdmam, fdsdevm;

  static float *px = NULL, *pdma = NULL, *pdsdev = NULL, *pff = NULL;
  static float *pfpow = NULL, *pfmle = NULL;

  static bool binit = true;


  /* first choose which passed variable is used as x-value of plots;
   * also set variables to build up 'psubtit'
   */
  switch (styp)
    {
    case SIM_A:

      fx = (float) dmsa;
      fstit1 = (float) dsdev;
      fstit2 = (float) dsrate;
      break;

    case SIM_S:

      fx = (float) dsdev;
      fstit1 = (float) dmsa;
      fstit2 = (float) dsrate;
      break;

    case SIM_R:

      fx = (float) dsrate;
      fstit1 = (float) dsdev;
      fstit2 = (float) dmsa;
      break;

    default:
      cts_merror ("%s: default in switch reached", CFUNCNAME);
    }


  if (binit)
    {
      /* init hplot and open ps-file
       */
      cts_vinit_hplot (0);
      cts_vkuopen (&ilun, pfilenam[styp], "UNKNOWN");

      cts_igmeta (-ilun, -111);


      /* allocate memory to store data for final plots
       */
      ins_bak = ins;
      px = cts_mrealloc (px, float, ins, CFUNCNAME);
      pdma = cts_mrealloc (pdma, float, ins, CFUNCNAME);
      pdsdev = cts_mrealloc (pdsdev, float, ins, CFUNCNAME);

      /* allocate memory for power spectrum
       */
      inf_bak = inf;
      pff = cts_mrealloc (pff, float, inf, CFUNCNAME);
      pfpow = cts_mrealloc (pfpow, float, inf, CFUNCNAME);
      pfmle = cts_mrealloc (pfmle, float, inf, CFUNCNAME);

      fx_min = fx;
      fx_max = fdma_max = fdsdev_max = fdmam = fdsdevm = 0.f;

      binit = false;
    }


  /* update arrays for final plots (also look for extrema and mean values)
   */
  if (instep <= ins_bak)
    {
      px[instep] = fx;
      pdma[instep] = (float) mdiv (dma, mle.a + mle.c);
      pdsdev[instep] = (float) mdiv (dsdev, mle.s);

      fdmam += pdma[instep];
      fdsdevm += pdsdev[instep];

      fx_min = min (fx_min, px[instep]);
      fx_max = max (fx_max, px[instep]);
      fdma_max = max (fdma_max, pdma[instep]);
      fdsdev_max = max (fdsdev_max, pdsdev[instep]);

      instep++;
    }

  else
    cts_merror ("%s: array index (final plots) exceeded limit", CFUNCNAME);



  /* set arrays for power spectrum plot (also derive max. power)
   */
  if (inf <= inf_bak)
    {
      fpowmax = 0.f;

      for (i = 0; i < inf; i++)
	{
	  pff[i] = (float) pf[i];
	  pfpow[i] = (float) ppow[i];
	  pfmle[i] = (float) (mle.a * exp (-m2 (d2PI * pf[i] * mle.s)) + mle.c);

	  fpowmax = max (fpowmax, ppow[i]);
	}
    }

  else
    cts_merror ("%s: array index (spectrum) exceeded limit", CFUNCNAME);



  /*
   * plot power spectrum  first
   */
  cts_igset ("TXCI", 1);
  cts_igset ("PLCI", 4);
  cts_hplsiz (20., 15., ' ');
  cts_hplfra (0.f, (float) dfmax, 0.f, 1.5f * fpowmax, " ");
  cts_hpltit (";frequency; power");
  cts_igraph (inf, pff, pfpow, "L*");

  cts_igset ("PLCI", 3);
  cts_igraph (inf, pff, pfmle, "CS");


  /* text
   */
  cts_iselnt (0);
  cts_igset ("TXCI", 1);
  cts_igset ("CHHE", 0.02f);
  pstr = cts_preal_to_str (dfmax_mle, 2, "fmax?mle!\"J# %6.2f");
  cts_itx (0.605f, 0.72f, pstr);

  pstr = cts_preal_to_str (mle.a, 2, "A?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.ae, 2, pstr);
  cts_itx (0.65f, 0.68f, pstr);

  pstr = cts_preal_to_str (mle.s, 2, "S?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.se, 2, pstr);
  cts_itx (0.65f, 0.64f, pstr);

  pstr = cts_preal_to_str (mle.c, 2, "C?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.ce, 2, pstr);
  cts_itx (0.65f, 0.60f, pstr);

  pstr = cts_preal_to_str (mle.l, 2, "L?mle!\"J# %.2f");
  cts_itx (0.655f, 0.56f, pstr);

  pstr = cts_preal_to_str (fx, 2, pxvar[styp]);
  cts_itx (0.2f, 0.72f, pstr);


  cts_igset ("TXCI", 4);
  pstr = (ieflag == 0) ? "(sample variance," : "(ind. data errors,";
  pstr = cts_pstrcat (2, pstr, psubtit[styp]);
  pstr = cts_preal_to_str (fstit1, 2, pstr);
  pstr = cts_preal_to_str (fstit2, 2, pstr);
  cts_itx (0.25f, 0.8f, pstr);

  cts_igset ("CHHE", 0.03f);
  cts_itx (0.38f, 0.85f, ptit[styp]);



  /* last plot - close higz window and reset binit
   */
  if (bfinal)
    {
      fx_min -= 0.1f * fx_max;
      fx_max += 0.1f * fx_max;
      fdmam /= (float) ins_bak;
      fdsdevm /= (float) ins_bak;

      cts_igset ("TXCI", 1);
      cts_igset ("PLCI", 4);
      cts_hplsiz (20., 15., ' ');
      cts_hplfra (fx_min, fx_max, 0.f, 1.5f * fdma_max, " ");
      cts_hpltit (phpltit1[styp]);
      cts_igraph (ins_bak, px, pdma, "L*");

      xline[0] = fx_min;
      xline[1] = fx_max;
      yline[0] = fdmam;
      yline[1] = fdmam;

      cts_igset ("PLCI", 4);
      cts_igset ("LTYP", 2);
      cts_hpline (xline, yline, 2, ' ');
      cts_igset ("LTYP", 1);
      cts_igset ("PLCI", 0);



      cts_igset ("TXCI", 1);
      cts_igset ("PLCI", 4);
      cts_hplsiz (20., 15., ' ');
      cts_hplfra (fx_min, fx_max, 0.f, 1.5f * fdsdev_max, " ");
      cts_hpltit (phpltit2[styp]);
      cts_igraph (ins_bak, px, pdsdev, "L*");

      xline[0] = fx_min;
      xline[1] = fx_max;
      yline[0] = fdsdevm;
      yline[1] = fdsdevm;

      cts_igset ("PLCI", 4);
      cts_igset ("LTYP", 2);
      cts_hpline (xline, yline, 2, ' ');
      cts_igset ("LTYP", 1);
      cts_igset ("PLCI", 0);


      /* reset everything
       */
      cts_igmeta (999, 0);
      cts_hplend ();
      cts_vkuclos (ilun, " ");

      ins_bak = instep = 0;
      binit = true;
    }

}

#undef CFUNCNAME

#define CFUNCNAME "vplot_sim_no"

void vplot_sim_no (bool bfinal, double *pf, double *ppow, int inf, double dmsa,
		   double dsrate, double dsdev, MLE_STRUCT mle, double dfmax,
		   double dfmax_mle, int ieflag)

  /* create ps-file for a simulated power spectrum
   * input:
   * bfinal    - triggers final plot and finishes higz
   * pf        - array of frequencies
   * ppow      - array containing power values (frequency dependent)
   * inf       - number of frequencies
   * dmsa      - mean squared amplitude of the flares
   * dsrate    - mean shot rate of the flares
   * dsdev     - stand. dev. of the gaussian type flares
   * mle       - struct which contains results from mle-fit
   * dfmax     - maximum frequency used for the plot
   * dfmax_mle - maximum frequency used for the mle-fit
   * ieflag    - take sample variance if 0, else use individual data errors
   *             (used for normalizing power spectrum)
   */
{
  static char *pstr, *pstr1, *pstr2, *pstr3, *psubtit = NULL;

  int i;
  static int ilun, inf_bak;

  float fpowmax;
  static float *pff = NULL, *pfpow = NULL, *pfmle = NULL;

  static bool binit = true;


  if (binit)
    {
      /* init hplot and open ps-file
       */
      cts_vinit_hplot (0);
      cts_vkuopen (&ilun, cNVAR_PS, "UNKNOWN");

      cts_igmeta (-ilun, -111);


      /* transform double arrays into float arrays, fill array of mle-fit
       * (also derive max. power)
       */
      inf_bak = inf;
      pff = cts_mrealloc (pff, float, inf, CFUNCNAME);
      pfpow = cts_mrealloc (pfpow, float, inf, CFUNCNAME);
      pfmle = cts_mrealloc (pfmle, float, inf, CFUNCNAME);

      binit = false;
    }


  /* set arrays for power spectrum plot (also derive max. power)
   */
  if (inf <= inf_bak)
    {
      fpowmax = 0.f;

      for (i = 0; i < inf; i++)
	{
	  pff[i] = (float) pf[i];
	  pfpow[i] = (float) ppow[i];
	  pfmle[i] = (float) (mle.a * exp (-m2 (d2PI * pf[i] * mle.s))
			      + mle.c * (d2PI * pf[i]));

	  fpowmax = max (fpowmax, ppow[i]);
	}
    }

  else
    cts_merror ("%s: array index (spectrum) exceeded limit", CFUNCNAME);


  /*
   * plot power spectrum
   */
  cts_igset ("TXCI", 1);
  cts_igset ("PLCI", 4);
  cts_hplsiz (20., 15., ' ');
  cts_hplfra (0.f, (float) dfmax, 0.f, 1.1f * fpowmax, " ");
  cts_hpltit (";frequency; power");
  cts_igraph (inf, pff, pfpow, "L*");

  cts_igset ("PLCI", 3);
  cts_igraph (inf, pff, pfmle, "CS");


  /* text
   */
  cts_iselnt (0);
  cts_igset ("TXCI", 1);
  cts_igset ("CHHE", 0.02f);
  pstr = cts_preal_to_str (dfmax_mle, 2, "fmax?mle!\"J# %6.2f");
  cts_itx (0.605f, 0.72f, pstr);

  pstr = cts_preal_to_str (mle.a, 2, "A?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.ae, 2, pstr);
  cts_itx (0.65f, 0.68f, pstr);

  pstr = cts_preal_to_str (mle.s, 2, "S?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.se, 2, pstr);
  cts_itx (0.65f, 0.64f, pstr);

  pstr = cts_preal_to_str (mle.c, 2, "C?mle!\"J# %.2f \"A# %%.2f");
  pstr = cts_preal_to_str (mle.ce, 2, pstr);
  cts_itx (0.65f, 0.60f, pstr);

  pstr = cts_preal_to_str (mle.l, 2, "L?mle!\"J# %.2f");
  cts_itx (0.655f, 0.56f, pstr);

  cts_igset ("TXCI", 4);
  pstr = (ieflag == 0) ? "(sample variance, " : "(ind. data errors, ";
  pstr1 = cts_preal_to_str (dmsa, 2, "msa: %.2f, ");
  pstr2 = cts_preal_to_str (dsdev, 2, "sdev: %.2f, ");
  pstr3 = cts_preal_to_str (dsrate, 2, "srate: %.2f)");

  psubtit = cts_pstrcat (4, pstr, pstr1, pstr2, pstr3);
  cts_itx (0.2f, 0.8f, psubtit);

  cts_igset ("CHHE", 0.03f);
  cts_itx (0.25f, 0.85f, "Simulated power spectrum");


  /* last plot
   */
  if (bfinal)
    {
      /* close higz window
       */
      cts_igmeta (999, 0);
      cts_hplend ();
      cts_vkuclos (ilun, " ");

      /* free arrays
       */
      cts_mfree (pff);
      cts_mfree (pfpow);
      cts_mfree (pfmle);


      inf_bak = 0;
      binit = true;
    }

}

#undef CFUNCNAME


#define CFUNCNAME "vplot_ct"

void vplot_ct (double *pf, double *ppow, double *ppowv, double *ppowst, int inf,
	       double dsrate, double dmsa, double dsdev, int ieflag,
	       double dbinset, CTTYPE cttyp, int inp, double dfmax)

  /* plot results from cross term simulations:
   * frequency vs. power / standard dev (power) and
   * frequency vs. standard dev (power) / theoretical standard dev (power)
   * input:
   * pf      - array of test frequencies
   * ppow    - array of power values
   * ppowv   - array of power spectrum variance
   * ppowst  - array of power spectrum standard deviations (theoretical)
   * inf     - size of arrays
   * dsrate  - mean shot rate of the flares
   * dmsa    - mean squared amplitude of the flares
   * dsdev   - stand. dev. of the gaussian type flares
   * ieflag  - take sample variance if 0, else use individual data errors
   *           (used for normalizing power spectrum)
   * dbinset - characterizes the used binning (in obs. times)
   *           0 - orig. binning up to 1 - aequidistant binning
   * cttyp   - characterizes the type of ct-simulation
   * inp     - number of plots (triggers closing of ps-file)
   * dfmax   - maximum frequency used for the plot
   */
{
  int i, inf_used;
  static int ilun, inp_bak, inf_bak, iplot = 0;

  char *pstr, *pstr1, *pstr2, *pstr3, *psubtit;
  char *pfilenam[MAX_CT_TYPE] = {cCTR_PS, cCTT_PS};
  char *ptit[MAX_CT_TYPE] = {"cross terms with variable srate",
			      "cross terms with variable obs. times"};

  float fx_min, fx_max, fdpow1_max, fdpow2_max, fdpow1m, fdpow2m;
  float xline[2], yline[2];

  static float *pff = NULL, *pfdpow1 = NULL, *pfdpow2 = NULL;

  static bool binit = true;


  if (binit)
    {
      /* init hplot and open ps-file
       */
      cts_vinit_hplot (0);
      cts_vkuopen (&ilun, pfilenam[cttyp], "UNKNOWN");

      cts_igmeta (-ilun, -111);
      cts_hplsiz (20., 20., ' ');
      cts_hplzon (1, 2, 1, ' ');


      /* remember passed values and allocate memory for plots
       */
      inp_bak = inp;
      inf_bak = inf;
      pff = cts_mrealloc (pff, float, inf, CFUNCNAME);
      pfdpow1 = cts_mrealloc (pfdpow1, float, inf, CFUNCNAME);
      pfdpow2 = cts_mrealloc (pfdpow2, float, inf, CFUNCNAME);

      binit = false;
    }


  /* set arrays for power spectrum plot (also derive maxima and mean values)
   */
  if (inf <= inf_bak)
    {
      inf_used = 0;
      fx_min = (float) pf[0];
      fx_max = (float) dfmax;

      fdpow1_max = fdpow2_max = fdpow1m = fdpow2m = 0.f;

      for (i = 0; i < inf; i++)
	{
	  pff[i] = (float) pf[i];
	  pfdpow1[i] = (float) mdiv (ppow[i], sqrt (ppowv[i]));
	  pfdpow2[i] = (float) mdiv (ppowst[i], sqrt (ppowv[i]));


	  if (pf[i] <= dfmax )
	    {
	      /* frequency is in the allowed range
	       */
	      inf_used++;
	      fdpow1m += pfdpow1[i];
	      fdpow2m += pfdpow2[i];
	      fdpow1_max = max (fdpow1_max, pfdpow1[i]);
	      fdpow2_max = max (fdpow2_max, pfdpow2[i]);
	    }
	}

      fdpow1m /= (float) inf_used;
      fdpow2m /= (float) inf_used;
    }

  else
    cts_merror ("%s: array index (spectrum) exceeded limit", CFUNCNAME);


  /*
   * produce plots
   */
  /* first frequency vs. power/sd(power)
   */
  iplot++;
  fx_min -= 0.1f * fx_max;
  fx_max += 0.1f * fx_max;

  cts_igset ("TXCI", 1);
  cts_igset ("PLCI", 4);
  cts_hplfra (fx_min, fx_max, 0.f, 1.5f * fdpow1_max, " ");
  cts_hpltit (";frequency;pow / sd (pow)");
  cts_igraph (inf_used, pff, pfdpow1, "L*");


  /* text
   */
  xline[0] = fx_min;
  xline[1] = fx_max;
  yline[0] = fdpow1m;
  yline[1] = fdpow1m;

  cts_igset ("PLCI", 4);
  cts_igset ("LTYP", 2);
  cts_hpline (xline, yline, 2, ' ');
  cts_igset ("LTYP", 1);
  cts_igset ("PLCI", 0);
  cts_igset ("TXCI", 4);

  cts_iselnt (0);
  pstr = (ieflag == 0) ? "(sample variance, " : "(ind. data errors, ";
  pstr1 = cts_preal_to_str (dmsa, 2, "msa: %.2f, ");
  pstr2 = cts_preal_to_str (dsdev, 2, "sdev: %.2f, ");
  pstr3 = cts_preal_to_str (dsrate, 2, "srate: %.2f)");

  psubtit = cts_pstrcat (4, pstr, pstr1, pstr2, pstr3);
  cts_itx (0.2f, 0.92f, psubtit);

  cts_igset ("CHHE", 0.03f);
  cts_itx (0.17f, 0.95f, ptit[cttyp]);
  cts_igset ("CHHE", 0.02f);

  /* some extra text if binning is varied
   */
  if (cttyp == CT_T)
    {
      pstr = cts_preal_to_str (dbinset, 2, "binset: %.2f");
      cts_itx (0.2f, 0.85f, pstr);
    }


  /* next frequency vs. sd(power)/sd_theory(power)
   */
  cts_igset ("TXCI", 1);
  cts_igset ("PLCI", 4);
  cts_hplfra (fx_min, fx_max, 0.f, 1.5f * fdpow2_max, " ");
  cts_hpltit (";frequency;sd (pow) / sd?theory! (pow)");
  cts_igraph (inf_used, pff, pfdpow2, "L*");

  xline[0] = fx_min;
  xline[1] = fx_max;
  yline[0] = fdpow2m;
  yline[1] = fdpow2m;

  cts_igset ("PLCI", 4);
  cts_igset ("LTYP", 2);
  cts_hpline (xline, yline, 2, ' ');
  cts_igset ("LTYP", 1);
  cts_igset ("PLCI", 0);
  cts_igset ("TXCI", 4);

  /* some extra text if binning is varied
   */
  cts_iselnt (0);
  if (cttyp == CT_T)
    {
      pstr = cts_preal_to_str (dbinset, 2, "binset: %.2f");
      cts_itx (0.2f, 0.41f, pstr);
    }



  /* last plot - close higz window and reset binit
   */
  if (iplot == inp_bak)
    {
      cts_igmeta (999, 0);
      cts_hplend ();
      cts_vkuclos (ilun, " ");

      inf_bak = inp_bak = iplot = 0;
      binit = true;
    }

}

#undef CFUNCNAME


#define CFUNCNAME "vplot_all"

void vplot_all (double dfmax)
{
  int i, j, k, ifs, isims, inf, ipbins, ieflg, isteps;
  int *pnr_powbin = NULL;

  double dsdev, dsrate, dmsa, dpmax, dfmax_mle, dperiod, dstime, damp;
  double dmfs, dmfvs, dma, dbinset, dfmaxp;
  double *pf = NULL, *ppow = NULL, *pmct = NULL, *ppowv = NULL, *pmctv = NULL;
  double *pvarff = NULL, *ppowst = NULL;

  char *ppdf_filenam [] = {cPDF_SIM_DAT, cPDF_BOOT_DAT, cPDF_GAUSS_DAT};
  char *pstr, *ptitle;

  bool bfinal;

  MLE_STRUCT mle;

  FILE_TYPE file, powfile, mlefile;


  /* check pdf files and plot content
   */
  for (i = 0; i < MAX_NHYP_TYPE; i++)
    {
      file.pname = ppdf_filenam[i];
      file.pfile = NULL;
      file.acm = "r";

      if (bopen_dat_file (&file))
	{
	  /* read in file content 
	   */
	  ifs = fscanf (file.pfile, cPDF_READ1, &dsdev, &dsrate, &dmsa, &ieflg);
	  pstr = cts_pint_to_str (1, "line %d");
	  cts_mcheck_ifs (ifs, iPDF_ARG1, pstr);

	  ifs = fscanf (file.pfile, cPDF_READ2, &isims, &inf, &dpmax, &ipbins);
	  pstr = cts_pint_to_str (2, "line %d");
	  cts_mcheck_ifs (ifs, iPDF_ARG2, pstr);

	  pf = cts_mrealloc (pf, double, inf, CFUNCNAME);
	  pnr_powbin = cts_mrealloc (pnr_powbin, int, inf * ipbins, CFUNCNAME);


	  for(k = 0; k < ipbins * inf; k++)
	    {
	      ifs = fscanf (file.pfile, cPDF_READ3, &pf[k%inf], &pnr_powbin[k]);
	      pstr = cts_pint_to_str (k+4, "line %d");
	      cts_mcheck_ifs (ifs, iPDF_ARG3, pstr);
	    }

	  cts_vclose_file (&file);


	  /* create plot
	   */
	  vplot_pdf (pf, pnr_powbin, dpmax, ipbins, inf, isims, (NHYP) i,
		     dsrate, dmsa, dsdev, ieflg);
	}
    }



  /* power spectrum plots next
   */
  file.pname = cPOW_DAT;
  file.pfile = NULL;
  file.acm = "r";

  if (bopen_dat_file (&file))
    {
      /* read in file content 
       */
      ptitle = pread_title (file);

      ifs = fscanf (file.pfile, cPOW_READ1, &dfmax_mle, &inf, &ieflg);
      pstr = cts_pint_to_str (2, "line %d");
      cts_mcheck_ifs (ifs, iPOW_ARG1, pstr);

      fscanf (file.pfile, cPOW_HEAD1);

      ifs = fscanf (file.pfile, cPOW_READ2a, &mle.a, &mle.ae, &mle.s, &mle.se);
      ifs += fscanf (file.pfile, cPOW_READ2b, &mle.c, &mle.ce, &mle.l);
      pstr = cts_pint_to_str (5, "line %d");
      cts_mcheck_ifs (ifs, iPOW_ARG2, pstr);

      fscanf (file.pfile, cPOW_HEAD2);


      /* allocate memory for arrays and fill them
       */
      pf = cts_mrealloc (pf, double, inf, CFUNCNAME);
      ppow = cts_mrealloc (ppow, double, inf, CFUNCNAME);

      for (i = 0; i < inf; i++)
	{
	  ifs = fscanf (file.pfile, cPOW_READ3, &pf[i], &ppow[i]);
	  pstr = cts_pint_to_str (10 + i, "line %d");
	  cts_mcheck_ifs (ifs, iPOW_ARG3, pstr);
	}

      /* set maximum frequency passed to function
       */
      dfmaxp = (dfmax < dEPSILON) ? pf[inf-1] : dfmax;


      /* create plot
       */
      vplot_powspec (pf, ppow, inf, mle, dfmaxp, dfmax_mle, ptitle, ieflg);
    }



  /* plot power spectrum from sinusoidal lightcurve
   */
  file.pname = cSIN_DAT;
  file.pfile = NULL;
  file.acm = "r";

  if (bopen_dat_file (&file))
    {
      /* read in file content 
       */
      ifs = fscanf (file.pfile, cSIN_READ1, &dperiod, &dstime, &damp, &inf,
		    &ieflg);
      pstr = cts_pint_to_str (1, "line %d");
      cts_mcheck_ifs (ifs, iSIN_ARG1, pstr);

      fscanf (file.pfile, cSIN_HEAD);

      /* allocate memory for arrays and fill them
       */
      pf = cts_mrealloc (pf, double, inf, CFUNCNAME);
      ppow = cts_mrealloc (ppow, double, inf, CFUNCNAME);

      for (i = 0; i < inf; i++)
	{
	  ifs = fscanf (file.pfile, cSIN_READ2, &pf[i], &ppow[i]);
	  pstr = cts_pint_to_str (7 + i, "line %d");
	  cts_mcheck_ifs (ifs, iSIN_ARG2, pstr);
	}

      /* set maximum frequency passed to function
       */
      dfmaxp = (dfmax < dEPSILON) ? pf[inf-1] : dfmax;


      /* create plot
       */
      vplot_sinpow (pf, ppow, inf, dfmaxp, dperiod, dstime, damp, ieflg);
    }



  /* plot results from vsim_a_var
   */
  file.pname = cAVAR_DAT;
  file.pfile = NULL;
  file.acm = "r";

  powfile.pname = cAVAR_POW_DAT;
  powfile.pfile = NULL;
  powfile.acm = "r";

  mlefile.pname = cAVAR_MLE_DAT;
  mlefile.pfile = NULL;
  mlefile.acm = "r";


  if (bopen_dat_file (&file) && bopen_dat_file (&powfile)
      && bopen_dat_file (&mlefile))
    {
      /* read in file contents
       */
      ifs = fscanf (file.pfile, cAVAR_READ1, &dsdev, &dsrate, &isims,
		    &dfmax_mle, &ieflg, &inf, &isteps);
      pstr = cts_pint_to_str (1, cts_pstrcat (2, cAVAR_DAT, ": line %d"));
      cts_mcheck_ifs (ifs, iAVAR_ARG1, pstr);

      fscanf (file.pfile, cAVAR_HEAD);

      /* allocate memory for power spectrum arrays
       */
      pf = cts_mrealloc (pf, double, inf, CFUNCNAME);
      ppow = cts_mrealloc (ppow, double, inf, CFUNCNAME);


      /* now loop over files and produce plots
       */
      for (i = 0; i < isteps; i++)
	{
	  ifs = fscanf (file.pfile, cAVAR_READ3, &dmsa, &dmfs, &dmfvs, &dma,
			&mle.a, &mle.s, &mle.c, &mle.l);
	  pstr = cts_pstrcat (2, cAVAR_DAT, ": line %d");
	  pstr = cts_pint_to_str (i + 5, pstr);
	  cts_mcheck_ifs (ifs, iAVAR_ARG3, pstr);

	  /* read mle errors from other file
	   */
	  ifs = fscanf (mlefile.pfile, cMLE_READ, &mle.a, &mle.ae, &mle.s,
			&mle.se, &mle.c, &mle.ce, &mle.l);
	  pstr = cts_pstrcat (2, cAVAR_MLE_DAT, ": line %d");
	  pstr = cts_pint_to_str (i + 1, pstr);
	  cts_mcheck_ifs (ifs, iMLE_ARG, pstr);


	  /* read power spectrum
	   */
	  for (j = 0; j < inf; j++)
	    {
	      ifs = fscanf (powfile.pfile, cAVAR_READ2, &pf[j], &ppow[j]);
	      pstr = cts_pstrcat (2, cAVAR_POW_DAT, ": line %d");
	      pstr = cts_pint_to_str (i * inf + j, pstr);
	      cts_mcheck_ifs (ifs, iAVAR_ARG2, pstr);
	    }

	  /* set maximum frequency passed to function
	   */
	  dfmaxp = (dfmax < dEPSILON) ? pf[inf-1] : dfmax;


	  /* bfinal triggers final plot
	   */
	  bfinal = (i + 1 < isteps) ? false : true;

	  vplot_sim (bfinal, isteps, dsdev, dsrate, dmsa, isims, dfmaxp,
		     dfmax_mle, dmfs, dmfvs, dma, mle, pf, ppow, inf, ieflg,
		     SIM_A);

	}

    }      /* END if (bopen_dat_file ...) */


  /* plot results from vsim_s_var
   */
  file.pname = cSVAR_DAT;
  file.pfile = NULL;
  file.acm = "r";

  powfile.pname = cSVAR_POW_DAT;
  powfile.pfile = NULL;
  powfile.acm = "r";

  mlefile.pname = cSVAR_MLE_DAT;
  mlefile.pfile = NULL;
  mlefile.acm = "r";


  if (bopen_dat_file (&file) && bopen_dat_file (&powfile)
      && bopen_dat_file (&mlefile))
    {
      /* read in file contents
       */
      ifs = fscanf (file.pfile, cSVAR_READ1, &dmsa, &dsrate, &isims,
		    &dfmax_mle, &ieflg, &inf, &isteps);
      pstr = cts_pint_to_str (1, cts_pstrcat (2, cSVAR_DAT, ": line %d"));
      cts_mcheck_ifs (ifs, iSVAR_ARG1, pstr);

      fscanf (file.pfile, cSVAR_HEAD);

      /* allocate memory for power spectrum arrays
       */
      pf = cts_mrealloc (pf, double, inf, CFUNCNAME);
      ppow = cts_mrealloc (ppow, double, inf, CFUNCNAME);


      /* now loop over files and produce plots
       */
      for (i = 0; i < isteps; i++)
	{
	  ifs = fscanf (file.pfile, cSVAR_READ3, &dsdev, &dmfs, &dmfvs, &dma,
			&mle.a, &mle.s, &mle.c, &mle.l);
	  pstr = cts_pstrcat (2, cSVAR_DAT, ": line %d");
	  pstr = cts_pint_to_str (i + 5, pstr);
	  cts_mcheck_ifs (ifs, iSVAR_ARG3, pstr);

	  /* read mle errors from other file
	   */
	  ifs = fscanf (mlefile.pfile, cMLE_READ, &mle.a, &mle.ae, &mle.s,
			&mle.se, &mle.c, &mle.ce, &mle.l);
	  pstr = cts_pstrcat (2, cSVAR_MLE_DAT, ": line %d");
	  pstr = cts_pint_to_str (i + 1, pstr);
	  cts_mcheck_ifs (ifs, iMLE_ARG, pstr);


	  /* read power spectrum
	   */
	  for (j = 0; j < inf; j++)
	    {
	      ifs = fscanf (powfile.pfile, cSVAR_READ2, &pf[j], &ppow[j]);
	      pstr = cts_pstrcat (2, cSVAR_POW_DAT, ": line %d");
	      pstr = cts_pint_to_str (i * inf + j, pstr);
	      cts_mcheck_ifs (ifs, iSVAR_ARG2, pstr);
	    }

	  /* set maximum frequency passed to function
	   */
	  dfmaxp = (dfmax < dEPSILON) ? pf[inf-1] : dfmax;


	  /* bfinal triggers final plot
	   */
	  bfinal = (i + 1 < isteps) ? false : true;

	  vplot_sim (bfinal, isteps, dsdev, dsrate, dmsa, isims, dfmaxp,
		     dfmax_mle, dmfs, dmfvs, dma, mle, pf, ppow, inf, ieflg,
		     SIM_S);

	}

    }       /* END if (bopen_dat_file ...) */


  /* plot results from vsim_r_var
   */
  file.pname = cRVAR_DAT;
  file.pfile = NULL;
  file.acm = "r";

  powfile.pname = cRVAR_POW_DAT;
  powfile.pfile = NULL;
  powfile.acm = "r";

  mlefile.pname = cRVAR_MLE_DAT;
  mlefile.pfile = NULL;
  mlefile.acm = "r";


  if (bopen_dat_file (&file) && bopen_dat_file (&powfile)
      && bopen_dat_file (&mlefile))
    {
      /* read in file contents
       */
      ifs = fscanf (file.pfile, cRVAR_READ1, &dmsa, &dsdev, &isims,
		    &dfmax_mle, &ieflg, &inf, &isteps);
      pstr = cts_pint_to_str (1, cts_pstrcat (2, cRVAR_DAT, ": line %d"));
      cts_mcheck_ifs (ifs, iRVAR_ARG1, pstr);

      fscanf (file.pfile, cRVAR_HEAD);

      /* allocate memory for power spectrum arrays
       */
      pf = cts_mrealloc (pf, double, inf, CFUNCNAME);
      ppow = cts_mrealloc (ppow, double, inf, CFUNCNAME);


      /* now loop over files and produce plots
       */
      for (i = 0; i < isteps; i++)
	{
	  ifs = fscanf (file.pfile, cRVAR_READ3, &dsrate, &dmfs, &dmfvs, &dma,
			&mle.a, &mle.s, &mle.c, &mle.l);
	  pstr = cts_pstrcat (2, cRVAR_DAT, ": line %d");
	  pstr = cts_pint_to_str (i + 5, pstr);
	  cts_mcheck_ifs (ifs, iRVAR_ARG3, pstr);

	  /* read mle errors from other file
	   */
	  ifs = fscanf (mlefile.pfile, cMLE_READ, &mle.a, &mle.ae, &mle.s,
			&mle.se, &mle.c, &mle.ce, &mle.l);
	  pstr = cts_pstrcat (2, cRVAR_MLE_DAT, ": line %d");
	  pstr = cts_pint_to_str (i + 1, pstr);
	  cts_mcheck_ifs (ifs, iMLE_ARG, pstr);


	  /* read power spectrum
	   */
	  for (j = 0; j < inf; j++)
	    {
	      ifs = fscanf (powfile.pfile, cRVAR_READ2, &pf[j], &ppow[j]);
	      pstr = cts_pstrcat (2, cRVAR_POW_DAT, ": line %d");
	      pstr = cts_pint_to_str (i * inf + j, pstr);
	      cts_mcheck_ifs (ifs, iRVAR_ARG2, pstr);
	    }

	  /* set maximum frequency passed to function
	   */
	  dfmaxp = (dfmax < dEPSILON) ? pf[inf-1] : dfmax;


	  /* bfinal triggers final plot
	   */
	  bfinal = (i + 1 < isteps) ? false : true;

	  vplot_sim (bfinal, isteps, dsdev, dsrate, dmsa, isims, dfmaxp,
		     dfmax_mle, dmfs, dmfvs, dma, mle, pf, ppow, inf, ieflg,
		     SIM_R);

	}

    }   /* END if (bopen_dat_file ...) */



  /* produce plots from cross-term simulations (variable shot rate)
   */
  file.pname = cCTR_DAT;
  file.pfile = NULL;
  file.acm = "r";


  if (bopen_dat_file (&file))
    {
      /* read in file contents
       */
      ifs = fscanf (file.pfile, cCTR_READ1, &dmsa, &dsdev, &isims,
		    &ieflg, &inf, &isteps);
      pstr = cts_pint_to_str (1, cts_pstrcat (2, cCTR_DAT, ": line %d"));
      cts_mcheck_ifs (ifs, iCTR_ARG1, pstr);

      fscanf (file.pfile, cCTR_HEAD);

      /* allocate memory for arrays (some of them are currently not used
       * in the plot)
       */
      pf = cts_mrealloc (pf, double, inf, CFUNCNAME);
      ppow = cts_mrealloc (ppow, double, inf, CFUNCNAME);
      pmct = cts_mrealloc (pmct, double, inf, CFUNCNAME);
      ppowv = cts_mrealloc (ppowv, double, inf, CFUNCNAME);
      pmctv = cts_mrealloc (pmctv, double, inf, CFUNCNAME);
      pvarff = cts_mrealloc (pvarff, double, inf, CFUNCNAME);
      ppowst = cts_mrealloc (ppowst, double, inf, CFUNCNAME);


      /* now loop over files and produce plots
       */
      for (i = 0; i < isteps; i++)
	{
	  for (j = 0; j < inf; j++)
	    {
	      ifs = fscanf (file.pfile, cCTR_READ2, &dsrate, &pf[j], &ppow[j],
			    &ppowv[j], &pmct[j], &pmctv[j], &pvarff[j],
			    &ppowst[j]);
	      pstr = cts_pstrcat (2, cCTR_DAT, ": line %d");
	      pstr = cts_pint_to_str (i * inf + j + 4, pstr);
	      cts_mcheck_ifs (ifs, iCTR_ARG2, pstr);
	    }

	  /* set maximum frequency passed to function
	   */
	  dfmaxp = (dfmax < dEPSILON) ? pf[inf-1] : dfmax;

	  vplot_ct (pf, ppow, ppowv, ppowst, inf, dsrate, dmsa, dsdev, ieflg,
		    0., CT_R, isteps, dfmaxp);
	}

    }   /* END if (bopen_dat_file ...) */


  /* produce plots from cross-term simulations (variable obs. times)
   */
  file.pname = cCTT_DAT;
  file.pfile = NULL;
  file.acm = "r";


  if (bopen_dat_file (&file))
    {
      /* read in file contents
       */
      ifs = fscanf (file.pfile, cCTT_READ1, &dsrate, &dmsa, &dsdev, &isims,
		    &ieflg, &inf, &isteps);
      pstr = cts_pint_to_str (1, cts_pstrcat (2, cCTT_DAT, ": line %d"));
      cts_mcheck_ifs (ifs, iCTT_ARG1, pstr);

      fscanf (file.pfile, cCTT_HEAD);

      /* allocate memory for arrays (some of them are currently not used
       * in the plot)
       */
      pf = cts_mrealloc (pf, double, inf, CFUNCNAME);
      ppow = cts_mrealloc (ppow, double, inf, CFUNCNAME);
      pmct = cts_mrealloc (pmct, double, inf, CFUNCNAME);
      ppowv = cts_mrealloc (ppowv, double, inf, CFUNCNAME);
      pmctv = cts_mrealloc (pmctv, double, inf, CFUNCNAME);
      pvarff = cts_mrealloc (pvarff, double, inf, CFUNCNAME);
      ppowst = cts_mrealloc (ppowst, double, inf, CFUNCNAME);


      /* now loop over files and produce plots
       */
      for (i = 0; i < isteps; i++)
	{
	  for (j = 0; j < inf; j++)
	    {
	      ifs = fscanf (file.pfile, cCTT_READ2, &dbinset, &pf[j], &ppow[j],
			    &ppowv[j], &pmct[j], &pmctv[j], &pvarff[j],
			    &ppowst[j]);
	      pstr = cts_pstrcat (2, cCTT_DAT, ": line %d");
	      pstr = cts_pint_to_str (i * inf + j + 4, pstr);
	      cts_mcheck_ifs (ifs, iCTT_ARG2, pstr);
	    }

	  /* set maximum frequency passed to function
	   */
	  dfmaxp = (dfmax < dEPSILON) ? pf[inf-1] : dfmax;

	  vplot_ct (pf, ppow, ppowv, ppowst, inf, dsrate, dmsa, dsdev, ieflg,
		    dbinset, CT_T, isteps, dfmaxp);
	}

    }   /* END if (bopen_dat_file ...) */
}

#undef CFUNCNAME
