#include <string.h>
#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "powspec.h"
#include "nr.h"

/*
 * no local functions declared
 */



#define CFUNCNAME "vsim_lc1"

void vsim_lc1 (double dx[], double dy[], double dz[], int inp, double dsrate,
	       double damp2, double dsigma, bool bnewdat, double **dt,
	       double **da, int *pns)

  /* this function simulates a lightcurve build out of a set of
   * exponentially distributed flares (this version returns information on
   * each single flare)
   * input:
   * dx[]    - array of mjds
   * dy[]    - array of obs. fluxes
   * dz[]    - array of obs. flux errors
   * inp     - number of points in dx[], dy[] and dz[]
   * dsrate  - mean shot rate of the flares
   * damp2   - mean squared amplitude of the flares
   * dsigma  - stand. dev. of the gaussian type flares
   * bnewdat - indicates a new data sample
   * output:
   * dy[]    - flux from simulated lightcurve at time dx[]
   * dt      - times of flare maxima (for the simulation)
   * da      - amplitudes of the simulated flares
   * pns     - number of simulated flares (or shots)
   */
{
  int i, ins;
  static int ins_max = 0;

  static long lidum = lSEEDVAL1;

  static double dmjdmin, dmjdmax, dtime, dfluxamp;
  static double *ds_amp = NULL, *ds_time = NULL;

  /* linked list to keep track of amplitude and time of all
   * simulated shots; the list is transferred into two double
   * arrays after the simulation finished;
   */
  static SDLIST_EL sd_root;
  SDLIST_EL *psd_el = &sd_root;


  /* prevent negative squared amplitudes, zero shot rate and
   * zero sigma
   */
  damp2 = fabs (damp2);
  dsrate = (dsrate > dEPSILON) ? dsrate : dEPSILON;
  dsigma = (dsigma > dMIN_SIGMA ) ? dsigma : dMIN_SIGMA;


  if (bnewdat)
    {
      /* get minimum and maximum mjd (dx must be in ascending order)
       */
      dmjdmin = dx[0];
      dmjdmax = dx[inp-1];

      /* define start time for flares (2. * dsigma^2 is the time after
       * which a shot dropped to 1/e of it's maximum value)
       */
      dmjdmin -= (double) iNR_PRE_INT * 2. * m2 (dsigma);
      dmjdmax += (double) iNR_POST_INT * 2. * m2 (dsigma);

    }


  /* start simulation of flares in the time interval
   * between dmjdmin and dmjdmax
   */
  ins = 0;
  dtime = dmjdmin;
  cts_vreset_darray (dy, inp);

  while (dtime <= dmjdmax)
    {
      /* calculate time and amplitude of next flare maximum
       * (for exp. distributed amplitudes the mean amplitude squared is
       * 0.5 times the mean squared amplitude)
       */
      ins++;
      dtime -= log (ran2 (&lidum)) / dsrate;
      dfluxamp = -sqrt (0.5 * damp2) * log (ran2 (&lidum));


      /* fill struct with flare information and allocate memory
       * for next loop if necessary (dtime is already updated)
       */
      psd_el->dtime  = dtime;
      psd_el->damp   = dfluxamp;

      if (dtime <= dmjdmax)
	{
	  /* if a linked list already exists, we first reuse their elements
	   * (their number is stored in ins_max, set below) 
	   */
	  if (ins >= ins_max)
	    {
	      psd_el->p2next = cts_mmalloc (SDLIST_EL, 1, CFUNCNAME);
	      psd_el = psd_el->p2next;
	      psd_el->p2next = NULL;
	    }

	  else
	    psd_el = psd_el->p2next;
	}


      /* add the fluxes of the flare to all data-points
       */
      for (i = 0; i < inp; i++)
	dy[i] += dfluxamp * exp (-0.5 * m2 ((dx[i] - dtime) / dsigma));
    }


  /* add gaussian noise to each day; the standard deviation is taken from
   * real data
   */
/*   for (i = 0; i < inp; i++) */
/*     dy[i] += dz[i] * gasdev (&lidum); */


  /* fill shot-structs into arrays
   * (reserve more memory for the arrays, if neccessary)
   */
  if (ins > ins_max)
    {
      ds_amp = cts_mrealloc (ds_amp, double, ins, CFUNCNAME);
      ds_time = cts_mrealloc (ds_time, double, ins, CFUNCNAME);
      ins_max = ins;
    }

  psd_el = &sd_root;

  /* the check for psd_el->p2next == NULL will not work here because
   * the list contains ins_max elements where ins_max might be
   * different from ins
   */
  for (i = 0; i < ins; i++)
    {
      ds_amp[i] = psd_el->damp;
      ds_time[i] = psd_el->dtime;

      psd_el = psd_el->p2next;
    }

  /* set return values
   */
  *da = ds_amp;
  *dt = ds_time;
  *pns = ins;
}

#undef CFUNCNAME


void vsim_lc2 (double dx[], double dy[], double dz[], int inp, double dsrate,
	       double damp2, double dsigma, bool bnewdat)

  /* this function simulates a lightcurve build out of a set of
   * exponentially distributed flares (this version does'nt return the
   * shot characteristics, but is faster)
   * input:
   * dx[]    - array of mjds
   * dy[]    - array of obs. fluxes
   * dz[]    - array of obs. flux errors
   * inp     - number of points in dx[], dy[] and dz[]
   * dsrate  - mean shot rate of the flares
   * damp2   - mean squared amplitude of the flares
   * dsigma  - stand. dev. of the gaussian type flares
   * bnewdat - indicates a new data sample
   * output:
   * dy[]    - flux from simulated lightcurve at time dx[]
   */
{
  int i, ins;

  static long lidum = lSEEDVAL2;

  static double dmjdmin, dmjdmax, dtime, dfluxamp;


  /* prevent negative squared amplitudes, zero shot rate and
   * zero sigma
   */
  damp2 = fabs (damp2);
  dsrate = (dsrate > dEPSILON) ? dsrate : dEPSILON;
  dsigma = (dsigma > dMIN_SIGMA ) ? dsigma : dMIN_SIGMA;


  if (bnewdat)
    {
      /* get minimum and maximum mjd (dx must be in ascending order)
       */
      dmjdmin = dx[0];
      dmjdmax = dx[inp-1];

      /* define start time for flares (2. * dsigma^2 is the time after
       * which a shot dropped to 1/e of it's maximum value)
       */
      dmjdmin -= (double) iNR_PRE_INT * 2. * m2 (dsigma);
      dmjdmax += (double) iNR_POST_INT * 2. * m2 (dsigma);
    }


  /* start simulation of flares in the time interval
   * between dmjdmin and dmjdmax
   */
  ins = 0;
  dtime = dmjdmin;
  cts_vreset_darray (dy, inp);

  while (dtime <= dmjdmax)
    {
      /* calculate time and amplitude of next flare maximum
       * (for exp. distributed amplitudes the mean amplitude squared is
       * 0.5 times the mean squared amplitude)
       */
      ins++;
      dtime -= log (ran2 (&lidum)) / dsrate;
      dfluxamp = -sqrt (0.5 * damp2) * log (ran2 (&lidum));


      /* add the fluxes of the flare to all data-points
       */
      for (i = 0; i < inp; i++)
	dy[i] += dfluxamp * exp (-0.5 * m2 ((dx[i] - dtime) / dsigma));
    }


  /* add gaussian noise to each day; the standard deviation is taken from
   * real data
   */
/*   for (i = 0; i < inp; i++) */
/*     dy[i] += dz[i] * gasdev (&lidum); */
}


#define CFUNCNAME "vsim_lc3"

void vsim_lc3 (double dx[], double dy[], double dz[], int inp, double dsrate,
	       double damp2, double dsigma, bool bnewdat)

  /* this function simulates a lightcurve by applying the bootstrap method
   * to the observed fluxes
   * input:
   * dx[]    - array of mjds
   * dy[]    - array of obs. fluxes
   * dz[]    - array of obs. flux errors
   * inp     - number of points in dx[], dy[] and dz[]
   * dsrate  - not used here
   * damp2   - not used here
   * dsigma  - not used here
   * bnewdat - indicates a new data sample
   * output:
   * dy[]    - flux from simulated lightcurve at time dx[]
   */
{
  int i;

  static long lidum = lSEEDVAL3;

  static double *pflux_orig = NULL;


  if (bnewdat)
    {
      pflux_orig = cts_mrealloc (pflux_orig, double, inp, CFUNCNAME);

      memcpy (pflux_orig, dx, (size_t) inp * sizeof (double));
    }


  /* perform bootstrap (fill dy with randomly choosen flux values)
   */
  for (i = 0; i < inp; i++)
    dy[i] = pflux_orig [(int) (ran2 (&lidum) * (double) inp)];
}

#undef CFUNCNAME


void vsim_lc4 (double dx[], double dy[], double dz[], int inp, double dsrate,
	       double damp2, double dsigma, bool bnewdat)

  /* this function simulates a lightcurve of gaussian random noise
   * input:
   * dx[]    - array of mjds
   * dy[]    - array of obs. fluxes
   * dz[]    - array of obs. flux errors
   * inp     - number of points in dx[], dy[] and dz[]
   * dsrate  - not used here
   * damp2   - multiplication factor
   * dsigma  - not used here
   * bnewdat - not used here
   * output:
   * dy[]    - flux from simulated lightcurve at time dx[]
   */
{
  int i;

  static long lidum = lSEEDVAL4;

  /* fill dy with gaussian random noise
   */
  for (i = 0; i < inp; i++)
    dy[i] = damp2 * gasdev (&lidum);
}


void vsim_lc5 (double dx[], double dy[], double dz[], int inp, double dperiod,
	       double dto, double damp)

  /* this function simulates a sinusoid lightcurve of given period
   * input:
   * dx[]    - array of mjds
   * dy[]    - array of obs. fluxes
   * dz[]    - array of obs. flux errors
   * inp     - number of points in dx[], dy[] and dz[]
   * dperiod - period of sinusoid
   * dto     - time origin
   * damp    - amplitude
   * output:
   * dy[]    - flux from simulated lightcurve at time dx[]
   */
{
  int i;

  /* fill dy with sinusoidal
   */
  for (i = 0; i < inp; i++)
    dy[i] = damp * sin (d2PI * (dx[i] - dto) / dperiod);
}


#define CFUNCNAME "vsim_no_var"

void vsim_no_var (double **p2al, int inp, int ieflag, int isims, int iofac,
		  int inf, double dsrate, double dmsa, double dsdev,
		  double dfmax, double dfmax_mle, int *pxfi, int inxf)

  /* this function runs several simulations where all the shot noise
   * parameters amplitude, sdev and lamda are kept fixed. From the
   * resulting power spectra I derive the mean shot noise parameters
   * and their variance and save them. The mean power spectrum is also
   * derived
   * input:
   * p2al      - array of 3 arrays (mjd, flux, flux error)
   * inp       - number of points in p2al arrays
   * ieflag    - take sample variance if 0, else use individual data errors
   *             (used for normalizing power spectrum)
   * isims     - mumber of simulations
   * iofac     - oversampling factor
   * inf       - number of independent frequencies
   * dsrate    - mean shot rate of the flares
   * dmsa      - mean squared amplitude of the flares
   * dsdev     - stand. dev. of the gaussian type flares
   * dfmax     - max. frequency used for power spectrum
   * dfmax_mle - max. frequency used for the mle-fit
   * pxfi      - integer array containing the index of all frequencies which
   *             are not used for the mle fit (in addition to dfmax_mle)
   *             the integers in pxfi are expected to be in ascending order!
   * inxf      - number of excluded frequencies (in addition to dfmax_mle)
   * output:
   * p2al[1]   - flux from (last) simulated lightcurve
   */
{
  int i, j;

  /* vars to store mean and stand. dev. of mle-parameters
   */
  double dsmean, dlmean, damean, dcmean;
  double dssdev, dlsdev, dasdev, dcsdev;

  /* pf    - array of test frequencies
   * ppow  - array of power spectrum
   * ppows - array of power spectrum variance
   * ppowm - array of mean power spectrum
   * ppowx - array of power spectrum (frequencies not used for mle-fit excluded)
   * pw2x  - array of angluar frequencies ( -     "      -)
   */
  double *pf, *ppow, *ppows, *ppowm, *ppowx, *pw2x;
  double *dx, *dy, *dz, *dz2;

  bool bnewdat, bfinal;

  MLE_STRUCT mle;

  static FILE_TYPE dat_file, mle_file;


  /* init array pointers (set dz2 to NULL, if normalization
   * on sample variance is wanted, else dz = dz2)
   */
  dx = p2al[0];
  dy = p2al[1];
  dz = p2al[2];
  dz2 = (ieflag == 0) ? NULL : dz;


  /* open file to store simulation results
   */
  dat_file.pname = cNVAR_DAT;
  dat_file.pfile = NULL;
  dat_file.acm = "w";

  cts_vopen_file (&dat_file);


  /* open file to store mle results
   */
  mle_file.pname = cNVAR_MLE_DAT;
  mle_file.pfile = NULL;
  mle_file.acm = "w";

  cts_vopen_file (&mle_file);


  /* allocate memory for mean and sdev of power values
   * and reset arrays to '0'
   */
  ppowm = cts_mmalloc (double, inf, CFUNCNAME);
  ppows = cts_mmalloc (double, inf, CFUNCNAME);

  cts_vreset_darray (ppowm, inf);
  cts_vreset_darray (ppows, inf);


  /* start the simulations
   * (bnewdat is modified within cts_ls_power)
   */
  bnewdat = true;
  dsmean = dlmean = damean = dcmean = 0.;
  dssdev = dlsdev = dasdev = dcsdev = 0.;

  for (j = 0; j < isims; j++)
    {
      /* simulate lightcurve and store it in dy
       */
      vsim_lc2 (dx, dy, dz, inp, dsrate, dmsa, dsdev, bnewdat);

      /* derive power spectrum in ppow and frequencies in pf
       */
      cts_ls_power (dx, dy, dz2, inp, iofac, dfmax, &bnewdat, &pf, &ppow, &inf);


      /* get mle-estimators and write them to mle_file
       */
      mle = get_mle_g (inf, pf, ppow, dfmax_mle, pxfi, inxf, mle_file, &pw2x,
		       &ppowx);


      /* produce plot; bfinal triggers final plot
       */
      bfinal = (j + 1 < isims) ? false : true;

      vplot_sim_no (bfinal, pf, ppow, inf, dmsa, dsrate, dsdev, mle, dfmax,
		    dfmax_mle, ieflag);


      /* update arrays for mean and standard deviation of power values
       */
      for (i = 0; i < inf; i++)
	{
	  ppowm[i] += ppow[i];
	  ppows[i] += m2 (ppow[i]);
	}


      /* update arrays for mean and standard deviation of shot noise
       * parameters
       */
      damean += mle.a;
      dsmean += mle.s;
      dcmean += mle.c;
      dlmean += mle.l;

      dasdev += m2 (mle.a);
      dssdev += m2 (mle.s);
      dcsdev += m2 (mle.c);
      dlsdev += m2 (mle.l);
    }


  /* calculate and write results (shot noise parameters) to data file
   */
  fprintf (dat_file.pfile, cNVAR_WRITE1, dmsa, dsdev, dsrate, isims, dfmax_mle,
	   ieflag, inf);

  damean /= (double) isims;
  dsmean /= (double) isims;
  dcmean /= (double) isims;
  dlmean /= (double) isims;

  dasdev = dasdev / (double) isims - m2 (damean);
  dssdev = dssdev / (double) isims - m2 (dsmean);
  dcsdev = dcsdev / (double) isims - m2 (dcmean);
  dlsdev = dlsdev / (double) isims - m2 (dlmean);


  fprintf (dat_file.pfile, cNVAR_WRITE2, damean, dasdev);
  fprintf (dat_file.pfile, cNVAR_WRITE3, dsmean, dssdev);
  fprintf (dat_file.pfile, cNVAR_WRITE4, dcmean, dcsdev);
  fprintf (dat_file.pfile, cNVAR_WRITE5, dlmean, dlsdev);


  /* calculate and write results (mean power spectrum) to data file
   */
  for (i = 0; i < inf; i++)
    {
      ppowm[i] = ppowm[i] / isims;
      ppows[i] = ppows[i] / isims - m2 (ppowm[i]);
    }

  mle = get_mle_g (inf, pf, ppowm, dfmax_mle, pxfi, inxf, mle_file, &pw2x,
		   &ppowx);

  fprintf (dat_file.pfile, cNVAR_WRITE6, mle.a, mle.s, mle.c, mle.l);
  fprintf (dat_file.pfile, cNVAR_HEAD);

  for (i = 0; i < inf; i++)
    fprintf (dat_file.pfile, cNVAR_WRITE7, pf[i], ppowm[i], sqrt (ppows[i]));


  cts_vclose_file (&dat_file);
  cts_vclose_file (&mle_file);
}

#undef CFUNCNAME


#define CFUNCNAME "vsim_a_var"

void vsim_a_var (double **p2al, int inp, int ieflag, int isims, int iofac,
		 double dsrate, double dmsa_min, double dmsa_max,
		 double dmsa_ss, double dsdev, double dfmax, double dfmax_mle,
		 int *pxfi, int inxf)

  /* this function runs several simulations where all the shot noise
   * parameters except the amplitude are kept fixed. From the
   * resulting mean power spectra I derive the shot noise parameters
   * and save them. This function also checks the mean flare amplitude
   * (according to the theoretical expectations)
   * input:
   * p2al      - array of 3 arrays (mjd, flux, flux error)
   * inp       - number of points in p2al arrays
   * ieflag    - take sample variance if 0, else use individual data errors
   *             (used for normalizing power spectrum)
   * isims     - mumber of simulations for a given dmsa
   * iofac     - oversampling factor
   * dsrate    - mean shot rate of the flares
   * dmsa_min  - mean squared amplitude of the flares - minimum
   * dmsa_max  - mean squared amplitude of the flares - maximum
   * dmsa_ss   - mean squared amplitude of the flares - step size
   * dsdev     - stand. dev. of the gaussian type flares
   * dfmax     - max. frequency used for power spectrum
   * dfmax_mle - max. frequency used for the mle-fit
   * pxfi      - integer array containing the index of all frequencies which
   *             are not used for the mle fit (in addition to dfmax_mle)
   *             the integers in pxfi are expected to be in ascending order!
   * inxf      - number of excluded frequencies (in addition to dfmax_mle)
   * output:
   * p2al[1]   - flux from (last) simulated lightcurve
   */
{
  int i, j, k, inf, ins, isteps;

  /* dmf     - mean flux for a given lightcurve
   * dmfs    - mean flux for a set of dmf values
   * dmfv    - flux variance for a given lightcurve
   * dmfvs   - mean flux variance for a set of dmfv values
   * dmsa    - mean squared amplitude
   * dma     - theoretical estimation of mle-parameter 'a' (amplitude)
   * dwmin   - minimum angluar frequency in power-spectrum
   * dtrange - time range covered by the data
   * dmnsf   - mean number of simulated flares
   */
  double dmf, dmfs, dmfv, dmfvs, dmsa, dma, dwmin, dtrange, dmnsf;

  /* pf    - array of test frequencies
   * ppow  - array of power spectrum
   * ppowm - array of mean power spectrum
   * ppowx - array of power spectrum (frequencies not used for mle-fit excluded)
   * pw2x  - array of angluar frequencies ( -     "      -)
   * dt    - array containing the time for each flare maximum
   * da    - array containing the amplitude for each flare maximum
   */
  double *pf, *ppow, *ppowm, *ppowx, *pw2x, *dt, *da;
  double *dx, *dy, *dz, *dz2;

  bool bnewdat, bfinal;

  MLE_STRUCT mle;

  static FILE_TYPE dat_file, mle_file, powspec_file;


  /* init array pointers (set dz2 to NULL, if normalization
   * on sample variance is wanted, else dz = dz2)
   */
  dx = p2al[0];
  dy = p2al[1];
  dz = p2al[2];
  dz2 = (ieflag == 0) ? NULL : dz;


  /* allocate memory for mean power values (the call to cts_vgetfreq is also
   * used to get the time range of the data)
   */
  cts_vgetfreq (dx, inp, iofac, &dfmax, &dwmin, &inf);

  dtrange = d2PI / (dwmin * (double) iofac);
  ppowm = cts_mmalloc (double, inf, CFUNCNAME);


  /* open file to store simulation results
   */
  dat_file.pname = cAVAR_DAT;
  dat_file.pfile = NULL;
  dat_file.acm = "w";

  cts_vopen_file (&dat_file);

  isteps = 1 + (int) (0.5 + mdiv (dmsa_max - dmsa_min, dmsa_ss));
  fprintf (dat_file.pfile, cAVAR_WRITE1, dsdev, dsrate, isims, dfmax_mle,
	   ieflag, inf, isteps);
  fprintf (dat_file.pfile, cAVAR_HEAD);


  /* open file to store mle results for all simulations
   */
  mle_file.pname = cAVAR_MLE_DAT;
  mle_file.pfile = NULL;
  mle_file.acm = "w";

  cts_vopen_file (&mle_file);


  /* open file to store power spectra for each set of simulations
   */
  powspec_file.pname = cAVAR_POW_DAT;
  powspec_file.pfile = NULL;
  powspec_file.acm = "w";

  cts_vopen_file (&powspec_file);


  /* loop over all amplitude values
   * (bnewdat is modified within ls_power)
   */
  bnewdat = true;
  dmsa = dmsa_min - dmsa_ss;

  for (i = 0; i < isteps; i++)
    {
      dmsa += dmsa_ss;
      dmnsf = dmfs = dmfvs = 0.;
      cts_vreset_darray (ppowm, inf);


      /* start the simulations for given dmsa
       */
      for (k = 0; k < isims; k++)
	{
	  /* simulate lightcurve and store it in dy
	   */
	  vsim_lc1 (dx, dy, dz, inp, dsrate, dmsa, dsdev, bnewdat, &dt, &da,
		    &ins);

	  /* derive power spectrum in ppow and frequencies in pf
	   */
	  cts_ls_power (dx, dy, dz2, inp, iofac, dfmax, &bnewdat, &pf, &ppow,
			&inf);

	  /* derive mean and variance from the simulated fluxes
	   */
	  cts_vget_svar (dy, inp, &dmf, &dmfv);

	  dmfs += dmf;
	  dmfvs += dmfv;
	  dmnsf += (double) ins;


	  /* update array for mean power values
	   */
	  for (j = 0; j < inf; j++)
	    ppowm[j] += ppow[j];
	}


      /* simulations for one given dmsa is done, so calculate
       * results and store them
       */
      dmnsf /= (double) isims;
      dmfs /= (double) isims;
      dmfvs /= (double) isims;

      for (j = 0; j < inf; j++)
	{
	  ppowm[j] = ppowm[j] / isims;
	  fprintf (powspec_file.pfile, cAVAR_WRITE2, pf[j], ppowm[j]);
	}

      mle = get_mle_g (inf, pf, ppowm, dfmax_mle, pxfi, inxf, mle_file, &pw2x,
		       &ppowx);

      dma = (double) inp * dmnsf * d2PI * m2 (dsdev) * dmsa * 2. /
	(dmfvs * m2 (dtrange));

      fprintf (dat_file.pfile, cAVAR_WRITE3, dmsa, dmfs, dmfvs, dma, mle.a,
	       mle.s, mle.c, mle.l);


      /* produce plot; bfinal triggers final plot
       */
      bfinal = (i + 1 < isteps) ? false : true;

      vplot_sim (bfinal, isteps, dsdev, dsrate, dmsa, isims, dfmax, dfmax_mle,
		 dmfs, dmfvs, dma, mle, pf, ppowm, inf, ieflag, SIM_A);

    }  /* END for (dmsa =  ... */


  cts_vclose_file (&dat_file);
  cts_vclose_file (&mle_file);
  cts_vclose_file (&powspec_file);
}

#undef CFUNCNAME


#define CFUNCNAME "vsim_s_var"

void vsim_s_var (double **p2al, int inp, int ieflag, int isims, int iofac,
		 double dsrate, double dmsa, double dsdev_min,
		 double dsdev_max, double dsdev_ss, double dfmax,
		 double dfmax_mle, int *pxfi, int inxf)

  /* this function runs several simulations where all the shot noise
   * parameters except 'sdev' are kept fixed. From the resulting mean
   * power spectra I derive the shot noise parameters and save them.
   * input:
   * p2al      - array of 3 arrays (mjd, flux, flux error)
   * inp       - number of points in p2al arrays
   * ieflag    - take sample variance if 0, else use individual data errors
   *             (used for normalizing power spectrum)
   * isims     - mumber of simulations for a given dmsa
   * iofac     - oversampling factor
   * dsrate    - mean shot rate of the flares
   * dmsa      - mean squared amplitude of the flares
   * dsdev_min - stand. dev. of the gaussian type flares - minimum
   * dsdev_max - stand. dev. of the gaussian type flares - maximum
   * dsdev_ss  - stand. dev. of the gaussian type flares - step size
   * dfmax     - max. frequency used for power spectrum
   * dfmax_mle - max. frequency used for the mle-fit
   * pxfi      - integer array containing the index of all frequencies which
   *             are not used for the mle fit (in addition to dfmax_mle)
   *             the integers in pxfi are expected to be in ascending order!
   * inxf      - number of excluded frequencies (in addition to dfmax_mle)
   * output:
   * p2al[1]   - flux from (last) simulated lightcurve
   */
{
  int i, j, k, inf, ins, isteps;

  /* dmf     - mean flux for a given lightcurve
   * dmfs    - mean flux for a set of dmf values
   * dmfv    - flux variance for a given lightcurve
   * dmfvs   - mean flux variance for a set of dmfv values
   * dsdev  - stand. dev. of the gaussian type flares
   * dma     - theoretical estimation of mle-parameter 'a' (amplitude)
   * dwmin   - minimum angluar frequency in power-spectrum
   * dtrange - time range covered by the data
   * dmnsf   - mean number of simulated flares
   */
  double dmf, dmfs, dmfv, dmfvs, dsdev, dma, dwmin, dtrange, dmnsf;

  /* pf    - array of test frequencies
   * ppow  - array of power spectrum
   * ppowm - array of mean power spectrum
   * ppowx - array of power spectrum (frequencies not used for mle-fit excluded)
   * pw2x  - array of angluar frequencies ( -     "      -)
   * dt    - array containing the time for each flare maximum
   * da    - array containing the amplitude for each flare maximum
   */
  double *pf, *ppow, *ppowm, *ppowx, *pw2x, *dt, *da;
  double *dx, *dy, *dz, *dz2;

  bool bnewdat, bfinal;

  MLE_STRUCT mle;

  static FILE_TYPE dat_file, mle_file, powspec_file;


  /* init array pointers (set dz2 to NULL, if normalization
   * on sample variance is wanted, else dz = dz2)
   */
  dx = p2al[0];
  dy = p2al[1];
  dz = p2al[2];
  dz2 = (ieflag == 0) ? NULL : dz;


  /* allocate memory for mean power values (the call to cts_vgetfreq is also
   * used to get the time range of the data)
   */
  cts_vgetfreq (dx, inp, iofac, &dfmax, &dwmin, &inf);

  dtrange = d2PI / (dwmin * (double) iofac);
  ppowm = cts_mmalloc (double, inf, CFUNCNAME);


  /* open file to store simulation results
   */
  dat_file.pname = cSVAR_DAT;
  dat_file.pfile = NULL;
  dat_file.acm = "w";

  cts_vopen_file (&dat_file);

  isteps = 1 + (int) (0.5 + mdiv (dsdev_max - dsdev_min, dsdev_ss));
  fprintf (dat_file.pfile, cSVAR_WRITE1, dmsa, dsrate, isims, dfmax_mle,
	   ieflag, inf, isteps);
  fprintf (dat_file.pfile, cSVAR_HEAD);


  /* open file to store mle results for all simulations
   */
  mle_file.pname = cSVAR_MLE_DAT;
  mle_file.pfile = NULL;
  mle_file.acm = "w";

  cts_vopen_file (&mle_file);

  /* open file to store power spectra for each set of simulations
   */
  powspec_file.pname = cSVAR_POW_DAT;
  powspec_file.pfile = NULL;
  powspec_file.acm = "w";

  cts_vopen_file (&powspec_file);


  /* loop over all sdev values
   * (bnewdat is modified within ls_power)
   */
  bnewdat = true;
  dsdev = dsdev_min - dsdev_ss;

  for (i = 0; i < isteps; i++)
    {
      dsdev += dsdev_ss;
      dmnsf = dmfs = dmfvs = 0.;
      cts_vreset_darray (ppowm, inf);


      /* start the simulations for a given sdev
       */
      for (k = 0; k < isims; k++)
	{
	  /* simulate lightcurve and store it in dy
	   */
	  vsim_lc1 (dx, dy, dz, inp, dsrate, dmsa, dsdev, bnewdat, &dt, &da,
		    &ins);

	  /* derive power spectrum in ppow and frequencies in pf
	   */
	  cts_ls_power (dx, dy, dz2, inp, iofac, dfmax, &bnewdat, &pf, &ppow,
			&inf);

	  /* derive mean and variance from the simulated fluxes
	   */
	  cts_vget_svar (dy, inp, &dmf, &dmfv);

	  dmfs += dmf;
	  dmfvs += dmfv;
	  dmnsf += (double) ins;


	  /* update array for mean power values
	   */
	  for (j = 0; j < inf; j++)
	    ppowm[j] += ppow[j];
	}


      /* simulations for one given dsdev is done, so calculate
       * results and store them
       */
      dmnsf /= (double) isims;
      dmfs /= (double) isims;
      dmfvs /= (double) isims;

      for (j = 0; j < inf; j++)
	{
	  ppowm[j] = ppowm[j] / isims;
	  fprintf (powspec_file.pfile, cSVAR_WRITE2, pf[j], ppowm[j]);
	}

      mle = get_mle_g (inf, pf, ppowm, dfmax_mle, pxfi, inxf, mle_file, &pw2x,
		       &ppowx);

      dma = (double) inp * dmnsf * d2PI * m2 (dsdev) * dmsa * 2. /
	(dmfvs * m2 (dtrange));

      fprintf (dat_file.pfile, cSVAR_WRITE3, dsdev, dmfs, dmfvs, dma, mle.a,
	       mle.s, mle.c, mle.l);


      /* produce plot; bfinal triggers final plot
       */
      bfinal = (i + 1 < isteps) ? false : true;

      vplot_sim (bfinal, isteps, dsdev, dsrate, dmsa, isims, dfmax, dfmax_mle,
		 dmfs, dmfvs, dma, mle, pf, ppowm, inf, ieflag, SIM_S);

    }  /* END for (dsdev ... */


  cts_vclose_file (&dat_file);
  cts_vclose_file (&mle_file);
  cts_vclose_file (&powspec_file);
}

#undef CFUNCNAME


#define CFUNCNAME "vsim_r_var"

void vsim_r_var (double **p2al, int inp, int ieflag, int isims, int iofac,
		 double dsrate_min, double dsrate_max, double dsrate_ss,
		 double dmsa, double dsdev, double dfmax, double dfmax_mle,
		 int *pxfi, int inxf)

  /* this function runs several simulations where all the shot noise
   * parameters except the 'shot-rate' are kept fixed. From the
   * resulting mean power spectra I derive the shot noise parameters
   * and save them.
   * input:
   * p2al       - array of 3 arrays (mjd, flux, flux error)
   * inp        - number of points in p2al arrays
   * ieflag     - take sample variance if 0, else use individual data errors
   *              (used for normalizing power spectrum)
   * isims      - mumber of simulations for a given dmsa
   * iofac      - oversampling factor
   * dsrate_min - mean shot rate of the flares - minimum
   * dsrate_max - mean shot rate of the flares - maximum
   * dsrate_ss  - mean shot rate of the flares - step width
   * dmsa       - mean squared amplitude of the flares
   * dsdev      - stand. dev. of the gaussian type flares
   * dfmax      - max. frequency used for power spectrum
   * dfmax_mle  - max. frequency used for the mle-fit
   * pxfi       - integer array containing the index of all frequencies which
   *              are not used for the mle fit (in addition to dfmax_mle)
   *              the integers in pxfi are expected to be in ascending order!
   * inxf       - number of excluded frequencies (in addition to dfmax_mle)
   * output:
   * p2al[1]    - flux from (last) simulated lightcurve
   */
{
  int i, j, k, inf, ins, isteps;

  /* dmf     - mean flux for a given lightcurve
   * dmfs    - mean flux for a set of dmf values
   * dmfv    - flux variance for a given lightcurve
   * dmfvs   - mean flux variance for a set of dmfv values
   * dsdev   - stand. dev. of the gaussian type flares
   * dma     - theoretical estimation of mle-parameter 'a' (amplitude)
   * dwmin   - minimum angluar frequency in power-spectrum
   * dtrange - time range covered by the data
   * dmnsf   - mean number of simulated flares
   */
  double dmf, dmfs, dmfv, dmfvs, dsrate, dma, dwmin, dtrange, dmnsf;

  /* pf    - array of test frequencies
   * ppow  - array of power spectrum
   * ppowm - array of mean power spectrum
   * ppowx - array of power spectrum (frequencies not used for mle-fit excluded)
   * pw2x  - array of angluar frequencies ( -     "      -)
   * dt    - array containing the time for each flare maximum
   * da    - array containing the amplitude for each flare maximum
   */
  double *pf, *ppow, *ppowm, *ppowx, *pw2x, *dt, *da;
  double *dx, *dy, *dz, *dz2;

  bool bnewdat, bfinal;

  MLE_STRUCT mle;

  static FILE_TYPE dat_file, mle_file, powspec_file;


  /* init array pointers (set dz2 to NULL, if normalization
   * on sample variance is wanted, else dz = dz2)
   */
  dx = p2al[0];
  dy = p2al[1];
  dz = p2al[2];
  dz2 = (ieflag == 0) ? NULL : dz;


  /* allocate memory for mean power values (the call to cts_vgetfreq is also
   * used to get the time range of the data)
   */
  cts_vgetfreq (dx, inp, iofac, &dfmax, &dwmin, &inf);

  dtrange = d2PI / (dwmin * (double) iofac);
  ppowm = cts_mmalloc (double, inf, CFUNCNAME);


  /* open file to store simulation results
   */
  dat_file.pname = cRVAR_DAT;
  dat_file.pfile = NULL;
  dat_file.acm = "w";

  cts_vopen_file (&dat_file);

  isteps = 1 + (int) (0.5 + mdiv (dsrate_max - dsrate_min, dsrate_ss));
  fprintf (dat_file.pfile, cRVAR_WRITE1, dmsa, dsdev, isims, dfmax_mle,
	   ieflag, inf, isteps);
  fprintf (dat_file.pfile, cRVAR_HEAD);

  /* open file to store mle results for all simulations
   */
  mle_file.pname = cRVAR_MLE_DAT;
  mle_file.pfile = NULL;
  mle_file.acm = "w";

  cts_vopen_file (&mle_file);

  /* open file to store power spectra for each set of simulations
   */
  powspec_file.pname = cRVAR_POW_DAT;
  powspec_file.pfile = NULL;
  powspec_file.acm = "w";

  cts_vopen_file (&powspec_file);


  /* loop over all srate values
   * (bnewdat is modified within ls_power)
   */
  bnewdat = true;
  dsrate = dsrate_min - dsrate_ss;

  for (i = 0; i < isteps; i++)
    {
      dsrate += dsrate_ss;
      dmnsf = dmfs = dmfvs = 0.;
      cts_vreset_darray (ppowm, inf);


      /* start the simulations for a given srate
       */
      for (k = 0; k < isims; k++)
	{
	  /* simulate lightcurve and store it in dy
	   */
	  vsim_lc1 (dx, dy, dz, inp, dsrate, dmsa, dsdev, bnewdat, &dt, &da,
		    &ins);

	  /* derive power spectrum in ppow and frequencies in pf
	   */
	  cts_ls_power (dx, dy, dz2, inp, iofac, dfmax, &bnewdat, &pf, &ppow,
			&inf);

	  /* derive mean and variance from the simulated fluxes
	   */
	  cts_vget_svar (dy, inp, &dmf, &dmfv);

	  dmfs += dmf;
	  dmfvs += dmfv;
	  dmnsf += (double) ins;


	  /* update array for mean power values
	   */
	  for (j = 0; j < inf; j++)
	    ppowm[j] += ppow[j];
	}


      /* simulations for one given shot rate is done, so calculate
       * results and store them
       */
      dmnsf /= (double) isims;
      dmfs /= (double) isims;
      dmfvs /= (double) isims;

      for (j = 0; j < inf; j++)
	{
	  ppowm[j] = ppowm[j] / isims;
	  fprintf (powspec_file.pfile, cRVAR_WRITE2, pf[j], ppowm[j]);
	}

      mle = get_mle_g (inf, pf, ppowm, dfmax_mle, pxfi, inxf, mle_file, &pw2x,
		       &ppowx);

      dma = (double) inp * dmnsf * d2PI * m2 (dsdev) * dmsa * 2. /
	(dmfvs * m2 (dtrange));

      fprintf (dat_file.pfile, cRVAR_WRITE3, dsrate, dmfs, dmfvs, dma, mle.a,
	       mle.s, mle.c, mle.l);


      /* produce plot; bfinal triggers final plot
       */
      bfinal = (i + 1 < isteps) ? false : true;

      vplot_sim (bfinal, isteps, dsdev, dsrate, dmsa, isims, dfmax, dfmax_mle,
		 dmfs, dmfvs, dma, mle, pf, ppowm, inf, ieflag, SIM_R);

    }  /* END  for (dsrate = ... */


  cts_vclose_file (&dat_file);
  cts_vclose_file (&mle_file);
  cts_vclose_file (&powspec_file);
}

#undef CFUNCNAME


#define CFUNCNAME "vget_ct_r"

void vget_ct_r (double **p2al, int inp, int ieflag, int isims, int iofac,
		double dsrate_min, double dsrate_max, double dsrate_ss,
		double dmsa, double dsdev, double dfmax)

  /* this function calculates the cross terms in the shot noise process
   * for variable shot rates
   * input:
   * p2al       - array of 3 arrays (mjd, flux, flux error)
   * inp        - number of points in p2al arrays
   * ieflag     - take sample variance if 0, else use individual data errors
   *              (used for normalizing power spectrum)
   * isims      - mumber of simulations for a given dmsa
   * iofac      - oversampling factor
   * dsrate_min - mean shot rate of the flares - minimum
   * dsrate_max - mean shot rate of the flares - maximum
   * dsrate_ss  - mean shot rate of the flares - step width
   * dmsa       - mean squared amplitude of the flares
   * dsdev      - stand. dev. of the gaussian type flares
   * dfmax      - max. frequency used for power spectrum
   * output:
   * p2al[1]    - flux from (last) simulated lightcurve
   */
{
  int i, j, k, n, m;
  int ins, ins_sum, inf, index, isteps;

  /* dmfv    - flux variance for a given lightcurve
   * dmfvs   - mean flux variance for a set of dmfv values
   * dsrate  - mean shot rate
   * ddt     - time between two flare maxima
   * dct     - cross-term for two single shots (ai*an*cos (w(ti-tn)))
   * dwmin   - minimum angluar frequency in power-spectrum
   * dtrange - time range covered by the data
   * dummy   - takes function values which are not used here
   */
  double dmfv, dmfvs, dsrate, ddt, dct, dwmin, dtrange, dummy;

  /* pf     - array of test frequencies
   * ppow   - array of power spectrum
   * ppowm  - array of mean power spectrum
   * ppowv  - array of power spectrum variance
   * ppowst - array of power spectrum standard dev. (theoretical value)
   * pmct   - array of mean cross-terms (frequency dependent)
   * pmctv  - array of cross-term variances (frequency dependent)
   * pvarff - array of variances for FF* (frequency dependent)
   * pcoswt - array of cos-values on a grid
   * dt     - array containing the time for each flare maximum
   * da     - array containing the amplitude for each flare maximum
   */
  double *pf, *ppow, *ppowm, *ppowv, *ppowst, *pmct, *pmctv, *pvarff, *pcoswt;
  double *dt, *da, *dx, *dy, *dz, *dz2;

  bool bnewdat, binit;

  FILE_TYPE dat_file;


  /* init array pointers (set dz2 to NULL, if normalization
   * on sample variance is wanted, else dz = dz2)
   */
  dx = p2al[0];
  dy = p2al[1];
  dz = p2al[2];
  dz2 = (ieflag == 0) ? NULL : dz;


  /* open file to store simulation results
   */
  dat_file.pname = cCTR_DAT;
  dat_file.pfile = NULL;
  dat_file.acm = "w";

  cts_vopen_file (&dat_file);


  /* allocate memory for mean power values, their variances and the theoretical
   * standard deviation (the call to cts_vgetfreq is also used to get the time
   * range of the data)
   */
  cts_vgetfreq (dx, inp, iofac, &dfmax, &dwmin, &inf);

  dtrange = d2PI / (dwmin * (double) iofac);
  ppowm = cts_mmalloc (double, inf, CFUNCNAME);
  ppowv = cts_mmalloc (double, inf, CFUNCNAME);
  ppowst = cts_mmalloc (double, inf, CFUNCNAME);


  /* allocate memory for (frequency dependent) cross terms, their
   * standard deviations and faktors to get variance of FF*
   */
  pmct = cts_mmalloc (double, inf, CFUNCNAME);
  pmctv = cts_mmalloc (double, inf, CFUNCNAME);
  pvarff = cts_mmalloc (double, inf, CFUNCNAME);


  /* pcoswt are calculated on a grid to estimate the exact
   * cosines (sines are not needed here)
   */
  pcoswt = cts_mmalloc (double, iSC_GRID_POINTS, CFUNCNAME);

  for (i = 0; i < iSC_GRID_POINTS; i++)
    pcoswt[i] = cos (cts_mput_angle (i));


  /* write first data to file
   */
  isteps = 1 + (int) (0.5 + mdiv (dsrate_max - dsrate_min, dsrate_ss));
  fprintf (dat_file.pfile, cCTR_WRITE1, dmsa, dsdev, isims, ieflag, inf,
	   isteps);
  fprintf (dat_file.pfile, cCTR_HEAD);


  /* loop over all shot rate values (bnewdat is modified within ls_power)
   */
  bnewdat = binit = true;
  dsrate = dsrate_min - dsrate_ss;

  for (i = 0; i < isteps; i++)
    {
      dmfvs = 0.;
      ins_sum = 0;
      dsrate += dsrate_ss;

      cts_vreset_darray (pmct, inf);
      cts_vreset_darray (pmctv, inf);
      cts_vreset_darray (ppowm, inf);
      cts_vreset_darray (ppowv, inf);


      /* start the simulations
       */
      for (k = 0; k < isims; k++)
	{
	  /* simulate lightcurve and store it in dy
	   */
	  vsim_lc1 (dx, dy, dz, inp, dsrate, dmsa, dsdev, bnewdat, &dt, &da,
		    &ins);

	  /* derive power spectrum in ppow and frequencies in pf
	   */
	  cts_ls_power (dx, dy, dz2, inp, iofac, dfmax, &bnewdat, &pf, &ppow,
			&inf);

	  /* derive variance from the simulated fluxes
	   */
	  cts_vget_svar (dy, inp, &dummy, &dmfv);

	  dmfvs += dmfv;
	  ins_sum += ins;


	  /* double loop over all shots
	   */
	  for (j = 0; j < ins; j++)
	    for (n = 0; n < ins; n++)
	      {
		ddt = dt[n] - dt[j];

		/* loop over all frequencies
		 */
		for (m = 0; m < inf; m++)
		  {
		    /* get cosine value from the grid
		     */
		    cts_mget_index (index, dummy, d2PI * pf[m] * ddt);

		    if (n != j)
		      {
			/* calculate frequency dependent cross terms
			 * and update arrays
			 * (ct := sum_j sum_n (a_j*a_n*cos (w(t_j-t_n))) / k;
			 *  j = 1, ..., k; n = 1, ..., k; a_j - shot amplitudes)
			 */
			dct = da[n] * da[j] * pcoswt[index];
			pmct[m] += dct;
			pmctv[m] += m2 (dct);
		      }
		  }
	      }


	  /* update array for mean power values and variances
	   */
	  for (j = 0; j < inf; j++)
	    {
	      ppowm[j] += ppow[j];
	      ppowv[j] += m2 (ppow[j]);
	    }

	} /* END  for (dsrate = ...) */


      /* simulations for one given dsrate is done, so calculate
       * results and store them (pvarff is calculated only once)
       */
      dmfvs /= (double) isims;

      for (j = 0; j < inf; j++)
	{
	  ppowm[j] /= (double) isims;
	  ppowv[j] = ppowv[j] / (double) isims - m2 (ppowm[j]);

	  pmct[j] /= (double) ins_sum;
	  pmctv[j] = pmctv[j] / (double) ins_sum - m2 (pmct[j]);
  
	  if (binit)
	    pvarff[j] = m2 (d2PI * dsdev * dsdev) * pmctv[j]
	      * exp (-2. * m2 (d2PI * pf[j] * dsdev));

	  ppowst[j] = sqrt (2. * (double) inp * pvarff[j]) / (dmfvs * dtrange);


	  fprintf (dat_file.pfile, cCTR_WRITE2, dsrate, pf[j], ppowm[j],
		   ppowv[j], pmct[j], pmctv[j], pvarff[j], ppowst[j]);
	}

      binit = false;

      /* produce plot
       */
      vplot_ct (pf, ppow, ppowv, ppowst, inf, dsrate, dmsa, dsdev, ieflag, 0.,
		CT_R, isteps, dfmax);


    }  /* END for (i = 0; i < isteps; i++) */


  cts_vclose_file (&dat_file);
}

#undef CFUNCNAME


#define CFUNCNAME "vget_ct_t"

void vget_ct_t (double **p2al, int inp, int ieflag, int isims, int iofac,
		int itstep, double dsrate, double dmsa, double dsdev,
		double dfmax)

  /* this function calculates the cross terms in the shot noise process
   * for sliding obs. times (from aequidistant obs. points to real obs. points)
   * input:
   * p2al    - array of 3 arrays (mjd, flux, flux error)
   * inp     - number of points in p2al arrays
   * ieflag  - take sample variance if 0, else use individual data errors
   *           (used for normalizing power spectrum)
   * isims   - mumber of simulations for a given dmsa
   * iofac   - oversampling factor
   * itstep  - number of 'obs. times' sets
   * dsrate  - mean shot rate of the flares
   * dmsa    - mean squared amplitude of the flares
   * dsdev   - stand. dev. of the gaussian type flares
   * dfmax   - max. frequency used for power spectrum
   * output:
   * p2al[1] - flux from (last) simulated lightcurve
   */
{
  int i, j, k, n, m;
  int ins, ins_sum, inf, index, ioffset;

  /* dmfv    - flux variance for a given lightcurve
   * ddt     - time between two flare maxima
   * dct     - cross-term for two single shots (ai*an*cos (w(ti-tn)))
   * dwmin   - minimum angluar frequency in power-spectrum
   * dtrange - time range covered by the data
   * dbinset - characterizes the used binning (in obs. times)
   *           0 - orig. binning up to 1 - aequidistant binning
   * dummy   - takes function values which are not used here
   */
  double dmfv, ddt, dct, dwmin, dtrange, dbinset, dummy;

  /* pf     - array of test frequencies
   * ppow   - array of power spectrum
   * ppowm  - array of mean power spectrum
   * ppowv  - array of power spectrum variance
   * ppowst - array of power spectrum standard dev. (theoretical value)
   * pmct   - array of mean cross-terms (frequency dependent)
   * pmctv  - array of cross-term variances (frequency dependent)
   * pvarff - array of variances for FF* (frequency dependent)
   * pcoswt - array of cos-values on a grid
   * dt     - array containing the time for each flare maximum
   * da     - array containing the amplitude for each flare maximum
   * px2    - array of new 'obs. times'
   * px_sw  - array of step width for 'obs. times'
   * pmfvs  - array of mean flux variance for given 'obs. times'
   */
  double *pf, *ppow, *ppowm, *ppowv, *ppowst, *pmct, *pmctv, *pvarff, *pcoswt;
  double *dt, *da, *px2, *px_sw, *pmfvs, *dx, *dy, *dz, *dz2;

  bool bnewdat;

  FILE_TYPE dat_file;


  /* init array pointers (set dz2 to NULL, if normalization
   * on sample variance is wanted, else dz = dz2)
   */
  dx = p2al[0];
  dy = p2al[1];
  dz = p2al[2];
  dz2 = (ieflag == 0) ? NULL : dz;


  /* open file to store simulation results
   */
  dat_file.pname = cCTT_DAT;
  dat_file.pfile = NULL;
  dat_file.acm = "w";

  cts_vopen_file (&dat_file);


  /* allocate memory for mean power values, their variances and the theoretical
   * standard deviation (the call to cts_vgetfreq is also used to get the time
   * range of the data)
   */
  cts_vgetfreq (dx, inp, iofac, &dfmax, &dwmin, &inf);

  dtrange = d2PI / (dwmin * (double) iofac);
  ppowm = cts_mmalloc (double, inf * itstep, CFUNCNAME);
  ppowv = cts_mmalloc (double, inf * itstep, CFUNCNAME);
  ppowst = cts_mmalloc (double, inf, CFUNCNAME);


  /* allocate memory for new 'obs. times' and the step width
   * and initialize step width
   * (sw[i] = (t1[i] - t2[i]) / (itstep - 1) where t1[i] is time of i'th point
   *  in aequidistant binning, t2[i] is time of i'th point in real data)
   */
  px2 = cts_mmalloc (double, inp, CFUNCNAME);
  px_sw = cts_mmalloc (double, inp, CFUNCNAME);

  for (i = 0; i < inp; i++)
    px_sw[i] = (mdiv (i * dtrange, inp) + dx[0] - dx[i])
      / (double) (itstep - 1);


  /* allocate memory for cross terms, their standard deviations,
   * the faktors to get variance of FF* and the flux variances
   */
  pmct = cts_mmalloc (double, inf, CFUNCNAME);
  pmctv = cts_mmalloc (double, inf, CFUNCNAME);
  pvarff = cts_mmalloc (double, inf, CFUNCNAME);
  pmfvs = cts_mmalloc (double, itstep, CFUNCNAME);


  /* pcoswt are calculated on a grid to estimate the exact
   * cosines (sines are not needed here)
   */
  pcoswt = cts_mmalloc (double, iSC_GRID_POINTS, CFUNCNAME);

  for (i = 0; i < iSC_GRID_POINTS; i++)
    pcoswt[i] = cos (cts_mput_angle (i));


  /* write first data to file
   */
  fprintf (dat_file.pfile, cCTT_WRITE1, dsrate, dmsa, dsdev, isims, ieflag, inf,
	   itstep);
  fprintf (dat_file.pfile, cCTT_HEAD);


  /* start the simulations
   * (since we want to use a given set of flares for all sets of 'obs. times'
   *  the outer loop runs over the number of simulations 'isims'. This forces
   *  us to use arrays with extended length - 'istep * inf'
   */
  ins_sum = 0;
  bnewdat = true;

  cts_vreset_darray (pmct, inf);
  cts_vreset_darray (pmctv, inf);
  cts_vreset_darray (pmfvs, itstep);
  cts_vreset_darray (ppowm, inf * itstep);
  cts_vreset_darray (ppowv, inf * itstep);


  for (j = 0; j < isims; j++)
    {
      /* reset array of obs. times
       */
      memcpy (px2, dx, (size_t) inp * sizeof (double));

      /* simulate lightcurve and store it in dy
       */
      vsim_lc1 (px2, dy, dz, inp, dsrate, dmsa, dsdev, bnewdat, &dt, &da,
		&ins);

      /* derive power spectrum in ppow and frequencies in pf
       */
      cts_ls_power (px2, dy, dz2, inp, iofac, dfmax, &bnewdat, &pf, &ppow,
		    &inf);

      ins_sum += ins;


      /* double loop over all shots to get cross-terms
       * (doesn't change with changing obs. times)
       */
      for (i = 0; i < ins; i++)
	for (n = 0; n < ins; n++)
	  {
	    ddt = dt[n] - dt[i];

	    /* loop over all frequencies
	     */
	    for (m = 0; m < inf; m++)
	      {
		/* get cosine value from the grid
		 */
		cts_mget_index (index, dummy, d2PI * pf[m] * ddt);

		if (n != i)
		  {
		    /* calculate frequency dependent cross terms
		     * and update arrays
		     * (ct := sum_i sum_n (a_i*a_n*cos (w(t_i-t_n))) / k;
		     *  i = 1, ..., k; n = 1, ..., k; a_i - shot amplitudes)
		     */
		    dct = da[n] * da[i] * pcoswt[index];
		    pmct[m] += dct;
		    pmctv[m] += m2 (dct);
		  }
	      }
	  }


      /* run over all obs. time simulations
       */
      for (k = 0; k < itstep; k++)
	{
	  ioffset = k * inf;

	  /* recalculations not necessary when for-loop is entered the
	   * first time
	   */
	  if (k > 0)
	    {
	      /* update px2 and dy for new set of 'obs. times'
	       * (first derive new obs. time for one point, then recalculate
	       *  flux-contribution of all flares for this point)
	       */
	      for (i = 0; i < inp; i++)
		{
		  px2[i] += px_sw[i];
		  dy[i] = 0.;

		  for (n = 0; n < ins; n++)
		    dy[i] += da[n] * exp (-0.5 * m2 ((dt[n] - px2[i]) / dsdev));
		}


	      /* derive power spectrum in ppow and frequencies in pf
	       * (use new 'obs. times')
	       */
	      cts_ls_power (px2, dy, NULL, inp, iofac, dfmax, &bnewdat, &pf,
			    &ppow, &inf);
	    }


	  /* derive variance from the simulated fluxes
	   */
	  cts_vget_svar (dy, inp, &dummy, &dmfv);

	  pmfvs[k] += dmfv;


	  /* update array for mean power values and variances
	   */
	  for (i = 0; i < inf; i++)
	    {
	      ppowm[ioffset+i] += ppow[i];
	      ppowv[ioffset+i] += m2 (ppow[i]);
	    }

	} /* END  for (k = 0; ...) */

    } /* END  for (j = 0; ...) */


  /* simulations are finishe, so loop over all obs. times, calculate results
   * and store them
   */
  for (k = 0; k < itstep; k++)
    {
      ioffset = k * inf;
      pmfvs[k] /= (double) isims;


      for (i = 0; i < inf; i++)
	{
	  n = ioffset + i;
	  ppowm[n] /= (double) isims;
	  ppowv[n] = ppowv[n] / (double) isims - m2 (ppowm[n]);

	  /* renormalize pmct and pmctv only once
	   */
	  if (k == 0)
	    {
	      pmct[i] /= (double) ins_sum;
	      pmctv[i] = pmctv[i] / (double) ins_sum - m2 (pmct[i]);
	    }

	  pvarff[i] = m2 (d2PI * dsdev * dsdev) * pmctv[i]
	    * exp (-2. * m2 (d2PI * pf[i] * dsdev));

	  ppowst[i] = sqrt (2. * (double) inp * pvarff[i])
	    / (pmfvs[k] * dtrange);

	  dbinset = mdiv (k, itstep - 1);


	  fprintf (dat_file.pfile, cCTT_WRITE2, dbinset, pf[i], ppowm[n],
		   ppowv[n], pmct[i], pmctv[i], pvarff[i], ppowst[i]);
	}


      /* produce plot
       */
      vplot_ct (pf, ppow, &ppowv[ioffset], ppowst, inf, dsrate, dmsa, dsdev,
		ieflag, dbinset, CT_T, itstep, dfmax);
    }


  cts_vclose_file (&dat_file);
}

#undef CFUNCNAME


void vsim_sin (double **p2al, int inp, int ieflag, int iofac, double dperiod,
	       double dstime, double damp, double dfmax)

  /* this function derives the power spectrum for a sinusoidal lightcurve
   * input:
   * p2al    - array of 3 arrays (mjd, flux, flux error)
   * inp     - number of points in p2al arrays
   * ieflag  - take sample variance if 0, else use individual data errors
   *           (used for normalizing power spectrum)
   * iofac   - oversampling factor
   * dperiod - period of sinusoidal lightcurve
   * dstime  - start time (t0)
   * damp    - amplitude
   * dfmax   - max. frequency used for power spectrum
   * output:
   * p2al[1] - flux from simulated lightcurve
   */
{
  int i, inf;

  double *dx, *dy, *dz, *pf, *ppow;

  bool bnewdat;

  FILE_TYPE sin_file;

  /* open file to store results
   */
  sin_file.pname = cSIN_DAT;
  sin_file.pfile = NULL;
  sin_file.acm = "w";

  cts_vopen_file (&sin_file);


  /* init pointers and variables (pass flux errors only, if normalization
   * on individual data errors is wanted - pointer is used as a flag)
   */
  bnewdat = true;
  dx = p2al[0];
  dy = p2al[1];
  dz = (ieflag == 0) ? NULL : p2al[2];


  /* simulate lightcurve and store it in dy
   */
  vsim_lc5 (dx, dy, dz, inp, dperiod, dstime, damp);

  cts_ls_power (dx, dy, dz, inp, iofac, dfmax, &bnewdat, &pf, &ppow, &inf);


  /* save results
   */
  fprintf (sin_file.pfile, cSIN_WRITE1, dperiod, dstime, damp, inf, ieflag);
  fprintf (sin_file.pfile, cSIN_HEAD);

  for (i = 0; i < inf; i++)
    fprintf (sin_file.pfile, cSIN_WRITE2, pf[i], ppow[i]);


  /* create plot
   */
  vplot_sinpow (pf, ppow, inf, dfmax, dperiod, dstime, damp, ieflag);
}
