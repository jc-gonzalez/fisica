/*
 *    header file for powspec.c
 *
 * (if necessary do-while is used to allow semicolons after the macro call
 *  in the source code)
 */

/* prevent multiple includes
 */
#ifndef POWSPEC_H
#define POWSPEC_H 1


#ifndef __cplusplus
/* we're not dealing with C++
 */
#ifndef bool
#define bool int
#define true 1
#define false 0
#endif

#endif

/*
 * defines
 *
 */
/* default setting for input parameters
 */
#define iPDF_DEF       0
#define iERRF_DEF      0
#define iOFAC_DEF      1
#define iSIMS_DEF      1000
#define iSTEPS_DEF     10    
#define dSRATE_DEF     1.
#define dSRATE_MIN_DEF 0.5
#define dSRATE_MAX_DEF 1.5
#define dSRATE_SS_DEF  0.1
#define dMSA_DEF       9.
#define dMSA_MIN_DEF   1.
#define dMSA_MAX_DEF   10.
#define dMSA_SS_DEF    1.
#define dSDEV_DEF      0.4
#define dSDEV_MIN_DEF  0.1
#define dSDEV_MAX_DEF  1.0
#define dSDEV_SS_DEF   0.1
#define dPERIOD_DEF    23.
#define dSINAMP_DEF    9.


/* seeds
 */
#define lSEEDVAL1 -35193L
#define lSEEDVAL2 -55199L
#define lSEEDVAL3 -11580L
#define lSEEDVAL4 -43605L


/* dMIN_SIGMA - minimum sigma (here: 8.64 sec)
 * simulation starts at: 'MJDmin - iNR_PRE_INT * 2. * msquare (dsigma)'
 * and ends at         : 'MJDmax + iNR_POST_INT * 2. * msquare (dsigma)'
 * (2. * dsigma^2 is the time after which a shot dropped to 1/e of it's
 * maximum value)
 */
#define dMIN_SIGMA 1e-4
#define iNR_PRE_INT 5
#define iNR_POST_INT 5


/* precision of mle minimum
 */
#define dMLE_PREC 1e-8

/* number of shot noise parameters
 */
#define iSN_PARS_G 3

/* maximum power for pdf-calculation (multiplied by dPOWMAXSF, if normalization
 * on individual data errors)
 */
#define dPOWMAX   200.
#define dPOWMAXSF 100.
#define iPOWBINS  500
#define iMAX_NHYP_NAME_LEN 20

/* number of frequencies summed up for producing pdf-plot
 */
#define iNRLOWFREQ 10
#define iNRHIGHFREQ 10


/* strings and arguments used for reading and writing data
 */
#define cPDF_WRITE1 "# used: sdev %5.3f, srate %5.3f, msa %5.3f, eflag %d\n"
#define cPDF_READ1  "# used: sdev %lf, srate %lf, msa %lf, eflag %d\n"
#define cPDF_WRITE2 "# sims %d, freq %d, max power %5.3f, nr bins %d\n#\n"
#define cPDF_READ2  "# sims %d, freq %d, max power %lf, nr bins %d\n#\n"
#define cPDF_WRITE3 "%6.4f\t %d\n"
#define cPDF_READ3  "%lf %d\n"

#define iPDF_ARG1 4
#define iPDF_ARG2 4
#define iPDF_ARG3 2


#define cPOW_WRITE1 "#  used: fmax_mle %5.3f, nr. freq %d, eflag %d\n"
#define cPOW_READ1  "#  used: fmax_mle %lf, nr. freq %d, eflag %d\n"
#define cPOW_READ2a "%lf %lf %lf %lf"
#define cPOW_READ2b "%lf %lf %lf\n"
#define cPOW_WRITE3 "%12.6f %12.6f\n"
#define cPOW_READ3  "%lf %lf\n"
#define cPOW_WRITE4 "%6d %13.6f\n"
#define cPOW_READ4  "%d %lf\n"

#define cPOW_HEAD1  "#  A_mle:   error:   S_mle:   error:   C_mle:   error:\
    -Log(L):\n#\n"
#define cPOW_HEAD2  "\n\n#  frequency:     power:\n#\n"
#define cPOW_HEAD3  "\n#  excluded frequencies:\n#  index:   frequency:\n#\n"

#define iPOW_ARG1 3
#define iPOW_ARG2 7
#define iPOW_ARG3 2
#define iPOW_ARG4 2


#define cAVAR_WRITE1 "# used: sdev %5.3f, srate %5.3f, sims %d, fmax_mle %5.3f,\
 eflag %d, nr. freq. %d, a-steps: %d\n#\n"
#define cAVAR_READ1  "# used: sdev %lf, srate %lf, sims %d, fmax_mle %lf, eflag\
 %d, nr. freq. %d, a-steps: %d\n#\n"
#define cAVAR_HEAD   "\n#  msa:       <f>:        var(f):      <A>:       A_mle:\
       S_mle:       C_mle:     -Log(L):\n#\n"
#define cAVAR_WRITE2 "%6.4f %6.4f\n"
#define cAVAR_READ2  "%lf %lf\n"
#define cAVAR_WRITE3 "%6.2f %12.4f %12.4f %11.4f %11.4f %12.4f %12.4f %12.4f\n"
#define cAVAR_READ3  "%lf %lf %lf %lf %lf %lf %lf %lf\n"

#define iAVAR_ARG1 7
#define iAVAR_ARG2 2
#define iAVAR_ARG3 8


#define cSVAR_WRITE1 "# used: msa %5.3f, srate %5.3f, sims %d, fmax_mle %5.3f,\
 eflag %d, nr. freq. %d, s-steps: %d\n#\n"
#define cSVAR_READ1  "# used: msa %lf, srate %lf, sims %d, fmax_mle %lf, eflag\
 %d, nr. freq. %d, a-steps: %d\n#\n"
#define cSVAR_HEAD   "\n# sdev:      <f>:        var(f):      <A>:       A_mle:\
       S_mle:       C_mle:     -Log(L):\n#\n"
#define cSVAR_WRITE2 "%6.4f %6.4f\n"
#define cSVAR_READ2  "%lf %lf\n"
#define cSVAR_WRITE3 "%6.2f %12.4f %12.4f %11.4f %11.4f %12.4f %12.4f %12.4f\n"
#define cSVAR_READ3  "%lf %lf %lf %lf %lf %lf %lf %lf\n"

#define iSVAR_ARG1 7
#define iSVAR_ARG2 2
#define iSVAR_ARG3 8


#define cRVAR_WRITE1 "# used: msa %5.3f, sdev %5.3f, sims %d, fmax_mle %5.3f,\
 eflag %d, nr. freq. %d, r-steps: %d\n#\n"
#define cRVAR_READ1  "# used: msa %lf, sdev %lf, sims %d, fmax_mle %lf, eflag\
 %d, nr. freq. %d, a-steps: %d\n#\n"
#define cRVAR_HEAD   "\n# srate:      <f>:        var(f):      <A>:       A_mle:\
       S_mle:       C_mle:     -Log(L):\n#\n"
#define cRVAR_WRITE2 "%6.4f %6.4f\n"
#define cRVAR_READ2  "%lf %lf\n"
#define cRVAR_WRITE3 "%6.2f %12.4f %12.4f %11.4f %11.4f %12.4f %12.4f %12.4f\n"
#define cRVAR_READ3  "%lf %lf %lf %lf %lf %lf %lf %lf\n"

#define iRVAR_ARG1 7
#define iRVAR_ARG2 2
#define iRVAR_ARG3 8


#define cSIN_HEAD   "#  frequency:     power:\n#\n"
#define cSIN_WRITE1 "#  used: period %5.3f, t0 %5.3f, amplitude %5.3f, \
nr. freq %d, eflag %d\n#\n\n"
#define cSIN_READ1  "#  used: period %lf, t0 %lf, amplitude %lf, nr. freq \
%d, eflag %d\n#\n\n"
#define cSIN_WRITE2 "%12.6f %12.6f\n"
#define cSIN_READ2  "%lf %lf\n"

#define iSIN_ARG1 5
#define iSIN_ARG2 2


#define cNVAR_WRITE1 "# used: msa %5.3f, sdev %5.3f, srate %5.3f, sims %d, \
 fmax_mle %5.3f, eflag %d, nr. freq. %d\n#\n\n"
#define cNVAR_READ1 "# used: msa %lf, sdev %lf, srate %lf, sims %d, \
 fmax_mle %lf, eflag %d, nr. freq. %d\n#\n\n"
#define cNVAR_WRITE2 "E(A) %5.3f\t V(A) %5.3f\n"
#define cNVAR_READ2  "E(A) %lf V(A) %lf\n"
#define cNVAR_WRITE3 "E(S) %5.3f\t V(S) %5.3f\n"
#define cNVAR_READ3  "E(S) %lf V(S) %lf\n"
#define cNVAR_WRITE4 "E(C) %5.3f\t V(C) %5.3f\n"
#define cNVAR_READ4  "E(C) %lf V(C) %lf\n"
#define cNVAR_WRITE5 "E(L) %5.3f\t V(L) %5.3f\n\n"
#define cNVAR_READ5  "E(L) %lf V(L) %lf\n\n"
#define cNVAR_WRITE6 "A(<pow>) %5.3f, S(<pow>) %5.3f, C(<pow>) %5.3f, \
L(<pow>) %5.3f\n\n"
#define cNVAR_READ6  "A(<pow>) %lf, S(<pow>) %lf, C(<pow>) %lf, \
L(<pow>) %lf\n\n"
#define cNVAR_WRITE7 "%10.5f %15.4f %16.4f\n"
#define cNVAR_READ7  "%lf %lf %lf\n"
#define cNVAR_HEAD   "# frequency:      mean power:     stand. dev.:\n#\n" 

#define iNVAR_ARG1 7
#define iNVAR_ARG2 2
#define iNVAR_ARG3 2
#define iNVAR_ARG4 2
#define iNVAR_ARG5 2
#define iNVAR_ARG6 4
#define iNVAR_ARG7 3

#define cCTR_WRITE1 "# used: msa %5.3f, sdev %5.3f, sims %d, eflag %d,\
 nr. freq. %d, steps %d\n"
#define cCTR_READ1 "# used: msa %lf, sdev %lf, sims %d, eflag %d,\
 nr. freq. %d, steps %d\n"
#define cCTR_WRITE2 "%7.2f %11.4f %11.4f %11.4f %11.4f %11.4f %13.4f %9.4f\n"
#define cCTR_READ2  "%lf %lf %lf %lf %lf %lf %lf %lf\n"
#define cCTR_HEAD   "# srate:   frequency:   <pow(f)>:  var(pow(f)):   ct.:\
     var(ct):     var(FF*):    sd(P):\n#\n"

#define iCTR_ARG1 6
#define iCTR_ARG2 8


#define cCTT_WRITE1 "# used: dsrate %5.3f, msa %5.3f, sdev %5.3f, sims %d,\
 eflag %d, nr. freq. %d, steps %d\n"
#define cCTT_READ1 "# used: dsrate %lf, msa %lf, sdev %lf, sims %d, eflag %d,\
 nr. freq. %d, steps %d\n"
#define cCTT_WRITE2 "%7.2f %11.4f %11.4f %11.4f %11.4f %11.4f %13.4f %9.4f\n"
#define cCTT_READ2  "%lf %lf %lf %lf %lf %lf %lf %lf\n"
#define cCTT_HEAD   "# sratio:  frequency:   <pow(f)>:  var(pow(f)):   ct.:\
     var(ct):     var(FF*):    sd(P):\n#\n"

#define iCTT_ARG1 7
#define iCTT_ARG2 8


#define cMLE_WRITE " %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f %10.4f\n"
#define cMLE_READ  "%lf %lf %lf %lf %lf %lf %lf\n"

#define iMLE_ARG 7


/* names of ouput files
 */
#define cPDF_SIM_PS    "pdf_sim.ps"
#define cPDF_BOOT_PS   "pdf_boot.ps"
#define cPDF_GAUSS_PS  "pdf_gauss.ps"
#define cPDF_SIM_DAT   "pdf_simlc.dat"
#define cPDF_BOOT_DAT  "pdf_bootstrap.dat"
#define cPDF_GAUSS_DAT "pdf_gaussrnd.dat"

#define cPOW_PS        "powspec.ps"
#define cPOW_DAT       "powspec_out.dat"

#define cSIN_PS        "sinpow.ps"
#define cSIN_DAT       "sinpow_out.dat"

#define cNVAR_DAT      "sim_no_var.dat"
#define cNVAR_MLE_DAT  "mle_no_var.dat"

#define cAVAR_PS       "sim_a_var.ps"
#define cAVAR_DAT      "sim_a_var.dat"
#define cAVAR_MLE_DAT  "mle_a_var.dat"
#define cAVAR_POW_DAT  "powspec_a_var.dat"

#define cSVAR_PS       "sim_s_var.ps"
#define cSVAR_DAT      "sim_s_var.dat"
#define cSVAR_MLE_DAT  "mle_s_var.dat"
#define cSVAR_POW_DAT  "powspec_s_var.dat"

#define cRVAR_PS       "sim_r_var.ps"
#define cRVAR_DAT      "sim_r_var.dat"
#define cRVAR_MLE_DAT  "mle_r_var.dat"
#define cRVAR_POW_DAT  "powspec_r_var.dat"

#define cNVAR_PS       "sim_no_var.ps"

#define cCTR_DAT       "ct_r.dat"
#define cCTR_PS        "ct_r.ps"

#define cCTT_DAT       "ct_t.dat"
#define cCTT_PS        "ct_t.ps"



/*
 * typedefs
 *
 */
/* structure to keep the prarameters of all simulated single
 * shots
 */
typedef struct shot_data_list
{
  double dtime;                   /* time of shot maximum */
  double damp;                    /* amplitude of shot */
  struct shot_data_list *p2next;
} SDLIST;

typedef struct shot_data_list SDLIST_EL;


/* structure to store log likelihood results
 */
typedef struct mle_struct
{
  int i;
  double l;
  double c;
  double a;
  double s;
  double ce;
  double ae;
  double se;
} MLE_STRUCT;


/* enum to choose null hypothesis for calculating the pdf. This enum is
 * used to acces array elements, so be careful with changes
 */
typedef enum {SIMLC = 0, BOOTSTRAP, GAUSSRND, MAX_NHYP_TYPE} NHYP;

/* enum to tell plot routine which simulation was done
 */
typedef enum {SIM_A = 0, SIM_S, SIM_R, MAX_SIM_TYPE} SIMTYPE;

/* enum to tell plot routine which cross term simulation was done
 */
typedef enum {CT_R = 0, CT_T, MAX_CT_TYPE} CTTYPE;


/*
 * function declarations
 *
 */
/* builds a lightcurve from the superposition of gaussian type flares
 * (this version returns information on each single flare)
 */
extern void vsim_lc1 (double [], double [], double [], int, double, double,
		      double, bool, double **, double **, int *);

/* builds a lightcurve from the superposition of gaussian type flares
 * (no flare information returned, but faster)
 */
extern void vsim_lc2 (double [], double [], double [], int, double, double,
		      double, bool);

/* builds a lightcurve by bootstrapping the input data
 */
extern void vsim_lc3 (double [], double [], double [], int, double, double,
		      double, bool);

/* simulates a lightcurve of gaussian random noise
 */
extern void vsim_lc4 (double [], double [], double [], int, double, double,
		      double, bool);

/* simulates a sinusoid lightcurve of given period
 */
extern void vsim_lc5 (double [], double [], double [], int, double, double,
		      double);


/* run 'vsim_lc2' with all mle-parameters fixed
 */
extern void vsim_no_var (double **, int, int, int, int, int, double, double,
			 double, double, double, int *, int);

/* run 'vsim_lc1' with variable mean squared amplitude
 */
extern void vsim_a_var (double **, int, int, int, int, double, double, double,
			double, double, double, double, int *, int);

/* run 'vsim_lc1' with variable flare shape
 * (stand. dev. of gaussian type flares)
 */
extern void vsim_s_var (double **, int, int, int, int, double, double, double,
			double, double, double, double, int *, int);

/* run 'vsim_lc1' with variable shot rate
 */
extern void vsim_r_var (double **, int, int, int, int, double, double, double,
			double, double, double, double, int *, int);

/* derives power spectrum for a sinusoidal lightcurve
 */
extern void vsim_sin (double **, int, int, int, double, double, double, double);

/* run'vsim_lc1' with variable shot rate and calculate cross terms
 */
extern void vget_ct_r (double **, int, int, int, int, double, double, double,
		       double, double, double);

/* run'vsim_lc1' with variable obs. times and calculate cross terms
 */
extern void vget_ct_t (double **, int, int, int, int, int, double, double,
		       double, double);

/* this function derives the maximum likeliehood estimator
 * for the parameters A and s. It's done by fitting the function
 * A * exp (-w^2 * s^2) (derived within the shot noise model) to
 * the power spectrum
 */
extern MLE_STRUCT get_mle_g (int, double *, double *, double, int *, int,
			     FILE_TYPE, double **, double **);

/* derive power spectrum (incl. plot)
 */
extern void vget_powspec (double **, int, int, int, double, double, int *, int,
			  char *);

/* this function derives the probability distribution function for a given
 * null hypothesis
 */
extern void vget_pdf (double **, int, int, int, double, double, double, NHYP,
		      double, int);


/* functions to produce ps-files
 */
extern void vplot_pdf (double *, int *, double, int, int, int, NHYP, double,
		       double, double, int);


extern void vplot_powspec (double *, double *, int, MLE_STRUCT, double, double,
			   char *, int);

extern void vplot_sinpow (double *, double *, int, double, double, double,
			  double, int);

extern void vplot_sim (bool, int, double, double, double, int, double, double,
		       double, double, double, MLE_STRUCT, double *, double *,
		       int, int, SIMTYPE);

extern void vplot_sim_no (bool, double *, double *, int, double, double, double,
			  MLE_STRUCT, double, double, int);

extern void vplot_ct (double *, double *, double *, double *, int, double,
		      double, double, int, double, CTTYPE, int, double);

extern void vplot_all (double);


#endif /* POWSPEC_H */
