#include <string.h>
#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctshbook.h"
#include "ctscuts.h"
#include "ctsmath.h"

/* definition of global array cut_names - used to access cuts
 * by name
 */
const char cut_names [MAX_CUT_TYPE] [iMAX_CUT_NAME_LEN] =
{"nocut", "filter", "scuts", "dscuts97", "dscuts", "dscuts2", "ds97",
 "ds97loz", "ds97hiz", "hzacuts", "fs_cut"};


/*
 * declaration of all local functions
 */
static bool bnocut       (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bfilter_cuts (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bscuts       (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bdscuts97    (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bdscuts      (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bdscuts2     (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bds97        (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bds97loz     (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bds97hiz     (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bhza_cuts    (HBOOK_FILE *, RUN_TYPE, bool *);
static bool bfs_cut      (HBOOK_FILE *, RUN_TYPE, bool *);


/* array of all defined cuts - must match definition of CUT_TYPE
 * in cut.h
 */
static bool (*const cut_funcs [MAX_CUT_TYPE]) () =
{bnocut, bfilter_cuts, bscuts, bdscuts97, bdscuts, bdscuts2, bds97, bds97loz,
 bds97hiz, bhza_cuts, bfs_cut};


/* array of all defined run types - must match definition of RUN_TYPE
 * in cut.h
 */
static const float fruntype [MAX_RUN_TYPE] = {0.f, 1.f, 2.f};


/*
 *
 *  cut functions
 *
 */
static bool bnocut (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* set bsig to true and return true
   * (event is considered signal event and not cut)
   */
{
  *bsig = true;

  return (true);
}


static bool bnocut2 (HBOOK_FILE *dummy)

  /* function used when ther's no user supplied cut function - it just
   * returns true (passed data are ignored)
   */
{
  return (true);
}


static bool bfilter_cuts (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply filter cuts
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...)
   * bsig    - true for events in signal region, else false
   */
{
  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to filter variables in event data of ntuple
   */
  static float *psize, *pmaxpix, *pnumpix, *pruntyp;

  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      psize   = cts_pset_nam (pntuple, cSIZE);
      pruntyp = cts_pset_nam (pntuple, cRUNTYPE);
      pmaxpix = cts_pset_nam (pntuple, cMAXPIX12);
      pnumpix = cts_pset_nam (pntuple, cNUMPIXCB);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* apply filter cuts;
   * use rtyp to get runtype from file scope array 'fruntype'
   */
  if (*psize > 60.f && *pmaxpix > 1999.f && *pnumpix > 4000.f
      && *pnumpix < 92000.f && fruntype[rtyp] == *pruntyp)
    {
      /* all passed events are considered signal events
       */
      *bsig = true;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}


static bool bscuts (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply scuts - filter cuts are included
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to scut variables in event data of ntuple
   */
  static float *pdist, *pconc, *pwidth, *plength, *palpha;

  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      pdist   = cts_pset_nam (pntuple, cDIST);
      pconc   = cts_pset_nam (pntuple, cCONC);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      palpha  = cts_pset_nam (pntuple, cALPHA);
      plength = cts_pset_nam (pntuple, cLENGTH);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* apply scuts - including filter cuts
   */
  if (cut_funcs[FILTER] (pntuple, rtyp, bsig) && *pdist < 1.1f && *pdist > 0.5f
      && *plength < 0.3f && *plength > 0.16f && *pwidth < 0.15f
      && *pwidth > 0.07f && *pconc > 0.4f)
    {
      /* set bsig (all events below dALPHA_CUT are considered signal events)
       */
      *bsig = (fabs ((float) *palpha) < dALPHA_CUT) ? true : false;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}


static bool bfs_cut (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply false source cut - filter cuts are included
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to scut variables in event data of ntuple
   */
  static float *pdist, *pconc, *pwidth, *plength;

  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */
    {
      /* get location of different variable names in event-data
       */
      pdist   = cts_pset_nam (pntuple, cDIST);
      pconc   = cts_pset_nam (pntuple, cCONC);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      plength = cts_pset_nam (pntuple, cLENGTH);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* apply scuts - including filter cuts
   */
  if (cut_funcs[FILTER] (pntuple, rtyp, bsig) && *plength < 0.3f
      && *plength > 0.16f && *pwidth < 0.15f && *pwidth > 0.07f
      && *pconc > 0.4f)
    {
      /* all passed events are considered signal events
       * (ALPHA will be changed and is therefore meaningless here)
       */
      *bsig = true;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}


static bool bdscuts97 (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply original dynamical scuts - filter cuts are included
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* auxiliary variables
   */
  double dls, dad, dht, dcutlh, dcutll, dcutwh, dcutwl, dcutch, dcutcl;
  double dcuthl;

  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to cut variables in event data of ntuple
   */
  static float *psize, *pdist, *pmdist, *paltdeg, *pwidth, *plength, *pconc;
  static float *palpha;

  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      psize   = cts_pset_nam (pntuple, cSIZE);
      pconc   = cts_pset_nam (pntuple, cCONC);
      pdist   = cts_pset_nam (pntuple, cDIST);
      pmdist  = cts_pset_nam (pntuple, cMDIST);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      palpha  = cts_pset_nam (pntuple, cALPHA);
      plength = cts_pset_nam (pntuple, cLENGTH);
      paltdeg = cts_pset_nam (pntuple, cALTDEG);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }

  dls = log10 ((double) *psize);
  dad = (double) *paltdeg;
  dht = (double) (*pdist - *pmdist);


  /* apply dynamical scuts - including filter cuts;
   * (first derive cut boundaries, then apply the cuts)
   */
  bkeep = false;
  dcutlh = dcutwh = dcutch = dcuthl = dcutll = dcutwl = dcutcl = 0.;

  if (0.5 < *pdist && *pdist < 0.8)
    {
      dcutlh = cts_mcuto (dls, dad, 1.19036, 0.980757, 0.170714, 0.0184124,
			  -0.0718053, 0.0913398, -0.0336004, 0.00376545,
			  0.000568473, -0.000748999, 0.000294762, -0.0000364597);

      dcutll = cts_mcuto (dls, dad, -2.34962, 2.84021, -1.05459, 0.123668,
			  0.0534697, -0.0584963, 0.0207569,-0.00218114,
			  -0.000373666, 0.000395676, -0.000133401, 0.0000129945);


      dcutwh = cts_mcuto (dls, dad, 0.911201, -0.713875, 0.124025, 0.011091,
			  -0.0494004, 0.0598582, -0.0219576, 0.00248479,
			  0.000418959, -0.000521647, 0.000203062, -0.0000248613);

      dcutwl = cts_mcuto (dls, dad, -1.44981, 1.85022, -0.758222, 0.100109,
			  0.0318973, -0.044205, 0.0200009, -0.00274656,
			  -0.000205256, 0.000295974, -0.000137701, 0.0000192769);


      dcutch = cts_mcuto (dls, dad, 2.71197, -1.25739, 0.274738, -0.004953,
			  -0.0519297, 0.0438086, -0.0166526, 0.00173974,
			  0.000204679, -0.000172535, 0.0000836936, -0.0000106198);

      dcutcl = cts_mcuto (dls, dad, 3.4969, -4.46318, 2.14281, -0.320405,
			  -0.0969334, 0.132604, -0.0600206, 0.00846923,
			  0.000728087, -0.000954548, 0.000410947, -0.0000555756);


      dls = (dls > 3.954) ? 3.954 : dls;
      dcuthl = cts_mcuto (dls, dad, 0.266089, -1.22827, 0.79616, -0.167995,
			  0.0591657, -0.0663533, 0.0225148, -0.00157907,
			  -0.000617422, 0.000784941, -0.000308484, 0.000033587);
    }

  else if (0.8 < *pdist && *pdist < 1.1)
    {
      dcutlh = cts_mcuto (dls, dad, 0.0540586, 0.668754, -0.419966, 0.0721758,
			  -0.0131913, 0.00945466, -0.00062049, -0.000313049,
			  0.000100435, -0.0000872456, 0.0000169673, 8.94965e-8);

      dcutll = cts_mcuto (dls, dad,-0.653292, 0.164615, 0.147703, -0.0373323,
			  0.0323574, -0.0178379, 0.00158194, 0.000326205,
			  -0.000281999, 0.000201988, -0.0000435241, 1.87936e-6);


      dcutwh = cts_mcuto (dls, dad, -0.860858, 1.04732, -0.326931, 0.0288026,
			  0.00159757, 0.00180096, -0.00331662, 0.000883303,
			  0.0000201473, -0.0000400465, 0.0000315345, -7.01631e-6);

      dcutwl = cts_mcuto (dls, dad, -0.60864, 0.857768, -0.368289, 0.0573878,
			  0.0363869, -0.0487501, 0.0210685, -0.00302578,
			  -0.000363995, 0.000480565, -0.00020514, 0.000028817);


      dcutch = cts_mcuto (dls, dad, -1.58731, 3.03812, -1.11798, 0.109713,
			  0.116266, -0.134667, 0.0469623, -0.00489749, -0.0010245,
			  0.00114931, -0.000396013, 0.0000418562);


      dcutcl = cts_mcuto (dls, dad, -0.555555, 1.96449, -0.890388, 0.112722,
			  0.108433, -0.161118, 0.0703312, -0.00948812,
			  -0.000901089, 0.00130207, -0.000571321, 0.0000783964);


      dls = (dls > 3.954) ? 3.954 : dls;
      dcuthl = cts_mcuto (dls, dad, 6.01018, -8.35925, 3.56221, -0.465929,
			  -0.200242, 0.27268, -0.115226, 0.014939, 0.00137508,
			  -0.00187826, 0.000789943, -0.000101889);
    }


  if (cut_funcs[FILTER] (pntuple, rtyp, bsig) && *plength > dcutll
      && *plength < dcutlh && *pwidth > dcutwl && *pwidth < dcutwh
      && *pconc > dcutcl && *pconc < dcutch && dht > dcuthl)
    {
      /* set bsig (all events below dALPHA_CUT are considered signal events)
       */
      *bsig = (fabs ((float) *palpha) < dALPHA_CUT) ? true : false;

      bkeep = true;
    }

  return (bkeep);
}


static bool bdscuts (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply dynamical scuts - filter cuts are included
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* auxiliary variables
   */
  double dm, dm2, dmls, dmls2, dmcza;

  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to cut variables in event data of ntuple
   */
  static float *psize, *pdist, *pmdist, *paltdeg, *pwidth, *plength, *palpha;

  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      psize   = cts_pset_nam (pntuple, cSIZE);
      pdist   = cts_pset_nam (pntuple, cDIST);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      pmdist  = cts_pset_nam (pntuple, cMDIST);
      palpha  = cts_pset_nam (pntuple, cALPHA);
      plength = cts_pset_nam (pntuple, cLENGTH);
      paltdeg = cts_pset_nam (pntuple, cALTDEG);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* calculate auxiliary variables
   */
  dm = (double) *pmdist;
  dm2 = m2 (dm);

  dmls = log10 ((double) *psize) - dNOMLOGSIZE;
  dmls2 = m2 (dmls);

  dmcza = cos ((double) (90.f - *paltdeg) * dPIDIV180) - dNOMCOSZA;


  /* apply dynamical scuts - including filter cuts;
   */
  if (cut_funcs[FILTER] (pntuple, rtyp, bsig) &&

      *plength < cts_mcut (0.312335, -0.066047, 0.757547, 0.135425, 0.143054,
			   -0.990116, -0.121686, 0.270666, dmls, dmcza, dm, dmls2, dm2) &&

      *pwidth  < cts_mcut (0.125132, -0.030704, 0.027329, 0.077992, -0.023585,
			   0.055679, 0.003924, 0.006753, dmls, dmcza, dm, dmls2, dm2)   &&

      *pdist   < cts_mcut (1.009947, 0.0, 0.273654, 0.311418, 0.0,
			   -0.330804, 0.360062, 0.0, dmls, dmcza, dm, dmls2, dm2)       &&

      *pdist   > cts_mcut (0.720948, 0.0, 0.321872, 0.120119, 0.0,
			   0.200394, 0.053261, 0.0, dmls, dmcza, dm, dmls2, dm2))
    {
      /* set bsig (all events below dALPHA_CUT are considered signal events)
       */
      *bsig = (fabs ((float) *palpha) < dALPHA_CUT) ? true : false;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}


static bool bdscuts2 (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply dynamical scuts - filter cuts are included
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* auxiliary variables
   */
  double dm, dm2, dmls, dmls2, dmcza;

  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to cut variables in event data of ntuple
   */
  static float *psize, *pdist, *pmdist, *paltdeg, *pwidth, *plength, *palpha;

  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      psize   = cts_pset_nam (pntuple, cSIZE);
      pdist   = cts_pset_nam (pntuple, cDIST);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      pmdist  = cts_pset_nam (pntuple, cMDIST);
      palpha  = cts_pset_nam (pntuple, cALPHA);
      plength = cts_pset_nam (pntuple, cLENGTH);
      paltdeg = cts_pset_nam (pntuple, cALTDEG);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* calculate auxiliary variables
   */
  dm = (double) *pmdist;
  dm2 = m2 (dm);

  dmls = log10 ((double) *psize) - dNOMLOGSIZE;
  dmls2 = m2 (dmls);

  dmcza = cos ((double) (90.f - *paltdeg) * dPIDIV180) - dNOMCOSZA;


  /* apply dynamical scuts - including filter cuts;
   */
  if (cut_funcs[FILTER] (pntuple, rtyp, bsig) &&

      *plength < cts_mcut (0.302323, 0.010247, 0.020944, 0.011791, -0.003160,
			   0.171213, 0.015246, -0.027715 , dmls, dmcza, dm, dmls2, dm2) &&

      *pwidth  < cts_mcut (0.139207, -0.004137, 0.018650, 0.026070, 0.005189,
			   0.026045, 0.006197, 0.006252, dmls, dmcza, dm, dmls2, dm2)   &&

      *pdist   < cts_mcut (1.105956, 0.0, 0.948818, 0.001871, 0.0,
			   0.039692, -0.001145, 0.0, dmls, dmcza, dm, dmls2, dm2)       &&


      *plength > cts_mcut (0.184105, 0.018415, 0.282785, 0.005554, 0.003405,
			   0.009151, -0.059313, -0.057837, dmls, dmcza, dm, dmls2, dm2)  &&


      *pwidth  > cts_mcut (0.14459, -0.021306, -0.009976, -0.037579, -0.048138,
			   0.012733, 0.019532, -0.002441, dmls, dmcza, dm, dmls2, dm2)  &&


      *pdist   > cts_mcut (0.499818, 0.0, 0.0008, 0.001552, 0.0,
			   0.000568, 0.000210, 0.0, dmls, dmcza, dm, dmls2, dm2))
    {
      /* set bsig (all events below dALPHA_CUT are considered signal events)
       */
      *bsig = (fabs ((float) *palpha) < dALPHA_CUT) ? true : false;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}


static bool bds97 (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply dynamical scuts (new version) - filter cuts are included
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* auxiliary variables
   */
  static double dd, dd2, dmls, dmls2, dmcza, dcut_val;

  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to cut variables in event data of ntuple
   */
  static float *psize, *pdist, *paltdeg, *pwidth, *plength, *palpha;

  /* cut parameters
   */
  static double lengthup[8] = {0.316877, 0.003380, 0.433328, 0.016768,
			       0.000281, -0.001531, -0.000958, -0.001075};

  static double widthup[8] = {0.149567, -0.000418, -0.000299, -0.000208,
			      0.000017, 0.000626, 0.000928, -0.000423};

  static double distup[8] = {1.099489, 0.000000, -0.000657, -0.000799,
			     0.000000, -0.000732, -0.000825, 0.000000};

  static double alphaup[8] = {9.983561, -0.000401, -0.000536, -0.000596,
			      -0.000836, -0.000407, -0.000077, 0.000027};

  static double lengthlow[8] = {0.171591, 0.012283, -0.003187, -0.007617,
				0.001203, 0.005626, -0.000465, -0.000113};

  static double widthlow[8] = {0.070984, 0.001570, 0.001948, -0.001250,
			       -0.002671, 0.000338, -0.000526, -0.000344};

  static double distlow[8] = {0.511863, 0.000000, 0.008283, -0.002812,
			      0.000000, -0.003647, -0.002201, 0.000000};

  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      psize   = cts_pset_nam (pntuple, cSIZE);
      pdist   = cts_pset_nam (pntuple, cDIST);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      palpha  = cts_pset_nam (pntuple, cALPHA);
      plength = cts_pset_nam (pntuple, cLENGTH);
      paltdeg = cts_pset_nam (pntuple, cALTDEG);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* calculate auxiliary variables
   */
  dd = (double) *pdist;
  dd2 = m2 (dd);

  dmls = log ((double) *psize) - dNOMLOGSIZE;
  dmls2 = m2 (dmls);

  dmcza = cos ((double) (90.f - *paltdeg) * dPIDIV180) - dNOMCOSZA;


  /* apply dynamical cuts - including filter cuts;
   */
  if (cut_funcs[FILTER] (pntuple, rtyp, bsig)

      && *plength < (float) cts_mncut (lengthup, dmls, dmcza, dd, dmls2, dd2)
      && *plength > (float) cts_mncut (lengthlow, dmls, dmcza, dd, dmls2, dd2)

      && *pwidth < (float) cts_mncut (widthup, dmls, dmcza, dd, dmls2, dd2)
      && *pwidth > (float) cts_mncut (widthlow, dmls, dmcza, dd, dmls2, dd2)

      && *pdist < (float) cts_mncut (distup, dmls, dmcza, dd, dmls2, dd2)
      && *pdist > (float) cts_mncut (distlow, dmls, dmcza, dd, dmls2, dd2))
    {
      /* set bsig (all events below ALPHA-cut are considered signal events)
       */
      dcut_val = cts_mncut (alphaup, dmls, dmcza, dd, dmls2, dd2);
      *bsig = (fabs (*palpha) < (float) dcut_val) ? true : false;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}



static bool bds97loz (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply dynamical scuts (new version, low zenith angles)
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* auxiliary variables
   */
  static double dd, dd2, dmls, dmls2, dmcza, dcut_val, ddens;

  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to cut variables in event data of ntuple
   */
  static float *psize, *pdist, *paltdeg, *pwidth, *plength, *palpha;

  /* cut parameters
   */
  static double lengthup[8] = {0.317507, -0.003819, 0.012768, 0.001838,
			       0.000191, 0.004380, -0.000039, -0.000497};

  static double widthup[8] = {0.151382, -0.002983, 0.112445, 0.002240,
			      -0.001192, 0.004066, 0.000834, -0.000202};

  static double distup[8] = {1.106671, 0.000000, 0.008922, -0.002099,
			     0.000000, 0.124454, 0.001286, 0.000000};

  static double alphaup[8] = {9.973193, -0.000543, -0.000696, -0.000897,
			      -0.000598, -0.000063, 0.000405, 0.000194};

  static double lengthlow[8] = {0.178361, 0.013808, 0.019847, 0.023938,
				-0.035866, 0.005932, -0.000365, -0.000711};

  static double widthlow[8] = {0.069608, -0.000505, -0.001057, -0.001280,
			       -0.001308, -0.001001, -0.000532, -0.000411};

  static double distlow[8] = {0.610508, 0.000000, 0.011111, -0.001965,
			      0.000000, 0.000171, -0.000309, 0.000000};

  static double densitylow[8] = {1984.750048, 0.000000, 0.000000, 0.000000,
				 0.000000, 0.000000, 0.000000, 0.000000};

  static double densityup[8] = {47134.375000, 0.000000, 0.000000, 0.000000,
				0.000000, 0.000000, 0.000000, 0.000000};


  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      psize   = cts_pset_nam (pntuple, cSIZE);
      pdist   = cts_pset_nam (pntuple, cDIST);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      palpha  = cts_pset_nam (pntuple, cALPHA);
      plength = cts_pset_nam (pntuple, cLENGTH);
      paltdeg = cts_pset_nam (pntuple, cALTDEG);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* calculate auxiliary variables
   */
  dd = (double) *pdist;
  dd2 = m2 (dd);

  dmls = log ((double) *psize) - dNOMLOGSIZE;
  dmls2 = m2 (dmls);

  dmcza = cos ((double) (90.f - *paltdeg) * dPIDIV180) - dNOMCOSZA;
  ddens = (double) (*pwidth > 0.001f) ? *psize / (*pwidth * *plength) : 0.f;

  /* apply dynamical cuts - including filter cuts;
   */
  if (cut_funcs[FILTER] (pntuple, rtyp, bsig)

      && *plength < (float) cts_mncut (lengthup, dmls, dmcza, dd, dmls2, dd2)
      && *plength > (float) cts_mncut (lengthlow, dmls, dmcza, dd, dmls2, dd2)

      && *pwidth < (float) cts_mncut (widthup, dmls, dmcza, dd, dmls2, dd2)
      && *pwidth > (float) cts_mncut (widthlow, dmls, dmcza, dd, dmls2, dd2)

      && *pdist < (float) cts_mncut (distup, dmls, dmcza, dd, dmls2, dd2)
      && *pdist > (float) cts_mncut (distlow, dmls, dmcza, dd, dmls2, dd2)

      && ddens < (float) cts_mncut (densityup, dmls, dmcza, dd, dmls2, dd2)
      && ddens > (float) cts_mncut (densitylow, dmls, dmcza, dd, dmls2, dd2))
    {
      /* set bsig (all events below ALPHA-cut are considered signal events)
       */
      dcut_val = cts_mncut (alphaup, dmls, dmcza, dd, dmls2, dd2);
      *bsig = (fabs (*palpha) < (float) dcut_val) ? true : false;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}


static bool bds97hiz (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply dynamical scuts (new version, high zenith angles)
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* auxiliary variables
   */
  static double dd, dd2, dmls, dmls2, dmcza, dcut_val, ddens;

  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to cut variables in event data of ntuple
   */
  static float *psize, *pdist, *paltdeg, *pwidth, *plength, *palpha;

  /* cut parameters
   */
  static double lengthup[8] = {0.410715, 0.114222, 0.511112, 0.119497, 0.106856, 0.102950, 0.096983, 0.099765};

  static double widthup[8] = {0.129227, 0.016048, 0.040507, -0.000616, -0.000496, -0.000470, -0.000563, 0.001413};

  static double distup[8] = {1.089345, 0.000000, -0.003437, -0.000153, 0.000000, 0.000891, 0.002094, 0.000000};

  static double alphaup[8] = {9.669569, 0.028491, 0.504996, -0.017645, 0.051642, 20.859912, 0.172164, 0.020238};

  static double lengthlow[8] = {0.170294, 0.004587, -0.001119, -0.006826, -0.012532, -0.018239, -0.023945, -0.029651};

  static double widthlow[8] = {0.052321, -0.034689, -0.031879, -0.026828, 0.002466, -0.003241, -0.008947, -0.003875};

  static double distlow[8] = {0.561491, 0.000000, 2.758400, 0.100000, 0.000000, 4.000000, 0.000000, 0.000000};

  static double densitylow[8] = {-471.191413, 0.131393, 0.141254, 0.147794, 0.240271, 0.220248, 0.200226, 0.180203};

  static double densityup[8] = {26407.226562, 0.140158, 0.120135, 0.100113, 0.080090, 0.060068, 0.040045, 0.020023};


  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      psize   = cts_pset_nam (pntuple, cSIZE);
      pdist   = cts_pset_nam (pntuple, cDIST);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      palpha  = cts_pset_nam (pntuple, cALPHA);
      plength = cts_pset_nam (pntuple, cLENGTH);
      paltdeg = cts_pset_nam (pntuple, cALTDEG);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* calculate auxiliary variables
   */
  dd = (double) *pdist;
  dd2 = m2 (dd);

  dmls = log ((double) *psize) - dNOMLOGSIZE;
  dmls2 = m2 (dmls);

  dmcza = cos ((double) (90.f - *paltdeg) * dPIDIV180) - dNOMCOSZA;
  ddens = (double) (*pwidth > 0.001f) ? *psize / (*pwidth * *plength) : 0.f;

  /* apply dynamical cuts - including filter cuts;
   */
  if (cut_funcs[FILTER] (pntuple, rtyp, bsig)

      && *plength < (float) cts_mncut (lengthup, dmls, dmcza, dd, dmls2, dd2)
      && *plength > (float) cts_mncut (lengthlow, dmls, dmcza, dd, dmls2, dd2)

      && *pwidth < (float) cts_mncut (widthup, dmls, dmcza, dd, dmls2, dd2)
      && *pwidth > (float) cts_mncut (widthlow, dmls, dmcza, dd, dmls2, dd2)

      && *pdist < (float) cts_mncut (distup, dmls, dmcza, dd, dmls2, dd2)
      && *pdist > (float) cts_mncut (distlow, dmls, dmcza, dd, dmls2, dd2)

      && ddens < (float) cts_mncut (densityup, dmls, dmcza, dd, dmls2, dd2)
      && ddens > (float) cts_mncut (densitylow, dmls, dmcza, dd, dmls2, dd2))
    {
      /* set bsig (all events below ALPHA-cut are considered signal events)
       */
      dcut_val = cts_mncut (alphaup, dmls, dmcza, dd, dmls2, dd2);
      *bsig = (fabs (*palpha) < (float) dcut_val) ? true : false;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}


static bool bhza_cuts (HBOOK_FILE *pntuple, RUN_TYPE rtyp, bool *bsig)

  /* apply high ZA. cuts - filter cuts are included
   * pntuple - contains variables to acces Ntuple events
   * rtyp    - run type of events (MC, ON, OFF ...) (not used here)
   * bsig    - true for events in signal region, else false
   */
{
  /* auxiliary variables
   */
  double dm, dm2, dmls, dmls2, dmcza;

  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  /* pointers to cut variables in event data of ntuple
   */
  static float *psize, *pdist, *pmdist, *paltdeg, *pwidth, *plength, *palpha;

  bool bkeep;


  if (strcmp (cfile_name, pntuple->pname))

    /* cuts are applied to a new ntuple, so check for position
     * of used variables
     */    
    {
      /* get location of different variable names in event-data
       */
      psize   = cts_pset_nam (pntuple, cSIZE);
      pdist   = cts_pset_nam (pntuple, cDIST);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      pmdist  = cts_pset_nam (pntuple, cMDIST);
      palpha  = cts_pset_nam (pntuple, cALPHA);
      plength = cts_pset_nam (pntuple, cLENGTH);
      paltdeg = cts_pset_nam (pntuple, cALTDEG);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* calculate auxiliary variables
   */
  dm = (double) *pmdist;
  dm2 = m2 (dm);

  dmls = log10 ((double) *psize) - dNOMLOGSIZE;
  dmls2 = m2 (dmls);

  dmcza = cos ((double) (90.f - *paltdeg) * dPIDIV180) - dNOMCOSZA;


  /* apply dynamical scuts - including filter cuts;
   */
  if (cut_funcs[FILTER] (pntuple, rtyp, bsig) &&

      *plength < cts_mcut (0.24103, 0.139459, 0.62207, 0.664643, 0.201824,
		       0.706092, 0.355163, 0.350056, dmls, dmcza, dm, dmls2, dm2)    &&

      *pwidth  < cts_mcut (0.120748, -0.013991, -0.040156, -0.008711, -0.006767,
		       0.062188, 0.057057, -0.001869, dmls, dmcza, dm, dmls2, dm2)   &&

      *pdist   < cts_mcut (1.126741, 0.0, 0.211918, -0.297835, 0.0, -0.165264,
		       0.235489, 0.0, dmls, dmcza, dm, dmls2, dm2)                   &&

      *plength > cts_mcut (0.162537072, 0.004144, 0.003119, -0.003170, -0.001088,
		       0.002088, -0.013223, -0.018980, dmls, dmcza, dm, dmls2, dm2)  &&

      *pwidth  > cts_mcut (0.05834475, -0.019649, -0.020870, -0.018830, -0.013345,
		       -0.012524, -0.011768, -0.002691, dmls, dmcza, dm, dmls2, dm2) &&

      *pdist   > cts_mcut (0.490393, 0.0, 0.581015, 0.465913, 0.0,
		       0.062311, 0.034295, 0.0, dmls, dmcza, dm, dmls2, dm2))
    {
      /* set bsig (all events below dALPHA_CUT are considered signal events)
       */
      *bsig = (fabs ((float) *palpha) < dALPHA_CUT) ? true : false;

      bkeep = true;
    }

  else
    bkeep = false;

  return (bkeep);
}


bool cts_bcuts (CUT_TYPE ctyp, HBOOK_FILE *pntuple, RUN_TYPE rtyp,
		bool (*pcut_func) (HBOOK_FILE *), bool *bsig)

  /* interface to all defined cuts;
   * checks rtyp and ctyp too
   * pcut_func - user supplied cut function (or null)
   */
{
  bool bcutval;

  /* use dummy cut if there's no user supplied cut
   */
  if (pcut_func == NULL)
    pcut_func = bnocut2;


  if (0 <= rtyp && rtyp < MAX_RUN_TYPE && 0 <= ctyp && ctyp < MAX_CUT_TYPE)
    {
      bcutval = (* cut_funcs[ctyp]) (pntuple, rtyp, bsig);
      bcutval &= pcut_func (pntuple);
    }

  else
    cts_merror ("%s: unknown cut or data type!\n", "cts_bcuts");

  return (bcutval);
}


CUT_TYPE cts_cut_type (char *pcutnam)

  /* this functions returns the enum corresponding to the passe cut-name
   * or 'MAX_CUT_TYPE' if cut is unknown
   */
{
  int i;

  CUT_TYPE ctyp;


  ctyp = MAX_CUT_TYPE;

  for (i = 0; i < MAX_CUT_TYPE; i++)
    {
      if (strncmp (pcutnam, &cut_names[i][0], (size_t) iMAX_CUT_NAME_LEN) == 0)
	ctyp = (CUT_TYPE) i;
    }

  return (ctyp);
}
