#include <string.h>
#include <stdio.h>
#include <math.h>
#include "ctshbook.h"
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctscuts.h"


/* iNR_BLOCK_EVTS - one block is able to store 5000 data structs
 */
#define iNR_BLOCK_EVTS 5000


/*
 * declaration of local functions
 */
static bool bnocut (HBOOK_FILE *);
static void *pfill_time_s   (HBOOK_FILE *, void *, bool, bool, int);
static void *pfill_energy_s (HBOOK_FILE *, void *, bool, bool, int);
static void *pfill_signal_s (HBOOK_FILE *, void *, bool, bool, int);
static void *pfill_data_s   (HBOOK_FILE *, void *, bool, bool, int);


/* array of all defined fill functions - must match definition of
 * DS_TYPE in cut.h
 */
static void *(*const fill_struct_funcs [MAX_DS_TYPE]) () =
{pfill_data_s, pfill_signal_s, pfill_time_s, pfill_energy_s};


/* array of struct-sizes - must match definition of DS_TYPE
 * in cut.h
 */
static const size_t sstruc_size[MAX_DS_TYPE] =
{sizeof (DLIST_EL), sizeof (SLIST_EL), sizeof (TLIST_EL), sizeof (ELIST_EL)};


static bool bnocut (HBOOK_FILE *dummy)

  /* function used when ther's no user supplied cut function - it just
   * returns true (passed data are ignored)
   */
{
  return (true);
}


static void *pfill_data_s (HBOOK_FILE *pntuple, void *pstruct, bool bfirst,
			   bool bsig, int inr)

  /* function fills calcutta type structures 'DLIST_EL';
   * pntuple - ntuple to get the data from
   * pstruct - position where to store the struct
   * bfirst  - bool indicating a new ntuple
   * bsig    - bool indicating an event in the signal region ( not used here)
   * inr     - event number (not used here)
   * return value: pointer right behind the filled struct
   */
{
  /* variables which are used to derive virtual source position:
   * da - signed alpha, dz - ZA, dd - dist, ds - size, dl - length,
   * dw - width, dp - phi (angle c.o.m. - x axis)
   */
  double da, dz, dd, ds, dl, dw, dp;

  /* some auxiliary variables; dpsi is needed but not used here
   */
  double dtmp, dpsi, dtheta;


  /* pointer to struct which is going to be filled
   */
  DLIST_EL *p2dlist_el;


  /* pointers to access ntuple event variables
   */
  static float *plength, *pwidth, *psize, *palpha, *pdist, *pdxbar, *paltdeg;
  static float *pdybar;

  if (bfirst)
    {
      /* get location of variable names in ntuple
       */
      plength  = cts_pset_nam (pntuple, cLENGTH);
      pwidth   = cts_pset_nam (pntuple, cWIDTH);
      psize    = cts_pset_nam (pntuple, cSIZE);
      paltdeg  = cts_pset_nam (pntuple, cALTDEG);
      palpha   = cts_pset_nam (pntuple, cALPHA);
      pdist    = cts_pset_nam (pntuple, cDIST);
      pdxbar   = cts_pset_nam (pntuple, cXBAR_DEG);
      pdybar   = cts_pset_nam (pntuple, cYBAR_DEG);
    }


  /* fill structure
   * (switch sign of alpha)
   */
  p2dlist_el = (DLIST_EL *) pstruct;

  p2dlist_el->dza       = dz = 90. - (double) *paltdeg;
  p2dlist_el->dmcza     = cos (dz * dPIDIV180) - dNOMCOSZA;

  p2dlist_el->dlength   = dl = (double) *plength;
  p2dlist_el->dwidth    = dw = (double) *pwidth;
  p2dlist_el->ddist     = dd = (double) *pdist;
  p2dlist_el->dalpha    = da = - (double) *palpha;
  p2dlist_el->ddist_sq  = m2 ((double) *pdist);

  dtmp = log ((double) *psize) - dNOMLOGSIZE;
  p2dlist_el->dmls      = dtmp;
  p2dlist_el->dmls_sq   = m2 (dtmp);

  dtmp = (double) (*plength * *pwidth) / (double) *psize;
  p2dlist_el->ddens     = (dtmp > 0.) ? 1. / dtmp : 0.;

  /* get position of virtual source into struct
   */
  ds = (double) *psize;
  dp = atan2 ((double) *pdybar, (double) *pdxbar);

  cts_msource_pos (da, dz, dd, ds, dl, dw, dp, dpsi, dtheta);

  p2dlist_el->dtheta_sq = m2 (dtheta);


  p2dlist_el->p2prev    = NULL;
  p2dlist_el->p2next    = NULL;


  return (++p2dlist_el);
}


static void *pfill_signal_s (HBOOK_FILE *pntuple, void *pstruct, bool bfirst,
			     bool bsig, int inr)

  /* function fills signal type structures 'SLIST_EL';
   * pntuple - ntuple to get the data from
   * pstruct - position where to store the struct
   * bfirst  - bool indicating a new ntuple
   * bsig    - bool indicating an event in the signal region
   * inr     - event
   * return value: pointer right behind the filled struct
   */
{
  /* variables which are used to derive virtual source position:
   * da - signed alpha, dz - ZA, dd - dist, ds - size, dl - length,
   * dw - width, dp - phi (angle c.o.m. - x axis)
   */
  double da, dz, dd, ds, dl, dw, dp;

  /* pointer to struct which is going to be filled
   */
  SLIST_EL *p2slist_el;


  /* pointers to access ntuple event variables
   */
  static float *paltdeg, *palpha, *prunnum, *pfra200ns, *pdxbar, *pdybar;
  static float *pnoonsecs, *pnoonmjd, *plength, *pwidth, *pdist, *psize;

  if (bfirst)
    {
      /* get location of variable names in ntuple
       */
      pnoonmjd  = cts_pset_nam (pntuple, cNOONMJD);
      pnoonsecs = cts_pset_nam (pntuple, cNOONSECS);
      pfra200ns = cts_pset_nam (pntuple, cFRA200NS);
      paltdeg   = cts_pset_nam (pntuple, cALTDEG);
      prunnum   = cts_pset_nam (pntuple, cCTRUNNUM);
      plength   = cts_pset_nam (pntuple, cLENGTH);
      pwidth    = cts_pset_nam (pntuple, cWIDTH);
      pdist     = cts_pset_nam (pntuple, cDIST);
      psize     = cts_pset_nam (pntuple, cSIZE);
      pdxbar    = cts_pset_nam (pntuple, cXBAR_DEG);
      pdybar    = cts_pset_nam (pntuple, cYBAR_DEG);
      palpha    = cts_pset_nam (pntuple, cALPHA);
    }


  /*
   * fill structure
   *
   */
  p2slist_el = (SLIST_EL *) pstruct;
  p2slist_el->bsig      = bsig;
  p2slist_el->inoonmjd  = (int) (*pnoonmjd + 0.5f);
  p2slist_el->irunnum   = (int) (*prunnum + 0.5f);
  p2slist_el->dxbar     = (double) *pdxbar;
  p2slist_el->dybar     = (double) *pdybar;
  p2slist_el->devt_time = (double) (*pnoonsecs * dSECS_IN_200ns + *pfra200ns);
  p2slist_el->dza       = dz = 90. - (double) *paltdeg;
  p2slist_el->dalpha    = da = - (double) *palpha;

  /* get position of virtual source into struct
   */
  dd = (double) *pdist;
  ds = (double) *psize;
  dw = (double) *pwidth;
  dl = (double) *plength;
  dp = atan2 ((double) *pdybar, (double) *pdxbar);

  cts_msource_pos (da, dz, dd, ds, dl, dw, dp, p2slist_el->dpsi,
		   p2slist_el->dtheta);

  p2slist_el->p2prev    = NULL;
  p2slist_el->p2next    = NULL;

  return (++p2slist_el);
}


static void *pfill_energy_s (HBOOK_FILE *pntuple, void *pstruct, bool bfirst,
			     bool bsig, int inr)

  /* function fills energy type structures 'ELIST_EL';
   * pntuple - ntuple to get the data from
   * pstruct - position where to store the struct
   * bfirst  - bool indicating a new ntuple
   * bsig    - bool indicating an event in the signal region ( not used here)
   * inr     - event number (not used here)
   * return value: pointer right behind the filled struct
   */
{
  double dimpact;
  float fdens;

  /* pointer to struct which is going to be filled
   */
  ELIST_EL *p2elist_el;


  /* pointers to access ntuple event variables
   */
  static float *paltdeg, *plength, *pwidth, *pdist, *psize, *pfilter, *pimpact;
  static float *pxbar, *pybar;


  if (bfirst)
    {
      /* get location of variable names in ntuple
       */
      psize     = cts_pset_nam (pntuple, cSIZE);
      pdist     = cts_pset_nam (pntuple, cDIST);
      pxbar     = cts_pset_nam (pntuple, cXBAR_DEG);
      pybar     = cts_pset_nam (pntuple, cYBAR_DEG);
      pwidth    = cts_pset_nam (pntuple, cWIDTH);
      paltdeg   = cts_pset_nam (pntuple, cALTDEG);
      plength   = cts_pset_nam (pntuple, cLENGTH);
      pfilter   = cts_pset_nam (pntuple, cFILTER);
      pimpact   = cts_pset_nam (pntuple, cIMPACTRP);
    }


  /*
   * fill structure
   *
   */
  p2elist_el = (ELIST_EL *) pstruct;

  p2elist_el->ievt_num = inr;
  p2elist_el->dza      = 90. - (double) *paltdeg;
  p2elist_el->dcza     = cos (p2elist_el->dza * dPIDIV180);
  p2elist_el->dlogsize = log ((double) *psize);
  p2elist_el->ddist    = (double) *pdist;
  p2elist_el->dwidth   = (double) *pwidth;
  p2elist_el->dlength  = (double) *plength;

  fdens = (*pwidth > 0.001f) ? *psize / (*pwidth * *plength) : 0.f;
  p2elist_el->ddens = (double) fdens;
  p2elist_el->de_mc = (double) *pfilter / 100.;

  /* impact par. and angle phi are stored in one number
   * pimpact = impact * 1000 + phi * 100
   */
  dimpact = (double) (*pimpact / 1000.f);
  p2elist_el->dphi_mc  = 10. * modf (dimpact, &p2elist_el->di_mc);
  p2elist_el->dphi_est = atan2 (-(double) *pybar, (double) *pxbar) + M_PI;
  p2elist_el->p2prev   = NULL;
  p2elist_el->p2next   = NULL;


  return (++p2elist_el);
}


static void vcorr_time (float *pdate, float *psecs, float *pfracsec,
			float *p2run_nr)

  /* undos corrections to rubidium clock time (implemented in Muchachos)
   * and transfers time into imager time format
   * used by 'pfill_time_s ()'
   */

{
  /* imager type time variables (int ifrac200ns is used to have the
   * second fractions in units of 200ns)
   */
  double dmjd, dnoonmjd, dnoonmjd_frac, dsecs;
  int ifrac200ns;



  dmjd = (double) *pdate + (double) *psecs / dSECS_PER_DAY;

  if (*p2run_nr < 2395)
    dmjd += 3. / dSECS_PER_DAY;

  else if (*p2run_nr < 3046)
    dmjd += 4. / dSECS_PER_DAY;

  else if (*p2run_nr < 3281)
      ;
  
  else if (*p2run_nr < 3454)
    dmjd -= 1.;

  else if (*p2run_nr < 3472)
    dmjd += 10. / dSECS_PER_DAY;

  else if (*p2run_nr < 3554)
    dmjd += 1. + 4. / dSECS_PER_DAY;

  else if (*p2run_nr < 3561)
      ;

  else if (*p2run_nr < 4079)
    dmjd += 10. / dSECS_PER_DAY;

  else
    dmjd += 11. / dSECS_PER_DAY;


  /* convert into imager time format
   */
  dnoonmjd_frac = modf (dmjd, &dnoonmjd);
  dsecs = dnoonmjd_frac * dSECS_PER_DAY;

  /* noonMJD and MJD differ by 1
   */
  if (dsecs < dMJD_DIFF_SECS)
    {
      dnoonmjd -= 1.;
      dsecs += dMJD_DIFF_SECS;
    }
  else
    {
      dsecs -= dMJD_DIFF_SECS;
    }

  ifrac200ns = (int) (*pfracsec * dSECS_IN_200ns + 0.5);


  /* change time vars. of event data to imager time format
   */
  *pdate    = (float) dnoonmjd;
  *psecs    = (float) dsecs;
  *pfracsec = (float) ifrac200ns;
}


static void *pfill_time_s (HBOOK_FILE *pntuple, void *pstruct, bool bfirst,
			   bool bsig, int inr)

  /* function fills coinc type structures 'TLIST_EL';
   * pntuple - ntuple to get the data from
   * pstruct - position where to store the struct
   * bfirst  - bool indicating a new ntuple
   * bsig    - bool indicating an event in the signal region ( not used here)
   * inr     - event number (not used here)
   * return value: pointer right behind the filled struct
   */
{
  /* pointers to access ntuple event variables
   */
  static float *pnoonsecs, *pnoonmjd, *pfra200ns, *prunnum;

  /* pointer to struct which is going to be filled
   */
  TLIST_EL *p2tlist_el;

  /* enum to mark the corresponding time representation
   */
  static TIME_REP etime_rep;


  if (bfirst)
    {
      /* check which time representation we're dealing with
       * (look for a unique variable name)
       */
      if (strstr (pntuple->pvarnam, cFRA200NS) != NULL)
	{
	  etime_rep = IMAGER;

	  /* get location of variable names in ntuple
	   */
	  pnoonmjd  = cts_pset_nam (pntuple, cNOONMJD);
	  pnoonsecs = cts_pset_nam (pntuple, cNOONSECS);
	  pfra200ns = cts_pset_nam (pntuple, cFRA200NS);
	}

      else if (strstr (pntuple->pvarnam, cMUCHACHOS_SECS) != NULL)
	{
	  etime_rep = MUCHACHOS;

	  /* get location of variable names in ntuple
	   */
	  pnoonmjd  = cts_pset_nam (pntuple, cMUCHACHOS_MJD);
	  pnoonsecs = cts_pset_nam (pntuple, cMUCHACHOS_SECS);
	  pfra200ns = cts_pset_nam (pntuple, cMUCHACHOS_200ns);
	  prunnum   = cts_pset_nam (pntuple, cMUCHACHOS_RUN);
	}

      else
	cts_merror ("%s: unknown time format!\n", pntuple->pname);
    }


  /* apply corrections and transform into IMAGER time format
   * if neccessary
   */
  if (etime_rep == MUCHACHOS)
    vcorr_time (pnoonmjd, pnoonsecs, pfra200ns, prunnum);



  /* fill structure
   */
  p2tlist_el = (TLIST_EL *) pstruct;

  p2tlist_el->ievent_nr = inr;
  p2tlist_el->inoonmjd  = (int) (*pnoonmjd + 0.5f);
  p2tlist_el->devt_time = (double) (*pnoonsecs * dSECS_IN_200ns + *pfra200ns);

  p2tlist_el->pcoinc_evts = NULL;
  p2tlist_el->p2prev      = NULL;
  p2tlist_el->p2next      = NULL;

  /* return pointer to position of next struct
   */
  return (++p2tlist_el);
}


#define CFUNCNAME "cts_pevtdat"

void *cts_pevtdat (HBOOK_FILE *pntuple, SORT_TYPE styp, DS_TYPE dstyp,
		   CUT_TYPE ctyp, RUN_TYPE rtyp, bool (*pcut) (HBOOK_FILE *),
		   int *ht_size)

  /* function to read event data into a linked list;
   * it provides an interface to all defined sort functions;
   * input:
   * pntuple   - input ntuple
   * styp      - sorting algorithm
   * dstyp     - evt. data struct to fill
   * ctyp      - cut used for ntuple after cuts
   * rtyp      - run type of events (MC, ON, OFF ...)
   * pcut      - user supplied cut function (or null)
   * ht_size   - size of hash table (if provided by the sorting routine)
   */
{
  /* pstruct     - points to position of the current/next struct
   * pfree_block - points to position where the unused
   *               event data block starts
   * plist       - points to root of the linked list
   */
  void *pstruct, *pfree_block, *plist;

  int ievts, imax, i, iblock_size;

  /* bfirst - marks first data struct passed to sort functions
   * bsig   - marks signal region event
   */
  static bool bfirst, bsig;

  /* check valid range of all enums
   */
  if (0 > rtyp || rtyp > MAX_RUN_TYPE || 0 > ctyp  || ctyp  > MAX_CUT_TYPE ||
      0 > styp || styp > MAX_SORT_TYPE || 0 > dstyp || dstyp > MAX_DS_TYPE)

    cts_merror ("%s: invalid enum!\n", CFUNCNAME);


  /* use dummy cut if there's no user supplied cut
   */
  if (pcut == NULL)
    pcut = bnocut;


  /* init variables
   */
  ievts = 0;
  bfirst = true;
  pstruct = plist = NULL;
  imax = pntuple->inr_events;
  iblock_size = sstruc_size[dstyp] * iNR_BLOCK_EVTS;

  /* loop over all events
   */
  for (i = 1; i <= imax; i++)
    {
      /* read in event information
       */
      cts_hgnf (pntuple, i);

      /* if event survives the cuts, put data into struct
       * and the struct in a linked list
       */
      if (cts_bcuts (ctyp, pntuple, rtyp, pcut, &bsig))
	{
	  /* allocate memory for data structs if needed
	   */
	  if (ievts % iNR_BLOCK_EVTS == 0 || bfirst)
	    pstruct = cts_mmalloc (char, iblock_size, CFUNCNAME);


	  /* fill data struct at position 'pstruct'; return pointer to
	   * beginning of unused memory block
	   */
 	  pfree_block = (* fill_struct_funcs[dstyp]) (pntuple, pstruct, bfirst,
						      bsig, i);

	  /* call sorting function - put pstruct into linked list;
	   * plist points to root of list or hash table; it might be
	   * changed within the sorting function
	   */
	  cts_vsort_struc (pstruct, &plist, bfirst, pntuple, ht_size, dstyp,
			   styp);


	  /* update pointers and variables;
	   */
	  ievts++;
	  bfirst = false;
	  pstruct = pfree_block;
	}
    }

  return (plist);
}

#undef CFUNCNAME


#define CFUNCNAME "cts_venergy"

void cts_venergy (HBOOK_FILE *pntuple)

  /* this function returns the estimated impact parameter and energy for
   * a given event
   * input:
   * pntuple - struct containing data for one event
   * output:
   * pntuple - energy and impact parameter variable is set
   */
{
  /* array to store hbook file name - it's used to check whether we're
   * dealing with a new ntuple (FILENAME_MAX is defined in stdio.h)
   */
  static char cfile_name[FILENAME_MAX] = "";

  float fdens, fcza;

  /* function parameters derived from MC-fit
   */
/*   float fepar[5] = {0.205844, 0.000172669, -0.00312944, 0.888975, 0.0017329}; */
/*   float fipar[4] = {26.1133, 131.337, -345.976, 0.486899}; */
  float fepar[5] = {0.0724594, 0.000209119, -0.00307397, 1.22249, 0.00157545};
  float fipar[4] = {27.1577, 127.474, -344.105, 0.811356};


  /* pointers to variables in event data of ntuple
   */
  static float *pdist, *psize, *pwidth, *plength, *paltdeg, *penergy, *pimpact;


  if (strcmp (cfile_name, pntuple->pname))
    {
      /* get location of different variable names in event-data
       */
      pdist   = cts_pset_nam (pntuple, cDIST);
      psize   = cts_pset_nam (pntuple, cSIZE);
      pwidth  = cts_pset_nam (pntuple, cWIDTH);
      plength = cts_pset_nam (pntuple, cLENGTH);
      penergy = cts_pset_nam (pntuple, cENERGY);
      paltdeg = cts_pset_nam (pntuple, cALTDEG);
      pimpact = cts_pset_nam (pntuple, cIMPACTRP);

      /* reset file name
       */
      strncpy (cfile_name, pntuple->pname, (size_t) FILENAME_MAX);
    }


  /* first calculate impact parameter, energy afterwards
   */
  fdens = (float) mdiv (*psize, *pwidth * *plength);
  fcza  = (float) cos ((90. - (double) *paltdeg) * dPIDIV180);

  *pimpact = (fipar[0] + fipar[1] * *pdist + fipar[2] * *pwidth)
    * (1.f / fcza + fipar[3] / m2 (fcza)) / (1.f + fipar[3]);

  *penergy = (fepar[0] + fepar[1] * fdens + fepar[2] * *pimpact
	      + fepar[3] * *plength + fepar[4] * *plength * fdens)
    * (1.f / m2 (fcza) + 0.2f / m4 (fcza)) / 1.2f;
}

#undef CFUNCNAME
