#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include "ctsbase.h"
#include "ctshbook.h"


/*
 * definition of global variables
 */
int pawc_[iNWPAWC];


/*
 *  variables with file scope:
 *
 */
/* counter for open luns; set by vopen_hbook_file and kuopen
 */
static int imaxlun = 20;

/* (since hgnf doesn't work correct if another hbook file
 *  was opened in between, we have to remember what lun was
 *  opened last)
 *  it's set by hropen and used by hgnf
 */
static int ilastlun;

#ifdef USE_LINKS
/* since hbook cannot handle upper case letters in file names
 * we create links (with lower case names) onto the hbook files;
 * ilink_nr is a counter for the number of links created so far
 * it's set by vopen_hbook_file and used by vrm_link
 */
static int ilink_nr = iLINK_START_NR;
#endif

/*
 * declaration of local functions
 *
 */
static void cts_hropen (int, char *, char *, char, int *);
static void cts_hrin (int, int, int);
static void cts_hidall (int *, int *);
static void cts_hgnpar (int *, char *);
static void cts_hnoent (int *, int *);
static void cts_hgiven (int *, char *, int *, char *, float *, float *);
static void cts_hbookn (int, int, int, char *, char *);
static void cts_vrm_link ();



/*
 *  functions
 *
 */
static void cts_hlimit (int bsize)
{
  extern void hlimit_ ();

  hlimit_ (&bsize); 
}


static void cts_hropen (int lun, char *clun, char *name, char opt, int *reclen)
{
  extern void  hropen_ ();
  int stat;
  static char popt[2] = " ";


  /* set file scope variable to remember last lun
   */
  ilastlun = lun;

  /* pass opt as null terminated string
   */
  sprintf (popt,"%c", opt);


  hropen_ (&lun, clun, name, popt, reclen, &stat, 
	   strlen (clun), strlen (name), strlen (popt));

   if (stat)
     cts_merror ("%s: cts_hropen failed.\n", name);
}


static void cts_hrin (int id, int cycle, int offset)
{
  extern void hrin_ ();
 
  hrin_ (&id, &cycle, &offset);
}


void cts_hrout (int id, int ilun, char *opt)
{
  /* output from hrout -- not used here
   */
  int cycle; 

  char *plun;
  extern void hrout_ ();

  plun = cts_pint_to_str (ilun, "//LUN%d");
  cts_hcdir (plun, "");

  hrout_ (&id, &cycle, opt, strlen (opt));
}


static void cts_hidall (int *idvec, int *inr)
{
  extern void hidall_ ();

  hidall_ (idvec, inr);
}


void cts_hgive (HISTO *h)
{
  extern void hgive_ ();
  /* inwt - number of words for title
   * ploc - address of histogram in pawc-block
   * both are not used here
   */
  int inwt, *ploc;

  /* allocate memory for histogram-name, if h->phname points to
   * NULL, otherwise it is assumed that the calling routine provides
   * this memory
   */
  if (h->phname == NULL)
    h->phname = cts_mmalloc (char, iTITLELEN, "cts_hgive");

  hgive_ (&h->iid, h->phname, &h->ixbin, &h->fxlow, &h->fxhigh,
	  &h->iybin, &h->fylow, &h->fyhigh, &inwt, &ploc, strlen (h->phname));
}


float cts_hstati (int id, int icase, char *choice, int inum)
{
  extern float hstati_ ();

  return (hstati_ (&id, &icase, choice, &inum, strlen (choice)));
}


void cts_hrend (HBOOK_FILE *hfile)
{
  extern void hrend_ ();
  char *plun;

  plun = cts_pint_to_str (hfile->ilun, "LUN%d");

  hrend_ (plun, strlen (plun));
}


static void cts_hgnpar (int *idvec, char *c)
{
  extern void hgnpar_ ();

  hgnpar_ (idvec, c, strlen (c));
}


static void cts_hnoent (int *idvec, int *inoent)
{
  extern void hnoent_ ();

  hnoent_ (idvec, inoent);
}


static void cts_hgiven (int *idvec, char *tit, int *nvar, char *var_nam,
			float *min, float *max)
{
  extern int  hgiven_ ();

  hgiven_ (idvec, tit, nvar, var_nam, min, max, strlen (tit), iVARNAMELEN);

  if (*nvar == 0)
    cts_merror ("Ntuple id %d: cts_hgiven failed!\n", *idvec);
}


void cts_hgnf (HBOOK_FILE *hfile, int evtnr)
{
  extern int hgnf_ ();
  int ierr;

  /* use file-scope variable to check last openend lun
   * if different from hfile->lun we have to close and reopen
   * ntuple to have hgnf working correctly
   */
  if (ilastlun != hfile->ilun)
    {
      cts_hcdir ("//", " ");
      cts_hrend (hfile);
      cts_hreopen (hfile);
    }


  hgnf_ (&hfile->iid, &evtnr, hfile->pevtdat, &ierr);

  if (ierr != 0)
    cts_merror ("%s: cts_hgnf failed.\n", hfile->pname);
}


void cts_hfn (HBOOK_FILE *hfile)
{
  extern void hfn_ ();

  hfn_ (&hfile->iid, hfile->pevtdat);
}


void cts_hfill (int id, float x, float y, float weight)
{
  extern int hfill_ ();

  hfill_ (&id, &x, &y, &weight);
}


void cts_hpak (int id, float *cont)
{
  extern void hpak_ ();

  hpak_ (&id, cont);
}


void cts_hpakad (int id, float *cont)
{
  extern void hpakad_ ();

  hpakad_ (&id, cont);
}


void cts_hpake (int id, float *err)
{
  extern void hpake_ ();

  hpake_ (&id, err);
}


void cts_hunpak (int id, float *cont, char *choice, int inum)
{
  extern void hunpak_ ();

  hunpak_ (&id, cont, choice, &inum, strlen (choice));
}


void cts_hunpke (int id, float *cont, char *choice, int inum)
{
  extern void hunpke_ ();

  hunpke_ (&id, cont, choice, &inum, strlen (choice));
}


void cts_hfpak1 (int id, int nid, float *v, int n)
{
  extern void hfpak1_ ();

  hfpak1_ (&id, &nid, v, &n);
}


static void cts_hbookn (int id, int nvar, int nwbuff, char *name, char *varnam)
{
  extern int hbookn_ ();
  char *htit = "";      /* title of Ntuple -- not needed */

  hbookn_ (&id, htit, &nvar, name, &nwbuff , varnam, strlen (htit),
	   strlen (name), iVARNAMELEN);
}


void cts_hcdir (char *path, char *opt)
{
  extern int hcdir_ ();

  hcdir_ (path, opt, strlen (path), strlen (opt));
}


void cts_hopera (int id1, char *op, int id2, int id3, float f1, float f2)
{
  extern void hopera_ ();

  hopera_ (&id1, op, &id2, &id3, &f1, &f2, strlen (op));
}


void cts_hreset (int id, char *ptit)
{
  extern void hreset_ ();

  hreset_ (&id, ptit, strlen (ptit));
}


void cts_hdelet (int id)
{
  extern void hdelet_ ();

  hdelet_ (&id);
}


void cts_hcopy (int id1, int id2, char *chtitl)
{
  extern void hcopy_ ();

  hcopy_ (&id1, &id2, chtitl, strlen (chtitl));
}


float cts_hxy (int id, float fx, float fy)
{
  extern float hxy_ ();

  return hxy_ (&id, &fx, &fy);
}


void cts_htitle (char *title)
{
  extern void htitle_ ();

  htitle_ (title, strlen (title));
}


void cts_vinit_hbook ()

  /* function used to initialise Common paw block
   */
{
  static bool binit = true;

  if (binit)
    {
      /* inform HBOOK about the memory block size
       */
      cts_hlimit (iNWPAWC);

      binit = false;
    }
}


#ifdef USE_LINKS

static void cts_vrm_link ()

  /* removes all created links when exit is called;
   * this function uses the file scope variable ilink_nr
   * (set by vopen_hbook_file) which is a counter for the
   * number of links created
   */
{
  /* FILENAME_MAX is defined in <stdio.h>
   */
  int i, inrchar;
  char clink_nam[FILENAME_MAX];
  char cfile_nam[FILENAME_MAX];

  for (i = 1; i <= ilink_nr; i++)
    {
      sprintf (clink_nam, cLINKNAME, i);

      /* read in file name (onto which clink_nam points) and delete
       * link afterwards;
       * readlink returns the number of chars written to cfile_nam
       */
      inrchar = readlink (clink_nam, cfile_nam, (size_t) FILENAME_MAX);

      if (inrchar > 0)
	{
	  /* readlink does not append a NUL character to cfile_nam
	   */
	  cfile_nam[inrchar] = '\0';

	  if (unlink (clink_nam) != 0)
	    {
	      fprintf (stderr, "%s\n", strerror (errno));
	      fprintf (stderr, "unable to remove link %s!\n", clink_nam);
	    }

	  else
	    fprintf (stderr, "(removed link %s to %s)\n", clink_nam, cfile_nam);
	}
    }
}

#endif


#define CFUNCNAME "cts_pget_varnam_str"

char *cts_pget_varnam_str (char *cvarnam[], int inpars)

  /* transforms array of variable names into one string, souitable for
   * creating the ntuple variable names
   */
{
  int i;
  char *pstr, *p;

  /* first fill string with blanks
   */
  i = inpars * iVARNAMELEN;
  pstr = cts_mmalloc (char, i, CFUNCNAME);
  memset ((void *) pstr, ' ', (size_t) i);

  /* copy variable names to string (without terminating '\0')
   */
  for (p = pstr, i = 0; i < inpars; i++)
    {
      memcpy ((void *) p, (const void *) cvarnam[i], strlen (cvarnam[i]));
      p += iVARNAMELEN;
    }

  return (pstr);
}

#undef CFUNCNAME


#define CFUNCNAME "cts_vopen_hbook_file"

void cts_vopen_hbook_file (HBOOK_FILE *phfile)

  /* determine information on hbook-file and open it;
   * the memory for 4 arrays (to store event data) is allocated;
   * this function sets the file scope variable ilink_nr
   * (used by vrm_link) which is a counter for the number of
   * links created and imaxlun
   */
{
  int ilun, inull = 0;
  int idvect[iMAX_HIST_NR] = {0};
  int ilrec, invar, inoent, inwb, inrh;

 /* array for maximum and minum values of Ntuple
  */
  static float *pntuple_rhigh, *pntuple_rlow;

  /* array in which data of one event is stored
   */
  static float *pevent_data;

  /* contains histogram and lun name
   */
  char ctitle[iTITLELEN];
  char *plun;

  /* array in which all variable names of one Ntuple are stored
   */
  static char *pcvar_names;

  /* used to retrieve information on histograms (including ntuples)
   */
  HISTO histo_dat;


#ifdef USE_LINKS

  /* create a symbolic link onto hbook-file; this link is in lower case
   * letters only and is used instead of the hbook-file name
   * (this is done because hbook cannot handle file names with
   *  upper case letters in)
   * FILENAME_MAX is defined in <stdio.h>; global variable errno is set
   */
  char *plink_name;
  char clink_buf[FILENAME_MAX];

  static bool binit = true;


  /* allocate memory for link name
   */
  plink_name = cts_mmalloc (char, FILENAME_MAX, CFUNCNAME);

  /* create a new link and replace file-name by link-name
   * (first check for old links)
   */
  ilink_nr++;
  sprintf (plink_name, cLINKNAME, ilink_nr);

  if (0 < readlink (plink_name, clink_buf, (size_t) FILENAME_MAX))
    {
      /* link already exists - give warning message and remove old link
       */
      fprintf (stdout, "found old cts-link - trying to remove it ...");

      errno = 0;
      if (unlink (plink_name) != 0)
	{
	  fprintf (stdout, "\n%s\n", strerror (errno));
	  cts_merror ("unable to remove link %s!\n", plink_name);
	}

      else
	fprintf (stdout, " done\n");
    }


  errno = 0;
  if (symlink (phfile->pname, plink_name) != 0)
    {
      fprintf (stderr, "%s\n", strerror (errno));
      cts_merror ("%s: symlink failed", phfile->pname);
    }
  phfile->pname = plink_name;


  /* start function to remove all created links when program
   * exits
   */
  if (binit)
    {
      if (atexit (cts_vrm_link))
	cts_merror ("%s: unable to launch atexit!\n", CFUNCNAME);

      binit = false;
    }

#endif


  /* update counter for luns and create LUN name
   */
  ilun = phfile->ilun = imaxlun++;
  plun = cts_pint_to_str (ilun, "LUN%d");

  /* change option character to upper-case
   */
  phfile->copt = toupper (phfile->copt);


  /* check for existing hfile
   */
  if (phfile->copt == 'U' || phfile->copt == ' ')
    {
      /* call hropen with ilrec = 0 to get the recordlength of the
       * hbook-file in ilrec;
       */
      ilrec = 0;
      cts_hropen (ilun, plun, phfile->pname, phfile->copt, &ilrec);


      /* read in all histograms; get id's & number of all histograms in
       * memory
       */
      cts_hrin (inull, inull, inull);
      cts_hidall (idvect, &inrh);


      /* call is used to distinguish between ntuple- and pure
       * histogram-files (only idvect[0] is used)
       * in a second step it's information (histo_dat.ixbin) is used
       * to get number 'invar' of variables for each ntuple- event;
       */
      histo_dat.iid = idvect[0];
      histo_dat.phname = ctitle;
      cts_hgive (&histo_dat);


      /* true if ntuple-file
       */
      if (histo_dat.fxhigh == histo_dat.fxlow)
	{
	  invar = histo_dat.ixbin;


	  /* memory for data of one event, maxima & minima of every
	   * variable and for variable names of the Ntuple
	   */
	  pevent_data   = cts_mmalloc (float, invar, CFUNCNAME);
	  pntuple_rhigh = cts_mmalloc (float, invar, CFUNCNAME);
	  pntuple_rlow  = cts_mmalloc (float, invar, CFUNCNAME);
	  pcvar_names   = cts_mmalloc (char, invar * iVARNAMELEN, CFUNCNAME);


	  /* reopen Ntuple as cts_hgnpar doesn't work after cts_hrin without;
	   */
	  cts_hrend (phfile);
	  cts_hropen (ilun, plun, phfile->pname, phfile->copt, &ilrec);


	  /* set internal hbook-parameters and read number of events in ntuple;
	   * hcts_noent must be called after cts_hgnpar to give expected results
	   */
	  cts_hgnpar (idvect, "");
	  cts_hnoent (idvect, &inoent);


	  /* read information about the ntuple
	   */
	  cts_hgiven (idvect, ctitle, &invar, pcvar_names, pntuple_rlow,
		      pntuple_rhigh);

	} /* end ntuple-file */

      else
	{
	  /* histogram-file
	   * put number of histograms in invar
	   */
	  invar = inrh;


	  /* there's no need to reopen file (hgnpar not used)
	   */

	  /* set unused hfile-struct variables to NULL
	   */
	  idvect[0] = 0;
	  inoent = 0;
	  pevent_data = NULL;
	  pntuple_rhigh = NULL;
	  pntuple_rlow = NULL;
	  pcvar_names = NULL;

	} /* end histogram-file */


      /* store information in hfile-struct
       */
      phfile->ilrec = ilrec;
      phfile->iid = idvect[0];
      phfile->invar = invar;
      phfile->inr_events = inoent;
      phfile->pevtdat = pevent_data;
      phfile->prhigh = pntuple_rhigh;
      phfile->prlow = pntuple_rlow;
      phfile->pvarnam = pcvar_names;

    } /* END if (phfile->copt ... ) */

  else if (phfile->copt == 'N')
    {
      /* create a new hbook-file
       * use iid to decide whether it is for an ntuple (>0) or histograms
       */
      if (phfile->iid > 0)
	{
	  /* check for valid record-length; take default one if not
	   */
	  ilrec = (phfile->ilrec > 0) ? phfile->ilrec : iNTUPLE_DEF_RL;

	  cts_hropen (ilun, plun, phfile->pname, phfile->copt, &ilrec);


	  /* memory for data of one event, maxima & minima of every
	   * variable; the array containing the variable names has to
	   * be provided by the calling routine
	   * check valid number of variables before
	   */
	  invar = phfile->invar;

	  if (invar <= 0 || invar > 512)
	    cts_merror ("%s: invalid number of variables.\n", phfile->pname);


	  pevent_data   = cts_mmalloc (float, invar, CFUNCNAME);    
	  pntuple_rhigh = cts_mmalloc (float, invar, CFUNCNAME);
	  pntuple_rlow  = cts_mmalloc (float, invar, CFUNCNAME);


	  /* book ntuple; take 4 times the record lenth as buffer size
	   * check whether variable names are defined
	   */
	  inwb = 4 * ilrec;

	  if (phfile->pvarnam == NULL)
	    cts_merror( "%s: no variable names defined.\n", phfile->pname);

	  cts_hbookn (phfile->iid, phfile->invar, inwb, plun, phfile->pvarnam);
	  cts_hgnpar (&phfile->iid, "");

	} /* END  if (phfile->iid > 0) */

      else
	{
	  /* histogram-file
	   */

	  /* check for valid record-length (take default one if not)
	   * and open file
	   */
	  ilrec = (phfile->ilrec > 0) ? phfile->ilrec : iHISTO_DEF_RL;
	  cts_hropen (ilun, plun, phfile->pname, phfile->copt, &ilrec);


	  /* set unused hfile-struct variables to NULL
	   */
	  phfile->invar = 0;
	  phfile->pvarnam = NULL;
	  pevent_data = NULL;
	  pntuple_rhigh = NULL;
	  pntuple_rlow = NULL;

	}


      /* store information to hfile-struct
       * (some parameters are already provided by the calling routine)
       */
      phfile->ilrec = ilrec;
      phfile->inr_events = 0;
      phfile->pevtdat = pevent_data;
      phfile->prhigh = pntuple_rhigh;
      phfile->prlow = pntuple_rlow;

    }  /* END else if (phfile->copt == 'N') */

  else

    /* unsupported option
     */
    cts_merror ("%s: unsupported option.\n", CFUNCNAME);
}
#undef CFUNCNAME


#define CFUNCNAME "cts_hreopen"

void cts_hreopen (HBOOK_FILE *phfile)

  /* reopens hbook file with complete HBOOK_FILE struct
   */
{
  int ilun;
  char *plun;


  /* create LUN name from 'ilun'
   */
  ilun = phfile->ilun;
  plun = cts_pint_to_str (ilun, "LUN%d");

  /* open file
   */
  cts_hropen (ilun, plun, phfile->pname, phfile->copt, &phfile->ilrec);


  /* call hgnpar afterwards for ntuples
   */
  if (phfile->iid > 0)
    cts_hgnpar (&phfile->iid, CFUNCNAME);
}

#undef CFUNCNAME


#define CFUNCNAME "cts_vbook_hist"

void cts_vbook_hist (HISTO *phist)

  /* book histogram, return value is dimension of the histogram
   */
{
  extern int hbook1_ ();
  extern int hbook2_ ();

  /* if number of bins in y is zero use hbook1
   */
  if (phist->iybin == 0)
    {
     hbook1_ ( &phist->iid,phist->phname,&phist->ixbin,&phist->fxlow,
	       &phist->fxhigh,&phist->fvmx, strlen (phist->phname));
    }

  else if (phist->iybin > 0)
    {
      hbook2_ (&phist->iid,phist->phname,&phist->ixbin,&phist->fxlow,
	       &phist->fxhigh,&phist->iybin,&phist->fylow,&phist->fyhigh,
	       &phist->fvmx,strlen (phist->phname));
    }

  else
    cts_merror ("%s: invalid number of y-bins.\n", CFUNCNAME);
}

#undef CFUNCNAME


float *cts_pset_nam (HBOOK_FILE *pevt, char *substr)

  /* find position of string 'substr' in array of variable names
   * 'pevt->pvarnam' of ntuple. Use this information to return
   * a pointer to the event data of variable 'substr' in the data
   * array pevt->pevtdat.
   */
{
  char *pname;

  /* check for existence of variable substr
   */
  pname = (pevt->pvarnam != NULL ) ? strstr (pevt->pvarnam, substr) : NULL;

  if (pname == NULL)
    cts_merror ("%s: unknown variable!\n", substr);

  return (pevt->pevtdat + (pname - pevt->pvarnam) / iVARNAMELEN);
}


float cts_fget_max (HBOOK_FILE *pevt, char *substr)

  /* find position of string 'substr' in array of variable names
   * 'pevt->pvarnam' of ntuple. Use this information to return
   * maximum value of the event variable 'substr' (stored in pevt->prhigh).
   */
{
  char *pname;

  /* check for existence of variable substr
   */
  pname = (pevt->pvarnam != NULL ) ? strstr (pevt->pvarnam, substr) : NULL;

  if (pname == NULL)
    cts_merror ("%s: unknown variable!\n", substr);

  return (*(pevt->prhigh + (pname - pevt->pvarnam) / iVARNAMELEN));
}


float cts_fget_min (HBOOK_FILE *pevt, char *substr)

  /* find position of string 'substr' in array of variable names
   * 'pevt->pvarnam' of ntuple. Use this information to return
   * minimum value of the event variable 'substr' (stored in pevt->prlow).
   */
{
  char *pname;

  /* check for existence of variable substr
   */
  pname = (pevt->pvarnam != NULL ) ? strstr (pevt->pvarnam, substr) : NULL;

  if (pname == NULL)
    cts_merror ("%s: unknown variable!\n", substr);

  return (*(pevt->prlow + (pname - pevt->pvarnam) / iVARNAMELEN));
}


void cts_vkuopen (int *fd, char *file_nam, char *status)

  /* sets file scope variable imaxlun
   */
{
  extern void kuopen_ ();
  int ierr;

  *fd = imaxlun++;

  kuopen_ (fd, file_nam, status, &ierr, strlen (file_nam), strlen (status));

  if (ierr != 0)
    cts_merror ("%s: cts_kuopen failed.\n", file_nam);
}


void cts_vkuclos (int fd, char *status)
{
  extern void kuclos_ ();
  int ierr;

  kuclos_ (&fd, status, &ierr, strlen (status));

  if (ierr != 0)
    cts_merror ("lun %d: cts_kuclos failed.\n", fd);
}
