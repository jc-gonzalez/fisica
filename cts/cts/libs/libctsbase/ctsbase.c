#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include "ctsbase.h"


/* local functions
 */
static void vfree_mem ();


/* pointers to root and end of linked list
 * this list contains the information needed to free
 * all allocated memory;
 * set and used in pmem_alloc
 *
 * mlc_root points to root of tree (is fixed)
 * mlc_end points to end of tree (changes with every new function call)
 */
static MLC_HANDLER *mlc_root = NULL;
static MLC_HANDLER *mlc_end = NULL;




static void vfree_mem ()

  /* function used to free all allocated memory
   * it's called via atexit set by pmem_alloc
   * uses file scope variable mlc_root
   */
{
  MLC_HANDLER *mlc;

  /* free willy
   */
  while (mlc_root != NULL)
    {
#ifdef DEBUG
      fprintf (stdout, "free mem %p\t handler %p\n", mlc_root->pmem, mlc_root);
#endif

      /* free memory of corresponding malloc
       */
      free (mlc_root->pmem);

      /* now we're able to free memory of mlc handler
       */
      mlc = mlc_root->p2next;
      free (mlc_root);
      mlc_root = mlc;
    }
}


#define CFUNCNAME "cts_vfree"

void cts_vfree (void *pmem)

  /* function used to free memory before the call to exit
   * (it removes the corresponding mlc handler from the list)
   * uses and sets file scope variable mlc_root and mlc_end
   */
{
  MLC_HANDLER *mlc, *mlc_last;


  /*
   * scan whole list for matching pmem entry
   */
  mlc = mlc_root;
  mlc_last = mlc_root;

  while (mlc != NULL && pmem != NULL)
    {
      if (mlc->pmem == pmem)
	{
	  /* we got it, so remove mlc handler from list
	   * and free mlc handler and memory;
	   * take care of mlc being root_mlc
	   */
	  if (mlc == mlc_root)
	    mlc_root = mlc->p2next;

	  else
	    mlc_last->p2next = mlc->p2next;

#ifdef DEBUG
	  fprintf (stdout, "vfree mem %p\t handler %p\n", pmem, mlc);
#endif

	  free (mlc);
	  free (pmem);

	  /* reset mlc_end
	   */
	  if (mlc_last->p2next == NULL)
	    mlc_end = mlc_last;

	  mlc = mlc_last = NULL;
	}
      else
	{
	  /* reset pointers
	   */
	  mlc_last = mlc;
	  mlc = mlc->p2next;
	}
    }


  /* if mlc_last == NULL, we found a matchin mlc-handler, else not
   */
  if (mlc_last != NULL || pmem == NULL)
    cts_merror ("%s: no corresponding mle-handler found!\n", CFUNCNAME);
}

#undef CFUNCNAME


#define CFUNCNAME "cts_pmem_alloc"

void *cts_pmem_alloc (size_t msize, char *perr_str)

  /* this function allocates memory of size 'msize'
   * and creates new element of the MLC_HANDLER list
   * the pointer to the allocated memory is stored in
   * the MLC_HANDLER element for a later call to free()
   * uses and sets the file scope variables mlc_root and mlc_end
   */
{
  static bool binit = true;
  static size_t mlc_size = sizeof (MLC_HANDLER);
  void *pmem;

  MLC_HANDLER *mlc;

#ifdef DEBUG
  fprintf (stdout, "mem size %d\t", msize);
#endif

  /* avoid zero msize (programs won't work on ALPHA machines)
   */
  msize = (msize == (size_t) 0) ? (size_t) 1 : msize;


  /* allocate the memory
   */
  errno = 0;
  if ((pmem = malloc (msize)) == NULL)
    {
      perror (perr_str);
      exit (iERRCODE);
    }


  /* create mlc handler element
   */
  errno = 0;
  if ((mlc = (MLC_HANDLER *) malloc (mlc_size)) == NULL)
    {
      perror ("malloc for mlc_handler failed");
      exit (iERRCODE);
    }

  /* fill mlc handler element
   * (it's the last element in the list)
   */
  mlc->p2next = NULL;
  mlc->pmem = pmem;


#ifdef DEBUG
  fprintf (stdout, "%s: allocated mem: %p\t handler %p\n", perr_str, pmem, mlc);
#endif


  if (binit)
    {
      /* call atexit to free all allocated memory at exit time
       */
      if (atexit (vfree_mem))
	cts_merror ("%s: unable to launch atexit!\n", CFUNCNAME);

      binit = false;
    }


  /* reset list pointers to include new mlc handler
   */
  if (mlc_root == NULL)
    {
      /* it's the first element in the list
       */
      mlc_root = mlc;
      mlc_end = mlc;
    }
  else
    {
      /* set pointer to next mlc handler in preceeding handler
       */
      mlc_end->p2next = mlc;

      /* set pointer to last element in list
       */
      mlc_end = mlc;
    }

 return (pmem);
}

#undef CFUNCNAME


#define CFUNCNAME "cts_pmem_realloc"

void *cts_pmem_realloc (void *pmem, size_t msize, char *perr_str)

  /* this function re-allocates memory of size 'msize'
   * and updates the corresponding mle-handler
   */
{
  bool bfound = false;

  MLC_HANDLER *mlc;

#ifdef DEBUG
  fprintf (stdout, "reallocating mem %p ...", pmem);
#endif


  /* avoid zero msize (programs won't work on ALPHA machines)
   */
  msize = (msize == (size_t) 0) ? (size_t) 1 : msize;


  /*
   * scan whole list for matching pmem entry
   */
  mlc = mlc_root;

  /* if pmem == NULL normal allocation is needed
   * (reset mlc to avoid entering the while loop)
   */
  if (pmem == NULL)
    {
      pmem = cts_pmem_alloc (msize, perr_str);
      bfound = true;
      mlc = NULL;
    }


  while (mlc != NULL && pmem != NULL)
    {
      if (mlc->pmem == pmem)
	{
	  /* we got it, so re-allocate memory and update mlc handler
	   */
	  errno = 0;
	  if ((pmem = realloc (pmem, msize)) == NULL)
	    {
	      perror (perr_str);
	      exit (iERRCODE);
	    }

	  bfound = true;
	  mlc->pmem = pmem;
	  mlc = mlc->p2next;

#ifdef DEBUG
	  fprintf (stdout, "at: %p\t in handler %p\n", pmem, mlc);
#endif
	}

      else
	{
	  /* reset pointer
	   */
	  mlc = mlc->p2next;
	}
    }


  if (!bfound)
    cts_merror ("%s: no corresponding mle-handler found!\n", CFUNCNAME);

  return (pmem);
}

#undef CFUNCNAME


void cts_vopen_file (FILE_TYPE *ft)

  /* open file of given name and access mode
   */
{
  /* functions below might choke on a NULL filename
   */
  if (ft->pname == NULL)
    ft->pname = "\0";

  /* write error message, if open failed
   */
  errno = 0;
  if ((ft->pfile = fopen (ft->pname, ft->acm)) == NULL) 
    {
      fprintf (stderr, "unable to open file '%s' ", ft->pname);
      fprintf (stderr, "(mode %s)!\n", ft->acm);
      fprintf (stderr, "%s\n", strerror (errno));
      exit (iERRCODE);
    }
}


void cts_vclose_file (FILE_TYPE *ft)

{
  /* ignore NULL file pointers
   */
  if (ft->pfile == NULL)
    return;

  /* write error message, if close failed
   */
  errno = 0;
  if (fclose (ft->pfile) == EOF)
   {
    fprintf (stderr, "Error while trying to close file '%s'!\n", ft->pname );
    fprintf (stderr, "%s\n", strerror (errno));
    exit (iERRCODE);
   }
}


void cts_vset_stream (FILE_TYPE *ft, FILE *stream)

  /* associates 'stream' to file 'ft'
   */
{
  /* functions below might choke on a NULL filename
   */
  if (ft->pname == NULL)
    ft->pname = "\0";

  /* reopen stream
   */
  if ((ft->pfile = freopen (ft->pname, ft->acm, stream)) == NULL) 
    {
      fprintf (stderr, "unable to set stream to file '%s'!\n", ft->pname);
      fprintf (stderr, "%s\n", strerror (errno));
      exit (iERRCODE);
    }
}


#define CFUNCNAME "cts_vget_dat_file"

void cts_vget_dat_file (int inc, char *p2f_name, double ***p2al, int *pnp)

  /* reads in data from a file and puts it into arrays (each column into
   * one array)
   * input:
   * inc      - number of columns
   * p2f_name - name of data file
   *
   * output:
   * p2al - list of arrays containing one data column each
   * pnp  - number of data points
   *
   * the memory for all arrays is allocated here
   */
{
  int iscanfcode, ientries, inp, i, j;

  double ddat;

  bool bfile_end = false;

  FILE_TYPE pdatfile;


  /* fill struct for file access and open parameter file
   */
  pdatfile.pname = p2f_name;
  pdatfile.pfile = NULL;
  pdatfile.acm = "r";

  cts_vopen_file (&pdatfile);


  /* first loop over dat-file to get number of entries
   */
  ientries = 0;
  while (!bfile_end)
    {
      iscanfcode = fscanf (pdatfile.pfile, "%lf", &ddat);

      if (iscanfcode == EOF)
	bfile_end = true;

      else
	if (iscanfcode != 1)
	  cts_merror ("error while reading file: %s\n", p2f_name);

	else
	  ientries++;
    }


  /* check result
   * (inc must be > 0 and ientries must be a multiple of inc)
   */
  inp = (inc > 0) ? ientries / inc : 0;

  if (inp == 0 || inp * inc != ientries)
    cts_merror ("%s: bad file content!\n", CFUNCNAME);

  else
    fprintf (stderr, "%d entries per column found.\n", inp);


  /* allocate memory (for the list and all the columns) & read in data
   */
  *p2al = cts_mmalloc (double *, inc, CFUNCNAME);

  for (i = 0; i < inc; i++)
    *(*p2al+i) = cts_mmalloc (double, inp, CFUNCNAME);


  fseek (pdatfile.pfile, 0L, SEEK_SET);

  for (j = 0; j < inp; j++)
    for (i = 0; i < inc; i++)
      fscanf (pdatfile.pfile, "%lf", *(*p2al+i)+j);

  cts_vclose_file (&pdatfile);

  /* set return value
   */
  *pnp = inp;
}

#undef CFUNCNAME


#define CFUNCNAME "cts_vreset_darray"

void cts_vreset_darray (double *parray, int inp)

  /* this function resets the array 'parray' with 'inp' entries
   * to '0.'
   */
{
  static int i, inpmax = 0;
  static double *pnull = NULL;

  if (inp > inpmax)
    {
      /* we have to create or enlarge the null array
       */

      if (pnull == NULL)
	pnull = cts_mmalloc (double, inp, CFUNCNAME);

      else
	pnull = cts_mrealloc (pnull, double, inp, CFUNCNAME);


      for (i = 0; i < inp; i++)
	pnull[i] = 0.;

      inpmax = inp;
    }


  /* reset array
   */
  memcpy (parray, pnull, (size_t) inp * sizeof (double));    
}

#undef CFUNCNAME


#define CFUNCNAME "cts_vreset_farray"

void cts_vreset_farray (float *parray, int inp)

  /* this function resets the array 'parray' with 'inp' entries
   * to '0.f'
   */
{
  static int i, inpmax = 0;
  static float *pnull = NULL;

  if (inp > inpmax)
    {
      /* we have to create or enlarge the null array
       */

      if (pnull == NULL)
	pnull = cts_mmalloc (float, inp, CFUNCNAME);

      else
	pnull = cts_mrealloc (pnull, float, inp, CFUNCNAME);


      for (i = 0; i < inp; i++)
	pnull[i] = 0.;

      inpmax = inp;
    }


  /* reset array
   */
  memcpy (parray, pnull, (size_t) inp * sizeof (float));    
}

#undef CFUNCNAME


#define CFUNCNAME "cts_vreset_iarray"

void cts_vreset_iarray (int *parray, int inp)

  /* this function resets the array 'parray' with 'inp' entries
   * to '0' (int)
   */
{
  static int i, inpmax = 0;
  static int *pnull = NULL;

  if (inp > inpmax)
    {
      /* we have to create or enlarge the null array
       */

      if (pnull == NULL)
	pnull = cts_mmalloc (int, inp, CFUNCNAME);

      else
	pnull = cts_mrealloc (pnull, int, inp, CFUNCNAME);


      for (i = 0; i < inp; i++)
	pnull[i] = 0;

      inpmax = inp;
    }


  /* reset array
   */
  memcpy (parray, pnull, (size_t) inp * sizeof (int));    
}

#undef CFUNCNAME


double cts_dstrtod (char *pnum)

  /* this function converts a given string into a double
   * (it's based on the strtod man-page)
   */
{
  char *cptr;
  double dnum;

  dnum = strtod (pnum, &cptr);

  if (errno != 0)
    {
      fprintf (stderr, "%s\n", strerror (errno));
      cts_merror ("unable to convert string '%s' to double!\n", pnum);
    }
  else if (cptr == '\0' || *cptr != '\0')
	 cts_merror ("unable to convert string '%s' to double!\n", pnum);

  return (dnum);
}


long int cts_lstrtol (char *pnum, int ibase)

  /* this function converts a given string into a long int
   * (it's based on the strtol man-page)
   */
{
  char *cptr;
  long int inum;

  inum = strtol (pnum, &cptr, ibase);

  if (errno != 0)
    {
      fprintf (stderr, "%s\n", strerror (errno));
      cts_merror ("unable to convert string '%s' to int!\n", pnum);
    }
  else if (cptr == '\0' || *cptr != '\0')
    cts_merror ("unable to convert string '%s' to int!\n", pnum);

  return (inum);
}


#define CFUNCNAME "cts_preal_to_str"

char *cts_preal_to_str (double dreal, int iprec, char *pstr)

  /* this function converts a given real number into a string
   * input:
   * dreal - number of interest
   * iprec - precision to be used
   * pstr  - string to incorporate real (optional, else NULL)
   */
{
  int idstr_siz, irstr_siz;

  double dabsr, dmin = 1e-99;

  char *pmem;
  static char *pstr_old = NULL;


  iprec = abs (iprec);

  if (pstr == NULL)
    {
      /* build up string to read in 'dreal'
       * +1 - '\0'
       * +1 - leading digit (log 1000 = 3 but we need 4 chars)
       * +3 - "%.f"
       */
      irstr_siz = (iprec > 0) ? (int) (log ((double) iprec)) + 5 : 5;
      pstr = cts_mrealloc (pstr_old, char, irstr_siz, CFUNCNAME);

      sprintf (pstr, "%%.%df", iprec);

      pstr_old = pstr;
    }


  /* calculate string length of real:
   * (check for minimum real)
   * iprec + length of 'pstr'
   * +1 - '\0'
   * +1 - sign,
   * +1 - dot,
   * +1 - leading digit (log 1000 = 3 but we need 4 chars)
   * +4 - if conversion character e,E is used '+exx' || '+Exx'
   */
  dabsr = (fabs (dreal) < dmin) ? 0. : fabs (dreal);

  idstr_siz = (dabsr > 1.) ? (int) (log (dabsr)) + iprec + 8 : iprec + 9;
  idstr_siz += strlen (pstr);

  pmem = cts_mmalloc (char, idstr_siz, CFUNCNAME);

  sprintf (pmem, pstr, dreal);

  return (pmem);
}

#undef CFUNCNAME


#define CFUNCNAME "cts_pint_to_str"

char *cts_pint_to_str (long int lnum, char *pstr)

  /* this function converts a given real number into a string
   * input:
   * dnum  - number of interest
   * pstr  - string to incorporate real (optional, else NULL)
   */
{
  int istr_siz;

  long int iabs;

  char *pmem;
  static char *pstr_def = "%d";


  pstr = (pstr == NULL) ? pstr_def : pstr;

  /* calculate string length of lnum:
   * length of 'pstr'
   * +1 - '\0'
   * +1 - sign,
   * +1 - leading digit (log 1000 = 3 but we need 4 chars)
   */
  iabs = abs (lnum);

  istr_siz = (iabs > 0) ? (int) (log ((double) iabs)) + 3 : 3;
  istr_siz += strlen (pstr);

  pmem = cts_mmalloc (char, istr_siz, CFUNCNAME);

  sprintf (pmem, pstr, lnum);

  return (pmem);
}

#undef CFUNCNAME


#define CFUNCNAME "cts_pstrcat"

char *cts_pstrcat (int iargs, char *pstr, ...)

  /* this function builds one string containing the content of
   * all passed strings (in the same order)
   * input:
   * iargs - number of passed strings (incl. pstr)
   * pstr  - first string passed
   * return:
   * final string
   */
{
  int i;
  static int iargs_max = 0;

  size_t ssiz;
  static size_t *psiz = NULL;

  char *p1, *pfin_str;
  static char **pargs = NULL;

  va_list ap;


  /* loop over passed args and store them in pargs-array (realloc memory
   * if not enough)
   */
  if (iargs_max < iargs)
    {
      iargs_max = iargs;
      psiz = cts_mrealloc (psiz, size_t, iargs, CFUNCNAME);
      pargs = cts_mrealloc (pargs, char *, iargs, CFUNCNAME);
    }

  pargs[0] = pstr;
  psiz[0] = strlen (pstr);
  ssiz = psiz[0] + (size_t) 1;   /* '\0' */

  va_start(ap, pstr);

  for (i = 1; i < iargs; i++)
    {
      pargs[i] = va_arg (ap, char *);
      psiz[i] = strlen (pargs[i]);
      ssiz += psiz[i];
    }


  /* build up final string
   */
  pfin_str = cts_mmalloc (char, ssiz, CFUNCNAME);
  p1 = pfin_str;

  for (i = 0; i < iargs; i++)
    {
      memcpy (p1, pargs[i], psiz[i]);
      p1 += psiz[i];
    }

  *p1 = '\0';
  va_end(ap);

  return (pfin_str);
}

#undef CFUNCNAME


int cts_icheck_flag (char *pflag, char *pflag_str, int iflen)

  /* this function checks flags, passed via command line
   * input:
   * pflag     - flag string of interest
   * pflag_str - string containing all flags
   * iflen     - expected string length of pflag
   * return:
   * offset of pflag or -1 if invalid flag
   */
{
  int iflagpos;
  char *pstr;

  pstr = strstr (pflag_str, pflag);

  if (pstr != NULL && strlen (pflag) == (size_t) iflen)
    iflagpos = pstr - pflag_str;

  else
    iflagpos = -1;

  return (iflagpos);
}


#define CFUNCNAME "cts_vsort_dat"

void cts_vsort_dat (double **p2al, int inr, int inp, int isrow, bool basc)

  /* function sorts set of data arrays (this is a simple version, but
   * should be enough for small data-samples or partly sorted data)
   * input:
   * p2al  - array of pointers to data arrays
   * inr   - number of data arrays
   * inp   - number of points per data array
   * isrow - specifies data array which is sorted
   * basc  - true, if sorting is in ascending order
   */
{
  int i, k, n;

  double dsval, dsign;

  bool bsorted;

  double *pdat, *pdat_root;

  SORT_LIST *psort_el, *psort_pos, *psort_root, *psort_free;


  /* sorting data is multiplied by dsign to have ascending
   * or descending order
   */
  dsign = (basc) ? 1. : -1.;

  
  /* allocate memory for data-block (contains data of all arrays)
   */
  pdat_root = cts_mmalloc (double, inp * inr, CFUNCNAME);


  /* allocate memory for sort list and init root struct
   */
  psort_free = cts_mmalloc (SORT_LIST, inp, CFUNCNAME);

  psort_root = psort_free;
  psort_root->index = 0;
  psort_root->dsval = **p2al * dsign;
  psort_root->p2prev = NULL;
  psort_root->p2next = NULL;

  /* put data of first column in block
   */
  pdat = pdat_root;
  
  for (k = 0; k < inr; k++)
    *pdat++ = **(p2al+k);

  /* loop over array entries, put data in block and set sort-structs
   */
  psort_pos = psort_el = psort_root;

  for (i = 1; i < inp; i++)
    {
      /* get next sorting data (isrow 1 -> +0)
       */
      dsval = *(*(p2al+isrow-1)+i) * dsign;


      /* find matching sort struct
       */
      bsorted = false;

      while (!bsorted)
	{
	  if (dsval >= psort_pos->dsval)
	    {
	      if (psort_pos->p2next == NULL || psort_pos->p2next->dsval > dsval)
		{
		  /* position found, set sort-struct
		   */
		  psort_el++;
		  psort_el->index = i;
		  psort_el->dsval = dsval;
		  psort_el->p2prev = psort_pos;
		  psort_el->p2next = psort_pos->p2next;
		  bsorted = true;

		  psort_pos->p2next = psort_el;

		  if (psort_el->p2next != NULL)
		    psort_el->p2next->p2prev = psort_el;

		  /* reset psort_pos for next loop
		   */
		  psort_pos = psort_el;
		}

	      else
		psort_pos = psort_pos->p2next;
	    }

	  else
	    {
	      if (psort_pos->p2prev == NULL)	    
		{
		  /* root struct found, put sort-struct before
		   */
		  psort_el++;
		  psort_el->index = i;
		  psort_el->dsval = dsval;
		  psort_el->p2prev = NULL;
		  psort_el->p2next = psort_pos;
		  bsorted = true;

		  psort_root = psort_el;
		  psort_pos->p2prev = psort_el;

		  /* reset psort_pos for next loop
		   */
		  psort_pos = psort_el;
		}

	      else
		psort_pos = psort_pos->p2prev;
	    }

	}  /* END  while (!bsorted) */


      /* put data in block
       */
      for (k = 0; k < inr; k++)
	*pdat++ = *(*(p2al+k)+i);

    }  /* END  for (i = 1; ... ) */


  /* whole data sample is sorted - put it back into the arrays
   * (undo multiplication with dsign)
   */
  pdat = pdat_root;
  psort_pos = psort_root;

  for (i = 0; i < inp; i++)
    {
      /* get position of data in block
       */
      n = psort_pos->index * inr;

      for (k = 0; k < inr; k++)
	*(*(p2al+k)+i) = *(pdat+n+k) * dsign;

      psort_pos = psort_pos->p2next;
    }

  cts_mfree (psort_free);
  cts_mfree (pdat_root);
}

#undef CFUNCNAME
