#include <string.h>
#include <stdio.h>
#include "ctshbook.h"
#include "ctsbase.h"
#include "ctscuts.h"


/* declaration of all local functions
 */
static void vno_sort      (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);
static void vs_not_sup    (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);
static void vs_za_sig     (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);
static void vs_za_cal     (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);
static void vs_tm_coi     (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);
static void vs_tm_sig     (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);
static void vs_day_za_sig (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);
static void vs_day_tm_sig (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);
static void vs_day_tm_coi (void *, void **, bool, HBOOK_FILE *, int *, DS_TYPE);


/* array of all defined sort functions - must match definition of
 * SORT_TYPE and DS_TYPE in ctscuts.h
 */
static void (*const sort_funcs [MAX_DS_TYPE] [MAX_SORT_TYPE]) () =
{{vno_sort, vs_not_sup, vs_za_cal,  vs_not_sup,    vs_not_sup   },
 {vno_sort, vs_tm_sig,  vs_za_sig,  vs_day_za_sig, vs_day_tm_sig},
 {vno_sort, vs_tm_coi,  vs_not_sup, vs_not_sup,    vs_day_tm_coi},
 {vno_sort, vs_not_sup, vs_not_sup, vs_not_sup,    vs_not_sup   }};


/* comment:
 * some of the functions  are nearly the same; the only difference is that they
 * access different structs and that the sorted variable is the ZA or the time;
 * I didn't find an elegant way to handle this within one function yet
 */
#define CFUNCNAME "vno_sort"

static void vno_sort (void *object, void **list, bool binit, HBOOK_FILE *pn,
		      int *hts, DS_TYPE dstyp)

  /* this function puts passed structure in linked list (not sorted)
   * input:
   * object - struct element
   * binit  - indicates a new list
   * pn     - pointer to ntuple, not used here
   * hts    - size of hash table, not used here
   * dstype - enum holding the structure type
   * output:
   * list   - pointer to first element in the list
   */
{
  static void *pprev_obj;

  SLIST_EL *pslist_el;
  DLIST_EL *pdlist_el;
  TLIST_EL *ptlist_el;
  ELIST_EL *pelist_el;


  /* not much left to be done if first data struct
   */
  if (binit)
    {
      pprev_obj = object;
      *list = object;

      return;
    }


  /* set pointers to p2prev and p2next in 'object' and
   * 'pprev_obj' (take care of different structure types)
   */
  switch (dstyp)
    {
      case DS_DATA:

	pdlist_el = (DLIST_EL *) object;
	pdlist_el->p2prev = (DLIST_EL *) pprev_obj;

	pdlist_el = (DLIST_EL *) pprev_obj;
	pdlist_el->p2next = (DLIST_EL *) object;

	break;


      case DS_SIGNAL:

	pslist_el = (SLIST_EL *) object;
	pslist_el->p2prev = (SLIST_EL *) pprev_obj;

	pslist_el = (SLIST_EL *) pprev_obj;
	pslist_el->p2next = (SLIST_EL *) object;

	break;


      case DS_TIME:

	ptlist_el = (TLIST_EL *) object;
	ptlist_el->p2prev = (TLIST_EL *) pprev_obj;

	ptlist_el = (TLIST_EL *) pprev_obj;
	ptlist_el->p2next = (TLIST_EL *) object;

	break;


      case DS_ENERGY:

	pelist_el = (ELIST_EL *) object;
	pelist_el->p2prev = (ELIST_EL *) pprev_obj;

	pelist_el = (ELIST_EL *) pprev_obj;
	pelist_el->p2next = (ELIST_EL *) object;

	break;


    default:

      cts_merror ("%s: default reached!\n", CFUNCNAME);
    }


  pprev_obj = object;

  return;
}

#undef CFUNCNAME


#define CFUNCNAME "vs_not_sup"

static void vs_not_sup (void *object, void **list, bool binit, HBOOK_FILE *pn,
			int *hts, DS_TYPE dstyp)

  /* used for unsupported sort routines (prints error message and exits)
   */
{
  cts_merror ("%s: sort routine not supported for this data!\n", CFUNCNAME);

  return;
}

#undef CFUNCNAME


static void vs_za_sig (void *object, void **list, bool binit, HBOOK_FILE *pn,
		       int *hts, DS_TYPE dstyp)

  /* put signal-struct object into linked list (za-sorted);
   * input:
   * object - struct to sort in
   * binit  - indicates first object
   * pn     - pointer to ntuple, not used here
   * hts    - size of hash table, not used here
   * dstype - enum holding the structure type
   * output:
   * list   - pointer to first element of list
   */
{
  /* pobj, pprev_obj  - pointers to current and previous object
   */
  static SLIST_EL *pobj, *pprev_obj;


  pobj = (SLIST_EL *) object;

  /* not much left to be done if first data struct
   */
  if (binit)
    {
      pprev_obj = pobj;
      *list = pobj;
      return;
    }


  /* put new data struct in existing list
   */
  if (pobj->dza >= pprev_obj->dza)

    /* search down the list for a struct with bigger ZA;
     * if one exists, put struct in between else append at the end
     */    
    {
      while (pprev_obj->p2next != NULL)
	{
	  if (pobj->dza < pprev_obj->p2next->dza)

	    /* we found the place to put struct in so reset pointers
	     */
	    {
	      pobj->p2next = pprev_obj->p2next;
	      pobj->p2prev = pprev_obj;

	      pprev_obj->p2next->p2prev = pobj;
	      pprev_obj->p2next = pobj;

	      /* remember the last object and return from function
	       */
	      pprev_obj = pobj;
	      return;
	    }

	  /* set pointer to next struct
	   */
	  pprev_obj = pprev_obj->p2next;
	}

      /* last struct in list
       */
      pobj->p2prev = pprev_obj;
      pprev_obj->p2next = pobj;
    }

  else

    /* search up the list for a struct with smaller ZA;
     * if one exists, put struct in between else put at the beginning
     */
    {
      while (pprev_obj->p2prev != NULL)
	{
	  if (pobj->dza >= pprev_obj->p2prev->dza)

	    /* we found the place to put struct in so reset pointers
	     */
	    {
	      pobj->p2prev = pprev_obj->p2prev;
	      pobj->p2next = pprev_obj;

	      pprev_obj->p2prev->p2next = pobj;
	      pprev_obj->p2prev = pobj;

	      /* remember the last object and return from function
	       */
	      pprev_obj = pobj;
	      return;
	    }

	  /* set pointer to next struct
	   */
	  pprev_obj = pprev_obj->p2prev;
	}

      /* first struct in list
       * (we also have to reset list)
       */
      pobj->p2next = pprev_obj;
      pprev_obj->p2prev = pobj;
      *list = pobj;
    }

  return;
}



static void vs_za_cal (void *object, void **list, bool binit, HBOOK_FILE *pn,
		       int *ht_size, DS_TYPE dstyp)

  /* put calcutta-struct object into linked list (za-sorted);
   * input:
   * object - struct to sort in
   * binit - indicates first object
   * pn     - pointer to ntuple, not used here
   * hts    - size of hash table, not used here
   * dstype - enum holding the structure type
   * output:
   * list   - pointer to first element of list
   */
{
  /* pobj, pprev_obj  - pointers to current and previous object
   */
  static DLIST_EL *pobj, *pprev_obj;


  pobj = (DLIST_EL *) object;

  /* not much left to be done if first data struct
   */
  if (binit)
    {
      pprev_obj = pobj;
      *list = pobj;
      return;
    }


  /* put new data struct in existing list
   */
  if (pobj->dza >= pprev_obj->dza)

    /* search down the list for a struct with bigger ZA;
     * if one exists, put struct in between else append at the end
     */    
    {
      while (pprev_obj->p2next != NULL)
	{
	  if (pobj->dza < pprev_obj->p2next->dza)

	    /* we found the place to put struct in so reset pointers
	     */
	    {
	      pobj->p2next = pprev_obj->p2next;
	      pobj->p2prev = pprev_obj;

	      pprev_obj->p2next->p2prev = pobj;
	      pprev_obj->p2next = pobj;

	      /* remember the last object and return from function
	       */
	      pprev_obj = pobj;
	      return;
	    }

	  /* set pointer to next struct
	   */
	  pprev_obj = pprev_obj->p2next;
	}

      /* last struct in list
       */
      pobj->p2prev = pprev_obj;
      pprev_obj->p2next = pobj;
    }

  else

    /* search up the list for a struct with smaller ZA;
     * if one exists, put struct in between else put at the beginning
     */
    {
      while (pprev_obj->p2prev != NULL)
	{
	  if (pobj->dza >= pprev_obj->p2prev->dza)

	    /* we found the place to put struct in so reset pointers
	     */
	    {
	      pobj->p2prev = pprev_obj->p2prev;
	      pobj->p2next = pprev_obj;

	      pprev_obj->p2prev->p2next = pobj;
	      pprev_obj->p2prev = pobj;

	      /* remember the last object and return from function
	       */
	      pprev_obj = pobj;
	      return;
	    }

	  /* set pointer to next struct
	   */
	  pprev_obj = pprev_obj->p2prev;
	}

      /* first struct in list
       * (we also have to reset list)
       */
      pobj->p2next = pprev_obj;
      pprev_obj->p2prev = pobj;
      *list = pobj;
    }

  return;
}


static void vs_tm_sig (void *object, void **list, bool binit, HBOOK_FILE *pn,
		       int *hts, DS_TYPE dstyp)

  /* put signal-struct object into linked list (time-sorted;
   * input:
   * object - struct to sort in
   * binit  - indicates first object
   * pn     - pointer to ntuple, not used here
   * hts    - size of hash table, not used here
   * dstype - enum holding the structure type
   * output:
   * list   - pointer to first element of list
   */
{
  /* pobj, pprev_obj  - pointers to current and previous object
   */
  static SLIST_EL *pobj, *pprev_obj;


  pobj = (SLIST_EL *) object;

  /* not much left to be done if first data struct
   */
  if (binit)
    {
      pprev_obj = pobj;
      *list = pobj;
      return;
    }


  /* put new data struct in existing list
   */
  if (pobj->devt_time >= pprev_obj->devt_time)

    /* search down the list for a struct with bigger ZA;
     * if one exists, put struct in between else append at the end
     */    
    {
      while (pprev_obj->p2next != NULL)
	{
	  if (pobj->devt_time < pprev_obj->p2next->devt_time)

	    /* we found the place to put struct in so reset pointers
	     */
	    {
	      pobj->p2next = pprev_obj->p2next;
	      pobj->p2prev = pprev_obj;

	      pprev_obj->p2next->p2prev = pobj;
	      pprev_obj->p2next = pobj;

	      /* remember the last object and return from function
	       */
	      pprev_obj = pobj;
	      return;
	    }

	  /* set pointer to next struct
	   */
	  pprev_obj = pprev_obj->p2next;
	}

      /* last struct in list
       */
      pobj->p2prev = pprev_obj;
      pprev_obj->p2next = pobj;
    }

  else

    /* search up the list for a struct with smaller ZA;
     * if one exists, put struct in between else put at the beginning
     */
    {
      while (pprev_obj->p2prev != NULL)
	{
	  if (pobj->devt_time >= pprev_obj->p2prev->devt_time)

	    /* we found the place to put struct in so reset pointers
	     */
	    {
	      pobj->p2prev = pprev_obj->p2prev;
	      pobj->p2next = pprev_obj;

	      pprev_obj->p2prev->p2next = pobj;
	      pprev_obj->p2prev = pobj;

	      /* remember the last object and return from function
	       */
	      pprev_obj = pobj;
	      return;
	    }

	  /* set pointer to next struct
	   */
	  pprev_obj = pprev_obj->p2prev;
	}

      /* first struct in list
       * (we also have to reset list)
       */
      pobj->p2next = pprev_obj;
      pprev_obj->p2prev = pobj;
      *list = pobj;
    }

  return;
}


static void vs_tm_coi (void *object, void **list, bool binit, HBOOK_FILE *pn,
		       int *hts, DS_TYPE dstyp)

  /* put coinc-struct object into linked list (time-sorted);
   * input:
   * object - struct to sort in
   * binit  - indicates first object
   * pn     - pointer to ntuple, not used here
   * hts    - size of hash table, not used here
   * dstype - enum holding the structure type
   * output:
   * list   - pointer to first element of list
   */
{
  /* pobj, pprev_obj  - pointers to current and previous object
   */
  static TLIST_EL *pobj, *pprev_obj;


  pobj = (TLIST_EL *) object;

  /* not much left to be done if first data struct
   */
  if (binit)
    {
      pprev_obj = pobj;
      *list = pobj;
      return;
    }


  /* put new data struct in existing list
   */
  if (pobj->devt_time >= pprev_obj->devt_time)

    /* search down the list for a struct with bigger ZA;
     * if one exists, put struct in between else append at the end
     */    
    {
      while (pprev_obj->p2next != NULL)
	{
	  if (pobj->devt_time < pprev_obj->p2next->devt_time)

	    /* we found the place to put struct in so reset pointers
	     */
	    {
	      pobj->p2next = pprev_obj->p2next;
	      pobj->p2prev = pprev_obj;

	      pprev_obj->p2next->p2prev = pobj;
	      pprev_obj->p2next = pobj;

	      /* remember the last object and return from function
	       */
	      pprev_obj = pobj;
	      return;
	    }

	  /* set pointer to next struct
	   */
	  pprev_obj = pprev_obj->p2next;
	}

      /* last struct in list
       */
      pobj->p2prev = pprev_obj;
      pprev_obj->p2next = pobj;
    }

  else

    /* search up the list for a struct with smaller ZA;
     * if one exists, put struct in between else put at the beginning
     */
    {
      while (pprev_obj->p2prev != NULL)
	{
	  if (pobj->devt_time >= pprev_obj->p2prev->devt_time)

	    /* we found the place to put struct in so reset pointers
	     */
	    {
	      pobj->p2prev = pprev_obj->p2prev;
	      pobj->p2next = pprev_obj;

	      pprev_obj->p2prev->p2next = pobj;
	      pprev_obj->p2prev = pobj;

	      /* remember the last object and return from function
	       */
	      pprev_obj = pobj;
	      return;
	    }

	  /* set pointer to next struct
	   */
	  pprev_obj = pprev_obj->p2prev;
	}

      /* first struct in list
       * (we also have to reset list)
       */
      pobj->p2next = pprev_obj;
      pprev_obj->p2prev = pobj;
      *list = pobj;
    }

  return;
}


#define CFUNCNAME "vs_day_za_sig"

static void vs_day_za_sig (void *object, void **list, bool binit,
			   HBOOK_FILE *pn, int *ht_size, DS_TYPE dstyp)

  /* put signal-struct object into linked list; use hash table to speed up data
   * access (hash table entry for each day, za-sorted within one day)
   * input:
   * object  - struct to sort in
   * binit   - indicates first object
   * pn      - pointer to ntuple, used to derive size of hash table
   *          (one entry for each single day)
   * ht_size - size of hash table
   * dstype  - enum holding the structure type
   * output:
   * list    - pointer to hash table - set here
   */
{
  /* a new hash table entry if true
   */
  bool bnew_day;

  /* inr_days - period covered by ntuple data
   * ioffset  - offset for hash table to access different days
   * imin_day - first day of data sample
   * imax_day - last day of data sample
   */
  static int inr_days, ioffset, imax_day, imin_day, i;

  /* phasht - pointer to hash table
   * phent  - pointer to hash table entry
   * pobj   - pointer to struct object
   */
  static SLIST_EL **phasht, **phent, *pobj;



  /* new ntuple, so create hash table;
   * get max and min date from ntuple
   */
  if (binit)
    {
      /* +0.5 added because floats are truncated  when converted to int
       * days = max - min + 1
       */
      imax_day = (int) (cts_fget_max (pn, cNOONMJD) + 0.5f);
      imin_day = (int) (cts_fget_min (pn, cNOONMJD) + 0.5f);
      inr_days = imax_day - imin_day + 1;

      /* allocate memory for hash table
       */
      phasht = cts_mmalloc (SLIST_EL *, inr_days, CFUNCNAME);

      /* init hash table
       */
      phent = phasht;
      for (i = 0; i < inr_days; i++)
	*phent++ = NULL;

      /* inform calling function about location of hash table
       * and it's size
       */
      *list = phasht;
      *ht_size = inr_days;
    }


  /* determine and check hash table offset
   */
  pobj = (SLIST_EL *) object;
  ioffset = pobj->inoonmjd - imin_day;

  if (ioffset < 0 || ioffset > inr_days)
    cts_merror ("%s: invalid hash table offset!\n", CFUNCNAME);


  /* put struct in list of corresponding day;
   * inform sort function if it's the first entry
   */
  phent = phasht + ioffset;
  bnew_day = (*phent != NULL) ? false : true;

  sort_funcs [DS_SIGNAL][SORT_ZA] (object, (void **) phent, bnew_day, pn,
				   ht_size, dstyp);

  return;
}

#undef CFUNCNAME


#define CFUNCNAME "vs_day_tm_sig"

static void vs_day_tm_sig (void *object, void **list, bool binit,
			   HBOOK_FILE *pn, int *ht_size, DS_TYPE dstyp)

  /* put signal-struct object into linked list; use hash table to speed up data
   * access (hash table entry for each day, time-sorted within one day)
   * input:
   * object  - struct to sort in
   * binit   - indicates first object
   * pn      - pointer to ntuple, used to derive size of hash table
   *          (one entry for each single day)
   * ht_size - size of hash table
   * dstype  - enum holding the structure type
   * output:
   * list    - pointer to hash table - set here
   */
{
  /* a new hash table entry if true
   */
  bool bnew_day;

  /* inr_days - period covered by ntuple data
   * ioffset  - offset for hash table to access different days
   * imin_day - first day of data sample
   * imax_day - last day of data sample
   */
  static int inr_days, ioffset, imax_day, imin_day, i;

  /* phasht - pointer to hash table
   * phent  - pointer to hash table entry
   * pobj   - pointer to struct object
   */
  static SLIST_EL **phasht, **phent, *pobj;



  /* new ntuple, so create hash table;
   * get max and min date from ntuple
   */
  if (binit)
    {
      /* +0.5 added because floats are truncated  when converted to int
       * days = max - min + 1
       */
      imax_day = (int) (cts_fget_max (pn, cNOONMJD) + 0.5f);
      imin_day = (int) (cts_fget_min (pn, cNOONMJD) + 0.5f);
      inr_days = imax_day - imin_day + 1;

      /* allocate memory for hash table
       */
      phasht = cts_mmalloc (SLIST_EL *, inr_days, CFUNCNAME);

      /* init hash table
       */
      phent = phasht;
      for (i = 0; i < inr_days; i++)
	*phent++ = NULL;

      /* inform calling function about location of hash table
       * and it's size
       */
      *list = phasht;
      *ht_size = inr_days;
    }


  /* determine and check hash table offset
   */
  pobj = (SLIST_EL *) object;
  ioffset = pobj->inoonmjd - imin_day;

  if (ioffset < 0 || ioffset > inr_days)
    cts_merror ("%s: invalid hash table offset!\n", CFUNCNAME);


  /* put struct in list of corresponding day;
   * inform sort function if it's the first entry
   */
  phent = phasht + ioffset;
  bnew_day = (*phent != NULL) ? false : true;

  sort_funcs [DS_SIGNAL][SORT_TIME] (object, (void **) phent, bnew_day, pn,
				     ht_size, dstyp);

  return;
}

#undef CFUNCNAME


#define CFUNCNAME "vs_day_tm_coi"

static void vs_day_tm_coi (void *object, void **list, bool binit,
			   HBOOK_FILE *pn, int *ht_size, DS_TYPE dstyp)

  /* put coinc-struct object into linked list; use hash table to speed up data
   * access (hash table entry for each day, time-sorted within one day)
   * input:
   * object  - struct to sort in
   * binit   - indicates first object
   * pn      - pointer to ntuple, used to derive size of hash table
   *           (one entry for each single day)
   * ht_size - size of hash table
   * dstype  - enum holding the structure type
   * output:
   * list    - pointer to hash table - set here
   */
{
  /* a new hash table entry if true
   */
  bool bnew_day;

  /* inr_days - period covered by ntuple data
   * ioffset  - offset for hash table to access different days
   * imin_day - first day of data sample
   * imax_day - last day of data sample
   */
  static int inr_days, ioffset, imax_day, imin_day, i;

  /* phasht - pointer to hash table
   * phent  - pointer to hash table entry
   * pobj   - pointer to struct object
   */
  static TLIST_EL **phasht, **phent, *pobj;



  /* new ntuple, so create hash table;
   * get max and min date from ntuple
   */
  if (binit)
    {
      /* check which time representation we're dealing with
       * (look for a unique variable name)
       */
      if (strstr (pn->pvarnam, cFRA200NS) != NULL)

	/* imager time format
	 */
	{
	  /* days = max - min + 1;
	   * +0.5f added because floats are truncated when converted to int
	   */
	  imax_day = (int) (cts_fget_max (pn, cNOONMJD) + 0.5f);
	  imin_day = (int) (cts_fget_min (pn, cNOONMJD) + 0.5f);
	  inr_days = imax_day - imin_day + 1;
	}

      else if (strstr (pn->pvarnam, cMUCHACHOS_SECS) != NULL)

	/* muchachos time format
	 */
	{
	  /* days = max - min + 1;
	   * +0.5f added because floats are truncated when converted to int
	   * -1.0f added to imin_days because 'noonmjd = mjd - 1' can occur
	   */
	  imax_day = (int) (cts_fget_max (pn, cMUCHACHOS_MJD) + 0.5f);
	  imin_day = (int) (cts_fget_min (pn, cMUCHACHOS_MJD) - 0.5f);
	  inr_days = imax_day - imin_day + 1;
	}

      else
	cts_merror ("%s: unknown time format!\n", pn->pname);


      /* allocate memory for hash table
       */
      phasht = cts_mmalloc (TLIST_EL *, inr_days, CFUNCNAME);

      /* init hash table
       */
      phent = phasht;
      for (i = 0; i < inr_days; i++)
	*phent++ = NULL;

      /* inform calling function about location of hash table
       */
      *list = phasht;
      *ht_size = inr_days;
    }


  /* determine and check hash table offset
   */
  pobj = (TLIST_EL *) object;
  ioffset = pobj->inoonmjd - imin_day;

  if (ioffset < 0 || ioffset > inr_days)
    cts_merror ("%s: invalid hash table offset!\n", CFUNCNAME);


  /* put struct in list of corresponding day;
   * inform sort function if it's the first entry
   */
  phent = phasht + ioffset;
  bnew_day = (*phent != NULL) ? false : true;

  sort_funcs [DS_TIME][SORT_TIME] (object, (void **) phent, bnew_day, pn,
				   ht_size, dstyp);

  return;
}

#undef CFUNCNAME


#define CFUNCNAME "cts_vsort_struc"

void cts_vsort_struc (void *pstruct, void **plist, bool binit, HBOOK_FILE *pn,
		      int *ht_size, DS_TYPE dstyp, SORT_TYPE styp)

  /* interface to all defined sorting routines
   * put pstruct into linked list;
   * plist points to root of list or hash table; it might be
   * changed within the sorting function
   */
{
  /* check enums
   */
  if (0 > styp || styp > MAX_SORT_TYPE || 0 > dstyp || dstyp > MAX_DS_TYPE)
    cts_merror ("%s: invalid enum!\n", CFUNCNAME);

  sort_funcs [dstyp] [styp] (pstruct, plist, binit, pn, ht_size, dstyp);
}

#undef CFUNCNAME
