#include <stdio.h>
#include "ctshbook.h"
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctscuts.h"

/* declaration of all local functions
 */
static ZAB_LIST_EL *get_zab_dl (DLIST_EL *, DLIST_EL **, int);
static ZAB_LIST_EL *get_zab_sl (SLIST_EL *, SLIST_EL **, int);
static ZAB_LIST_EL *get_zab_tl (TLIST_EL *, TLIST_EL **, int);
static ZAB_LIST_EL *get_zab_el (ELIST_EL *, ELIST_EL **, int);

/* array of all defined zab functions - must match definition of
 * DS_TYPE in cut.h
 */
static ZAB_LIST_EL *(*zab_funcs [MAX_DS_TYPE]) () =
{get_zab_dl, get_zab_sl, get_zab_tl, get_zab_el};



/* comment:
 * the functions 'get_zab_sl', 'get_zab_dl'  and 'get_zab_el' are
 * nearly the same; the only difference is that they access different
 * structs (SLIST_EL, DLIST_EL and ELIST_ELtype)
 * I didn't find an elegant way to handle this within one function yet
 */
#define CFUNCNAME "get_zab_dl"

static ZAB_LIST_EL *get_zab_dl (DLIST_EL *pevt_on, DLIST_EL **ppevt_off,
				int imin_evts_bin)

  /* this function takes two lists (ON and OFF data) and calculates
   * the borders of all zenith angle bins (ordered for increasing za).
   * the two lists are expected to be sorted for increasing zenith
   * angle
   * pevt_on       - linked list of ON data event structures
   * ppevt_off     - linked list of OFF data event structures
   * imin_evts_bin - minimum number of events needed for one za bin
   */
{
  int ievt_on, ievt_off;
  double dza_on, dza_off, dzab_max_on, dzab_min_on, dzab_max_off, dzab_min_off;

  bool bfinished, bbin_on_read, bbin_off_read, blast_off_evt, blast_on_evt;

  ZAB_LIST_EL *pzab_root, *pzab_el, *pzab_prev_el;
  DLIST_EL *pevt_off;


  /*
   * check for valid lists
   */
  if (pevt_on == NULL)
    cts_merror ("%s: empty ON data list.\n", CFUNCNAME);

  /* we use only OFF data within the ON data ZA range - so
   * set OFF pointer to first event within ON data ZA range
   * (reset ppevt_off afterwards)
   */
  pevt_off = *ppevt_off;

  while (pevt_off != NULL && pevt_off->dza < pevt_on->dza)
    pevt_off = pevt_off->p2next;

  if (pevt_off == NULL)
    cts_merror ("%s: no OFF evt in ON za range.\n", CFUNCNAME);

  *ppevt_off = pevt_off;


  /* allocate memory for first element of zab-list
   */
  pzab_root = cts_mmalloc (ZAB_LIST_EL, 1, CFUNCNAME);
  pzab_el = pzab_root;
  pzab_el->p2prev = pzab_el->p2next = NULL;


  /* init variables
   */
  ievt_on = ievt_off = 0;
  blast_on_evt = blast_off_evt = false;
  bfinished = bbin_on_read = bbin_off_read = false;
  dzab_min_on = dzab_min_off = 180.;
  dza_on = dzab_max_on = dza_off = dzab_max_off = 0.;


  /*
   * loop over all events and find ZA-bin borders
   */
  while (!bfinished)

    {
      /* read in ZA bin ('imin_evts_bin' events) and fill
       * ON histogram
       */
      bbin_on_read = false;

      while (!bbin_on_read)

	{
	  ievt_on++;
	  dzab_max_on = max (pevt_on->dza, dzab_max_on);
	  dzab_min_on = min (pevt_on->dza, dzab_min_on);


	  /* check break conditions
	   */
	  if (pevt_on->p2next == NULL)

	    /* we reached last event; set flags and upper za boundary
	     */
	    {
	      bbin_on_read = true;
	      blast_on_evt = true;

	      dza_on = dzab_max_on;
	    }

	  else if (ievt_on >= imin_evts_bin && pevt_on->p2next->dza >= dza_off)

	    /* enough events read for this bin and on bin boundary matches
	     * off bin boundary
	     */
	    {
	      /* set flag, upper za boundary and struct pointer to next
	       * event
	       */
	      bbin_on_read = true;
	      dza_on = dzab_max_on;

	      pevt_on = pevt_on->p2next;
	    }

	  else

	    /* still some events needed for this bin
	     */
	    pevt_on = pevt_on->p2next;
	}


      /* fill zab-struct; if there're not enough events
       * left to fill this bin, it's merged with the
       * preceeding bin below
       */
      pzab_el->dza_max_on = dzab_max_on;
      pzab_el->dza_min_on = dzab_min_on;
      pzab_el->ievt_on = ievt_on;



      /* read in ZA bin ('imin_evts_bin' events) and fill
       * OFF histogram;
       * comment: bbin_off_read is reset to 'false' below (when a bin is
       *          completed) but not here.
       *          reason: it might happen that additional
       *          ON evts are needed to fill the bin up to the OFF ZA
       *          boundary. In this case the ON while loop above is entered
       *          again, but this OFF while loop must not be entered again
       *          -> therefore the bool bbin_off_read must be kept true
       */
      while (!bbin_off_read && !blast_off_evt)

	{
	  ievt_off++;
	  dzab_max_off = max (pevt_off->dza, dzab_max_off);
	  dzab_min_off = min (pevt_off->dza, dzab_min_off);


	  /* check break conditions
	   */
	  if (pevt_off->p2next == NULL)

	    /* we reached last event
	     */
	    {
	      bbin_off_read = true;
	      blast_off_evt = true;

	      /* set ZA to maximum value - this forces the program to read
	       * in all ON evts during the next loop
	       */
	      dza_off = 180.;
	    }

	  else if (ievt_off >= imin_evts_bin && pevt_off->dza >= dza_on)

	    /* enough events read for this bin and off bin boundary is
	     * above (or equal) on bin boundary
	     */
	    {
	      /* set flag, upper za boundary and struct pointer to next
	       * event
	       */
	      bbin_off_read = true;
	      dza_off = pevt_off->dza;

	      pevt_off = pevt_off->p2next;

	      /* reset on flag, if on bin boundary doesn't match
	       * new off bin boundary (the on while-loop is then
	       * entered again)
	       */
	      if (!blast_on_evt && pevt_on->dza < dza_off)
		bbin_on_read = false;
	    }

	  else

	    /* still some events needed for this bin
	     */
	    pevt_off = pevt_off->p2next;
	}


      /* fill zab-struct; if there're not enough events
       * left to fill this bin, it's merged with the
       * preceeding bin below
       */
      pzab_el->dza_max_off = dzab_max_off;
      pzab_el->dza_min_off = dzab_min_off;
      pzab_el->ievt_off = ievt_off;



      /* use flags from ON and OFF loop to decide if a complete bin
       * was read; allocate memory for the next zab-struct if still
       * some events left
       */
      if (!blast_on_evt && !blast_off_evt && bbin_off_read && bbin_on_read)

	{
	  /* reset counters and variables for next bin and reset off bin flag
	   */
	  ievt_on = ievt_off = 0;
	  bbin_off_read = false;
	  dzab_min_on = dzab_max_on;
	  dzab_min_off = dzab_max_off;

	  /* allocate memory for next zab-element and reset pointers
	   */
	  pzab_prev_el = pzab_el;
	  pzab_el = cts_mmalloc (ZAB_LIST_EL, 1, CFUNCNAME);
	  pzab_prev_el->p2next = pzab_el;
	  pzab_el->p2prev = pzab_prev_el;
	  pzab_el->p2next = NULL;
	}


      /* check if we're finished and check whether we have to merge
       * the last two bins
       */
      if (blast_on_evt)
	{
	  if ((ievt_off < imin_evts_bin || ievt_on < imin_evts_bin)
	      && pzab_el->p2prev != NULL)

	    /* we have at least two za-bins and the last bin contains
	     * not enough events - so we have to merge bins
	     */
	    {
	      pzab_el->p2prev->ievt_on += pzab_el->ievt_on;
	      pzab_el->p2prev->ievt_off += pzab_el->ievt_off;
	      pzab_el->p2prev->dza_max_on = pzab_el->dza_max_on;
	      pzab_el->p2prev->dza_max_off = pzab_el->dza_max_off;
	      pzab_el->p2prev->p2next = NULL;
	    }

	  bfinished = true;
	}

    }   /* END of while (!bfinished) */

  return (pzab_root);
}

#undef CFUNCNAME


#define CFUNCNAME "get_zab_sl"

static ZAB_LIST_EL *get_zab_sl (SLIST_EL *pevt_on, SLIST_EL **ppevt_off,
				int imin_evts_bin)

  /* this function takes two lists (ON and OFF data) and calculates
   * the borders of all zenith angle bins (ordered for increasing za).
   * the two lists are expected to be sorted for increasing zenith
   * angle
   * pevt_on       - linked list of ON data event structures
   * ppevt_off     - linked list of OFF data event structures
   * imin_evts_bin - minimum number of events needed for one za bin
   */
{
  int ievt_on, ievt_off;
  double dza_on, dza_off, dzab_max_on, dzab_min_on, dzab_max_off, dzab_min_off;

  bool bfinished, bbin_on_read, bbin_off_read, blast_off_evt, blast_on_evt;

  ZAB_LIST_EL *pzab_root, *pzab_el, *pzab_prev_el;
  SLIST_EL *pevt_off;


  /*
   * check for valid lists
   */
  if (pevt_on == NULL)
    cts_merror ("%s: empty ON data list.\n", CFUNCNAME);

  /* we use only OFF data within the ON data ZA range - so
   * set OFF pointer to first event within ON data ZA range
   * (reset ppevt_off afterwards)
   */
  pevt_off = *ppevt_off;

  while (pevt_off != NULL && pevt_off->dza < pevt_on->dza)
    pevt_off = pevt_off->p2next;

  if (pevt_off == NULL)
    cts_merror ("%s: no OFF evt in ON za range.\n", CFUNCNAME);

  *ppevt_off = pevt_off;


  /* allocate memory for first element of zab-list
   */
  pzab_root = cts_mmalloc (ZAB_LIST_EL, 1, CFUNCNAME);
  pzab_el = pzab_root;
  pzab_el->p2prev = pzab_el->p2next = NULL;


  /* init variables
   */
  ievt_on = ievt_off = 0;
  blast_on_evt = blast_off_evt = false;
  bfinished = bbin_on_read = bbin_off_read = false;
  dzab_min_on = dzab_min_off = 180.;
  dza_on = dzab_max_on = dza_off = dzab_max_off = 0.;


  /*
   * loop over all events and find ZA-bin borders
   */
  while (!bfinished)

    {
      /* read in ZA bin ('imin_evts_bin' events) and fill
       * ON histogram
       */
      bbin_on_read = false;

      while (!bbin_on_read)

	{
	  ievt_on++;
	  dzab_max_on = max (pevt_on->dza, dzab_max_on);
	  dzab_min_on = min (pevt_on->dza, dzab_min_on);


	  /* check break conditions
	   */
	  if (pevt_on->p2next == NULL)

	    /* we reached last event; set flags and upper za boundary
	     */
	    {
	      bbin_on_read = true;
	      blast_on_evt = true;

	      dza_on = dzab_max_on;
	    }

	  else if (ievt_on >= imin_evts_bin && pevt_on->p2next->dza >= dza_off)

	    /* enough events read for this bin and on bin boundary matches
	     * off bin boundary
	     */
	    {
	      /* set flag, upper za boundary and struct pointer to next
	       * event
	       */
	      bbin_on_read = true;
	      dza_on = dzab_max_on;

	      pevt_on = pevt_on->p2next;
	    }

	  else

	    /* still some events needed for this bin
	     */
	    pevt_on = pevt_on->p2next;
	}


      /* fill zab-struct; if there're not enough events
       * left to fill this bin, it's merged with the
       * preceeding bin below
       */
      pzab_el->dza_max_on = dzab_max_on;
      pzab_el->dza_min_on = dzab_min_on;
      pzab_el->ievt_on = ievt_on;



      /* read in ZA bin ('imin_evts_bin' events) and fill
       * OFF histogram;
       * comment: bbin_off_read is reset to 'false' below (when a bin is
       *          completed) but not here.
       *          reason: it might happen that additional
       *          ON evts are needed to fill the bin up to the OFF ZA
       *          boundary. In this case the ON while loop above is entered
       *          again, but this OFF while loop must not be entered again
       *          -> therefore the bool bbin_off_read must be kept true
       */
      while (!bbin_off_read && !blast_off_evt)

	{
	  ievt_off++;
	  dzab_max_off = max (pevt_off->dza, dzab_max_off);
	  dzab_min_off = min (pevt_off->dza, dzab_min_off);


	  /* check break conditions
	   */
	  if (pevt_off->p2next == NULL)

	    /* we reached last event
	     */
	    {
	      bbin_off_read = true;
	      blast_off_evt = true;

	      /* set ZA to maximum value - this forces the program to read
	       * in all ON evts during the next loop
	       */
	      dza_off = 180.;
	    }

	  else if (ievt_off >= imin_evts_bin && pevt_off->dza >= dza_on)

	    /* enough events read for this bin and off bin boundary is
	     * above (or equal) on bin boundary
	     */
	    {
	      /* set flag, upper za boundary and struct pointer to next
	       * event
	       */
	      bbin_off_read = true;
	      dza_off = pevt_off->dza;

	      pevt_off = pevt_off->p2next;

	      /* reset on flag, if on bin boundary doesn't match
	       * new off bin boundary (the on while-loop is then
	       * entered again)
	       */
	      if (!blast_on_evt && pevt_on->dza < dza_off)
		bbin_on_read = false;
	    }

	  else

	    /* still some events needed for this bin
	     */
	    pevt_off = pevt_off->p2next;
	}


      /* fill zab-struct; if there're not enough events
       * left to fill this bin, it's merged with the
       * preceeding bin below
       */
      pzab_el->dza_max_off = dzab_max_off;
      pzab_el->dza_min_off = dzab_min_off;
      pzab_el->ievt_off = ievt_off;



      /* use flags from ON and OFF loop to decide if a complete bin
       * was read; allocate memory for the next zab-struct if still
       * some events left
       */
      if (!blast_on_evt && !blast_off_evt && bbin_off_read && bbin_on_read)

	{
	  /* reset counters and variables for next bin and reset off bin flag
	   */
	  ievt_on = ievt_off = 0;
	  bbin_off_read = false;
	  dzab_min_on = dzab_max_on;
	  dzab_min_off = dzab_max_off;

	  /* allocate memory for next zab-element and reset pointers
	   */
	  pzab_prev_el = pzab_el;
	  pzab_el = cts_mmalloc (ZAB_LIST_EL, 1, CFUNCNAME);
	  pzab_prev_el->p2next = pzab_el;
	  pzab_el->p2prev = pzab_prev_el;
	  pzab_el->p2next = NULL;
	}


      /* check if we're finished and check whether we have to merge
       * the last two bins
       */
      if (blast_on_evt)
	{
	  if ((ievt_off < imin_evts_bin || ievt_on < imin_evts_bin)
	      && pzab_el->p2prev != NULL)

	    /* we have at least two za-bins and the last bin contains
	     * not enough events - so we have to merge bins
	     */
	    {
	      pzab_el->p2prev->ievt_on += pzab_el->ievt_on;
	      pzab_el->p2prev->ievt_off += pzab_el->ievt_off;
	      pzab_el->p2prev->dza_max_on = pzab_el->dza_max_on;
	      pzab_el->p2prev->dza_max_off = pzab_el->dza_max_off;
	      pzab_el->p2prev->p2next = NULL;
	    }

	  bfinished = true;
	}

    }   /* END of while (!bfinished) */

  return (pzab_root);
}

#undef CFUNCNAME


#define CFUNCNAME "get_zab_el"

static ZAB_LIST_EL *get_zab_el (ELIST_EL *pevt_on, ELIST_EL **ppevt_off,
				int imin_evts_bin)

  /* this function takes two lists (ON and OFF data) and calculates
   * the borders of all zenith angle bins (ordered for increasing za).
   * the two lists are expected to be sorted for increasing zenith
   * angle
   * pevt_on       - linked list of ON data event structures
   * ppevt_off     - linked list of OFF data event structures
   * imin_evts_bin - minimum number of events needed for one za bin
   */
{
  int ievt_on, ievt_off;
  double dza_on, dza_off, dzab_max_on, dzab_min_on, dzab_max_off, dzab_min_off;

  bool bfinished, bbin_on_read, bbin_off_read, blast_off_evt, blast_on_evt;

  ZAB_LIST_EL *pzab_root, *pzab_el, *pzab_prev_el;
  ELIST_EL *pevt_off;


  /*
   * check for valid lists
   */
  if (pevt_on == NULL)
    cts_merror ("%s: empty ON data list.\n", CFUNCNAME);

  /* we use only OFF data within the ON data ZA range - so
   * set OFF pointer to first event within ON data ZA range
   * (reset ppevt_off afterwards)
   */
  pevt_off = *ppevt_off;

  while (pevt_off != NULL && pevt_off->dza < pevt_on->dza)
    pevt_off = pevt_off->p2next;

  if (pevt_off == NULL)
    cts_merror ("%s: no OFF evt in ON za range.\n", CFUNCNAME);

  *ppevt_off = pevt_off;


  /* allocate memory for first element of zab-list
   */
  pzab_root = cts_mmalloc (ZAB_LIST_EL, 1, CFUNCNAME);
  pzab_el = pzab_root;
  pzab_el->p2prev = pzab_el->p2next = NULL;


  /* init variables
   */
  ievt_on = ievt_off = 0;
  blast_on_evt = blast_off_evt = false;
  bfinished = bbin_on_read = bbin_off_read = false;
  dzab_min_on = dzab_min_off = 180.;
  dza_on = dzab_max_on = dza_off = dzab_max_off = 0.;


  /*
   * loop over all events and find ZA-bin borders
   */
  while (!bfinished)

    {
      /* read in ZA bin ('imin_evts_bin' events) and fill
       * ON histogram
       */
      bbin_on_read = false;

      while (!bbin_on_read)

	{
	  ievt_on++;
	  dzab_max_on = max (pevt_on->dza, dzab_max_on);
	  dzab_min_on = min (pevt_on->dza, dzab_min_on);


	  /* check break conditions
	   */
	  if (pevt_on->p2next == NULL)

	    /* we reached last event; set flags and upper za boundary
	     */
	    {
	      bbin_on_read = true;
	      blast_on_evt = true;

	      dza_on = dzab_max_on;
	    }

	  else if (ievt_on >= imin_evts_bin && pevt_on->p2next->dza >= dza_off)

	    /* enough events read for this bin and on bin boundary matches
	     * off bin boundary
	     */
	    {
	      /* set flag, upper za boundary and struct pointer to next
	       * event
	       */
	      bbin_on_read = true;
	      dza_on = dzab_max_on;

	      pevt_on = pevt_on->p2next;
	    }

	  else

	    /* still some events needed for this bin
	     */
	    pevt_on = pevt_on->p2next;
	}


      /* fill zab-struct; if there're not enough events
       * left to fill this bin, it's merged with the
       * preceeding bin below
       */
      pzab_el->dza_max_on = dzab_max_on;
      pzab_el->dza_min_on = dzab_min_on;
      pzab_el->ievt_on = ievt_on;



      /* read in ZA bin ('imin_evts_bin' events) and fill
       * OFF histogram;
       * comment: bbin_off_read is reset to 'false' below (when a bin is
       *          completed) but not here.
       *          reason: it might happen that additional
       *          ON evts are needed to fill the bin up to the OFF ZA
       *          boundary. In this case the ON while loop above is entered
       *          again, but this OFF while loop must not be entered again
       *          -> therefore the bool bbin_off_read must be kept true
       */
      while (!bbin_off_read && !blast_off_evt)

	{
	  ievt_off++;
	  dzab_max_off = max (pevt_off->dza, dzab_max_off);
	  dzab_min_off = min (pevt_off->dza, dzab_min_off);


	  /* check break conditions
	   */
	  if (pevt_off->p2next == NULL)

	    /* we reached last event
	     */
	    {
	      bbin_off_read = true;
	      blast_off_evt = true;

	      /* set ZA to maximum value - this forces the program to read
	       * in all ON evts during the next loop
	       */
	      dza_off = 180.;
	    }

	  else if (ievt_off >= imin_evts_bin && pevt_off->dza >= dza_on)

	    /* enough events read for this bin and off bin boundary is
	     * above (or equal) on bin boundary
	     */
	    {
	      /* set flag, upper za boundary and struct pointer to next
	       * event
	       */
	      bbin_off_read = true;
	      dza_off = pevt_off->dza;

	      pevt_off = pevt_off->p2next;

	      /* reset on flag, if on bin boundary doesn't match
	       * new off bin boundary (the on while-loop is then
	       * entered again)
	       */
	      if (!blast_on_evt && pevt_on->dza < dza_off)
		bbin_on_read = false;
	    }

	  else

	    /* still some events needed for this bin
	     */
	    pevt_off = pevt_off->p2next;
	}


      /* fill zab-struct; if there're not enough events
       * left to fill this bin, it's merged with the
       * preceeding bin below
       */
      pzab_el->dza_max_off = dzab_max_off;
      pzab_el->dza_min_off = dzab_min_off;
      pzab_el->ievt_off = ievt_off;



      /* use flags from ON and OFF loop to decide if a complete bin
       * was read; allocate memory for the next zab-struct if still
       * some events left
       */
      if (!blast_on_evt && !blast_off_evt && bbin_off_read && bbin_on_read)

	{
	  /* reset counters and variables for next bin and reset off bin flag
	   */
	  ievt_on = ievt_off = 0;
	  bbin_off_read = false;
	  dzab_min_on = dzab_max_on;
	  dzab_min_off = dzab_max_off;

	  /* allocate memory for next zab-element and reset pointers
	   */
	  pzab_prev_el = pzab_el;
	  pzab_el = cts_mmalloc (ZAB_LIST_EL, 1, CFUNCNAME);
	  pzab_prev_el->p2next = pzab_el;
	  pzab_el->p2prev = pzab_prev_el;
	  pzab_el->p2next = NULL;
	}


      /* check if we're finished and check whether we have to merge
       * the last two bins
       */
      if (blast_on_evt)
	{
	  if ((ievt_off < imin_evts_bin || ievt_on < imin_evts_bin)
	      && pzab_el->p2prev != NULL)

	    /* we have at least two za-bins and the last bin contains
	     * not enough events - so we have to merge bins
	     */
	    {
	      pzab_el->p2prev->ievt_on += pzab_el->ievt_on;
	      pzab_el->p2prev->ievt_off += pzab_el->ievt_off;
	      pzab_el->p2prev->dza_max_on = pzab_el->dza_max_on;
	      pzab_el->p2prev->dza_max_off = pzab_el->dza_max_off;
	      pzab_el->p2prev->p2next = NULL;
	    }

	  bfinished = true;
	}

    }   /* END of while (!bfinished) */

  return (pzab_root);
}

#undef CFUNCNAME


#define CFUNCNAME "get_zab_tl"

static ZAB_LIST_EL *get_zab_tl (TLIST_EL *pevt_on, TLIST_EL **ppevt_off,
				int imin_evts_bin)

  /* TLIST_EL structs do not contain the za - so justs print an error
   * message and quit
   */
{
   cts_merror ("%s zab routine not supported for this struct!\n", CFUNCNAME);

  return (NULL);
}

#undef CFUNCNAME


#define CFUNCNAME "cts_pza_bins"

ZAB_LIST_EL *cts_pza_bins (void *pevt_on, void **ppevt_off, int imin_evts_bin,
			   DS_TYPE dstyp)

  /* interface to all functions for deriving za-binnings:
   * this function takes two lists (ON and OFF data) and calculates
   * the borders of all zenith angle bins (ordered for increasing za).
   * the two lists are expected to be sorted for increasing zenith
   * angle
   * pevt_on       - linked list of ON data event structures
   * ppevt_off     - linked list of OFF data event structures
   * imin_evts_bin - minimum number of events needed for one za bin
   */
{
  ZAB_LIST_EL *pzab_root;


  /* check enums
   */
  if (0 > dstyp || dstyp > MAX_DS_TYPE)
    cts_merror ("%s: invalid enum!\n", CFUNCNAME);

  pzab_root = zab_funcs [dstyp] (pevt_on, ppevt_off, imin_evts_bin);

  return (pzab_root);
}
