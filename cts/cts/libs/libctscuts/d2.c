static void vs_za_sig (void *object, void **list, bool binit, HBOOK_FILE *pn,
		       int *hts, DS_TYPE dstyp)

  /* put signal-struct object into linked list;
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
