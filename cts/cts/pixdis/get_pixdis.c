/*
 * Atmospheric Cherenkov Telescope Data Analysis Software
 * MPI f"ur Physik, M"unchen
 *
 */

/*
 * This File belongs to the program
 *
 *         P I X D I S
 *
 * Purpose: Read in Bitmaps from events-Ntuple and create histogram
 *          containing distribution of pixels with signal
 *
 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "ctsbase.h"
#include "ctshbook.h"
#include "pixdis.h"



/*  local defines
 */
#define iHISTID 100

static void vgen_pix_coords (double [], double []);


#define CFUNCNAME "vgen_pix_coords"

static void vgen_pix_coords (double dxval[], double dyval[])

  /* generate pixel coordinates, return value is number of pixels;
   * (modified version 'gen_pixel_coordinates' from imaging.c)
   */
{

  int i, itot_inside_ring, iN, in, ipixno, iring_no, ipix_in_ring, isegment;
  float fsegment_fract = 0.f;
  double dtsize = 0.;
  double dhsize = 0.;
  double dxfirst_pix = 0.;
  double dyfirst_pix = 0.;
  double ddxseg1 = 0., ddxseg2 = 0., ddxseg3 = 0.;
  double ddxseg4 = 0., ddxseg5 = 0., ddxseg6 = 0.;
  double ddyseg1 = 0., ddyseg2 = 0., ddyseg3 = 0.;
  double ddyseg4 = 0., ddyseg5 = 0., ddyseg6 = 0.;
  
  dtsize = dPIXSIZE * sqrt (3.) / 2.;
  dhsize = dPIXSIZE / 2.;

  /* Loop over inumpixels to generate co-ordinates for every pixel in camera
   */
  for (ipixno=1; ipixno <= iMAXNUMPIX; ipixno++)
    {
      /* Initialise variables. The central pixel = ipixno 1 in ring iring_no 0
       */
      in = 0;
      i = 0;
      itot_inside_ring = 0;
      iring_no = 0;

      /* Calculate the number of pixels out to and including the ring containing
       * pixel number ipixno e.g. for pixel number 17 in ring number 2
       * itot_inside_ring = 19
       */
      while (itot_inside_ring == 0)
	{
	  iN = 3*(i*(i+1)) + 1;
      
	  if (ipixno <= iN)
	    {
	      iring_no = i;
	      itot_inside_ring = iN;
	    }
	  i++;
	}
    
    
      /* Find the number of pixels which make up ring number iring_no e.g.
       * ipix_in_ring = 6 for ring 1
       */        
      ipix_in_ring = 0;
      for (i = 0; i < iring_no; ++i)
	{
	  ipix_in_ring = ipix_in_ring + 6;
	}

      /* The camera is viewed as 6 radial segments ("pie slices"). Knowing the
       * number of pixels in its ring calculate which segment the pixel ipixno is
       * in. Then find how far across this segment it is as a fraction of the number
       * of pixels in this sixth of the ring (ask SMB).
       */
      isegment = 0;
      fsegment_fract = 0.;
      if (iring_no > 0)
	{
	  /* integer division ! numbering starts at 1
	   */
	  isegment = (ipixno - itot_inside_ring + ipix_in_ring - 0.5) / iring_no + 1;
      
	  fsegment_fract = (ipixno - (itot_inside_ring - ipix_in_ring)) -
	    ((isegment-1) * iring_no) - 1 ;
	}

      /* the first pixel in each ring lies on the positive x axis at a distance
       * dxfirst_pix = iring_no * the pixel width (flat to flat) dPIXSIZE.
       */
      dxfirst_pix = dPIXSIZE * iring_no;
      dyfirst_pix = 0.;

      /* the vector between the first and last pixels in a segment n is
       * (ddxsegn, ddysegn)
       */
      ddxseg1 = - dhsize*iring_no;
      ddyseg1 = dtsize*iring_no;
      ddxseg2 = -dPIXSIZE*iring_no;
      ddyseg2 = 0.;
      ddxseg3 = ddxseg1;
      ddyseg3 = -ddyseg1;
      ddxseg4 = -ddxseg1;
      ddyseg4 = -ddyseg1;
      ddxseg5 = -ddxseg2;
      ddyseg5 = 0.;
      ddxseg6 = -ddxseg1;
      ddyseg6 = ddyseg1;
    
      /* to find the position of pixel ipixno take the position of the first pixel
       * in the ring and move anti-clockwise around the ring by adding the segment
       * to segment vectors.
       */

      switch (isegment)
	{
      
	case 0:

	  dxval[ipixno-1] = 0.;
	  dyval[ipixno-1] = 0.; 

	  break;

	case 1: 
	  dxval[ipixno-1] = dxfirst_pix - dhsize * fsegment_fract;
	  dyval[ipixno-1] = dyfirst_pix + dtsize * fsegment_fract;
      
	  break;
      
	case 2:
      
	  dxval[ipixno-1] = dxfirst_pix + ddxseg1 - dPIXSIZE * fsegment_fract;
	  dyval[ipixno-1] = dyfirst_pix + ddyseg1 + 0.;
      
	  break;
      
	case 3:
      
	  dxval[ipixno-1] = dxfirst_pix + ddxseg1 + ddxseg2 - dhsize * fsegment_fract;
	  dyval[ipixno-1] = dyfirst_pix + ddyseg1 + ddyseg2 - dtsize * fsegment_fract;
      
	  break;

	case 4:
      
	  dxval[ipixno-1] = dxfirst_pix + ddxseg1 + ddxseg2 + ddxseg3
	    + dhsize * fsegment_fract;

	  dyval[ipixno-1] = dyfirst_pix + ddyseg1 + ddyseg2 + ddyseg3
	    - dtsize * fsegment_fract;
      
	  break;
      
	case 5:

	  dxval[ipixno-1] = dxfirst_pix + ddxseg1 + ddxseg2 + ddxseg3 + ddxseg4
	    + dPIXSIZE * fsegment_fract;

	  dyval[ipixno-1] = dyfirst_pix + ddyseg1 + ddyseg2 + ddyseg3 + ddyseg4 + 0.;
    
	  break;
      
	case 6:
      
	  dxval[ipixno-1] = dxfirst_pix + ddxseg1 + ddxseg2 + ddxseg3 + ddxseg4
	    + ddxseg5 + dhsize * fsegment_fract;

	  dyval[ipixno-1] = dyfirst_pix + ddyseg1 + ddyseg2 + ddyseg3 + ddyseg4
	    + ddyseg5 + dtsize * fsegment_fract;
      
	  break;
      
	default:

	  cts_merror ("%s: switch reached default.\n", CFUNCNAME);

	} /* end switch */

    } /* end for */
}

#undef CFUNCNAME


#define CFUNCNAME "vget_pix_bitmap"

void vget_pix_bitmap (HBOOK_FILE *pntuple)

  /* reads in events from passed hbook-file and fills
   * 2dim histogram with bitmap information
   */
{
  int i, k, n, imax, ibitnr, iintnr, ibitsperint, imaxbyte;
  int *ppixbitmap;                   /* array to contain Bitmap information */
  char cvarname[iVARNAMELEN];

  float *ppmtbit1;                   /* points to first bitmap variable */
  float fx, fy, fweight = 1;

  double dxval[iMAXNUMPIX], dyval[iMAXNUMPIX]; /* arrays to store pixel-coordinates */

  HISTO hist_sig = {"pixdist", iHISTID, 27, 15, -1.75f, 1.75f, -1.6238f,
		    1.6238f, 0.f};

  HBOOK_FILE hfile;


  /* book 2-dim histogram
   */
  cts_vbook_hist (&hist_sig);


  /* create hbook-file for histogram
   */
  hfile.pname = "pixdist.hbook";
  hfile.copt = 'N';
  hfile.ilrec = 1024;
  hfile.iid = -1;

  cts_vopen_hbook_file (&hfile);


  /* check whether all iPMTBITNUM Bitmap variables are present
   * (it's enough to check for the maximum one)
   */
  sprintf (cvarname, cBITTEMPLATE, iPMTBITNUM);
  (void *) cts_pset_nam (pntuple, cvarname);

  /* get location of bitmap variables in pevent_data
   */
  sprintf (cvarname, cBITTEMPLATE, 1);
  ppmtbit1 = cts_pset_nam (pntuple, cvarname);


  /* allocate memory for ppixbitmap
   */
  ibitsperint = iBITSPERBYTE * sizeof (int);
  ppixbitmap = cts_mmalloc (int, (1 + iMAXNUMPIX / ibitsperint), CFUNCNAME);

  /* number of bytes needed for Bitmap
   */
  imaxbyte = (int) (iMAXNUMPIX / iBITSPERBYTE) + 1;

  /* get pixel-coordinates
   */
  vgen_pix_coords (dxval, dyval);


  /* loop over all events
   */
  imax = pntuple->inr_events;

  for (i = 1; i <= imax; i++)
    {
      /* read in event information
       */
      cts_hgnf (pntuple, i);


      /* get Bitmap variables into ppixbitmap-array;
       * it is absolutely necessary that the Ntuple-parameters to hold
       * the Bitmap succeed each other!
       */
      memcpy (ppixbitmap, ppmtbit1, imaxbyte);


      /* get bitmap content into histogram
       */
      for (n = 1, k = 0; k < iMAXNUMPIX; k++)
	{
	  iintnr = (int) k / ibitsperint;
	  ibitnr = k % ibitsperint;

	  /* check if bit is set
	   */
	  if (*(ppixbitmap + iintnr) & (n << ibitnr))
	    {
	      /* coordinates of pixel #k
	       */
	      fx = (float) dxval[k];
	      fy = (float) dyval[k];

	      cts_hfill (iHISTID, fx, fy, fweight);
	    }

	} /* end get bitmap content */
    }


  /* write histogram to hbook-file
   */
  cts_hrout (iHISTID, hfile.ilun, &hfile.copt);
  cts_hrend (&hfile);
}

#undef CFUNCNAME
