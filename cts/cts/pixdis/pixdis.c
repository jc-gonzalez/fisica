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
 * Purpose: driver for get_pixdis
 *
 */

#include <stdio.h>
#include "ctsbase.h"
#include "ctsmath.h"
#include "ctshbook.h"
#include "pixdis.h"


int main (int argc, char **argv)

  /* driver for 'vget_pix_bitmap ()' routine
   */
{
  HBOOK_FILE hfile;



  if (argc < 2)
    {
      fprintf (stdout, "call: pixdis 'hbook-file-name' \n");
      exit (iERRCODE);
    }

  /* init hfile-struct
   */
  hfile.pname = *++argv;
  hfile.copt = ' ';
  

  /* initialize hbook
   */
  cts_vinit_hbook ();

  /* get information on hbook-file
   */
  cts_vopen_hbook_file (&hfile);
   
  /* fill bitmap into histogram and store it to disk
   */
  vget_pix_bitmap (&hfile);

  /* close hbook-file
   */
  cts_hrend (&hfile);

  exit(0);
}
