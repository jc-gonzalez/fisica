//=//////////////////////////////////////////////////////////////////////
//=
//= pixels                
//=
//= @file        pixels.h
//= @desc        Header file
//= @author      J C Gonzalez
//= @email       gonzalez@mppmu.mpg.de
//= @date        Fri Feb 11 11:50:31 MET 2000
//=
//=----------------------------------------------------------------------
//=
//= Created: Fri Feb 11 11:50:31 MET 2000
//= Author:  Jose Carlos Gonzalez
//= Purpose: Header file
//= Notes:   See files README for details
//=    
//=----------------------------------------------------------------------
//=
//= $RCSfile$
//= $Revision$
//= $Author$ 
//= $Date$
//=
//=//////////////////////////////////////////////////////////////////////

// @T \newpage

//!@section Source code of |pixels.h|.

/*!@"

  In this section you can find the source code for the file
  |pixels.h|.  This file is mainly needed by
  |pixels.cxx|.
  
@"*/

//!@{

#ifndef _pixels_
#define _pixels_

#ifndef _this_
#define _this_ pixels
#endif

//!@}

//!@subsection Macro-definitions and shortcuts.

//!@{

#include "templmac.h"
#include "Histogram.h"

#define PIXELS_FILE "pixels.dat"

#define PIX_ARRAY_SIDE       40
#define PIX_ARRAY_HALF_SIDE  20

#define LINE_MAX_LENGTH  400

//++
// inline functions
//--

inline int SumNumPixelInRings(int r) { return ( 3*r*(r+1)+1 ); }
inline int FirstInRing(int r) { return ( SumNumPixelInRings(r-1) ); }
inline int NumPixelsInRing(int r) { return ( 6*r ); }
inline int FirstInRingSector(int r, int s) {
  return ( SumNumPixelInRings(r-1)+s*r );
}

//++
// shortcuts
//--

#define Make_Pixel(n, x, y, i, j, k) \
{ pki[n] = i, pkj[n] = j, pkk[n] = k, px[n] = x, py[n] = y; }

#define Map_Pixel(i, j, n) \
{ pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXNUM] = n; }

#define Map_Multiple_Pixel(i, j, n) \
{ int p = int(pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXNUM]); \
  p = (p<0)? 0 : p; \
  int p2 = (p/1000)%1000, p3 = p/1000000; \
  if (p3==0) { p += 1000000 * n;} \
  else if (p2==0) { p += 1000 * n;} \
  else { p += n; } \
  pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXNUM] = p; \
}

//!@}

//!@subsection Prototypes of functions.

//!@{

//++
// prototypes
//--

void read_pixels(void);
void create_pixels(void);
static int pixels_are_neig(int pix1, int pix2);
int pixel_bit_mask(int i, float q0, Histogram<float> **d, int b);
static void hex2coord (int ki, int kj, int kk, float &x, float &y);
static void coord2hex (float cx, float cy, int &ki, int &kj, int &kk);
static void ijk2xy (float di, float dj, float dk, float &x, float &y);
static void xy2ijk (float cx, float cy, float &di, float &dj, float &dk);
int pixel_id (float x, float y);

//!@}

//!@{

#endif // ! _pixels_

//!@}

//=------------------------------------------------------------
//!@subsection Log of this file.

//!@{

/*
 * $Log$
 * Revision 1.1.1.1  2000/06/07 21:03:49  gonzalez
 * camera-simple created from camera-complex
 *
 * Revision 1.1  2000/06/06  16:26:01  gonzalez
 * *** empty log message ***
 *
 * Revision 1.12  2000/01/27  10:48:51  gonzalez
 * Trigger patterns + timing seem to work
 *
 * Revision 1.11  1999/12/13  14:57:28  gonzalez
 * pre-navidades 2000
 *
 * Revision 1.10  1999/12/03  17:18:09  gonzalez
 * *** empty log message ***
 *
 * Revision 1.9  1999/11/22  21:02:11  gonzalez
 * *** empty log message ***
 *
 * Revision 1.8  1999/10/05  11:39:52  gonzalez
 * Sep. 1999
 *
 * Revision 1.7  1999/03/15  14:59:09  gonzalez
 * camera-1_1
 *
 * Revision 1.6  1999/03/02  09:56:13  gonzalez
 * *** empty log message ***
 *
 * Revision 1.5  1999/01/14  17:32:43  gonzalez
 * Added to camera the STDIN input option (data_from_input)
 *
 */

//!@}
//=EOF
