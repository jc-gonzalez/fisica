//=//////////////////////////////////////////////////////////////////////
//=
//= moments                
//=
//= @file        moments.h
//= @desc        Header file
//= @author      J C Gonzalez
//= @email       gonzalez@mppmu.mpg.de
//= @date        Thu May  7 16:24:22 1998
//=
//=----------------------------------------------------------------------
//=
//= Created: Thu May  7 16:24:22 1998
//= Author:  Jose Carlos Gonzalez
//= Purpose: Program for reflector simulation
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

//# Source code of |moments.h|.

/*"
  In this section you can find the source code for the file
  |moments.h|.  This file is mainly needed by |moments.cxx|.
**/

//{
#ifndef _moments_
#define _moments_

#ifndef _this_
#define _this_ moments
#endif
//}

//## Include files.

//{
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>

#include <vector>
#include <algorithm>

#include "jcmacros.h"
#include "jcdebug.h"

#include "camera-v.h"
//}

//## Macro-definitions, and constants.

//{

typedef struct {

  // moments
  float m1x, m1y;                       // first moments (mean)
  float m2xx, m2xy, m2yy;               // second moments (around origin)
  float m2cxx, m2cxy, m2cyy;            // second moments (around mean)
  float m3xxx, m3xxy, m3xyy, m3yyy;     // third moments (around origin)
  float m3cxxx, m3cxxy, m3cxyy, m3cyyy; // third moments (around mean)

  // charges
  float charge;                         // total charge in the image
  float xmax, ymax;                     // position of the maximum
  float smax;                           // charge of the block of maximum
  float maxs[10];                       // charges of the first 10 max.
  int nmaxs[10];                        // number of pixels of 10 max.
  
  // parameters of the image
  float length, width, dist, xdist, azw, miss, alpha, conc[9], beta; 
  float phi, asymx, asymy;
  
} Moments_Info;


typedef struct {
  float *fi;
  int *isl, *islands; 
  float *vislands; 
  int numisl;
} Islands_Info;


typedef struct {
  float length1, length2;
  float width1,  width2;
} LenWid_Info;

//}

//## Prototypes of functions.

//{

//++
// prototypes
//--

Moments_Info * moments( int n, float *image, float **pix, 
                        float plateScale, int flag,
                        int **pixneig, int *npixneig);
Islands_Info * islands( int n, float *f, int **pixneig, int *npixneig,
                        int cleanning, int ipixcut );
LenWid_Info * lenwid( int n, float *image, float **pix, 
                      float plateScale, float max_distance);

void crosspt( float ax, float ay,
              float bx, float by,
              float cx, float cy,
              float dx, float dy,
              float * pcrossx, float * pcrossy);

//}

//{
#endif // ! _moments_
//}

//=------------------------------------------------------------
//## Log of this file.

//{

/*
 *$Log$
 *Revision 1.1.1.1  2000/06/07 21:03:49  gonzalez
 *camera-simple created from camera-complex
 *
 * Revision 1.7  2000/05/29  16:01:28  gonzalez
 * *** empty log message ***
 *
 * Revision 1.6  1999/11/17  13:32:04  gonzalez
 * Nov.1999
 *
 * Revision 1.5  1999/10/05  11:42:35  gonzalez
 * Sep. 1999
 *
 * Revision 1.4  1999/03/15  14:59:10  gonzalez
 * camera-1_1
 *
 * Revision 1.3  1999/03/02  09:56:15  gonzalez
 * *** empty log message ***
 *
 */

//}
//=EOF
