//=//////////////////////////////////////////////////////////////////////
//=
//= camera                
//=
//= @file        camera.h
//= @desc        Header file
//= @author      J C Gonzalez
//= @date        Time-stamp: "Mon Jun  5 18:31:34 CEST 2000"
//= @email       gonzalez@mppmu.mpg.de
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

//# Source code of |camera.h|

/*"
  This section shows the include file |camera.h|
**/

//=-----------------------------------------------------------
//## Compilation flags

/*"

  The following set of flags are used in time of compilation. They do
  not affect directly the behaviour of the program at run-time
  (though, of course, if you disconnected the option for
  implementation of the Trigger logic, you will not be able to use any
  trigger at all. The 'default' values mean default in the sense of
  what you got from the server when you obtained this program.

**/

//{

// flag for debugging (default: OFF )
#define __DEBUG__
#undef  __DEBUG__

// flag for NNT in CT1 camera (default: ON )
#undef  __CT1_NO_NEIGHBOURS__
#define __CT1_NO_NEIGHBOURS__

// flag for calculation of NSB (default: ON )
#undef  __NSB__
#define __NSB__

// flag for calculation of QE for pixels (default: ON )
#undef  __QE__
#define __QE__

// flag for taking into account different efficiencies (default: ON )
#undef  __EFFICIENCIES__
#define __EFFICIENCIES__

// flag for taking num.phes. as Poisson mean (default: OFF ) MUST BE OFF!!!
#define __NUM_CPHOTONS_POISSON__
#undef  __NUM_CPHOTONS_POISSON__

// flag for implementation of TRIGGER (default: ON )
#undef  __TRIGGER__
#define __TRIGGER__

// flag for implementation of Tail-Cut (default: ON )
#undef  __TAILCUT__
#define __TAILCUT__

// flag for calculation of islands stat. (default: ON )
#undef  __ISLANDS__
#define __ISLANDS__

// flag for calculation of image parameters (default: ON )
#undef  __MOMENTS__
#define __MOMENTS__

// flag for generation of global timing data files (default: OFF )
#undef  __PULSE_TIMING__
#define __PULSE_TIMING__

// flag for generation of timing data files (default: OFF )
#undef  __PULSE_TIMING_FILES__
#define __PULSE_TIMING_FILES__

// flag for generation of timing data files (default: OFF )
#undef  __PULSE_TIMING_PIXELS_FILES__
#define __PULSE_TIMING_PIXELS_FILES__

// flag for generation of HBOOK files (default: ON )
#undef  __HBOOK_FILES__
#define __HBOOK_FILES__

// flag for compilation on Linux boxes (compilation dependent)
#define __LINUX_INPUT__
#undef  __LINUX_INPUT__

//}


//=-----------------------------------------------------------
//## Include files

//{

#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <libgen.h>

#include "camera-v.h"

#include "jcmacros.h"
#include "jcdebug.h"

// #include "jchbook.h"                  //@< HBOOK routines through C++

#include "templmac.h"

#include "creadparam.h"
#include "atm.h"
#include "moments.h"

#include "lagrange.h"

#include "Histogram.h"

#include "MCEventHeader.hxx"
#include "MCCphoton.hxx"

/*"
  This is C++, but RANLIB routines are written in pure ANSI C.

  The files |cts*.h| were also written for C (not C++).

  In order to read easily these routines, we must include
  the following directive
**/

extern "C" { 
  
#include "ranlib.h"

#include "ctsbase.h"
#include "ctshbook.h"
  
  // #include "ctshplot.h" 
  // #include "ctsmath.h" 
  // #include "ctsminuit.h" 
  // #include "ctscuts.h" 
  // #include "nr.h" 
  
}

//}


//=------------------------------------------------------------
//## Macro-definitions, and constants.

//{

// version of the reflector program that can read

#define REFL_PROGRAM reflector
#define REFL_VERSION 0.3

const char REFL_SIGNATURE[] = GLUE_postp( REFL_PROGRAM, REFL_VERSION );

// command line options available

#define COMMAND_LINE_OPTIONS    "f:h"

#define SIN60   0.866025403784439
#define COS60   0.500000000000000
#define COS30   SIN60
#define SIN30   COS60

#define PIX_ARRAY_SIDE       60
#define PIX_ARRAY_HALF_SIDE  30

#define RandomNumber  ranf()

// number of the 1st. pixel of a sector s in a ring r (central pixel: ring=0)
#define FIRST_PIXEL(r,s)   ( ((r)>0) ? (3*(r)*((r)-1) + (r)*(s) + 1) : 0 )

// number of the pixels include in a camera of r pixels
#define NUMBER_PIXELS(r)   ( ((r)>0) ? FIRST_PIXEL((r)+1,0) : 1 )

// now we define the list CT_ITEM_LIST of possible items in the CT def. file
#define CT_ITEM_LIST  /* LIST OF ITEMS IN THE CT DEFINITION FILE */  \
T(type),              /* type of definition file */                  \
T(focal_distance),    /* std(focal distance) */                      \
T(focal_std),         /* focal distance */                           \
T(point_spread),      /* std(point spread)   */                      \
T(point_std),         /* point spread   */                           \
T(adjustment_dev),    /* std of adjustment deviation   */            \
T(black_spot),        /* radius of the black spot in center of mirror */ \
T(n_mirrors),         /* number of mirrors */                        \
T(r_mirror),          /* radius of one mirror */                     \
T(camera_width),      /* camera width */                             \
T(n_pixels),          /* number of pixels in the camera (small/gap/big) */ \
T(n_rings),           /* number of pixels in the camera (small/big) */ \
T(pixel_width),       /* pixel width */                              \
T(define_mirrors)     /* this entry is followed by the def. of pixels */
  
#define T(x)  x               //@< define T() as the name as it is

enum CT_ITEM_TYPE {
  CT_ITEM_LIST
};

#undef T

#define T(x)  #x              //@< define T() as the string of x

const char *const CT_ITEM_NAMES[] = {
  CT_ITEM_LIST
};

#undef T


// TYPE=0  (CT1)
//     i   s   rho   theta   x   y   z   thetan  phin  xn   yn   zn
//
//      i : number of the mirror
//      s : arc length [cm]
//    rho : polar rho of the position of the center of the mirror [cm]
//  theta : polar angle of the position of the center of the mirror [cm]
//      x : x coordinate of the center of the mirror [cm]
//      y : y coordinate of the center of the mirror [cm]
//      z : z coordinate of the center of the mirror [cm]
// thetan : polar theta angle of the direction where the mirror points to
//   phin : polar phi angle of the direction where the mirror points to
//     xn : xn coordinate of the normal vector in the center (normalized)
//     yn : yn coordinate of the normal vector in the center (normalized)
//     zn : zn coordinate of the normal vector in the center (normalized)
//
// TYPE=1  (MAGIC)
//     i  f   sx   sy   x   y   z   thetan  phin 
//
//      i : number of the mirror
//      f : focal distance of that mirror
//     sx : curvilinear coordinate of mirror's center in X[cm]
//     sy : curvilinear coordinate of mirror's center in X[cm]
//      x : x coordinate of the center of the mirror [cm]
//      y : y coordinate of the center of the mirror [cm]
//      z : z coordinate of the center of the mirror [cm]
// thetan : polar theta angle of the direction where the mirror points to
//   phin : polar phi angle of the direction where the mirror points to
//     xn : xn coordinate of the normal vector in the center (normalized)
//     yn : yn coordinate of the normal vector in the center (normalized)
//     zn : zn coordinate of the normal vector in the center (normalized)

#define CT_I       0

#define CT_S       1
#define CT_RHO     2
#define CT_THETA   3

#define CT_FOCAL   1
#define CT_SX      2
#define CT_SY      3

#define CT_X       4
#define CT_Y       5
#define CT_Z       6
#define CT_THETAN  7
#define CT_PHIN    8
#define CT_XN      9
#define CT_YN     10
#define CT_ZN     11

#define CT_NDATA  12
 
//}

//=------------------------------------------------------------
//## Pre-defined file names.

//{

#define QE_FILE               "qe.dat"
#define PIXELS_FILE           "pixels-real.dat"
#define SINGLE_PHE_SPEC_FILE  "sphes.dat"

//}

//=------------------------------------------------------------
//## Pre-defined (hardcoded) data.

//{

float SinglePheSpectrum[] = {2.14, 2.06, 2.05, 2.05, 2.06, 2.07, 2.08,
                             2.15, 2.27, 2.40, 2.48, 2.55, 2.50, 2.35,
                             2.20, 2.10, 1.90, 1.65, 1.40, 1.25, 1.00,
                             0.80, 0.65, 0.50, 0.35, 0.27, 0.20, 0.18,
                             0.16, 0.14, 0.12, 0.10, 0.08, 0.06, 0.04,
                             0.02, 0.01, 0.005,0.003, 0.001};

const int nSinglePheSpectrum = sizeof( SinglePheSpectrum ) / sizeof( float );

//}


//=------------------------------------------------------------
//## Prototypes of functions.

//{

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
{ pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE] = n; }

#define Map_Multiple_Pixel(i, j, n) \
{ int &p = pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE]; \
  p = (p<0)? 0 : p; \
  int p2 = (p/1000)%1000, p3 = p/1000000; \
  if (p3==0) { p += 1000000 * n;} \
  else if (p2==0) { p += 1000 * n;} \
  else { p += n; } \
}

//++
// prototypes
//--

#define ONoff(x)  ((x==TRUE) ? "[ ON ]" : "[ off]")

void         present(void);
void         usage(void);
void         clean(void);
void         log(const char *funct, char *fmt, ...);
void         error(const char *funct, char *fmt, ...);
int          isA( char * s1, const char * flag );
void         read_ct_file(void);
static float dist_r_P(float a, float b, float c, 
                      float l, float m, float n,
                      float x, float y, float z);

void         read_pixels(void);
void         create_pixels(void);
static bool  pixels_are_neig(int pix1, int pix2);
int          pixel_bit_mask(int i, float q0, Histogram<float> **d, int b);
static void  hex2coord (int ki, int kj, int kk,
                        float &x, float &y, float unit);
static void  coord2hex (float cx, float cy,
                        int &ki, int &kj, int &kk, float unit);
static void  ijk2xy (float di, float dj, float dk,
                     float &x, float &y, float unit);
static void  xy2ijk (float cx, float cy,
                     float &di, float &dj, float &dk, float unit);

int          pixel_id (float x, float y);

void         create_sps (float* & x, float* & y, float mymean);

static void  add_phe_signal (float t,
                             Histogram<float> *h, Histogram<float> *htot,
                             float *sX, float *sY, float sigma,
                             float amplitude_factor=1.0);
static float random_von_neumann(float *x, float *y, int n,
                                float maxy, float a, float b);
static void  read_bin_data( char *pdata, int bytes,
                            int source, ifstream &file );
//}

//=------------------------------------------------------------
//## Log of this file

//{

/*
 *$Log$
 * Revision 1.23  2000/06/06  15:25:22  gonzalez
 * *** empty log message ***
 *
 * Revision 1.22  2000/05/29  16:01:26  gonzalez
 * *** empty log message ***
 *
 * Revision 1.21  2000/02/20  11:38:38  gonzalez
 * *** empty log message ***
 *
 * Revision 1.20  2000/02/15  10:29:52  gonzalez
 * *** empty log message ***
 *
 * Revision 1.19  2000/02/12  14:25:55  gonzalez
 * *** empty log message ***
 *
 * Revision 1.18  2000/02/11  19:36:08  gonzalez
 * *** empty log message ***
 *
 * Revision 1.17  2000/02/11  06:47:49  gonzalez
 * Real camera for MAGIC: test 1
 *
 * Revision 1.16  2000/02/01  16:08:10  gonzalez
 * Implementation of new pixelization routine.
 * Tests are still on the way.
 *
 * Revision 1.15  2000/01/30  08:13:20  gonzalez
 * *** empty log message ***
 *
 * Revision 1.14  2000/01/28  06:40:21  gonzalez
 * Time in trigger + trigger patterns working ...
 *
 * Revision 1.13  2000/01/27  10:48:51  gonzalez
 * Trigger patterns + timing seem to work
 *
 * Revision 1.12  1999/12/03  17:18:09  gonzalez
 * *** empty log message ***
 *
 * Revision 1.11  1999/11/25  07:48:15  gonzalez
 * *** empty log message ***
 *
 * Revision 1.10  1999/11/22  21:02:11  gonzalez
 * *** empty log message ***
 *
 * Revision 1.9  1999/10/05  11:39:52  gonzalez
 * Sep. 1999
 *
 * Revision 1.8  1999/03/15  14:59:06  gonzalez
 * camera-1_1
 *
 * Revision 1.7  1999/03/02  09:56:11  gonzalez
 * *** empty log message ***
 *
 * Revision 1.6  1999/01/14  17:32:40  gonzalez
 * Added to camera the STDIN input option (data_from_input)
 *
 */

//}
//=EOF
