//=//////////////////////////////////////////////////////////////////////
//=
//= reflector               
//=
//= @file        reflector.h
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
//= Notes:                                    
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

//!@section Source code of |reflector.h|.

/*!@"
  
  This section shows the include file |reflector.h|

@"*/

//!@{

#ifndef _this_
#define _this_ reflector
#endif

//!@}

//=-----------------------------------------------------------
//!@subsection Include files.

//!@{

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <string.h>
#include <stdarg.h>
#include <cmath>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>

#include "jcmacros.h"
#include "jcdebug.h"

#include "reflector-v.h"

#include "readparam.h"
#include "atm.h"

#include "lagrange.h"

#include "COREventHeader.hxx"
#include "CORParticle.hxx"
#include "CORStatfile.hxx"
#include "MCEventHeader.hxx"
#include "MCCphoton.hxx"

// command line options available
#define COMMAND_LINE_OPTIONS    "f:h"

// This is C++, but RANLIB routines are written in pure ANSI C.
// In order to read easily these routines, we must include
// the following directive
extern "C" {
#include "ranlib.h"
}

//!@}

// @subsection Macro-definitions, and constants

//!@{

#define NORM(v) (sqrt( SQR(v[0]) + SQR(v[1]) + SQR(v[2]) ))
#define RAD(x)  ((x)*0.0174532925199433)
#define DEG(x)  ((x)*57.2957795130823)

// random numbers
#define RandomNumber  ranf()

// Speed of Light in vacuum, in m/s
const float Speed_of_Light_vacuum = 299792458.0;  // EXACT!!
const float Speed_of_Light_air = Speed_of_Light_vacuum / 1.000293;

// Speed of Light in vacuum, in cm/ns
const float Speed_of_Light_vacuum_cmns = Speed_of_Light_vacuum / 1.0e7;
const float Speed_of_Light_air_cmns = Speed_of_Light_air / 1.0e7;

// now we define the list CT_ITEM_LIST of possible items in
// the CT definition file.

#define CT_ITEM_LIST  /* LIST OF ITEMS IN THE CT DEFINITION FILE */  \
T(type),              /* type of definition file */                  \
T(focal_distance),    /* focal distance */                           \
T(focal_std),         /* std(focal distance) */                      \
T(point_spread),      /* point spread   */                           \
T(point_std),         /* std(point spread)   */                      \
T(adjustment_dev),    /* std of adjustment deviation   */            \
T(black_spot),        /* radius of the black spot in center of mirrors */ \
T(n_mirrors),         /* number of mirrors */                        \
T(r_mirror),          /* radius of one mirror */                     \
T(camera_width),      /* camera width */                             \
T(n_pixels),          /* number of pixels in the camera */           \
T(pixel_width),       /* pixel width */                              \
T(define_mirrors)     /* this entry is followed by the def. of pixels */
  
#define T(x)  x               // define T() as the name as it is

  enum CT_ITEM_TYPE {
    CT_ITEM_LIST
  };

#undef T

#define T(x)  #x              // define T() as the string of x

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
#define CT_XC      9
#define CT_YC     10
#define CT_ZC     11

#define CT_NDATA  12
 
// pre-defined filenames / values

#define REFLECTIVITY_FILE             "reflectivity.dat"
#define FOCALS_FILE                   "focals.dat"
#define AXISDEVIATION_FILE            "axisdev.dat"

// values for read-ot from STDIN

#define BUFFER_LENGTH           5733
#define SIZE_OF_BLOCK           22932
static char EVTH[] = "EVTH";

// maximum deviation from the original direction with
// "random_pointing" option
//
// now is 6 degrees = 0.104719755119660 radians

#define RANDOM_POINTING_MAX_SEPARATION     0.104719755119660

//!@}

//=-----------------------------------------------------------
//!@subsection Prototypes of functions.

//!@{

//++
// prototypes
//--

void present(void);
void usage(void);
void clean(void);
void log(const char *funct, const char *fmt, ...);
void logerr(const char *funct, const char *fmt, ...);
void error(const char *funct, const char *fmt, ...);
void makeOmega(float theta, float phi);
void makeOmegaI(float theta, float phi);
void applyMxV(float M[3][3], float *V, float *Vp);
void read_ct_file(void);
void read_reflectivity(void);
void read_axisdev(void);
void read_focals(void);
void rnormal(double *r, int n);
float dist_r_P(float a, float b, float c, 
               float u, float v, float w,
               float x, float y, float z);
void write_mark(ofstream &os, char mark=0xff, int length=1);
int isA( char * s1, const char * flag );
float Curv2Lin(float s);
float Lin2Curv(float x);
int get_stdin_files(int ncerf,
                    float El=0., float Eu=1000000., 
                    int flag=FALSE);
void generate_parallel_beam_if_needed(COREventHeader & evth);
static float get_new_ct_pointing(float theta, float phi,
                                 float minang, float maxang,
                                 float *newtheta, float *newphi,
                                 int isotropic);

//!@}

//=------------------------------------------------------------
//!@subsection Log of this file.

//!@{

/*
 * $Log$
 * Revision 1.12  2000/03/22  15:56:42  gonzalez
 * *** empty log message ***
 *
 * Revision 1.11  1999/10/05  11:11:12  gonzalez
 * Sep.1999
 *
 * Revision 1.10  1999/03/24  16:33:03  gonzalez
 * REFLECTOR 1.1: Release
 *
 * Revision 1.9  1999/01/21  16:03:53  gonzalez
 * Only small modifications
 *
 * Revision 1.8  1999/01/19  18:07:17  gonzalez
 * Bugs in STDIN-STDOUT version corrected.
 *
 * Revision 1.7  1999/01/14  17:35:44  gonzalez
 * Both reading from STDIN (data_from_stdin) and
 * writing to STDOUT (data_to_STDOUT) working.
 *
 */

//!@}
//=EOF









