//=//////////////////////////////////////////////////////////////////////
//=
//= readparam                
//=
//= @file        readparam.h
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

//!@section Source code of |readparam.h|.

/*!@"

  In this section you can find the source code for the file
  |readparam.h|.  This file is mainly needed by |readparam.cxx|. Here
  is located the definition of the commands you can use in the
  parameters file. In this file, the first line must be |reflector|
  |version|, where |version| is the appropiate version program which
  can read the commands written in that parameters file. You cannot
  mix parameters files and executables with different versions. The
  current version is |reflector 0.3|. Each line starting with a \#
  will be treated as a comment.

  The commands now available for the parameters file will be explained
  in the documentation.

  A parameters file looks like this:

  |reflector 0.2|

  |verbose_level 2|

  |fixed_target  0. 0.|

  |max_events   1000|

  |data_paths 1|

  |/hd80/MC/CT1/data/low-za/p/run9|

  |output_file   reflector.rfl|

  |ct_file       ct1.def|

  |atm_model     ATM_CORSIKA|

  |end_file|

  @"*/

//!@{
#ifndef _readparam_
#define _readparam_

#ifndef _this_
#define _this_ readparam
#endif
//!@}

//!@subsection Include files.

//!@{

#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <unistd.h>

#include "jcmacros.h"
#include "jcdebug.h"

#include "reflector-v.h"

#include "atm.h"

//!@}

//!@subsection Macro-definitions, and constants.

//!@{

// now we define the list ITEM_LIST of possible items in
// the parameters file. note that they are functions of
// T(x). we will change T(x) to be the real item or the
// string of this item when needed

#define ITEM_LIST   /* LIST OF ITEMS IN THE PARAMETERS FILE */     \
T(data_paths),      /* begin of the list of source data paths */   \
T(output_file),     /* output file */                              \
T(ct_file),         /* file with the characteristics of the CT */  \
T(atm_model),       /* changes the atmospheric model to be used */ \
T(verbose_level),   /* defines verbose level of the output */      \
T(fixed_target),    /* defines the position towards the CT is pointing */ \
T(max_events),      /* maximum number of event to read */ \
T(range_events),    /* analyze from event A to B */ \
T(energy_cuts),     /* lowest/highest energy allowed */ \
T(parallel_beam),   /* defines a parallel beam of light */ \
T(pm_parallel_beam),/* defines a parallel beam of light, out of a pixmap */ \
T(seeds),           /* seeds for random number generation */ \
T(block),           /* size of the block of files, when 'blocking' */ \
T(random_pointing), /* random CT pointing from each shower (hadrons) */ \
T(repeat_random),   /* number of times a random pointing is to be done */ \
T(data_from_stdin), /* read data from STDIN */ \
T(data_to_stdout),  /* read data from STDIN */ \
T(end_file)         /* end of the parameters file */
  
#define T(x)  x               // define T() as the name as it is

enum ITEM_TYPE {
  ITEM_LIST
};

#undef T

#define T(x)  #x              // define T() as the string of x

const char *const ITEM_NAMES[] = {
  ITEM_LIST
};

#undef T

#define LINE_MAX_LENGTH  400
#define ITEM_MAX_LENGTH  40
#define PATH_MAX_LENGTH  120

// Verbose Levels
enum VerboseLevel {
  VERBOSE_QUIET,
  VERBOSE_MINIMAL,
  VERBOSE_NORMAL,
  VERBOSE_MAXIMAL
};

#define VERBOSE_DEFAULT  VERBOSE_QUIET

// directory for intermediate storage of data from STDIN

#define TMP_STDIN_DIR    "/hd90/gonzalez/tmp"

//!@}

//!@subsection Prototypes of functions.

//!@{

//++
// prototypes
//--

void readparam(char * filename);
int get_num_of_paths(void);
char *get_path_name(int i);
char *get_output_filename(void);
char *get_ct_filename(void);
int get_verbose(void);
int get_fixed_target(float *theta, float *phi);
int get_max_events(void);
void get_range_events(int *a, int *b);
void get_energy_cuts(float *low, float *high);
void get_parallel_beam(float *pb_theta, float *pb_phi, 
                       float *pb_x, float *pb_y, 
                       float *pb_lengthx, float *pb_lengthy, 
                       float *pb_nx, float *pb_ny, float *pb_height);
int is_parallel_beam(void);
char * get_parallel_beam_pm(float *pb_scale, float *pb_height);
int is_parallel_beam_pm(void);
long int get_seeds(int n);
int get_block(void);
int get_data_from_stdin(void);
int get_data_to_stdout(void);
int get_random_pointing(float *mindist, float *maxdist, int *isotropic);
int get_repeat_random(void);

//!@}

//!@{

#endif // ! _readparam_

//!@}

//=------------------------------------------------------------
//!@subsection Log of this file.

//!@{

/*
 * $Log$
 * Revision 1.15  2000/03/22  15:56:42  gonzalez
 * *** empty log message ***
 *
 * Revision 1.14  2000/01/27  10:47:54  gonzalez
 * JAN2000-STABLE
 *
 * Revision 1.13  1999/10/05  11:06:37  gonzalez
 * Sep. 1999
 *
 * Revision 1.12  1999/03/24  16:33:02  gonzalez
 * REFLECTOR 1.1: Release
 *
 * Revision 1.11  1999/01/21  16:03:41  gonzalez
 * Only small modifications
 *
 * Revision 1.10  1999/01/19  18:07:18  gonzalez
 * Bugs in STDIN-STDOUT version corrected.
 *
 * Revision 1.9  1999/01/14  17:35:40  gonzalez
 * Both reading from STDIN (data_from_stdin) and
 * writing to STDOUT (data_to_STDOUT) working.
 *
 */

//!@}
//=EOF
