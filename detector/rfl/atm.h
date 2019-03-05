//=//////////////////////////////////////////////////////////////////////
//=
//= atm    
//=
//= @file        atm.cxx
//= @desc        Header file
//= @author      J C Gonzalez
//= @email       gonzalez@mppmu.mpg.de
//= @date        Thu May  7 16:24:22 1998
//=
//=----------------------------------------------------------------------
//= 
//= Created: Thu May  7 16:24:22 1998         
//= Author:  Jose Carlos Gonzalez             
//= Purpose: Simulation of atmospheric absorption 
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

//!@section Source code of |atm.h|.

/*!@"

  In this section you can find the source code for the file |atm.h|.

  @"*/

//!@{
#ifndef _atm_
#define _atm_

#ifdef _this_
#define _define_extern_
#endif

#ifndef _this_
#define _this_ atm
#endif
//!@}

//!@subsection Include files.

//!@{

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <string.h>
#include <cmath>

#include "jcmacros.h"
#include "jcdebug.h"

#include "reflector-v.h"

//!@}

//!@subsection Macro-definitions, and constants.

//!@{

// now we define the list ITEM_LIST of possible items in
// the parameters file. note that they are functions of
// T(x). we will change T(x) to be the real item or the
// string of this item when needed

#define ATM_MODELS_LIST   /* LIST OF POSIBLE MODELS OF ATMOSPHERE */       \
T(ATM_NOATMOSPHERE),      /* no atmosphere at all: transmittance = 100% */ \
T(ATM_90PERCENT),         /* atmosphere with transmittance = 90% */        \
T(ATM_ISOTHERMAL),        /* isothermal approximation */                   \
T(ATM_CORSIKA)            /* atmosphere as defined in CORSIKA */         
  
#define T(x)  x               // define T() as the name as it is
enum ATM_MODELS_TYPE { ATM_MODELS_LIST };
#undef T

extern const char * ATM_MODELS_NAMES[];

#define ATM_NAME_MAX_LENGTH 40

float atm(float wavelength, float height, float theta);
#ifdef _define_extern_
extern void set_atm_mod(ATM_MODELS_TYPE i);
extern ATM_MODELS_TYPE conv_atm_mod(char *atmod);
#else
void set_atm_mod(ATM_MODELS_TYPE i);
ATM_MODELS_TYPE conv_atm_mod(char *atmod);
#endif
char *get_atm_mod();
char *get_atm_mod_n(ATM_MODELS_TYPE i);


// routines written in Fortran 

#define attenu attenu_
extern "C" {
  void attenu(float *, float *, float *, float *);
}

//!@}

//!@{

#endif // ! _atm_

//!@}


//=------------------------------------------------------------
//!@subsection Log of this file.

//!@{

/*
 * $Log$
 * Revision 1.3  1999/03/24  16:33:02  gonzalez
 * REFLECTOR 1.1: Release
 *
 * Revision 1.2  1999/01/21  16:03:32  gonzalez
 * Only small modifications
 *
 */

//!@}
//=EOF
