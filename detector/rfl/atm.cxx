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

//!@section Source code of |atm.cxx|.

/*!@"

  This section describes briefly the source code for the file
  |atm.cxx|.

  @"*/

//!@subsection Include files.

/*!@"
 
  All the defines are located in the file {\tt atm.h}.

  @"*/

//!@{

#include "atm.h"

//!@}

//!@subsection Definition of global variables.

//!@{

static ATM_MODELS_TYPE Atm_mod_N = ATM_NOATMOSPHERE;
static char Atm_mod[ATM_NAME_MAX_LENGTH];

#define T(x)  #x              // define T() as the string of x
const char * ATM_MODELS_NAMES[] = { ATM_MODELS_LIST };
#undef T


//!@}

//!@subsection The function |atm()|.

//!---------------------------------------------------------------------
// @name  atm
//                                                
// @desc  simulation of atmospheric absorption
//
// @var   wavelength    Wavelength of the photon (nm)
// @var   height        Height of generation (cm)
// @var   theta         Zenith angle of trajectory(rad)
//
// @return    Transmission coefficient
//
// @date Mon Sep 14 13:27:56 MET DST 1997
//----------------------------------------------------------------------
// @function

//!@{ 
float 
atm(float wavelength, float height, float theta)
{

  float transmittance;        //@< final transmittance of the atm. (returned)

  transmittance = 1.0;

  switch ( Atm_mod_N ) {
    
  case ATM_NOATMOSPHERE:      //@< no atmosphere at all: transmittance = 100%

    break;

  case ATM_90PERCENT:         //@< atmosphere with transmittance = 90%

    transmittance = 0.9;
    break;

  case ATM_ISOTHERMAL:        //@< isothermal approximation

    // here the transmittance function

    break;

  case ATM_CORSIKA:           //@< atmosphere as defined in CORSIKA

    attenu( &wavelength, &height, &theta, &transmittance);
    
    break;

  }

  return ( transmittance );

}
//!@}

//!---------------------------------------------------------------------
// @name set_atm_mod        
//                                                
// @desc set model of atmospheric absorption
//
// @var  i     Model of atmospheric absorption to be used
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void
set_atm_mod(ATM_MODELS_TYPE i)
{
  Atm_mod_N = i;
  strcpy(Atm_mod, ATM_MODELS_NAMES[ Atm_mod_N ]);
}
//!@}

//!---------------------------------------------------------------------
// @name conv_atm_mod
//                                                
// @desc convert string with name of atm.model to number
//
// @var  *s    Name of model of atmospheric absorption
//
// @return     Number for that model
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
ATM_MODELS_TYPE 
conv_atm_mod(char *s)
{
  int i;
  char s2[40];

  // look at each model
  for (i=0; i<=ATM_CORSIKA; i++) {
    strcpy(s2, ATM_MODELS_NAMES[i]);
    if (strcmp(s, s2) == 0)
      break;
  }

  // return the name of the model
  return ( (ATM_MODELS_TYPE) ((i<=ATM_CORSIKA) ? i : -1) );

  //   return ( (ATM_MODELS_TYPE) 0 );
}
//!@}

//!---------------------------------------------------------------------
// @name get_atm_mod
//                                                
// @desc get name of the atmospheric model to be used
//
// @return    Name of the current atmospheric model to be used
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
char *
get_atm_mod(void)
{
  return (Atm_mod);
}
//!@}

//!---------------------------------------------------------------------
// @name get_atm_mod_n
//                                                
// @desc get name of the atmospheric model number i
//
// @var   i   Number of atmospheric model
//
// @return    Name of corresponding atmospheric model
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
char *
get_atm_mod_n(ATM_MODELS_TYPE i)
{
  return( (char*)(ATM_MODELS_NAMES[ Atm_mod_N ]) );
}
//!@}


//=------------------------------------------------------------
//!@subsection Log of this file.

//!@{
//
// $Log$
// Revision 1.4  2000/01/27  10:47:54  gonzalez
// JAN2000-STABLE
//
// Revision 1.3  1999/03/24  16:32:59  gonzalez
// REFLECTOR 1.1: Release
//
//
//
//!@}

//=EOF
