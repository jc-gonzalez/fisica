//=//////////////////////////////////////////////////////////////////////
//=
//= creadparam                
//=
//= @file        creadparam.h
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

//# Source code of |creadparam.h|.

/*"

  In this section you can find the source code for the file
  |creadparam.h|.  This file is mainly needed by
  |creadparam.cxx|. Here is located the definition of the commands you
  can use in the parameters file. In this file, the first line must be
  |camera 'version'|, where |'version'| is the appropiate version of
  the output format (NOT the version of the camera program) which can
  read the commands written in that parameters file. You cannot mix
  parameters files and executables with different versions. The
  current version is |camera 0.2|.

  The commands now available for the parameters file are:

  @itemize
  
  @- |input_file| filename :    
     Sets the name of the input file (|.rfl|).
  @- |output_file| filename :    
     Sets the name of the output file (|.phe|).
  @- |data_file| filename :    
     Sets the name of the output data file (|.dat|).
  @- |hbook_file| filename :    
     Sets the name of the output HBOOK data file (|.hbook|).
  @- |ct_file| filename :    
     Sets the name of the CT definition file (usually |.def|).
  @- |ana_pixels| numbers :    
     Number of pixels used in image parameters calculation.
  @- |nsb_on| :    
     Activates the NSB simulation. This is the default.
  @- |nsb_off| :    
     De-activates the NSB simulation.
  @- |nsb_mean| number :    
     Sets the mean value for the NSB.
     Default values hard coded.
     This implies always |nsb_on|.
  @- |threshold| number :    
     Sets the Threshold value q0. Default value: 10.
  @- |tail_cut| number : 
     Sets the Tail-Cut value.
     Default value: 7.
  @- |islands_on| :    
     Activates the islands calculation.
  @- |islands_off| :    
     De-activates the islands calculation.
  @- |islands_cut| number :    
     Sets the Islands-Cut value i0.
     Default value: 10.
  @- |seeds| seed1 seed2:           
     Sets seeds for random number generation 
  @- |data_from_stdin| : 
     Tells |camera| to read data directly from STDIN 
  @- |skip| N n1 n2 n3 ... :            
     Skip pathological showers 
  @- |write_all_data| :  
     Write to file .dat ALL image data 
  @- |write_all_images| :
     Write to file .phe ALL images (even those without trigger)
  @- |read_phe| :        
     Read an already camera processed file 
  @- |read_phe_all| :    
     Id., but the file was processed with |write_all_images| 
  @- |select_energy| E1 E2 :   
     Energy range to read: only for .phe files 
  @- |trigger_radius| deg :  
     Select trigger radius for the camera 
  @- |time_histo| bins Xmin Xmax :      
     Sets the characteristics of time histograms 
  @- |time_phisto| bins Xmin Xmax :     
     Sets the characteristics of time pixel histograms 
  @- |time_dischisto| bins Xmin Xmax :  
     Sets the characteristics of discrim. pixel histograms 
  @- |trigger_bins| nbins nbefore nafter :    
     Sets number of time bins for the trigger to hold 
  @- |correction| factor :      
     Factor for correction in the pixel values 
  @- |trigger_pattern| mask : 
     Composed trigger pattern mask to be used in the run 
  @- |sps_mean| value :        
     Sets single phe spectrum mean required 
  @- |sps_fwhm| value :        
     Sets FWHM of the signals for each phe 
  @- |end_file|
     Last command in the parameters file.

  @enditemize

**/

//{

#ifndef _creadparam_
#define _creadparam_

#ifndef _this_
#define _this_ creadparam
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

#include "jcmacros.h"
#include "jcdebug.h"

#include "camera-v.h"

//}

//## Macro-definitions, and constants.

//### Commands in parameters file.

//{

// now we define the list ITEM_LIST of possible items in
// the parameters file. note that they are functions of
// T(x). we will change T(x) to be the real item or the
// string of this item when needed

#define ITEM_LIST   /* LIST OF ITEMS IN THE PARAMETERS FILE */     \
T(input_file),      /* input file */                              \
T(output_file),     /* output file */                              \
T(data_file),       /* data file */                              \
T(hbook_file),      /* HBOOK file */                              \
T(ct_file),         /* file with the characteristics of the CT */  \
T(ana_pixels),      /* size of the camera for parameters calculation */  \
T(nsb_only),        /* simulate cherenkov-empty events, only NSB is added */ \
T(nsb_on),          /* activates NSB simulation */ \
T(nsb_off),         /* de-activates NSB simulation */ \
T(nsb_mean),        /* mean value of NSB contribution per pixel */ \
T(threshold),       /* value of q0 for trigger */ \
T(tail_cut),        /* value of tail cut (t0) */ \
T(islands_on),      /* DO count islands */ \
T(islands_off),     /* do NOT count islands */ \
T(islands_cut),     /* value of islands cut (i0) */ \
T(seeds),           /* seeds for random number generation */ \
T(data_from_stdin), /* to read data from STDIN */ \
T(skip),            /* skip pathological showers */ \
T(read_phe_all),    /* id., but was processed with write_all_images */ \
T(read_phe),        /* read an already camera processed file */ \
T(write_all_images),/* write to file .phe ALL images (even w.o. trigger)*/ \
T(write_all_data),  /* write to file .dat ALL image data */ \
T(select_energy),   /* energy range to read: only for .phe files */ \
T(trigger_radius),  /* trigger radius for the camera */ \
T(time_histo),      /* sets the characteristics of time histos. */ \
T(time_phisto),     /* sets the characteristics of time pixel histos. */ \
T(time_dischisto),  /* sets the characteristics of discrim. pixel histos. */ \
T(trigger_bins),    /* sets number of time bins for the trigger to hold */ \
T(correction),      /* factor for correction in the pixel values */ \
T(trigger_pattern), /* trigger pattern to be used in the run */ \
T(sps_mean),        /* single phe spectrum mean required */ \
T(sps_fwhm),        /* FWHM of the signals for each phe */ \
T(end_file)         /* end of the parameters file */
  
#define T(x)  x             // define T() as the name as it is

enum ITEM_TYPE {
  ITEM_LIST
};

#undef T

#define T(x)  #x              // define T() as the string of x

const char *const ITEM_NAMES[] = {
  ITEM_LIST
};

#undef T

//}

//### Trigger patterns defined.

//{

// definitions of patterns (enum types)
enum TriggerPattern_type {
  TP_default    = 8,
    
  TP_3pix_NotNN = 1,
  TP_3pix_NN    = 2,
  TP_4pix_NotNN = 4,
  TP_4pix_NN    = 8,
  TP_5pix_NotNN = 16,
  TP_5pix_NN    = 32,
  
  TP_3pix_all   = 3,
  TP_4pix_all   = 12,
  TP_5pix_all   = 48,

  TP_3or4pix    = 15,
  TP_4or5pix    = 60
};

// names used in the parameters file
const char *const TP_NAMES[] = {
  "default",
   
  "3pix_NotNN",
  "3pix_NN",
  "4pix_NotNN",
  "4pix_NN",
  "5pix_NotNN",
  "5pix_NN",
   
  "3pix_all",
  "4pix_all",
  "5pix_all",
   
  "3or4pix",
  "4or5pix"
};

const int nTP_NAMES = sizeof( TP_NAMES ) / sizeof( char* );

//}

//{

// some constants for the length of character variables

#define LINE_MAX_LENGTH  400
#define ITEM_MAX_LENGTH  40
#define PATH_MAX_LENGTH  120

// mean values of NSB contribution per pixel

static const float Mean_NSB_MAGIC = 5.0; //@< for MAGIC
static const float Mean_NSB_CT1 = 5.0;   //@< for CT1

//}

//## Prototypes of functions.

//{

//++
// prototypes
//--

void                  readparam(char * filename);
char*                 get_input_filename(void);
char*                 get_output_filename(void);
char*                 get_data_filename(void);
char*                 get_hbook_filename(void);
char*                 get_ct_filename(void);
int                   get_nsb(float *n);
int                   get_nsb_only(int *n);
float                 get_threshold(void);
float                 get_tail_cut(void);
int                   get_islands_cut(int *n);
long int              get_seeds(int n);
int                   get_ana_pixels(void);
void                  get_skip_showers( int *s ); 
int                   get_nskip_showers( void ); 
int                   get_data_from_stdin(void);
int                   get_read_phe(void);
int                   get_read_phe_all(void);
int                   get_write_all_images(void);
int                   get_write_all_data(void);
int                   get_select_energy(float *le, float *ue);
int                   get_trigger_radius(float *radius);
int                   get_correction(float *corr);
int                   get_time_histo(float *mintime, float *maxtime);
int                   get_time_phisto(float *mintime, float *maxtime);
int                   get_time_dischisto(float *mintime, float *maxtime);
int                   get_trigger_bins(void);
void                  get_time_intgate(int *a, int *b);
TriggerPattern_type   get_trigger_pattern_id(void);
int                   get_trigger_npatterns(void);
int *                 get_trigger_patterns(void);
void                  binaryString(int value, char * s);
void                  displayBinaryStrings();
void                  get_sps_params(float &mean, float &fwhm, float &sigma);
//}

//{

#endif // ! _creadparam_

//}

//=------------------------------------------------------------
//## Log of this file.

//{

/*
 * $Log$
 * Revision 1.13  2000/02/15  10:29:52  gonzalez
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

//}
//=EOF
