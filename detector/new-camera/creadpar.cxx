//=//////////////////////////////////////////////////////////////////////
//=
//= creadparam            
//=
//= @file        creadparam.cxx
//= @desc        Reading of parameters file
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

//# Source code of |creadparam.cxx|.

/*"
  This section describes briefly the source code for the file
  |creadparam.cxx|. This file is very closely related to the file
  |readparams.cxx| from the |reflector| program. Actually, this later
  file was the ancestror of the file you are looking at.

  All the defines it uses are located in the file |creadparam.h|. In
  the first one we can see the definitions of the commands available
  for the parameters file. We describe these commands in a later
  section.  
**/

//## Includes and Global variables definition.

/*"
  All the defines are located in the files |creadparam.h| and
  |trigpatt.h|.
**/
 
//{

#include "creadparam.h"

// the trigger patterns are defined in file patterns.dat, this file
// being processed by patterns.c to give as output trigpatt.h
#include "trigpatt.h"

//}

//## Definition of global variables.

/*"
  Here we define the global variables where the values from the
  parameters file are stored.
**/

//{

static char    Input_filename[PATH_MAX_LENGTH];  //@< input filename
static char    Output_filename[PATH_MAX_LENGTH]; //@< output filename
static char    Data_filename[PATH_MAX_LENGTH];   //@< data filename
static char    HBOOK_filename[PATH_MAX_LENGTH];  //@< data filename
static char    CT_filename[PATH_MAX_LENGTH];     //@< name of the CT def.file
static int     anaPixels = -1;         //@< number of pixels for the analysis
static int     simulateNSB = TRUE;     //@< Will we simulate NSB?
static float   meanNSB = 0.11;         //@< NSB mean value (per pixel in 1ns)
static int     onlyNSB = FALSE;        //@< simulate only NSB events
static int     eventsNSB = 0;          //@< number of NSB events to simulate
static float   qThreshold;             //@< Threshold value
static float   qTailCut;               //@< Tail Cut value
static int     nIslandsCut;            //@< Islands Cut value
static int     countIslands = TRUE;    //@< Will we count the islands?
static long    Seeds[2]; 
static int *   Skip;
static int     nSkip=0;
static int     Data_From_STDIN = FALSE;
static int     Read_Phe = FALSE;
static int     Read_Phe_All = FALSE;
static int     Write_All_Images = FALSE;
static int     Write_All_Data = FALSE;
static int     Select_Energy = TRUE;
static float   Select_Energy_le = 0.0;           //@< GeV
static float   Select_Energy_ue = 100000.0;      //@< GeV
static float   Trigger_Radius;
static int     Set_Trigger_Radius = FALSE;
static float   fCorrection;
static int     Apply_Correction=FALSE;
static int     iTime_nbins=100;
static float   fTime_mintime=0.;
static float   fTime_maxtime=100.;
static int     iTime_pnbins=30;
static float   fTime_pmintime=0.;
static float   fTime_pmaxtime=100.;
static int     iTime_discnbins=1000;
static float   fTime_discmintime=0.;
static float   fTime_discmaxtime=100.;
static int     iTime_triggerbins=30;
static int     nBefIntegBin=10; 
static int     nAftIntegBin=40;
static int     trigger_npatterns = 0;
static int *   trigger_patterns;
static char    strigger_pattern[40];
static TriggerPattern_type trigger_pattern_id = TP_default;

const double   FWHM2Sigma = 0.424660900144010;  // = 1 / (2*sqrt(2*ln(2)))

static float   sps_Mean  = 1.0;
static float   sps_FWHM  = 2.0;
static float   sps_Sigma = sps_FWHM * FWHM2Sigma;

// vector to get indexes for each pattern
TriggerPattern_type triggerPattern_type[] = {
  TP_default    ,
                
  TP_3pix_NotNN ,
  TP_3pix_NN    ,
  TP_4pix_NotNN ,
  TP_4pix_NN    ,
  TP_5pix_NotNN ,
  TP_5pix_NN    ,
                
  TP_3pix_all   ,
  TP_4pix_all   ,
  TP_5pix_all   ,
                
  TP_3or4pix    ,
  TP_4or5pix    
};

//}


//## The function |readparam()|.

//!-----------------------------------------------------------
// @name  creadparam
//                                                
// @desc  read parameters from the stdin / parameters file
//
// @var   *filename  Name of the parameters file (NULL->STDIN)
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function

//{ 
void 
readparam(char * filename)
{
  char sign[] = GLUE_postp( PROGRAM, VERSION ); //@< initialize sign
  char line[LINE_MAX_LENGTH];    //@< line to get from the stdin
  char token[ITEM_MAX_LENGTH];   //@< a single token
  int i, j, k, l;                   //@< dummy counters
  ifstream ifile;

  // use cin or ifile (reading from STDIN or from parameters file?
  if ( filename != NULL )
    ifile.open( filename );

  // get signature
  if ( filename != NULL ) {
    ifile.getline(line, LINE_MAX_LENGTH);
  } else {
    cin.getline(line, LINE_MAX_LENGTH);
  }
  
  line[strlen(SIGNATURE)] = '\0';
  strcpy(line, sign);

  if (strcmp(sign, SIGNATURE) != 0) {
    cerr << "ERROR: Signature of parameters file is not correct\n";
    cerr << '"' << sign << '"' << '\n';
    cerr << "should be: " << SIGNATURE << '\n';
    exit(1);
  }

  trigger_pattern_id = TP_default;
  strcpy(strigger_pattern, "default");
  
  // loop till the "end" directive is reached
  int is_end = FALSE;

  while (! is_end) {          

    // get line from file or stdin
    if ( filename != NULL )
      ifile.getline(line, LINE_MAX_LENGTH);
    else
      cin.getline(line, LINE_MAX_LENGTH);

    // skip comments (start with '#')
    if (line[0] == '#')
      continue;

    // show user comments (start with '>')
    if (line[0] == '>') {
      cout << line << endl << flush;
      continue;
    }

    // look for each item at the beginning of the line
    for (i=0; i<=end_file; i++) 
      if (strstr(line, ITEM_NAMES[i]) == line)
        break;
        
    // if it is not a valid line, just ignore it
    if (i == end_file+1) {
      cerr << "Skipping unknown token in [" << line << "]\n";
      continue;
    }

    cout << line << endl << "keyword #" << i << " = "
         << ITEM_NAMES[i] << endl << flush;
    
    // case block for each directive
    switch ( i ) {

    case input_file:          //@< name of the input file
          
      // get the name of the input_file from the line
      sscanf(line, "%s %s", token, Input_filename);

      break;

    case output_file:         //@< name of the output file
          
      // get the name of the output_file from the line
      sscanf(line, "%s %s", token, Output_filename);

      break;

    case data_file:           //@< name of the data file
          
      // get the name of the data_file from the line
      sscanf(line, "%s %s", token, Data_filename);

      break;

    case hbook_file:          //@< name of the HBOOK file
          
      // get the name of the data_file from the line
      sscanf(line, "%s %s", token, HBOOK_filename);
      cout << '[' << HBOOK_filename << ']' << endl << flush;

      break;

    case ct_file:             //@< name of the telescope file
          
      // get the name of the ct_file from the line
      sscanf(line, "%s %s", token, CT_filename);

      break;

    case nsb_on:              //@< simulate NSB?
          
      // we will simulate NSB
      simulateNSB = TRUE;
          
      break;

    case nsb_off:             //@< simulate NSB?
          
      // we will NOT simulate NSB
      simulateNSB = FALSE;
          
      break;

    case nsb_mean:            //@< value of <NSB> per pixel
          
      // get value of <NSB> (in photons)
      sscanf(line, "%s %f", token, &meanNSB);
      simulateNSB = TRUE;

      break;

    case nsb_only:            //@< simulate only NSB events, and say how many
          
      // number of NSB-only events to simulate
      sscanf(line, "%s %d", token, &eventsNSB);
      cerr << "eventsNSB <- " << eventsNSB << endl << flush;
      simulateNSB = TRUE;
      onlyNSB = TRUE;

      break;

    case ana_pixels:          //@< number of pixels for analysis
          
      // number of pixels for analysis
      sscanf(line, "%s %d", token, &anaPixels);

      break;

    case threshold:           //@< value of threshold for trigger (q0)
          
      // get value of threshold (in ph.e.)
      sscanf(line, "%s %f", token, &qThreshold);

      break;

    case tail_cut:            //@< value of tail_cut (t0)
          
      // get value of tail_cut (in ph.e.)
      sscanf(line, "%s %f", token, &qTailCut);

      break;

    case islands_on:          //@< DO count islands
          
      // DO count islands
      countIslands = TRUE;
          
      break;

    case islands_off:         //@< do NOT count islands
          
      // do NOT count islands
      countIslands = FALSE;
          
      break;

    case islands_cut:         //@< value of islands_cut (i0)
          
      // get value of islands_cut (in ph.e.)
      sscanf(line, "%s %d", token, &nIslandsCut);
      countIslands = TRUE;

      break;

    case seeds:               //@< values of seeds for random numbers
          
      // get seeds
      sscanf(line, "%s %ld %ld", token, &Seeds[0], &Seeds[1]);

      break;

    case skip:                //@< skip pathological showers
          
      // get showers to skip
      if ( filename != NULL ) {
        ifile >> nSkip;
        Skip = new int[nSkip];
        for (j=0; j<nSkip; ++j) {
          ifile >> Skip[j];
          cout << Skip[j] << endl << flush;
        }
      } else {
        cin >> nSkip;
        Skip = new int[nSkip];
        for (j=0; j<nSkip; ++j) {
          cin >> Skip[j];
          cout << Skip[j] << endl << flush;
        }
      }
      
      break;

    case data_from_stdin:     //@< to read data from stdin
          
      // change boolean value
      Data_From_STDIN = TRUE;

      break;

    case read_phe:            //@< to read PHE files
          
      // change boolean value
      Read_Phe = TRUE;

      break;

    case read_phe_all:        //@< to read PHE files from write_all_images
          
      // change boolean value
      Read_Phe_All = TRUE;

      break;

    case write_all_images:    //@< to write ALL the images
          
      // change boolean value
      Write_All_Images = TRUE;

      break;

    case write_all_data:      //@< to write single pixel data
          
      // change boolean value
      Write_All_Data = TRUE;

      break;

    case select_energy:       //@< energy range
          
      // get energy range
      sscanf(line, "%s %f %f", token, &Select_Energy_le, &Select_Energy_ue);
      Select_Energy = TRUE;

      break;

    case trigger_radius:      //@< set radius of trigger area 
          
      // get trigger radius
      sscanf(line, "%s %f", token, &Trigger_Radius);
      Set_Trigger_Radius = TRUE;

      break;

    case trigger_pattern:     //@< set trigger pattern to be used
          
      // get trigger radius
      sscanf(line, "%s %s", token, strigger_pattern);

      // look for each possible trigger pattern coincident with this
      for (j=0; j<nTP_NAMES; j++) {
        if (strstr(strigger_pattern, TP_NAMES[j]) == strigger_pattern)
          break;
      }
      
      if ( j == nTP_NAMES ) {
        cerr << "Skipping unknown trigger pattern in [" << line << "]\n";
        trigger_pattern_id = TP_default;
        strcpy(strigger_pattern, "default");
      } else {
        trigger_pattern_id = triggerPattern_type[ j ];
      }
      
      break;

    case time_histo:          //@< parameters of time histo.
          
      // get trigger radius
      sscanf(line, "%s %d %f %f", token,
             &iTime_nbins, &fTime_mintime, &fTime_maxtime);

      break;

    case time_phisto:         //@< parameters of pixel time histos.
          
      // get trigger radius
      sscanf(line, "%s %d %f %f", token,
             &iTime_pnbins, &fTime_pmintime, &fTime_pmaxtime);

      break;

    case time_dischisto:      //@< parameters of discriminator histos.
          
      // get trigger radius
      sscanf(line, "%s %d %f %f", token,
             &iTime_discnbins, &fTime_discmintime, &fTime_discmaxtime);

      break;

    case trigger_bins:        //@< number of time bins in trigger
          
      // get trigger bins
      sscanf(line, "%s %d %d %d", token,
             &iTime_triggerbins, &nBefIntegBin, &nAftIntegBin);

      break;

    case correction:          //@< apply a kind of correction to pixel values
          
      // get trigger radius
      sscanf(line, "%s %f", token, &fCorrection);
      Apply_Correction = TRUE;

      break;

    case sps_mean:            //@< set single phe spectrum mean
          
      // get sps mean
      sscanf(line, "%s %f", token, &sps_Mean);

      break;

    case sps_fwhm:            //@< set single phe signals' FWHM
          
      // get sps fwhm
      sscanf(line, "%s %f", token, &sps_FWHM);
      sps_Sigma = sps_FWHM * FWHM2Sigma; // conversion FWHM->sigma 

      break;

    case end_file:            //@< end of the parameters file

      // show a short message
      is_end = TRUE;

      break;

    } // switch ( i ) 

  } // while (! is_end)

  /*'
    The trigger pattern id can be one of the "Exclusive Trigger
    Patterns" IDs (see file trigpatt.h) or a combination of them.
    Each of these trigger pattern IDs has associated a given pattern
    (with different orientations) or criterion
    We look here for which criteria we want, and make a list of
    all the possible trigger patterns we want to use
  **/
  
  j = (int)trigger_pattern_id;
  
  // first calculate how many patterns
  trigger_npatterns = 0;
  for ( l=0; l<nTP_ExclusivePatterns; l++ ) {
    if ( j & iTP_IDs[l] ) {
      trigger_npatterns += iTP_Numbers[l];
    }        
  }
  
  // then allocate space for list of trigger patterns, and get them
  trigger_patterns = new int [ trigger_npatterns ];
  
  trigger_npatterns = 0;
  for ( l=0; l<nTP_ExclusivePatterns; l++ ) {
    if ( j & iTP_IDs[l] ) {
      for ( k=0; k<iTP_Numbers[l]; k++ ) {
        trigger_patterns[ trigger_npatterns ] = iTP_Lists[l][k];
        trigger_npatterns++;
      }
    }        
  }

  displayBinaryStrings();
  
  // after the loop is finished, return to the main function
  return;
}
//}


//!-----------------------------------------------------------
// @name binaryString
//
// @desc returns a string with a 8 bit binary number on it
//
// @date Fri May  7 11:07:43 MET DST 1999
//------------------------------------------------------------
// @function 

//{
void binaryString(int value, char * s)
{
  for(int i=0; i<8; i++)
    s[7-i] = (value & (1 << i)) ? '#' : '+';  
}
//}


//!-----------------------------------------------------------
// @name binaryString
//
// @desc returns a string with a 8 bit binary number on it
//
// @date Fri May  7 11:07:43 MET DST 1999
//------------------------------------------------------------
// @function 

//{
void displayBinaryStrings()
{
  int i;
  char line1[71];
  char line2[71];
  char line3[71];
  char code[9];
  int f;

  code[8]=0;

  puts("----------------------------------------------------------------------");

  printf(" Trigger Patterns to be used : %s    %d  (ID=%d):\n\n",
         strigger_pattern, trigger_npatterns, trigger_pattern_id);
  
  for(i=0; i<trigger_npatterns; i++) {
    binaryString( trigger_patterns[i], code);
    printf("%2d  0x%02x (%4d) [%7s]\n",
           i+1, trigger_patterns[i], trigger_patterns[i], code);
  }

  f=0;
  for(i=0; i<trigger_npatterns; i++) {

    binaryString( trigger_patterns[i], code);

    sprintf(line1+f, " %c %c   ",  code[4], code[5]);
    sprintf(line2+f, "%c %c %c  ", code[3], code[7], code[6]);
    sprintf(line3+f, " %c %c   ",  code[2], code[1]);

    f += 7;
    
    if ((i%10)==9) {
      f=0;
      printf("\n%s\n%s\n%s\n", line1, line2, line3);
    }

  }

  if (f>0)
    printf("\n%s\n%s\n%s\n", line1, line2, line3);

  puts("----------------------------------------------------------------------");

}
//}


//!-----------------------------------------------------------
// @name get_input_filename
//                                                
// @desc get name of the input file
//
// @return   Name of the Input file
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
char *
get_input_filename(void)
{
  return (Input_filename);
}
//}


//!-----------------------------------------------------------
// @name get_output_filename
//                                                
// @desc get name of the output file
//
// @return   Name of the Output file
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
char *
get_output_filename(void)
{
  return (Output_filename);
}
//}


//!-----------------------------------------------------------
// @name get_data_filename
//                                                
// @desc get name of the data file
//
// @return   Name of the Data file
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
char *
get_data_filename(void)
{
  return (Data_filename);
}
//}


//!-----------------------------------------------------------
// @name get_hbook_filename
//                                                
// @desc get name of the HBOOK file
//
// @return   Name of the HBOOK file
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
char *
get_hbook_filename(void)
{
  return (HBOOK_filename);
}
//}


//!-----------------------------------------------------------
// @name get_ct_filename
//                                                
// @desc get name of CT definition file
//
// @return   Name of the CT definition file
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
char *
get_ct_filename(void)
{
  return (CT_filename);
}
//}


//!-----------------------------------------------------------
// @name get_nsb
//                                                
// @desc are we going to simulate NSB ?
//
// @var  *n  Mean value for the NSB (ph.e./pixel)
// @return   TRUE: we'll simulate NSB; FALSE: we won't
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
int
get_nsb(float *n)
{
  *n = meanNSB;
  return ( simulateNSB );
}
//}


//!-----------------------------------------------------------
// @name get_nsb_only
//                                                
// @desc are we going to simulate only NSB events?
//
// @var  *n   number of events of NSB to simulate
// @return    TRUE: we'll simulate only NSB; FALSE: we won't
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
int
get_nsb_only(int *n)
{
  *n = eventsNSB;
  return ( onlyNSB );
}
//}


//!-----------------------------------------------------------
// @name get_threshold   
//                                                
// @desc get threshold value
//
// @return   Value of the threshold q$_0$
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
float 
get_threshold(void)
{
  return( qThreshold );
}
//}


//!-----------------------------------------------------------
// @name get_tail_cut   
//                                                
// @desc get tail cut value
//
// @return   Value for the Tail-cut
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
float 
get_tail_cut(void)
{
  return( qTailCut );
}
//}


//!-----------------------------------------------------------
// @name get_islands_cut
//                                                
// @desc are we going to count the islands ?
//
// @var  *n  Cut on islands number
// @return   TRUE: we'll count the islands; FALSE: we won't
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
int
get_islands_cut(int *n)
{
  *n = nIslandsCut;
  return ( countIslands );
}
//}


//!-----------------------------------------------------------
// @name get_ana_pixels
//                                                
// @desc number of pixels for the analysis
//
// @return  Number of pixels to use in the image parameters
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
int
get_ana_pixels(void)
{
  return ( anaPixels );
}
//}


//!-----------------------------------------------------------
// @name get_seeds
//                                                
// @desc are we going to count the islands ?
//
// @var  *n  Number of the seed
// @return   N-th random-number Seed
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
long int
get_seeds(int n)
{
  return ( Seeds[n] );
}
//}


//!-----------------------------------------------------------
// @name get_skip_showers
//                                                
// @desc get list of showers to skip
//
// @var *s1  Pointer to a vector of number of showers
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
void 
get_skip_showers( int *s )
{
  int i;
  for (i=0; i<nSkip; ++i)
    s[i] = Skip[i];
}
//}


//!-----------------------------------------------------------
// @name get_nskip_showers
//                                                
// @desc get number of showers to skip
//
// @return  Number of showers to be skipped
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//------------------------------------------------------------
// @function 

//{
int 
get_nskip_showers( void )
{
  return( nSkip );
}
//}


//!-----------------------------------------------------------
// @name get_data_from_stdin
//                                                
// @desc get whether we will read the data from the STDIN
//
// @return  TRUE: we'll read data from STDIN; FALSE: we won't
//
// @date Wed Nov 25 13:21:00 MET 1998
//------------------------------------------------------------
// @function 

//{
int
get_data_from_stdin(void)
{
  return ( Data_From_STDIN );
}
//}


//!-----------------------------------------------------------
// @name get_read_phe
//                                                
// @desc get whether we will read PHE files
//
// @return  TRUE: we'll read PHE files; FALSE: we won't
//
// @date Wed Nov 25 13:21:00 MET 1998
//------------------------------------------------------------
// @function 

//{
int
get_read_phe(void)
{
  return ( Read_Phe );
}
//}


//!-----------------------------------------------------------
// @name get_read_phe_all
//                                                
// @desc get whether we will read PHE files, with write_all_images
//
// @return  TRUE: we'll do it; FALSE: we won't
//
// @date Wed Nov 25 13:21:00 MET 1998
//------------------------------------------------------------
// @function 

//{
int
get_read_phe_all(void)
{
  return ( Read_Phe_All );
}
//}


//!-----------------------------------------------------------
// @name write_all_images
//                                                
// @desc write all images to .phe, even those without trigger
//
// @return  TRUE: we'll write everything; FALSE: we won't
//
// @date Wed Nov 25 13:21:00 MET 1998
//------------------------------------------------------------
// @function 

//{
int
get_write_all_images(void)
{
  return ( Write_All_Images );
}
//}


//!-----------------------------------------------------------
// @name write_all_data
//                                                
// @desc write single pixel data to file .dat
//
// @return  TRUE: we'll write everything; FALSE: we won't
//
// @date Wed Nov 25 13:21:00 MET 1998
//------------------------------------------------------------
// @function 

//{
int
get_write_all_data(void)
{
  return ( Write_All_Data );
}
//}


//!-----------------------------------------------------------
// @name get_select_energy                                      
//
// @desc return energy range allowed for showers from .phe file
//
// @var *le  Lower limit in the allowed energy range
// @var *ue  Lower limit in the allowed energy range
// @return  TRUE: we make selection on the energy; FALSE: we don't
//
// @date Wed Nov 25 13:21:00 MET 1998
//------------------------------------------------------------
// @function 

//{
int
get_select_energy(float *le, float *ue)
{
  *le = Select_Energy_le;
  *ue = Select_Energy_ue;
  return ( Select_Energy );
}
//}


//!-----------------------------------------------------------
// @name get_trigger_radius                                  
//
// @desc return the radius of the trigger area in the camera (if set)
//
// @var *radius  Radius of the trigger area in the camera
// @return  TRUE: we choose a given radius for the trigger area
//
// @date Fri May  7 11:07:43 MET DST 1999
//------------------------------------------------------------
// @function 

//{
int get_trigger_radius(float *radius)
{
  *radius = Trigger_Radius;
  return ( Set_Trigger_Radius );
}
//}


//!-----------------------------------------------------------
// @name get_trigger_radius                                  
//
// @desc return the radius of the trigger area in the camera (if set)
//
// @var *radius  Radius of the trigger area in the camera
// @return  TRUE: we choose a given radius for the trigger area
//
// @date Fri May  7 11:07:43 MET DST 1999
//------------------------------------------------------------
// @function 

//{
int get_correction(float *corr)
{
  *corr = fCorrection;
  return ( Apply_Correction );
}
//}


//!-----------------------------------------------------------
// @name get_trigger_pattern_id                           
//
// @desc return the trigger pattern identifier
//
// @return  trigger pattern identifier
//
// @date Wed Jan 19 14:51:57 MET 2000
//------------------------------------------------------------
// @function 

//{
TriggerPattern_type get_trigger_pattern_id(void)
{
  return ( trigger_pattern_id );
}
//}


//!-----------------------------------------------------------
// @name get_trigger_npatterns                           
//
// @desc return the number of trigger patterns
//
// @return  trigger number of trigger patterns
//
// @date Wed Jan 19 14:51:57 MET 2000
//------------------------------------------------------------
// @function 

//{
int get_trigger_npatterns(void)
{
  return ( trigger_npatterns );
}
//}


//!-----------------------------------------------------------
// @name get_trigger_patterns                           
//
// @desc return the trigger patterns list
//
// @return  pointer to the trigger patterns list
//
// @date Wed Jan 19 14:51:57 MET 2000
//------------------------------------------------------------
// @function 

//{
int * get_trigger_patterns(void)
{
  return ( trigger_patterns );
}
//}


//!-----------------------------------------------------------
// @name get_time_histo
//
// @desc Gives information about time histos.
//
// @return  number of bins in time histograms
//
// @date Fri Nov 26 12:57:36 MET 1999
//------------------------------------------------------------
// @function 

//{
int get_time_histo(float *mintime, float *maxtime)
{
  *mintime = fTime_mintime;
  *maxtime = fTime_maxtime;
  return iTime_nbins;
}
//}


//!-----------------------------------------------------------
// @name get_time_phisto
//
// @desc Gives information about pixel time histos.
//
// @return  number of bins in time histograms
//
// @date Fri Nov 26 12:57:36 MET 1999
//------------------------------------------------------------
// @function 

//{
int get_time_phisto(float *mintime, float *maxtime)
{
  *mintime = fTime_pmintime;
  *maxtime = fTime_pmaxtime;
  return iTime_pnbins;
}
//}


//!-----------------------------------------------------------
// @name get_time_dischisto
//
// @desc Gives information about pixel time histos.
//
// @return  number of bins in time histograms
//
// @date Wed Jan 26 16:40:45 MET 2000
//------------------------------------------------------------
// @function 

//{
int get_time_dischisto(float *mintime, float *maxtime)
{
  *mintime = fTime_discmintime;
  *maxtime = fTime_discmaxtime;
  return iTime_discnbins;
}
//}


//!-----------------------------------------------------------
// @name get_trigger_bins
//
// @desc Gives number of time bins to look for trigger condition
//
// @return  number of time bins in trigger condition
//
// @date Wed Jan 26 16:40:45 MET 2000
//------------------------------------------------------------
// @function 

//{
int get_trigger_bins(void)
{
  return iTime_triggerbins;
}
//}


//!-----------------------------------------------------------
// @name get_time_intgate
//
// @desc Limits of integration time gate
//
// @var *minbin  bins to use before the trigger is fired
// @var *maxbin  bins to use after the trigger is fired
//
// @date Wed Jan 26 16:40:45 MET 2000
//------------------------------------------------------------
// @function 

//{
void get_time_intgate(int *a, int *b)
{
  *a = nBefIntegBin;
  *b = nAftIntegBin;
}
//}


//!-----------------------------------------------------------
// @name get_sps_params
//
// @desc Returns the desired Single Ph.e. Spectrum (SPS) params.
//
// @var mean   desired mean for the SPS
// @var fwhm   FWHM of single ph.e. signals
// @var sigma  sigma of single ph.e. signals
//
// @date Mon Feb 14 12:33:32 MET 2000
//------------------------------------------------------------
// @function 

//{
void get_sps_params(float &mean, float &fwhm, float &sigma)
{
  mean  = sps_Mean;
  fwhm  = sps_FWHM;
  sigma = sps_Sigma;
}
//}


//=------------------------------------------------------------
//## Log of this file.

//{
//
// $Log$
// Revision 1.13  2000/02/15  10:29:52  gonzalez
// *** empty log message ***
//
// Revision 1.12  2000/02/11  06:47:49  gonzalez
// Real camera for MAGIC: test 1
//
// Revision 1.11  2000/01/27  10:48:51  gonzalez
// Trigger patterns + timing seem to work
//
// Revision 1.10  1999/12/13  14:57:28  gonzalez
// pre-navidades 2000
//
// Revision 1.9  1999/12/03  17:18:09  gonzalez
// *** empty log message ***
//
// Revision 1.8  1999/10/08  08:00:02  gonzalez
// Bug in ISLANDS algorithm fixed
//
// Revision 1.7  1999/10/05  11:39:52  gonzalez
// Sep. 1999
//
// Revision 1.6  1999/03/15  14:59:08  gonzalez
// camera-1_1
//
// Revision 1.5  1999/03/02  09:56:12  gonzalez
// *** empty log message ***
//
// Revision 1.4  1999/01/14  17:32:41  gonzalez
// Added to camera the STDIN input option (data_from_input)
//
//}

//=EOF
