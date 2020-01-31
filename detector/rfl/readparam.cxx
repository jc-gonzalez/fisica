//=//////////////////////////////////////////////////////////////////////
//=
//= readparam            
//=
//= @file        readparam.cxx
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

//!@section Source code of |readparam.cxx|.

/*!@"

  This section describes briefly the source code for the file
  |readparam.cxx|. All the defines it uses are located in the files
  |readparam.h| and |atm.h|. In the first one we can see the
  definitions of the commands available for the parameters file. We
  will describe these commands later on. Please, note that this
  program is still in development, and more commands will be added
  soon.

  @"*/

//!@subsection Includes and Global variables definition.

/*!@" 

  All the defines are located in the file |readparam.h|.

  @"*/
 
//!@{

#include "readparam.h"

//!@}

//!@subsection Definition of global variables.

/*!@"

  Here we define the global variables where the values from the
  parameters file are stored.

  @"*/

//!@{

//@: list of paths to get data from
static char **Paths_list;   

//@: number of paths
static int Num_of_paths = 1; 

//@: output filename
static char Output_filename[PATH_MAX_LENGTH];

//@: name of the CT def. file
static char CT_filename[PATH_MAX_LENGTH]; 

//@: verbosity
static VerboseLevel verbose = VERBOSE_DEFAULT;

//@: fixed target zenith angle (theta)
static float fixed_Theta;

//@: fixed target zenith angle (theta)
static float fixed_Phi;

//@: lower limits for the energy of the showers to be used
static float low_Ecut = 0.0;        

//@: lower limits for the energy of the showers to be used
static float high_Ecut = 100000.0;

//@: are we using fixed target?
static int is_Fixed_Target = FALSE; 

//@: maximum number of events to read
static int max_Events = 50000; 

//@: range of events to read
static int first_Event=0, last_Event=1000000; 

//@: random seeds
static long int Seeds[2] = {3141592L, 2718182L}; 

//@: flag: blocking?
static int Block=0;

//@: flag: random pointing?
static int Random_Pointing = FALSE;

//@: flag: random pointing isotropic?
static int Random_Pointing_Isotropic = TRUE;

//@: flag: random pointing minimum distance
static float Random_Pointing_MinDist = 0.0;

//@: flag: random pointing maximum distance
static float Random_Pointing_MaxDist = 0.0;

//@: flag: do we read from STDIN?
static int Data_From_STDIN = FALSE;

//@: flag: do we write to  STDOUT?
static int Data_To_STDOUT = FALSE;

//@: number of times a shower is going to be processed
static int nRepeat_Random = 1;

//!@}

/*!@"
  
  Here we define the global variables where the parameters of a
  parallel beam of light will be stored.

  @"*/

//!@{

#define NORM(v) (sqrt( SQR(v[0]) + SQR(v[1]) + SQR(v[2]) ))
#define RAD(x)  ((x)*0.0174532925199433)
#define DEG(x)  ((x)*57.2957795130823)

static int   pb_ParallelBeam = FALSE;
static int   pb_ParallelBeamPM = FALSE;
static float pb_Theta, pb_Phi, pb_X, pb_Y; 
static float pb_Height; 
static float pb_LengthX, pb_LengthY, pb_NX, pb_NY;
static float pb_Scale;
static char  pb_Filename[40];

//!@}

//!@subsection The function |readparam()|.

//!---------------------------------------------------------------------
// @name  creadparam
//                                                
// @desc  read parameters from the stdin / parameters file
//
// @var   *filename  Name of the parameters file (0->STDIN)
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{
void 
readparam(char * filename)
{
  char sign[] = GLUE_postp( PROGRAM, VERSION ); // initialize sign
  char line[LINE_MAX_LENGTH];    // line to get from the stdin
  char token[ITEM_MAX_LENGTH];   // a single token
  char atm[ATM_NAME_MAX_LENGTH]; // name of the atm. model from stdin
  int i, j;                      // dummy counters
  ifstream ifile;

  // use cin or ifile (reading from STDIN or from parameters file?
  if ( filename != 0 )
    ifile.open( filename );

  // get signature
  if ( filename != 0 )
    ifile.getline(line, LINE_MAX_LENGTH);
  else
    cin.getline(line, LINE_MAX_LENGTH);
  line[strlen(SIGNATURE)] = '\0';
  strcpy(line, sign);
  if (strcmp(sign, SIGNATURE) != 0) {
    cerr << "ERROR: Signature of parameters file is not correct\n";
    cerr << '"' << sign << '"' << '\n';
    cerr << "should be: " << SIGNATURE << '\n';
    exit(1);
  }

  // loop till the "end" directive is reached
  int is_end = FALSE;
  while (! is_end) {          

    // get line from file or stdin
    if ( filename != 0 )
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

    // case block for each directive
    switch ( i ) {

    case data_paths:          // beginning of the list of valid paths
          
      // get number of paths to read
      sscanf(line, "%s %d", token, &Num_of_paths);

      if ( verbose == TRUE )
        cout << Num_of_paths << " path(s) will be used.\n";

      // allocate memory for paths list
      Paths_list = (char**)malloc(Num_of_paths * sizeof(char*));
      for (i=0; i<Num_of_paths; i++) 
        Paths_list[i] = (char*)malloc(PATH_MAX_LENGTH * sizeof(char));

      // read each single path
      for (i=0; i<Num_of_paths; i++) {

        if ( filename != 0 )
          ifile.getline(Paths_list[i], PATH_MAX_LENGTH);
        else
          cin.getline(Paths_list[i], PATH_MAX_LENGTH);

        // remove empty spaces at the end
        for (j=0; j<strlen(Paths_list[i]); j++) {
          if (Paths_list[i][j] == ' ') {
            Paths_list[i][j] = '\0';
            break;
          }                     
        }

        // show the path
        if ( verbose == TRUE )
          cout << '[' << Paths_list[i] << ']' << '\n';
      }

      break;

    case output_file:         // name of the output file
          
      // get the name of the output_file from the line
      sscanf(line, "%s %s", token, Output_filename);

      break;

    case ct_file:             // name of the telescope file
          
      // get the name of the ct_file from the line
      sscanf(line, "%s %s", token, CT_filename);

      break;

    case verbose_level:      // verbose level 0-4
          
      // get the verbose level
      sscanf(line, "%s %d", token, &verbose);
          
      break;

    case atm_model:           // name of the atmospheric model
          
      // get the name of the atm_model from the line
      sscanf(line, "%s %s", token, atm);
      // set the name and the number of the model with the functions
      // defined in atm.h
      set_atm_mod( conv_atm_mod( atm ) );

      break;

    case fixed_target:        // we use a fixed target
          
      // get fixed target's coordinates (is degrees)
      sscanf(line, "%s %f %f", token, &fixed_Theta, &fixed_Phi);
          
      // convert them to radians
      fixed_Theta *= DEG2RAD;
      fixed_Phi   *= DEG2RAD;
      is_Fixed_Target = TRUE;
          
      break;

    case max_events:          // maximum number of events
          
      // get the number of events
      sscanf(line, "%s %d", token, &max_Events);
          
      break;

    case range_events:          // maximum number of events
          
      // get the number of events
      sscanf(line, "%s %d %d", token, &first_Event, &last_Event);
      max_Events = 1000000;
          
      break;

    case energy_cuts:         // range of energies allowed
          
      // get the cuts
      sscanf(line, "%s %f %f", token, &low_Ecut, &high_Ecut);
          
      break;

    case parallel_beam:       // parallel beam of light
          
      // get the parameters of the beam 
      sscanf(line, "%s %f %f %f %f %f %f %f %f %f", token, 
             &pb_Theta, &pb_Phi, 
             &pb_X, &pb_Y, 
             &pb_LengthX, &pb_LengthY,
             &pb_NX, &pb_NY, &pb_Height );

      pb_Theta = RAD(pb_Theta);
      pb_Phi = RAD(pb_Phi);

      pb_ParallelBeam = TRUE;
      pb_ParallelBeamPM = FALSE;

      Num_of_paths = 1;
      strcpy( Paths_list[0], "." );

      break;

    case pm_parallel_beam:    // parallel beam of light out of a pixmap
          
      // get the parameters of the beam 
      sscanf(line, "%s %s %f %f", token, 
             pb_Filename, &pb_Scale, &pb_Height);

      pb_ParallelBeam = TRUE;
      pb_ParallelBeamPM = TRUE;

      Num_of_paths = 1;
      strcpy( Paths_list[0], "." );

      break;

    case seeds:               // values of seeds for random numbers
          
      // get seeds
      sscanf(line, "%s %ld %ld", token, &Seeds[0], &Seeds[1]);
      
      break;
    
    case random_pointing:     // uses a random CT pointing for each shower
          
      // set flag
      Random_Pointing = TRUE;
      sscanf(line, "%s %f %f %d %ld %ld", token, 
             &Random_Pointing_MinDist,
             &Random_Pointing_MaxDist,
             &Random_Pointing_Isotropic,
             &Seeds[0], &Seeds[1]);

      if (Random_Pointing_Isotropic > 0)
        Random_Pointing_Isotropic = TRUE;
      
      cerr << "random_pointing: "
           << Random_Pointing_MinDist << '-'
           << Random_Pointing_MaxDist << endl << flush;
      
      Random_Pointing_MinDist = RAD(Random_Pointing_MinDist);
      Random_Pointing_MaxDist = RAD(Random_Pointing_MaxDist);

      break;
      
    case repeat_random:       // number of times a random pointing to be done
          
      // set nRepeat_Random
      sscanf(line, "%s %d", token, &nRepeat_Random);

      cerr << "repeat_random: " << nRepeat_Random << '\n' << flush;

      break;
    
    case block:               // analyzes data in blocks
      
      // get block value
      sscanf(line, "%s %d", token, &Block);
      
      break;
      
    case data_from_stdin:     // analyzes data in blocks
      
      // set flag
      Data_From_STDIN = TRUE;
      
      // set other parameters
      Num_of_paths = 1;
      strcpy( Paths_list[0], TMP_STDIN_DIR );

      break;
      
    case data_to_stdout:      // analyzes data in blocks
      
      // set flag
      Data_To_STDOUT = TRUE;
      
      break;
      
    case end_file:            // end of the parameters file

      // show a short message
      is_end = TRUE;

      break;

    } // switch ( i ) 

  } // while (! is_end)

  if ( filename != 0 ) {
    ifile.close();
  }

  // after the loop is finished, return to the main function
  return;
}
//!@} 


//!---------------------------------------------------------------------
// @name get_num_of_paths                 
//                                                
// @desc get number of defined paths
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int
get_num_of_paths(void)
{
  return (Num_of_paths);
}
//!@}


//!---------------------------------------------------------------------
// @name get_path_name
//                                                
// @desc get path name number "i" from the list of paths
//
// @var     i  number of path name to return
//
// @return  i-th path name
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
char *
get_path_name(int i)
{
  return (Paths_list[i]);
}
//!@}


//!---------------------------------------------------------------------
// @name get_output_filename
//                                                
// @desc get name of the output file
//
// @return  output file name
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
char *
get_output_filename(void)
{
  return (Output_filename);
}
//!@}


//!---------------------------------------------------------------------
// @name get_ct_filename
//                                                
// @desc get name of CT definition file
//
// @return  CT definition file name
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
char *
get_ct_filename(void)
{
  return (CT_filename);
}
//!@}


//!---------------------------------------------------------------------
// @name get_verbose
//                                                
// @desc get the "verbose" status
//
// @return  Verbosity level
//
// @date Sat Jun 27 05:58:56 MET DST 1998
// 
//----------------------------------------------------------------------
// @function

//!@{ 
int
get_verbose(void)
{
  return (verbose);
}
//!@}


//!---------------------------------------------------------------------
// @name get_fixed_target   
//                                                
// @desc get fixed target's coordinates
//
// @var  *theta    returned fixed theta
// @var  *phi      returned fixed phi
//
// @return  TRUE if fixed target is selected; FALSE othw.
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int 
get_fixed_target(float *theta, float *phi)
{
  if ( is_Fixed_Target == FALSE )
    return (FALSE);
  *theta = fixed_Theta;
  *phi   = fixed_Phi;
  return(TRUE);
}
//!@}


//!---------------------------------------------------------------------
// @name get_max_events             
//                                          
// @desc get max events number
//
// @return   Maximum number of events to read
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int 
get_max_events(void)
{
  return( max_Events );
}
//!@}


//!---------------------------------------------------------------------
// @name get_range_events              
//                                          
// @desc get range of events
//
// @var  *a   returned first event number to read
// @var  *b   returned last  event number to read
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
get_range_events(int *a, int *b)
{
  *a = first_Event;
  *b = last_Event;
}
//!@}


//!---------------------------------------------------------------------
// @name get_energy_cuts            
//                                          
// @desc get energy cuts (lowest/highest energy allowed)
//
// @var  *low    returned lower edge of energy range
// @var  *high   returned lower edge of energy range
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
get_energy_cuts(float *low, float *high)
{
  *low  = low_Ecut;
  *high = high_Ecut;
}
//!@}


//!---------------------------------------------------------------------
// @name get_parallel_beam            
//                                          
// @desc get parallel beam parameters
//
// @var *pb_theta   returned zenith angle of the parallel beam
// @var *pb_phi     returned aximuth angle of the parallel beam
// @var *pb_x       returned x coordinate of the center
// @var *pb_y       returned y coordinate of the center
// @var *pb_lengthx returned length of the square in the ground
// @var *pb_lengthy returned width of the square in the ground
// @var *pb_nx      returned number of rows to generate
// @var *pb_ny      returned number of columns to generate
// @var *pb_height  returned height of the point-like source
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
get_parallel_beam(float *pb_theta, float *pb_phi, 
                  float *pb_x, float *pb_y, 
                  float *pb_lengthx, float *pb_lengthy, 
                  float *pb_nx, float *pb_ny, float *pb_height)
{

  *pb_theta = pb_Theta;
  *pb_phi = pb_Phi;
  *pb_x = pb_X;
  *pb_y = pb_Y;
  *pb_lengthx = pb_LengthX;
  *pb_lengthy = pb_LengthY;
  *pb_nx = pb_NX;
  *pb_ny = pb_NY;
  *pb_height = pb_Height;

  return;
}
//!@}


//!---------------------------------------------------------------------
// @name is_parallel_beam            
//                                          
// @desc should I use a parallel beam of light
//
// @return  TRUE: we are generating a parallel beam of light
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int 
is_parallel_beam(void)
{
  return( pb_ParallelBeam );
}
//!@}


//!---------------------------------------------------------------------
// @name get_parallel_beam_pm            
//                                          
// @desc get parallel beam PM parameters
//
// @var *pb_scale   returned scale to be applied to the pixmap
// @var *pb_height  returned height were the pixmap is located
//
// @return          filename of the pixmap
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
char *
get_parallel_beam_pm(float *pb_scale, float *pb_height)
{
  *pb_scale = pb_Scale;
  *pb_height = pb_Height;

  return(pb_Filename);
}
//!@}


//!---------------------------------------------------------------------
// @name is_parallel_beam_pm          
//                                          
// @desc should I use a parallel beam of light(from pixmap)
//
// @return  TRUE: we are generating a parallel beam of light from a pixmap
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int 
is_parallel_beam_pm(void)
{
  return( pb_ParallelBeamPM );
}


//!---------------------------------------------------------------------
// @name get_seeds
//                                                
// @desc are we going to count the islands ?
//
// @var  n    number of seed to get
//
// @return    n-th random seed
//
// @date Mon Sep 14 13:27:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
long int
get_seeds(int n)
{
  return ( Seeds[n] );
}
//!@}


//!---------------------------------------------------------------------
// @name get_block
//                                                
// @desc get the block size (=0  -> no blocking)
//
// @return    Block value
//
// @date Wed Nov 25 13:21:00 MET 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int
get_block(void)
{
  return ( Block );
}
//!@}


//!---------------------------------------------------------------------
// @name get_data_from_stdin
//                                                
// @desc get whether we will read the data from the STDIN
//
// @return    TRUE: we get data from STDIN; FALSE: oth.
//
// @date Wed Nov 25 13:21:00 MET 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int
get_data_from_stdin(void)
{
  return ( Data_From_STDIN );
}
//!@}


//!---------------------------------------------------------------------
// @name get_data_to_stdout
//                                                
// @desc get whether we will write the output data to the STDOUT
//
// @return    TRUE: we put data into the STDOUT; FALSE: oth.
//
// @date Wed Nov 25 13:21:00 MET 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int
get_data_to_stdout(void)
{
  return ( Data_To_STDOUT );
}
//!@}


//!---------------------------------------------------------------------
// @name get_random_pointing
//       
// @desc get whether random pointing is selected
//
// @return    TRUE: we use a random pointing
//
// @date Wed Nov 25 13:21:00 MET 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int
get_random_pointing(float *mindist, float *maxdist, int *isotropic)
{
  *mindist = Random_Pointing_MinDist;
  *maxdist = Random_Pointing_MaxDist;
  *isotropic = Random_Pointing_Isotropic;
  return ( Random_Pointing );
}
//!@}


//!---------------------------------------------------------------------
// @name get_repeat_random
//       
// @desc get number of times a random displacement is going to be done
//
// @return   number of times
//
// @date 
//----------------------------------------------------------------------
// @function

//!@{ 
int
get_repeat_random(void)
{
  return ( nRepeat_Random );
}
//!@}


//=------------------------------------------------------------
//!@subsection Log of this file.

//!@{
//
// $Log$
// Revision 1.1.1.1  2000/11/04 17:08:16  gonzalez
// ...
//
// Revision 1.15  2000/03/22  15:56:42  gonzalez
// *** empty log message ***
//
// Revision 1.14  2000/01/27  10:47:54  gonzalez
// JAN2000-STABLE
//
// Revision 1.13  1999/10/05  11:06:38  gonzalez
// Sep. 1999
//
// Revision 1.12  1999/03/24  16:33:00  gonzalez
// REFLECTOR 1.1: Release
//
// Revision 1.11  1999/01/21  16:03:37  gonzalez
// Only small modifications
//
// Revision 1.10  1999/01/19  18:07:18  gonzalez
// Bugs in STDIN-STDOUT version corrected.
//
// Revision 1.9  1999/01/14  17:35:46  gonzalez
// Both reading from STDIN (data_from_stdin) and
// writing to STDOUT (data_to_STDOUT) working.
//
// Revision 1.8  1999/01/13  12:41:11  gonzalez
// THIS IS A WORKING (last?) VERSION
//
// Revision 1.7  1998/12/15  10:47:24  gonzalez
// RELEASE-1.0
//
//!@}

//=EOF
