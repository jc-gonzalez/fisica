//////////////////////////////////////////////////////////////////
//
// splitter                  
//
// @file        splitter.cxx
// @title       CORSIKA Filter
// @subtitle    Filter for CORSIKA 5.20m files
// @author      J C Gonz\'alez
// @email       gonzalez@mppmu.mpg.de
//
// @maintitle
//_______________________________________________________________
//
//  Created: Thu May  7 16:24:22 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Filter for CORSIKA 5.20m files
//  Notes:   
//    
//    
//_______________________________________________________________
// $RCSfile$
// $Revision$
// $Author$ 
// $Date$
//////////////////////////////////////////////////////////////////

// @tableofcontents 

// @section Source code of {\tt splitter.cxx}

/* @text
In this section we show the (commented) code of the program 
for the filtering of CORSIKA 5.20m raw-output files. We select those 
files within a given criteria.
@endtext */

// @subsection Includes and Global variables definition

// @code

// @comment 
// All the defines are located in the file {\tt reflector.h}.
// @endcomment
 
#include "splitter.h"

// @comment
// We define the number of the output file.
// @endcomment

// Number of the output file
static int CerFile = 0;

// @comment
// This is a flag to change the {\it verboseness\/} of the output
// @endcomment

// verbose or not
int verbose = FALSE;

// @endcode

// @subsection Main program

/* @text
   In the main program we read the input from the STDIN and write
   to the current directory those files within the given criteria.
   (In this version the only criterium is "Energy in [Elow:Eup]".)
@endtext */

// @code
//++++++++++++++++++++++++++++++++++++++++
// MAIN PROGRAM 
//----------------------------------------
                   
int
main(int argc, char **argv) 
{

  ifstream inputfile;         // input file (stream)

  ofstream runfile;           // output file (stream)
  ofstream cerfile;           // output file (stream)
  ofstream datfile;           // output file (stream)
  ofstream stafile;           // output file sta (stream)

  char inputname[256];        // input filename
  char pathname[256];         // output directory
  char cername[256];       // output filename
  char datname[256];       // output filename
  char staname[256];       // output filename
  char runname[256];       // output filename

  COREventHeader evth;        // Event Header class (from CORSIKA)
  CORParticle photon;         // Particle (photon) class (from CORSIKA)
  CORStatfile sta;            // Stat. variables

  int i, j, k, n;             // simple counters

  int c, errflg = 0;          // used by getopt

  char *cbuffer;
  float *buffer;              // buffer for read-out
  float *sbuffer;              // buffer for read-out

#define RUNH   sbuffer                // the same as above
#define RUNE   sbuffer                // the same as above
#define EVTH   sbuffer                // the same as above
#define EVTE   sbuffer                // the same as above
#define DATA   sbuffer                // the same as above

  char cRUNH[]="RUNH";
  char cRUNE[]="RUNE";
  char cEVTH[]="EVTH";
  char cEVTE[]="EVTE";

  int bytes;

  int escape = FALSE;

  static nsubblock = 1;

  float tmin, tmax;

  float lower_wl = 290.;
  float upper_wl = 600.;

  /* @comment 

     Hello!

     @endcomment */

  //++
  //  START
  //--

  // parse command line options
  
  optarg = NULL;
  while ( !errflg && ((c = getopt(argc, argv, COMMAND_LINE_OPTIONS)) != -1) )
    switch (c) {
    case 'p':
      strcpy(pathname, optarg);
      break;
    case 'f':
      strcpy(inputname, optarg);
      break;
    case 'v':
      verbose = TRUE;
      break;
    case 'q':
      verbose = FALSE;
      break;
    default :
      errflg++;
    }
  
  // make some sort of presentation
  
  if (verbose)
    present();

  // set all random numbers seeds

  setall( 314192L, 278182L );
  
  // allocate memory for buffer

  if (verbose)
    log("splitter", "Allocating memory for buffer...\n");

  buffer = new float [BLOCK_LENGTH];
  
  // open input file

  inputfile.open( inputname );

  if ( inputfile.bad() )
    error("splitter", "Cannot open input file %s. Exiting.", inputname);

  // read block 

  read_mark(inputfile);
  read_block(inputfile, buffer);  
  read_mark(inputfile);

  // RUNH :: read mark + runh + mark
  sbuffer = buffer;
  
  // write RUNH to run000001 file
  sprintf(runname, "%s/run000001", pathname);
  runfile.open(runname);
  write_subblock(runfile, RUNH);
  runfile.close();

  if (verbose)
    log("splitter", " RUNH\n");

  tmin = 999999.;
  tmax = -1.0;

  // main loop
  
  while ( ! escape ) {

    // get subblock

    ++nsubblock;
    if (nsubblock>NUM_SUBBLOCKS) {

      nsubblock=1;
      read_mark(inputfile);
      read_block(inputfile, buffer);
      read_mark(inputfile);
      sbuffer=buffer;

    } else {
  
      sbuffer = sbuffer + SUBBLOCK_LENGTH;

    }
    cbuffer = (char*)sbuffer;

    /*
    if (verbose)
      log("splitter", "Reading block . . . (%db)\n", bytes);
    */

    // is EVTH ?
    
    if ( memcmp(cbuffer, cEVTH, 4) == 0 ) {

      if (verbose)
        log("splitter", " + EVTH\n");

      // write to new cer-file
      ++CerFile;
      sprintf(cername, "%s/cer%06d", pathname, CerFile);
      cerfile.open(cername);
      write_subblock(cerfile, EVTH);
      memcpy(&sta.evth[0], EVTH, SUBBLOCK_LENGTH * sizeof(float));
      
      // loop!
      continue;

    }

    // is EVTE ?
    
    if ( memcmp(cbuffer, cEVTE, 4) == 0 ) {

      if (verbose)
        log("splitter", " + EVTE\n");

      // write to cer-file, and close it
      write_subblock(cerfile, EVTE);
      cerfile.close();

      sprintf(staname, "%s/sta%06d", pathname, CerFile);
      stafile.open(staname);
      memcpy(&sta.evte[0], EVTE, SUBBLOCK_LENGTH * sizeof(float));
      sta.timefirst = tmin;
      sta.timelast = tmax;
      sta.write(stafile);
      stafile.close();

      // loop!
      continue;

    }

    // is RUNE ?
    
    if ( memcmp(cbuffer, cRUNE, 4) == 0 ) {

      if (verbose)
        log("splitter", " RUNE\n");

      // write to end-file, and quit
      sprintf(runname, "%s/end000001", pathname);
      runfile.open(runname);
      write_subblock(runfile, RUNE);
      runfile.close();
      escape = TRUE;
      continue;

    }

    // then, it must be real DATA

     if (verbose)
        log("splitter", " + + DATA\n");
     
     // generate wavelengths, and calculate times

     for (i=0; i<NUM_CPHOTONS; ++i) {

       tmax = MAX(tmax, DATA[i*7+6]);

       if (DATA[6]>0.0)
         tmin = MIN(tmin, DATA[i*7+6]);

       DATA[i*7] = 1000. +
         1. / (1./lower_wl - ranf()/
               (lower_wl*upper_wl/(upper_wl-lower_wl)));
     }

    write_subblock(cerfile, DATA);
    
  }

  // program finished

  if ( verbose )
    log( "splitter", "Done.\n");

  return ( 0 );
}
// @endcode

// @T \newpage

// @subsection Functions definition

//------------------------------------------------------------
// @name present
//
// @desc Make some presentation
//
// @date Sat Jun 27 05:58:56 MET DST 1998
// @function @code 
//------------------------------------------------------------
// present
//
// make some presentation
//------------------------------------------------------------

void 
present(void)
{
  cout << "##################################################\n"
       << "splitter\n\n"
       << "Filter for CORSIKA 5.20m raw-output files\n"
       << "J C Gonzalez, October 1998\n"
       << "##################################################\n\n"
       << flush;
}
// @endcode

//------------------------------------------------------------
// @name log                             
//                                   
// @desc function to send log information
//
// @date Sat Jun 27 05:58:56 MET DST 1998
// @function @code 
//------------------------------------------------------------
// log
//
// function to send log information
//------------------------------------------------------------

void
log(const char *funct, char *fmt, ...)
{
  va_list args;
  
  //  Display the name of the function that called error
  printf("[%s]: ", funct);
  
  // Display the remainder of the message
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}
// @endcode


//------------------------------------------------------------
// @name error                                                    
//                                                           
// @desc function to send an error message, and abort the program
//
// @date Sat Jun 27 05:58:56 MET DST 1998
// @function @code 
//------------------------------------------------------------
// error
//
// function to send an error message, and abort the program
//------------------------------------------------------------

void
error(const char *funct, char *fmt, ...)
{
  va_list args;

  //  Display the name of the function that called error
  fprintf(stderr, "ERROR in %s: ", funct);

  // Display the remainder of the message
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);

  abort();
}
// @endcode


//------------------------------------------------------------
// @name read_mark                                                    
//                                                           
// @desc read a block mark from ifstream
//
// @date Sat Jun 27 05:58:56 MET DST 1998
// @function @code 
//------------------------------------------------------------
// read_mark
//
// read a block mark from ifstream
//------------------------------------------------------------

static void
read_mark(ifstream &f)
{
  static char mark[MARK_LENGTH];

  f.read(mark, MARK_LENGTH);

  return;
}
// @endcode


//------------------------------------------------------------
// @name read_block                                                    
//                                                           
// @desc read a block from ifstream
//
// @date Sat Jun 27 05:58:56 MET DST 1998
// @function @code 
//------------------------------------------------------------
// read_block
//
// read a block from ifstream
//------------------------------------------------------------

static void
read_block(ifstream &f, float * blk)
{
  f.read((char*)blk, BLOCK_LENGTH * sizeof(float));

  return;
}
// @endcode


//------------------------------------------------------------
// @name write_subblock                                                    
//                                                           
// @desc write a sub-block to ifstream
//
// @date Sat Jun 27 05:58:56 MET DST 1998
// @function @code 
//------------------------------------------------------------
// write_subblock
//
// write a sub-block to ifstream
//------------------------------------------------------------

static void
write_subblock(ofstream &f, float * blk)
{
  f.write((char*)blk, SUBBLOCK_LENGTH * sizeof(float));

  return;
}
// @endcode

// EOF
