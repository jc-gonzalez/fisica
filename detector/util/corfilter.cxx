//////////////////////////////////////////////////////////////////
//
// corfilter                  
//
// @file        corfilter.cxx
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

// @section Source code of {\tt corfilter.cxx}

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
 
#include "corfilter.h"

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

  char cername[256];          // output filename
  ofstream cerfile;           // output file (stream)
  char staname[256];          // output filename sta
  ofstream stafile;           // output file sta (stream)

  COREventHeader *evth;       // Event Header class (from CORSIKA)
  CORParticle photon;         // Particle (photon) class (from CORSIKA)
  CORStatfile sta;            // Stat. variables

  int maxCerFile=9999;             // maximum number of files to write

  float Elow=0., Eup=1000000.;  // range for Energy

  int i, j, k, n;             // simple counters

  int c, errflg = 0;          // used by getopt

  float *buffer, *buffer2;    // buffer for read-out of STDIN
  char *result, *chkbuffer;
  int dif, nfound;
  int bytes;

  int reject;

  float energy;
             
  /* @comment 

     We start in the main program. First we (could) make
     some presentation, and follows the reading of the
     STDIN. The files are assumed to be saved in the order\par
     {\centering \tt \ldots, dat(N), cer(N), sta(N), dat(N+1), 
     cer(N+1), sta(N+1), \ldots\\}\par
     
     The algorithm is something like this:
     \begin{enumerate}
     \item Read block in buffer
     \item Is the string "EVTH" in buffer? NO: Go to 1.
     \item YES: Number of "EVTH"s encountered is (2n)? 
     NO: Is sta-file, go to 1.
     \item YES: Is cer-file. evth is filled with the contents of
     buffer till its end, and completed from the STDIN
     \item Does this event fullfil the criteria? NO: Go to 1
     \item YES: Open output file, and save evth.
     \item Read CORParticle blocks, till new "EVTH" is encountered.
     \item New "EVTH" corresponds to sta-file. Put CORParticle block 
     in output sta-file, get the rest of the sta-file from the STDIN,
     and put it in the output sta-file.
     \item Go to 1.
     \end{enumerate}

     @endcomment */

  //++
  //  START
  //--

  // parse command line options
  
  optarg = NULL;
  while ( !errflg && ((c = getopt(argc, argv, COMMAND_LINE_OPTIONS)) != -1) )
    switch (c) {
    case 'l':
      Elow = atof(optarg);
      break;
    case 'u':
      Eup = atof(optarg);
      break;
    case 'f':
      CerFile = atoi(optarg) - 1;
      break;
    case 'n':
      maxCerFile = atoi(optarg);
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
  
  // show help if error
  
  if ((errflg>0) || (argc<2))
    error("corfilter","\n\nusage:: corfilter %s%s\n\t%s%s%s\n\n",
          " -l <low.energy>",
          " -u <upp.energy>",
          " [ -f <no.cer.files> ]",
          " [ -n <max.no.cer.files> ]",
          " [ -v | -q ]");

  // make some sort of presentation
  
  if (verbose)
    present();

  // allocate memory for buffer

  if (verbose)
    log("corfilter", "Allocating memory for buffer...\n");

  buffer = new float [BUFFER_LENGTH];
  buffer2 = new float [BUFFER_LENGTH];
  evth = (COREventHeader*)buffer;

  // main loop
  
  while ( (!cin.eof()) && (CerFile < maxCerFile) ) {

    // read buffer from STDIN

    cin.read( (char*) buffer, BUFFER_LENGTH * sizeof(float) );
    bytes = cin.gcount();

    if (bytes==0)
      break;

    // check whether EVTH is there

    chkbuffer = (char*)buffer;
    if ( (result=strstr(chkbuffer,EVTH)) == 0) {
      if (verbose) 
        log("corfilter", "EVTH not found in this block\n");
      continue;
    }

    // show log
    
    if (verbose)
      log("corfilter", "Reading event %d... ",
          (int)evth->EvtNumber);
    
    // CRITERIA

    energy = evth->Etotal;
    
    if ( (energy<Elow) || (energy>Eup) ) {
      if (verbose)
        log("", "E = %8.1f GeV  REJECTED\n", energy);
      reject = TRUE;
    } else {
      if (verbose)
        log("", "E = %8.1f GeV  ACCEPTED\n", energy);
      reject = FALSE;
    }

    if ( !reject ) {

      // open new cer-file
      
      ++CerFile;
      sprintf(cername, "./cer%06d", CerFile);
      
      cerfile.open( cername );
      
      if (verbose)
        log("corfilter", "Writing event %d, will be %s\n",
            (int)evth->EvtNumber, cername);

      // write it
      
      cerfile.write( (char*)buffer, BUFFER_LENGTH * sizeof(float) );
      
    }

    // read blocks till new EVTH is found 
    // (now must be at the beginning of a block)

    while ( TRUE ) {
      
      // read photon

      cin.read( (char*)buffer, 7 * sizeof(float) );

      // is this the last photon?

      if ( strstr(chkbuffer, EVTH) != NULL )
        break;

      // if not, write it to the cer-file

      if ( !reject ) 
        cerfile.write( (char*)buffer, 7 * sizeof(float) );

    }

    if ( !reject ) {
      // close cer-file
      cerfile.close();
    }
    
    // at this point, we must be reading sta-file
    
    // read the rest of the sta-file

    cin.read( (char*)buffer + 7*sizeof(float), 5956 - 7*sizeof(float));
    
    if ( !reject ) {

      // log

      if (verbose)
        log("corfilter", "Saving sta-file...\n");
      
      // open file

      sprintf(staname, "./sta%06d", CerFile);
      
      stafile.open( staname );
      
      // write it
      
      stafile.write( (char*)buffer, 5956 );
      
      // close sta-file
      
      stafile.close();

    }

  }

  // program finished

  if ( verbose )
    log( "corfilter", "Done.\n");

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
       << "corfilter\n\n"
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

// EOF
