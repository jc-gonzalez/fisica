//////////////////////////////////////////////////////////////////
//
// allan                  
//
// @file        allan.cxx
// @title       Allan statistics plot
// @desc        Allan statistics plot
// @author      J C Gonz\'alez
// @email       gonzalez@mppmu.mpg.de
//
// @maintitle
//_______________________________________________________________
//
//  Created: Thu Oct 15 17:09:04 MET DST 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Allan statistics plot
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

// @section Source code of {\tt allan.cxx}

/* @text
In this section we show the (commented) code of this program.
@endtext */

//@code
//////////////////////////////////////////////////////////////////
// allan                  
//_______________________________________________________________
//
//  Created: Thu Oct 15 17:09:04 MET DST 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Allan plots
//  Notes:   
//_______________________________________________________________
// $RCSfile$
// $Revision$
// $Author$ 
// $Date$
//////////////////////////////////////////////////////////////////
// @endcode

// @subsection Includes and Global variables definition

// @code

// @comment 
// All the defines are located in the file {\tt reflector.h}.
// @endcomment
 
#include "allan.h"

// @comment
// This is a flag to change the {\it verboseness\/} of the output
// @endcomment

// verbose or not
int verbose = FALSE;

#define Float_t  double

// @endcode

// @subsection Main program

/* @text
@endtext */

// @code
//++++++++++++++++++++++++++++++++++++++++
// MAIN PROGRAM 
//----------------------------------------
            
int
main(int argc, char **argv) 
{

  int c, errflg = 0;          // used by getopt

  Float_t        *x, *y;  // data
  Float_t        *allan, *delta;
  Float_t        sdelta, sdelta2;
  Float_t        adelta, adelta2;
  Float_t        x1, x2, y1, y2;
  char         infilename[40]; 
  ifstream     inf;
  int          npts=0, nlevels;
  int          i, j, k, l, m, n;

  //++
  //  START
  //--

  // parse command line options
  
  optarg = NULL;
  while ( !errflg && ((c = getopt(argc, argv, COMMAND_LINE_OPTIONS)) != -1) )
    switch (c) {
    case 'v':
      verbose = TRUE;
      break;
    case 'q':
      verbose = FALSE;
      break;
    case 'f':
      strcpy( infilename, optarg );
      break;
    case 'n':
      npts = atoi( optarg );
      break;
    case 'h':
      usage(argv[0]);
      break;
    default :
      errflg++;
    }
  
  if ((errflg) || (npts < 1))
    usage(argv[0]);

  // make some sort of presentation
  
  if (verbose)
    present();

  // START!!

  if (verbose)
    log("allan", "Start . . .\n");

  // get data
  
  x = new Float_t[npts];
  y = new Float_t[npts];

  inf.open( infilename );

  for ( i=0; i<npts; ++i )
    inf >> x[i] >> y[i];

  inf.close();

  // calculate number of levels

  nlevels = npts / 4;

  // create output data

  allan = new Float_t[nlevels];

  for ( k=1; k<=nlevels; ++k ) {

    n = npts/(k+1);
    delta = new Float_t[n];
    sdelta = sdelta2 = 0.0;

    for ( i=0, m=0; i<npts; i+=2*k, ++m ) {

      if ( (i + (k-1) + k) > npts ) 
        break;
      
      x1 = x2 = y1 = y2 = 0.0;
    
      for ( j=0; j<k; ++j) {
        x1 += x[i+j];
        y1 += y[i+j];
//         if (k>4)
//           cout << "@1" << ' ' << i+j << endl;
      }
      
      for ( j=0; j<k; ++j) {
        x2 += x[i+j+k];
        y2 += y[i+j+k];
//         if (k>4)
//           cout << "@2" << ' ' << i+j+k << endl;
      }
      
      delta[m] = (y1-y2)/(x1-x2);
      sdelta += delta[m];
      sdelta2 += SQR(delta[m]);

//       if (k>4)
//         cout << '@' << m << ' '
//              << x1 << ' ' << x2 << ' '
//              << y1 << ' ' << y2 << ' ' 
//              << endl << flush;
      
    }

    delete delta;
    delta = NULL;

    adelta = sdelta / (Float_t)m;    
    adelta2 = sdelta2 / (Float_t)m;
    
    allan[k] = adelta2 - SQR(adelta);

    if ( verbose )
      cout << k << ' ' 
           << setw(20) << setprecision(10) << setiosflags(ios::scientific)
           << allan[k] 
           << endl << flush;

  }

  // program finished

  if ( verbose )
    log( "allan", "Done.\n");

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
       << "allan\n\n"
       << "Allan statistics\n"
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
// @name usage                                                    
//                                                           
// @desc show how to call this program
//
// @date Sat Jun 27 05:58:56 MET DST 1998
// @function @code 
//------------------------------------------------------------
// usage                        
//                              
// show how to call this program
//------------------------------------------------------------

void
usage(char *cmd)
{
  cout << "\nusage::\n\n"
       << cmd 
       << " -n <num.points> -f <filename> [-v | -q] [-h] "
       << endl << endl << flush;
  exit(0);
}
// @endcode

//EOF
