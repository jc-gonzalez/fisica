//////////////////////////////////////////////////////////////////
//
// afs                  
//
// @file        afs.cxx
// @title       Afs statistics plot
// @desc        Afs statistics plot
// @author      J C Gonz\'alez
// @email       gonzalez@mppmu.mpg.de
//
// @maintitle
//_______________________________________________________________
//
//  Created: Thu Oct 15 17:09:04 MET DST 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Afs statistics plot
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

// @section Source code of {\tt afs.cxx}

/* @text
In this section we show the (commented) code of this program.
@endtext */

//@code
//////////////////////////////////////////////////////////////////
// afs                  
//_______________________________________________________________
//
//  Created: Thu Oct 15 17:09:04 MET DST 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Afs plots
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
 
#include "afs.h"

// @comment
// This is a flag to change the {\it verboseness\/} of the output
// @endcomment

// verbose or not
int verbose = FALSE;

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

  float        **x, *y, **xcluster;  // data
  float        *a, *b, num, den;
  float        distance, mindistance;
  float        radius, sigma, sigma2;
  int            nmindistance;
  char           infilename[40]; 
  ifstream       inf;
  int            npts=0, ninputs=1, ntrain;
  int            i, j, k, l, m, n;

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
    case 'p':
      ninputs = atoi( optarg );
      break;
    case 't':
      ntrain = atoi( optarg );
      break;
    case 'r':
      radius = atof( optarg );
      break;
    case 's':
      sigma = atof( optarg );
      sigma2 = SQR(sigma);
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
    log("afs", "Npts=%d Dim=%d File=%s r=%f s=%f\n",
        npts, ninputs, infilename, radius, sigma);
  if (verbose)
    log("afs", "Start . . .\n");

  // get data
  
  x = new float * [ninputs];
  for ( j=0; j<ninputs; ++j )
    x[j] = new float[npts];
  y = new float[npts];

  inf.open( infilename );

  for ( i=0; i<npts; ++i ) {
    for ( j=0; j<ninputs; ++j )
      inf >> x[j][i];
    inf >> y[i] ;
  }

  inf.close();
  
  // train the system

  // maximum number of clusters == number of points
  xcluster = new float*[npts];
  for ( j=0; j<ninputs; ++j )
    xcluster[j] = new float[npts];

  // first point defines first cluster
  m = 1; // number of clusters
  a = new float[npts];
  b = new float[npts];

  for ( j=0; j<ninputs; ++j )
    xcluster[j][0] = x[j][0];
  
  a[0] = y[0];
  b[0] = 1.0;

  // main loop for training
  for (i=1; i<ntrain; ++i) {

    // compute distance from point i to each cluster
    for (n=0; n<m; ++n) {

      distance = 0.;
      mindistance = 2.*radius;
      nmindistance = -1;

      for ( j=0; j<ninputs; ++j )
        distance += SQR(xcluster[j][n] - x[j][i]);
  
      distance = sqrt(distance);

      if ( distance > radius )
        continue;

      if ( distance < mindistance ) {
        mindistance = distance;
        nmindistance = n;
      }

    }

    if ( nmindistance == -1 ) {

      // if there's no nearby cluster, this point makes a new cluster

      for ( j=0; j<ninputs; ++j )
        xcluster[j][m] = x[j][m];
      
      a[m] = y[i];
      b[m] = 1.0;
      
      ++m;

      cerr << '.';

    } else {

      a[nmindistance] += y[i];
      b[nmindistance] += 1.0;
      
    }
    
  }
  
  // with this trained system, try to reproduce the output values

  // do it for each point
  for (i=0; i<npts; ++i) {

    num = den = 0.0;

    // for each cluster
    for (n=0; n<m; ++n) {
  
      // calculate distance from this point to the center of the cluster
      distance = 0.;
      for ( j=0; j<ninputs; ++j )
        distance += SQR(xcluster[j][n] - x[j][i]);
  
      num += a[n]*exp(-distance/sigma2);
      den += b[n]*exp(-distance/sigma2);

    }

    if ( den != 0 )
      cout << y[i] << ' ' << num/den << '\n';
    else 
      cout << y[i] << ' ' << 0.0 << '\n';

  }

  // program finished

  if ( verbose ) 
    log( "afs", "%d clusters used.\n", m);

  if ( verbose ) 
    log( "afs", "Done.\n");

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
  cerr << "##################################################\n"
       << "afs\n\n"
       << "Adaptive Fuzzy System (tests I)\n"
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
  vfprintf(stderr, fmt, args);
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
       << " -n <num.points> -f <filename> -p <dimen> -r <radius> -s <sigma>"
       << " [-v | -q] [-h] "
       << endl << endl << flush;
  exit(0);
}
// @endcode

//EOF
