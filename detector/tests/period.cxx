//////////////////////////////////////////////////////////////////
//
// period                  
//
// @file        period.cxx
// @title       Period statistics plot
// @desc        Period statistics plot
// @author      J C Gonz\'alez
// @email       gonzalez@mppmu.mpg.de
//
// @maintitle
//_______________________________________________________________
//
//  Created: Thu Oct 15 17:09:04 MET DST 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Period statistics plot
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

// @section Source code of {\tt period.cxx}

/* @text
In this section we show the (commented) code of this program.
@endtext */

//@code
//////////////////////////////////////////////////////////////////
// period                  
//_______________________________________________________________
//
//  Created: Thu Oct 15 17:09:04 MET DST 1998
//  Author:  Jose Carlos Gonzales
//  Purpose: Period plots
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
 
#include "period.h"

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

  char         infilename[40]; 
  ifstream     inf;
  int          npts=0;
  int          i, j, k, l, m, n;
  int          nskip=0;
  
  float tan2wtau;
  float sinsum;
  float cossum;
  float tau;

  float wa, wb, w, iw;
  int nw;
  float tt;
  float *t, *x, xmean, sigma2;
  float s1, s2, s3, s4, c, s;
  float p, pn;
  float spws, spwc;

  float lf, nf;

  char line[80];
  
  //++
  //  START
  //--

  // parse command line options
  
  extern char *optarg;
  extern int optind;
  extern int opterr;
  extern int optopt;
  int ch, errflg = 0;    

  optarg = NULL;
  while ( !errflg && ((ch = getopt(argc, argv, COMMAND_LINE_OPTIONS)) != -1) )
    switch (ch) {
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
    case 's':
      nskip = atoi( optarg );
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
  
  if ( verbose )
    present();

  //= START!!

  if ( verbose )
    log("period", "Start . . .\n");

  //= let's get the data ==========

  // we define the vector data
  t = new float[npts];
  x = new float[npts];

  // open file
  inf.open( infilename );

  // skip header
  for ( i=0; i<nskip; ++i ) 
    inf.getline(line, 80);

  // get data
  for ( i=0; i<npts; ++i ) 
    inf >> t[i] >> x[i];
  
  // done
  inf.close();

  if ( verbose )
    log("period", "Read %d data points\n", npts);

  // calculate mean and variance
  xmean = 0;
  for ( i=0; i<npts; ++i ) 
    xmean += x[i];
  xmean /= npts;

  sigma2 = 0.0;
  for ( i=0; i<npts; ++i ) 
    sigma2 += SQR(x[i] - xmean);
  sigma2 /= npts;
  
  // let's substract the mean
  for ( i=0; i<npts; ++i ) 
    x[i] -= xmean;

  if ( verbose )
    log("period", "<X> = %g    Var(X) = %g\n", xmean, sigma2);

  //= loop over frequencies ==========

  tt = t[npts-1] - t[0];

  /*
    // minimum frequency
    wa = 2 * M_PI / tt;
    
    // maximum (Nyquist) frequency
    wb = npts * M_PI / tt;
    
    // number of independent frequencies (ups!)
    //nw = (int)((-6.362 + 1.193*npts + 0.00098*npts*npts));
    //nw = npts - 1;
    
    // increment in frequencies
    //iw = (wb - wa) / nw;
    iw = 1.0 / tt;
    nw = (int) ((wb - wa) / iw);
  */

  // l = total data span, in days
  l = (int) (tt * ((24.0 * 3600.0) / 90.0));
  lf = (float) l;
  nf = (float) n;
  
  // minimum frequency
  wa = (2 * M_PI * (lf - 1.)) / (lf * (nf - 1.));

  // maximum (Nyquist) frequency
  wb = (2 * M_PI * (lf - 1.) * (nf / 2.)) / (lf * (nf - 1.));

  // number of independent frequencies (ups!)
  nw = npts/2;

  // increment in frequencies
  iw = (wb - wa) / nw;

  if ( verbose ) {
    log("period", "Total data span (L): %d\n", l);
    log("period", "Period of time scanned: %g (%g:%g)\n",
        tt, t[0], t[npts-1]);
    log("period", "Minimum frequency scanned: %g\n", wa);
    log("period", "Maximum frequency scanned: %g\n", wb);
    log("period", "Estimated number of independent frequencies: %d\n", nw);
  }

  // this is the loop
  for ( w = wa, j=0 ; 
        j < nw; 
        w += iw, j++ ) {

    tan2wtau = 0.0;
    sinsum = 0.0;
    cossum = 0.0;
    
    //= calculate tau =====

    for ( i=0; i<npts; ++i ) 
      sinsum += sin( 2 * w * t[i] );

    for ( i=0; i<npts; ++i ) 
      cossum += cos( 2 * w * t[i] );
      
    tan2wtau = sinsum / cossum;

    tau = atan(tan2wtau) / (2.0 * w);

    //= periodogram value =====

    // init. variables
    // p(w) = 1/2 (s1^2/s2 + s3^2/s4)
    s1 = s2 = s3 = s4 = 0.0;

    spws = spwc = 0.0;
    
    // loop over data
    for ( i=0; i<npts; ++i ) {
      c = cos(w*(t[i] - tau));
      s = sin(w*(t[i] - tau));
      s1 += x[i] * c;
      s2 += c * c;
      s3 += x[i] * s;
      s4 += s * s;
      spwc += cos(w*t[i]);
      spws += sin(w*t[i]);
    }

    spwc /= npts;
    spws /= npts;
    
    p = 0.5*(s1*s1/s2 + s3*s3/s4);

    pn = p / sigma2;

    cout << w   << ' ' 
         << tau << ' ' 
         << p   << ' ' 
         << pn  << ' ' 
         << spwc << ' ' 
         << spws << ' ' 
         << endl;

    if ( verbose )
      log("period", "(%d/%d) w = %g   tau = %g   P = %g   Pn = %g\n", 
          j, nw, w, tau, p, pn);

  }

  // program finished

  if ( verbose )
    log( "period", "Done.\n");

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
       << "period\n\n"
       << "Periodogram generation (tests)\n"
       << "J C Gonzalez, April 1999\n"
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
  fprintf(stderr, "[%s]: ", funct);
  
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
  cerr << "\nusage::\n\n"
       << cmd 
       << " -n <num.points> -f <filename> [-v | -q] [-h] "
       << endl << endl << flush;
  exit(0);
}
// @endcode

//EOF
