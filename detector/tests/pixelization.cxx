// This is -*- C++ -*-
// Test of the pixelization routine
//

#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>

#define __DEBUG__

#define LINE_MAX_LENGTH    5000

#define SIN60   0.866025403784439
#define COS60   0.500000000000000

#define RandomNumber  drand48()

#define PIX_ARRAY_SIDE       40
#define PIX_ARRAY_HALF_SIDE  20
#define PIXNUM               0
#define PIXX                 1
#define PIXY                 2

#define PIXELS_FILE "pixels.dat"
#define QE_FILE     "qe.dat"

static const float ct_PixelWidth = 3.0;
static const float ct_PixelWidth_corner_2_corner = ct_PixelWidth / COS60; 
static const float ct_PixelWidth_corner_2_corner_half =
ct_PixelWidth_corner_2_corner / 2; 

static const int ct_NPixels = 919;

static const float ct_Apot = ct_PixelWidth / 2;          
static const float ct_2Apot = 2 * ct_Apot;         

static float pixels[PIX_ARRAY_SIDE][PIX_ARRAY_SIDE][4];   
static float **pixary;  
static int **pixneig;   
static int *npixneig;   

static float ***QE;
static int pointsQE;
static float *QElambda;

inline float  sqr(float x) { return (x*x); }
inline int    sqr(int x) { return (x*x); }
inline double sqr(double x) { return (x*x); }

void log(const char *funct, char *fmt, ...);
void error(const char *funct, char *fmt, ...);
void read_pixels(void);
int  pixels_are_neig(int pix1, int pix2);

#define PROGRAM "pixelization"

int
main(int argc, char **argv) 
{

  float thetaCT, phiCT, xiCT; // parameters of a given shower
  float thetashw, phishw;     // parameters of a given shower
  float coreD, coreX, coreY;  // core position and distance
  float impactD;              // impact parameter
  float u, v, w;              // auxiliary variables
  float l1, m1, n1;           // auxiliary variables
  float l2, m2, n2;           // auxiliary variables
  float num, den;             // auxiliary variables

  int i, j, k, n, m;          // simple counters
  float x, y;                 // intermediate variables

  float cx, cy, ci, cj;       // coordinates in the XY and IJ systems
  int ici, icj, iici, iicj;   // coordinates in the IJ (integers)

  int nPMT;                   // number of pixel

  // START
  log( PROGRAM, "Test of the Pixelization process.\n");
  
  // read pixels data
  log( PROGRAM, "Read pixels file . . .\n");
  read_pixels();

  //+++
  // Pixelization
  //---

  // calculate ij-coordinates
        
  // We use a change of coordinate system, using the following 
  // matrix of change (m^-1) (this is taken from Mathematica output).
  /*
   * In[1]:= m={{1,cos60},{0,sin60}}; MatrixForm[m]
   *
   * Out[1]//MatrixForm= 1       cos60
   * 
   *                     0       sin60
   * 
   * In[2]:= inv=Inverse[m]; MatrixForm[inv]
   * 
   * Out[2]//MatrixForm=              cos60
   *                                -(-----)
   *                       1          sin60
   * 
   *                                    1
   *                                  -----
   *                       0          sin60
   * 
   */

  log( PROGRAM, "Start . . .\n");
  
  for ( cx=-10 ; cx<10.1 ; cx+=0.05 ) {
    
    log( PROGRAM, "Column %.1f . . .\n", cx);
    
    for ( cy=-10 ; cy<10.1 ; cy+=0.05 ) {
    
      // go to IJ-coordinate system
      ci = floor( (cx - cy*COS60/SIN60)/ ct_2Apot + 0.5 );
      cj = floor( (cy/SIN60) / ct_2Apot + 0.5 );
          
      ici = (int)(ci);
      icj = (int)(cj);

      iici = ici+PIX_ARRAY_HALF_SIDE;
      iicj = icj+PIX_ARRAY_HALF_SIDE;
        
      // is it outside the array?
      if ( (iici < 0) || (iici >= PIX_ARRAY_SIDE) ||
           (iicj < 0) || (iicj >= PIX_ARRAY_SIDE) ) {
        // go to beginning of loop, the photon is lost
        continue;
      }
        
      // put into pixel

      // obtain the pixel number for this photon
      nPMT = (int)pixels[ iici ][ iicj ][ PIXNUM ];
        
      // check if outside the camera
      if ( (nPMT < 0) || (nPMT >= ct_NPixels) ) {
        // go to beginning of loop, the photon is lost
        continue;
      }
          
#ifdef __DEBUG__  
      cerr << cx << ' ' << cy << ' '
           << ci << ' ' << cj << ' '
           << nPMT << endl;
#endif // __DEBUG__  

    }

  }
  
  log( PROGRAM, "Done.\n");
  
  return( 0 );
}

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

  perror(funct);

  abort();
}

void
read_pixels(void)
{
  ifstream pixfile, qefile;
  char line[LINE_MAX_LENGTH];
  int n, i, j;
  float x, y, qe;

  //------------------------------------------------------------
  // first, pixels' coordinates

  // try to open the file

  log("read_pixels", "Openning the file \"%s\" . . .\n", PIXELS_FILE);
  
  pixfile.open( PIXELS_FILE );
  
  // if it is wrong or does not exist, go away
  
  if ( ! pixfile.good() )
    error( "read_pixels", "Cannot open \"%s\". Exiting.\n", PIXELS_FILE);
  
  // initialize pixel numbers

  for ( i=0; i<PIX_ARRAY_SIDE; ++i ) 
    for ( j=0; j<PIX_ARRAY_SIDE; ++j ) 
      pixels[i][j][PIXNUM] = -1;

  pixary = new float* [2*ct_NPixels];
  for ( i=0; i<2*ct_NPixels; ++i ) 
    pixary[i] = new float[2];

  pixneig = new int* [ct_NPixels];
  for ( i=0; i<ct_NPixels; ++i ) {
    pixneig[i] = new int[6];
    for ( j=0; j<6; ++j ) 
      pixneig[i][j] = -1;
  }

  npixneig = new int[ct_NPixels];
  for ( i=0; i<ct_NPixels; ++i ) 
    npixneig[i] = 0;

  // read file

  log("read_pixels", "Reading data . . .\n");

  n=0;
  while ( (! pixfile.eof()) && (n<ct_NPixels) ) {          

    // get line from the file

    pixfile.getline(line, LINE_MAX_LENGTH);

    // skip if comment

    if ( *line == '#' )
      continue;

    // get the value (dx, dy)

    sscanf(line, "%d %d %d %f %f", &n, &i, &j, &x, &y);

    // WARNING!! The pixel coordinates are normalized to a
    // hypothetical camera with Pixel Diameter = 1
    // In order to use the right coordinates, we must multiply them
    // by ct_PixelWidth

    x *= ct_PixelWidth;
    y *= ct_PixelWidth;

    // save the values to the array

    pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXNUM] = n-1;
    pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXX] = x;
    pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE][PIXY] = y;
   
    pixary[n-1][0] = x;
    pixary[n-1][1] = y;

  }

  // close file
  
  pixfile.close();

  // calculate tables of neighbours
  
#ifdef __DEBUG__
  for ( n=0 ; n<ct_NPixels ; ++n ) {
    cout << "Para el pixel " << n << ": ";	
    for ( i=n+1 ; (i<ct_NPixels)&&(npixneig[n]<6) ; ++i) {
      if ( pixels_are_neig(n,i) == TRUE ) {
        pixneig[n][npixneig[n]] = i;
        pixneig[i][npixneig[i]] = n;
        cout << i << ' ';
        ++npixneig[n];
        ++npixneig[i];
      }
    }
    cout << endl << flush;
  }
#else // ! __DEBUG__
  for ( n=0 ; n<ct_NPixels ; ++n ) 
    for ( i=n+1 ; (i<ct_NPixels)&&(npixneig[n]<6) ; ++i) 
      if ( pixels_are_neig(n,i) == TRUE ) {
        pixneig[n][npixneig[n]] = i;
        pixneig[i][npixneig[i]] = n;
        ++npixneig[n];
        ++npixneig[i];
      }
#endif // ! __DEBUG__
  
#ifdef __DEBUG__
  for ( n=0 ; n<ct_NPixels ; ++n ) {
    cout << n << ':';
    for ( j=0; j<npixneig[n]; ++j) 
      cout << ' ' << pixneig[n][j];
    cout << endl << flush;
  }
#endif // __DEBUG__  

  //------------------------------------------------------------
  // second, pixels' QE

  // try to open the file

  log("read_pixels", "Openning the file \"%s\" . . .\n", QE_FILE);
  
  qefile.open( QE_FILE );
  
  // if it is wrong or does not exist, go away
  
  if ( ! qefile.good() )
    error( "read_pixels", "Cannot open \"%s\". Exiting.\n", QE_FILE );
  
  // read file

  log("read_pixels", "Reading data . . .\n");

  i=-1;

  while ( ! qefile.eof() ) {          

    // get line from the file

    qefile.getline(line, LINE_MAX_LENGTH);

    // skip if comment

    if ( *line == '#' )
      continue;

    // if it is the first valid value, it is the number of QE data points

    if ( i < 0 ) {

      // get the number of datapoints 

      sscanf(line, "%d", &pointsQE);
      
      // allocate memory for the table of QEs
      
      QE = new float ** [ct_NPixels];

      for ( i=0; i<ct_NPixels; ++i ) {
        QE[i] = new float * [2];
        QE[i][0] = new float[pointsQE];
        QE[i][1] = new float[pointsQE];
      }
      
      QElambda = new float [pointsQE];

      for ( i=0; i<pointsQE; ++i ) {
        qefile.getline(line, LINE_MAX_LENGTH);
        sscanf(line, "%f", &QElambda[i]);
      }

      i=0;

      continue;
    }

    // get the values (num-pixel, num-datapoint, QE-value)
    
    sscanf(line, "%d %d %f", &i, &j, &qe);

    if ( ((i-1) < ct_NPixels) && ((i-1) > -1) &&
         ((j-1) < pointsQE)   && ((j-1) > -1) ) {
      QE[i-1][0][j-1] = QElambda[j-1];
      QE[i-1][1][j-1] = qe;
    }

  }

  // close file

  qefile.close();

  // end

  log("read_pixels", "Done.\n");

}

int
pixels_are_neig(int pix1, int pix2)
{ 
  if ( sqrt(sqr( pixary[pix1][0] - pixary[pix2][0] ) +
            sqr( pixary[pix1][1] - pixary[pix2][1] ) ) 
       > ct_PixelWidth_corner_2_corner ) 
    return ( FALSE );
  else
    return ( TRUE );
}

//=EOF
