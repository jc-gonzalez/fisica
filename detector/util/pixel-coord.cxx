//////////////////////////////////////////////////////////////////
//
// Pixel coordinates calculation
//
// @file        pixel-coord.cxx
// @title       Calculation of pixel coordinates in bi-axis hexagonal frame
// @author      J C Gonz\'alez
// @email       gonzalez@mppmu.mpg.de
//
// @maintitle
//_______________________________________________________________
//
//
//  Created: Tue Feb  1 10:43:54 MET 2000
//  Author:  Jose Carlos Gonzales
//  Purpose: Calculate pixel coordinates in the tri-axis hexagonal
//           frame (see definition below)
//    
//////////////////////////////////////////////////////////////////

// @T \newpage

//# Comments on {\tt pixel-coord.cxx}.

/*"
  In this section we show the (commented) code of the program for
  the pre-calculation of the coordinates of the pixels in the camera,
  in a tri-axis hexagonal frame, defined below. This definition of
  hexagonal coordinates is taken from Charles Fu, who proposed these
  coordinates long ago for hexagonal grids on the plane. Thanks.
**/

//## Definition of the tri-axis hexagonal frame.

/*"
  In the figure \ref{fig:pixels} is shown the definition of the
  tri-axis frame used for the numbering of the pixels. The unit in
  each of the three axes is $1.5\times$Corner-to-Corner/2 of a pixel
  with edge-to-edge width of 1.0. With this definition the coordinates
  of the pixels are, for example for the first seven pixels:
  1:(0,0,0), 2:(1,-1,0), 3:(1,0,-1), 4:(0,1,-1), 5:(-1,1,0),
  6:(-1,0,1), 7:(0,-1,1).

  Then, with this definitions, the formula for which hexagon a point
  belongs is then:
  
  \begin{equation}
  \vec{\mathcal{R}} \left\{ \vec{\mathcal{R}}(\vec r) -
  \left[ 0.5 - \underset{i}{\max} |\mathcal{R}(r_i) - r_i | \right]
  \underset{i}{\sum} \mathcal{R}(r_i) \right\}
  \end{equation}
  
  where $\vec r$ is your position, $\mathcal{R}$ is a rounding function,
  and the top arrow indicates a vector.
  
  \begin{figure}[htbp]
  \begin{center}
  \includegraphics[width=0.8\textwidth]{pixels.eps}
  \caption{Tri-axis hexagonal coordinate system 
  used in the pixel numbering}
  \label{fig:pixels}
  \end{center}
  \end{figure}

  With this definition the the axis, we pre-calculate the
  $x'y'z'$-coordinates, in order to use them afterwards. Only two of
  these coordinates are independent, having always $x'+y'+z'=0$.  Note
  also that the algorithm is symmetric in x',y',z' since the axes are
  symmetric.  These two facts make algorithms more intuitive and bugs
  much easier to spot.  Similarity in instructions may also improve
  chances for compiler or hand optimization.  If desired, z can be
  computed whenever needed to reduce the memory usage by a few measly
  bytes.
  
  Now, two of the coordinates (only two are independent, $x'y'$ for
  instance) are used to identify each pixel in an array, which has in
  their cells the number of pixel to which those coordinates belong.
  
  How was the above formula derived?  Quoting to Charles Fu:
  
  \begin{quotation}
  The insight is that a uniform hex grid is the isometric projection of
  an infinite grid of unit cubes whose centers satisfy the equation
  x'+y'+z'=0.  Thus, hairy problems with hexes in 2-D become nicer
  problems with cubes in 3-D in very much the same way that using
  homogenous coordinates linearizes projective transformations.
  
  (An isometric projection is an orthographic, i.e., non-perspective,
  projection onto the x'+y'+z'=0 plane.  It is one of the standard views
  used by draftsmen: the one with x, y, and z 120 degrees from each
  other, just like my axes.)
  
  In this particular case, the problem of determining which hex contains
  a given point becomes the trivial problem of which cube contains a
  point.  The rest of the code just transforms from the x+y+z=0 plane to
  the cube grid and vice versa.  That's it.  With the cube grid system,
  problems like counting the number of hexes between points also becomes
  trivial.  The system also has interesting bizarre properties such as
  lines of constant x zigzagging to follow hex sides as shown in the
  diagram above.  If you want a Euclidean metric, just stick with the
  rotated coordinate system and avoid the fancy projection except when
  discrete hexes are needed.
  \end{quotation}
  
  Following is the code, which uses the function \textrm{hexcoord} to
  calculate the coordinates of the pixel to which a point $(x',y')$
  belongs.  Since the coordinates we are using do not correspond
  directly to the new coordinates, a corversion must be done.
  
  This program, however, does not have {\it prior} information of the
  position of the center the pixels. It gets as an input only the number
  of rings desired.  So, the program has to calculate in addition the
  coordinates of the centers of the pixels. For this purpose, yet
  another different coordinate system is used, the one shown in
  \ref{fig:pixels_pre}. The units of the axes X and Y are millimeters,
  and the units of the axes I and J are both two times the apotheme of
  one pixel ($2 \times a$).
  
  \begin{figure}[htbp]
  \begin{center}
  \includegraphics[width=0.8\textwidth]{pixels_pre.eps}
  \caption{Bi-axis hexagonal coordinate system 
  used in the pixel numbering}
  \label{fig:pixels_pre}
  \end{center}
  \end{figure}
  
  With this definition the the axis, we pre-calculate the
  $ij$-coordinates, in order to use them for calculating the
  $xy$-coordinates.
  
  In figure \ref{fig:pixnums} we show the numbering of the
  pixels in the IJ-plane.
  
  \begin{figure}[htbp]
  \begin{center}
  \includegraphics[width=0.8\textwidth]{pixnums.eps}
  \caption{Numbers of the pixels, in the IJ-plane}
  \label{fig:pixnums}
  \end{center}
  \end{figure}
  
  In order to calculate the coordinates, we use the change of
  system described here. Then, we will use simply the matrix
  of change from one system to the other. In our case, this is:
  
  \begin{equation}
  \begin{bmatrix}X\\Y\\\end{bmatrix}                                
  =
  \begin{bmatrix}
  1 & \cos(60^\circ)\\
  0 & \sin(60^\circ)\\
  \end{bmatrix}                                
  \begin{bmatrix}I\\J\\\end{bmatrix}                                
  \end{equation}
  and hence
  \begin{equation}
  \begin{bmatrix}I\\J\\\end{bmatrix}                                
  =
  \begin{bmatrix}    
  1 & -\frac{\cos(60^\circ)}{\sin(60^\circ)}\\
  0 &\frac{1}{\sin(60^\circ)}\\
  \end{bmatrix}                                
  \begin{bmatrix}X\\Y\\\end{bmatrix}                                
  \end{equation}
  
  Needless to say, this intermediate coordinate system will be removed,
  and the whole algorithm for the pre-calculation of the
  centers of the pixels re-implemented.

**/

//# The code.

//## Includes and macro definitions

//{
#include <iostream.h>
#include <fstream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define SIN60   0.866025403784439
#define COS60   0.500000000000000
#define SIN30   COS60
#define COS30   SIN60

/*'
{\bf Warning!}: The coordinates x,y of the center of the pixels
are normalized to a pixel width = 1. They must be multiplied
(afterwards, they are in {\tt camera.cxx} bye the actual
pixel width, stored in the variable ct_PixelWidth.
This is similar to setting in this program the following constant 
to {\tt ct_PixelWidth = 3.0} for MAGIC and {\tt ct_PixelWidth = 2.1}
for CT1.
'*/

static const float ct_PixelWidth = 1.0;
static const float ct_PixelWidth_corner_2_corner = ct_PixelWidth / COS30; 
static const float ct_PixelWidth_corner_2_corner_half =
  ct_PixelWidth_corner_2_corner / 2; 
static const float ct_Apot = ct_PixelWidth / 2;          
static const float ct_2Apot = 2 * ct_Apot;         

template<class T>
T sqr(T x) { return (x*x); }

inline int    rint(float x) { return int(floor(x + 0.5)); }
inline float frint(float x) { return     floor(x + 0.5);  }

void hexcoord (float cx, float cy, int &ki, int &kj, int &kk);

//}

//## Implementation.

//{
int main(int argc, char **argv)
{

  int ring; // number of the outermost ring
  int r;    // variable with the current ring number
  int px[1000], py[1000]; // coordinates of the pixels
  int i, nx, signo, ii, npixels;
  int npix;
  float x, y;
  int ki, kj, kk;

  if ( argc<2 ) {
    cout << "usage :: " << argv[0] << " <number-of-rings> \n" << flush;
    exit(1);
  } 

  ring = atoi(argv[1]);

  npix = 1;

  cout << npix << ' ' << 0 << ' ' << 0 << ' ' << 0 << ' ' << 0 << endl;

  for ( r=0; r<ring; ++r ) {
    
    npixels = 6*r;

    // set X coordinates

    nx = r;
    signo = -1;

    for ( i=0; i<npixels; ++i ) {
      
      px[i] = nx;
      // cout << i << ' ' << px[i] << ' ' << signo << endl;

      if ( ( (nx==r) && (signo==+1) ) || ( (nx==-r) && (signo==-1) ) ) {
        for ( ii=1; (ii<=r) && ((i+ii)<npixels); ++ii ){
          px[i+ii] = nx;
          // cout << i+ii << ' ' << px[i+ii] << ' ' << signo << endl;
        }
        i+=r;
        signo = -signo;
      }

      nx += signo;

    }

    // set Y coordinates

    nx = 0;
    signo = +1;

    for ( i=0; i<npixels; ++i ) {
      
      py[i] = nx;
      // cout << i << ' ' << py[i] << ' ' << signo << endl;

      if ( ( (nx==r) && (signo==+1) ) || ( (nx==-r) && (signo==-1) ) ) {
        for ( ii=1; (ii<=r) && ((i+ii)<npixels); ++ii ){
          py[i+ii] = nx;
          // cout << i+ii << ' ' << py[i+ii] << ' ' << signo << endl;
        }
        i+=r;
        signo = -signo;
      }

      nx += signo;

    }

    // show coordinates

    // calculate x'y'z'-coordinates
    
    // We use a change of coordinate system, using the following 
    // matrix of change (m^-1)
    /*
    
      In[1]:= m={{1,cos60},{0,sin60}};
      
      In[2]:= MatrixForm[m]
      
      Out[2]//MatrixForm= 1       cos60
      
                          0       sin60
      
      In[3]:= inv=Inverse[m];
      
      In[4]:= MatrixForm[inv]
      
      Out[4]//MatrixForm=              cos60
                                     -(-----)
                          1            sin60
      
                                       1
                                     -----
                          0          sin60
      
    */
    
    for ( i=0; i<npixels; ++i ) {
      
      ++npix;

      x = (px[i]+py[i]*COS60) * ct_PixelWidth;
      y = (py[i]*SIN60) * ct_PixelWidth;

      hexcoord(x, y, ki, kj, kk);

      cout << npix << ' ' 
           << ki << ' ' << kj << ' ' << kk << ' ' 
           << x << ' ' << y << endl;
    }

  }

  return 0;
}

static void hexcoord (float cx, float cy, int &ki, int &kj, int &kk)
{
  float x, y;                 // intermediate variables
  float dx, dy, dz;
  float rx, ry, rz;
  float ax, ay, az;
  int   s;
  static const float unit = 1.5*ct_PixelWidth_corner_2_corner_half;

  x = (  cx*COS30 + cy*SIN30);
  y = (- cx*SIN30 + cy*COS30);
  
  dx = x / unit;
  dy = (- x*SIN30 + y*COS30) / unit;
  dz = (- x*SIN30 - y*COS30) / unit;
  
  rx = frint(dx);
  ry = frint(dy);
  rz = frint(dz);
  
  ki = int(rx);
  kj = int(ry);
  kk = int(rz);
  
  s = ki + kj + kk;
  if (s) {
    ax = fabs(rx-dx);
    ay = fabs(ry-dy);
    az = fabs(rz-dz);
    
    if ((ax >= ay) && (ax >= az)) { ki -= s; }
    else if ((ay >= ax) && (ay >= az)) { kj -= s; }
    else { kk -= s; }
  }
  
  return;
}
//}
