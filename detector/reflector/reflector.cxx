//!/////////////////////////////////////////////////////////////////////
//
// reflector                  
//
// @file        reflector.cxx
// @title       Simulation of the Reflector
// @desc        Program for the simulation of CT1 and MAGIC reflectors
// @author      J C Gonzalez
// @email       gonzalez@mppmu.mpg.de
// @date        Thu May  7 16:24:22 1998
//
//----------------------------------------------------------------------
//
// Created: Thu May  7 16:24:22 1998
// Author:  Jose Carlos Gonzalez
// Purpose: Program for reflector simulation
// Notes:   
//
//----------------------------------------------------------------------
//
// $RCSfile$
// $Revision$
// $Author$ 
// $Date$
//
////////////////////////////////////////////////////////////////////////
// @tableofcontents @coverpage

//= Below you can find a sort of, incomplete for sure, documentation 
//= concerning this program. Please, do not hesiate to contact the 
//= author in case of problems.

//=---------------------------------------------------------------------
//!@section Details of the simulation of the detectors MAGIC and CT1.

//=---------------------------------------------------------------------
//!@subsection Overview of the simulation.

/*!@"

  This short section gives an idea about how the simulation
  developes itself. The details on each step will be explained
  later. At the beginning we have the Generated output file.
  
  @itemize
  
  @- B@Atmospheric Absorption@: A detailed description of the
  atmospheric absorption is provided. This description takes into
  account the effects of the Rayleigh scattering, the aerosol (Mie)
  absorption, and the effect of the Ozone. It works for the wavelength
  range @$290 \div 600@$ nm.
  
  @- B@Reflection in the mirrors@: Each remaining photon is traced
  till the mirrors, and then reflected till the camera plane. The
  steps are the following:
  
  @enumerate
    
  @- Calculation of the mirror where the photon is
  hitting.
    
  @- We apply the E@reflectivity@ effect, using a continuous random
  number in the range [0:1] : the photon can be lost.
    
  @- If the photon is hitting in the E@black spot@ of the center,
  the photon is also lost.
    
  @- We take the E@mathematicaly exact@ normal of the mirror, and
  apply the E@axis-deviation@ effect: the normal changes a little
  bit.

  @- We apply the E@imperfections@ in the mirrors (measurements are
  needed).

  @- Finally, with the displaced normal, we calculate the position of
  the reflected photon in the camera plane. Here, even with an exact,
  mathematical ray-tracing the image is E@blurred@, due to the
  intrinsic behaviour of the telescope optics.

  @endenumerate

  At this step we have a B@RAW-Monte-Carlo@ output file.

  @- B@Collection in the PMTs/IPCs@: The collection in the active
  devices follows the next steps:

  @enumerate
  
  @- We apply a certain probability of transmision of the photon in
  the E@plexiglas@, dependent on the incident angle of the photon in
  the camera (measurements are needed).

  @- We apply the transmitance factor of the E@light guides@.

  \item Then we apply the E@Quantum efficiency@ of the PMT,
  depending on the incident angle.

  @endenumerate

  @- B@Correction of the obtained signal@ In every pixel we have a
  certain number of Cherenkov photons, what we take as a B@mean value@
  of the measured charge in that pixel. Together to that value, we
  take into account the E@Night Sky Background@ (NSB): we have for
  this a mean value measured, and with this mean value we calculate
  the contribution of the sky for each pixel.
  
  At this level we will have a B@RAW-Data@ output file.
  
  @enditemize

  @"*/

//=---------------------------------------------------------------------
//!@subsection Simulation of the detector.

//=---------------------------------------------------------------------
//!@subsubsection Coordinate systems.

/*!@"
  
  The different angles used in the simulation of the detector are
  shown in the next figures. The telescope (CT) is pointing to the
  direction @$(\theta,\phi)@$, while the particle (background) is coming
  from the direction @$(\theta_p,\phi_p)@$. This direction is calculated
  using a pair of random generated angles @$\theta_s@$ and @$\phi_s@$, and
  spherical trigonometry.

  @F
  \begin{figure}[htbp]
  \begin{center}
  \includegraphics[height=0.45\textheight]{coordinates.eps}
  \caption{Coordinate system used in the reflector simulation}
  \label{fig:coord}
  \end{center}
  \end{figure}
  @F

  @F
  \stepcounter{figure}
  \begin{figure}[htbp]
  \begin{center}
  \includegraphics[height=0.45\textheight]{reflector.eps}
  \caption{Angles used in the reflector simulation}
  \label{fig:reflector}
  \end{center}
  \end{figure}
  @F
  
  The angles are defined in the next table. Using this definitions, we
  can start to explain how the reflector program works.

  @F
  \begin{table}
  \begin{tabular}[hbt]{cl}
  $\theta$   & Zenith angle of the position where the CT points to\\
  $\phi$     & Horizontal angle from the North of the position\\
  & where the CT points to\\
  $\theta_s$ & Angular distance between the axis of the\\
  & incoming shower and the direction where the CT is \\
  & pointing to.\\
  $\phi_s$   & Polar angle with axis equal to the axis of the CT.\\
  $\theta_p$ & Zenith angle of the incoming shower.\\
  $\phi_p$   & Horizontal angle (from the North) of the\\
  & direction of the incoming shower.\\
  $\theta_{CT}$ & Zenith angle of the dish ($= \theta$).\\
  $\phi_{CT}$   & Horizontal angle of the dish ($= \phi$).\\
  $\theta_m$ & Angle of the mirror axis relative to the CT axis.\\
  $\phi_m$   & Polar angle of the position of the mirror
  (arbitrary origin).\\
  \end{tabular}
  \caption{Definition of the different angles in the reflector program.}
  \end{table}
  @F
  
  At the CORSIKA level, and also at the reflector simulation level, we
  calculate the angles @$\theta_p@$ and @$\phi_p@$, which give the
  direction of the incoming shower. To calculate them we use the
  following.

  @F
  \begin{eqnarray}
  \cos\theta_p &=& \cos\theta \cos\theta_s 
  + \sin\theta \sin\theta_s \cos\phi_s 
  \\
  \sin\phi_p &=& \frac{\sin\theta_s}{\sin\theta_p} \sin\phi_s
  \end{eqnarray}
  @F
  
  To finish with the definitions, we just write down some of
  the parameters we will use in the reflexion algorithm, in
  the next table.

  @F
  \stepcounter{table}
  \begin{table}
  \begin{tabular}[hbt]{cl}
  $\mathbf{x}=(x,y,z)$ & 
  Position of the Cherenkov photon on the ground\\
  & in the global system OXYZ.\\

  $\mathbf{r}=(u,v,w)$ & 
  Vector of the Cherenkov photon's trayectory\\
  & in the global system OXYZ.\\

  $\mathbf{x_{CT}}=(x_{CT},y_{CT},z_{CT})$ & 
  Position of the Cherenkov photon on the ground\\
  & in the system OXYZ$_{CT}$ of the CT.\\

  $\mathbf{r_{CT}}=(u_{CT},v_{CT},w_{CT})$ & 
  Vector of the Cherenkov photon's trayectory\\
  & in the system OXYZ$_{CT}$ of the CT.\\

  $\mathbf{x_{m}}=(x_{m},y_{m},z_{m})$ & 
  Position of the Cherenkov photon on the ground\\
  & in the system OXYZ$_{m}$ of the mirror.\\

  $\mathbf{r_{m}}=(u_{m},v_{m},w_{m})$ & 
  Vector of the Cherenkov photon's trayectory\\
  & in the system OXYZ$_{m}$ of the mirror.\\

  $\mathbf{x_{cut}}=(x_{cut},y_{cut},z_{cut})$ & 
  Cut of the trayectory of the photon with the mirror,\\
  & in the system OXYZ$_{m}$ of the mirror.\\

  $\mathbf{r_\perp}=(u_\perp,v_\perp,w_\perp)$ & 
  Vector perpendicular to the mirror in the point $\mathbf{x_{cut}}$\\
  & in the system OXYZ$_{m}$ of the mirror.\\

  $\mathbf{r_r}=(u_r,v_r,w_r)$ & 
  Vector of the reflected photon, \\
  & in the system OXYZ$_{m}$ of the mirror.\\

  $\mathbf{r_r^{CT}}=(u_r^{CT},v_r^{CT},w_r^{CT})$ & 
  Vector of the reflected photon, \\
  & in the system OXYZ$_{CT}$ of the CT.\\

  $\mathbf{x_{cam}}=(x_{cam},y_{cam},z_{cam})$ & 
  Position of the photon in the camera plane,\\
  & in the system OXYZ$_{CT}$ of the CT.\\

  \end{tabular}
  \caption{Different vectors and points used in the reflector 
  program.}
  \end{table}
  @F
  
  @"*/

//=---------------------------------------------------------------------
//!@subsubsection Algorithm of the reflexion.

/*!@"
  
  We start calculating the matrices for changing of system of
  coordinates, @$\Omega(\theta,\phi)@$ and
  @$\Omega^{-1}(\theta,\phi)@$. They are defined to be:

  @F
  \begin{equation}
  \begin{split}
  \Omega(\theta,\phi)      &= R_y(-\theta) R_z(-\phi) \\
  \Omega^{-1}(\theta,\phi) &= R_z(\phi) R_y(\theta) 
  \end{split}
  \end{equation}
  @F

  where the matrices @$R_x@$, @$R_y@$ and @$R_z@$ are rotations of
  angle @$\alpha@$ around each axis OX, OY and OZ.

  @F
  \begin{eqnarray}
  R_x(\alpha) = 
  \begin{bmatrix}    
  1 & 0 & 0 \\
  0 & \cos\alpha & -\sin\alpha \\
  0 & \sin\alpha &  \cos\alpha \\
  \end{bmatrix}                                
  \label{eq:rotx}
  \\
  R_y(\alpha) = 
  \begin{bmatrix}    
  \cos\alpha & 0 & \sin\alpha \\
  0 & 1 & 0 \\
  -\sin\alpha & 0 & \cos\alpha \\
  \end{bmatrix}                                
  \label{eq:roty}
  \\
  R_z(\alpha) = 
  \begin{bmatrix}    
  \cos\alpha & -\sin\alpha & 0\\
  \sin\alpha &  \cos\alpha & 0\\
  0 & 0 & 1 \\
  \end{bmatrix}                                
  \label{eq:rotz}
  \end{eqnarray}
  @F
  
  With this, the matrices @$\Omega(\theta,\phi)@$ and
  @$\Omega^{-1}(\theta,\phi)@$ result to be:

  @F
  \begin{eqnarray}
  \Omega(\theta,\phi) = 
  \begin{bmatrix}                                
  \cos\phi \cos\theta&\sin\phi \cos\theta&-\sin\theta \\
  -\sin\phi       &     \cos\phi      &     0      \\
  \cos\phi \sin\theta&\sin\phi \sin\theta& \cos\theta \\      
  \end{bmatrix}                                
  \\
  \Omega^{-1}(\theta,\phi) = 
  \begin{bmatrix}                                
  \cos\phi \cos\theta&-\sin\phi&\cos\phi \sin\theta\\
  \sin\phi \cos\theta& \cos\phi&\sin\phi \sin\theta\\  
  -\sin\theta     &    0    &     \cos\theta    \\       
  \end{bmatrix}                                
  \end{eqnarray}
  @F
  
  and each change of system of coordinates uses these
  matrices. In order to calculate the new coordinates X' of a
  point X in the new system O', and viceversa, we apply the
  following equations:

  @F
  \begin{equation}
  \begin{split}
  X' &= \Omega(\theta,\phi) X\\
  X  &= \Omega^{-1}(\theta,\phi) X'\\
  \end{split}
  \end{equation}
  @F
  
  The steps taken in the reflector simulation are the following:

  @enumerate
  
  @- We look for the mirror @$m@$ which is closer to the
     trayectory of the photon (presumably, the photon will hit
     it).
     
  @- We move to the system @$\mbox{OXYZ}_{CT}@$ of the CT.

  @- We move again, now to the system @$\mbox{OXYZ}_{m}@$ of the
     mirror @$m@$.
  
  @- Then the point where the trayectory of the photon intersects with
     the mirror is calculated.
  
  @- In this point, the normal (perpendicular) to the mirror is
     obtained.

  @- The reflected trayectory is calculated with the normal in the
     mirror and the original trayectory.

  @- We move back to the system @$\mbox{OXYZ}_{CT}@$, and calculate
     there the point where the new trayectory of the photon hits the
     camera plane.

  @endenumerate

  In between the previous points the effects of the atmospheric
  absorption and those mentioned in the next section are included. At
  the end, the position of the photon and the incident angle in the
  camera plane is saved.

  @"*/

//=---------------------------------------------------------------------
//!@subsection Effects to be included in the simulation of the reflector.

/*!@"
  
  @itemize

  @- B@Incidence in the mirrors@:
  For each single photon we trace the point where it hits a
  given mirror. We can use either hexagonal mirrors or
  circular mirrors with the same surface of the hexagonal ones.

  @- B@Reflectivity of the mirrors@:
  The reflectivity of the mirrors is a function @$R(\lambda)@$
  of the wavelength @$\lambda@$ of the photon.

  @- B@Black spots@:
  In the center of each mirror there is a ``bad'' region,
  where no reflection can be done at all. The radii of these
  regions vary, but they can be @$r \simeq 2 \mathrm{\,cm}@$.

  @- B@Imperfections@: 
  The surface of each mirror is locally not-perfect. We need
  measurements to include this effect.

  @- B@Axis--deviation@:
  The axis of each mirror can be deviated from the
  ``teorical'' point in the center of the camera. (@$\sigma
  \simeq 2.5 \mathrm{\,mm}@$).

  @- B@Blurring@: 
  Pure effect of optics: each mirror is ``seen'' by the camera
  with a different angle. 

  @enditemize

  @"*/

//=---------------------------------------------------------------------
//!@subsection Effects to be included in the simulation of the camera.

/*!@"
  
  @itemize

  @- B@Plexiglas@:
  In front of the light guides there is a ``plexiglas'' layer,
  with a given transmitance (dependent on the incidence angle).

  @- B@Light Guides@:
  The looses in the transmitance in the light guides are
  around @$15-20@$\%.

  @- B@Quantum Efficiency@:
  The QE of each PMT depends on the impact point of the photon
  in the window of the PMT and the angle of incidence in this
  point.

  @- B@Excess noise factor@:
  It produces a smearing of the signal with a sigma
  @$\sqrt{2N}@$ instead of the typical @$\sqrt{N}@$.

  @- B@Pick-up noise?@:

  @- B@First dynode collection efficiency?@:

  @enditemize

  @"*/

//=---------------------------------------------------------------------
// @T \newpage

//=---------------------------------------------------------------------
//!@section Source code of |reflector.cxx|.

/*!@"

  In this section we show the (commented) code of the program 
  for the simulation of the reflector, in the current version.
  
  @"*/

//=---------------------------------------------------------------------
//!@subsection Preliminary notes in the code.

//!@{
//=================================================================
//
// NOTE: Most of the equations used in this program can be found 
//       in Bronstein, Semendjajew, Musiol and Muehlig, "Taschenbuch
//       der Mathematik", Verlag Harri Deutsch, Thun und Frankfurt
//       am Main, 1997. Some others were "deduced". 
//
//
// Matrices for rotation of coordinate systems
//---------------------------------------------
//
//    Omega(theta,phi) = Ry(theta) Rz(phi)
//  Omega-1(theta,phi) = Rz(-phi) Ry(-theta)
//
//  X` = Omega(theta,phi) X
//  X  = Omega-1(theta,phi) X`
//
// Omega(theta,phi) = 
//        /                                                   \
//       | cos(phi)cos(theta)  sin(phi)cos(theta)  -sin(theta) |     
//       |    -sin(phi)            cos(phi)             0      |
//       | cos(phi)sin(theta)  sin(phi)sin(theta)   cos(theta) |        
//        \                                                   /
//
// OmegaI(theta,phi) = 
//        /                                                   \
//       | cos(phi)cos(theta)   -sin(phi)   cos(phi)sin(theta) |     
//       | sin(phi)cos(theta)    cos(phi)   sin(phi)sin(theta) |
//       |    -sin(theta)           0           cos(theta)     |        
//        \                                                   /
//
//=================================================================
//!@}

//=---------------------------------------------------------------------
//!@subsection Includes and Global variables definition.

/*!@"

  All the defines are located in the file |reflector.h|.

  @"*/

//!@{
#include "reflector.h"
//!@}

//=---------------------------------------------------------------------
//!@subsubsection Definition of global variables.

/*!@"

  We first define the matrices to change of coordinates system. The
  definition of the matrices was already seen in the previous section.
  All matrices are in the form @$m[i][j]@$ with i the running index
  for rows, and j the one for columns.

  @"*/

//!@{

//@: matrices to change to the system where the optical axis is OZ
static float OmegaCT[3][3];

//@: matrices to change to the system where the optical axis is OZ (inverse)
static float OmegaICT[3][3];

//@: matrices to change the system of coordinates
static float Omega[3][3];

//@: matrices to change the system of coordinates (inverse)
static float OmegaI[3][3];

//!@}

/*!@"
  
  Now we define some global variables with data about the telescope,
  such as E@focal distance@, E@number of pixels/mirrors@, E@size of
  the camera@, and so on.

  @"*/

/*!@"
  
  Depending on the telescope we are using (CT1 or MAGIC), the
  information stored in the definition file is different.  The
  variable |ct_Type| has the value 0 when we use CT1, and 1 when we
  use MAGIC.

  @"*/

//!@{  
 
//@: Type of telescope (CT1: 0; MAGIC: 1)
static int   ct_Type;

//!@}

/*!@"

   And this is the information about the whole telescope.

@"*/

//!@{
// parameters of the CT (from the CT definition file)

//@: Focal distances [cm]
static float *ct_Focal;

//@: Mean Focal distances [cm]
static float ct_Focal_mean;

//@: STDev. Focal distances [cm]
static float ct_Focal_std;

//@: Mean Point Spread function [cm]
static float ct_PSpread_mean;

//@: STDev. Point Spread function [cm]
static float ct_PSpread_std;

//@: STDev. Adjustmente deviation [cm]
static float ct_Adjustment_std;

//@: Radius of the Black Spot in mirror [cm]
static float ct_BlackSpot_rad;

//@: Radius of one mirror [cm]  
static float ct_RMirror;      

//@: Camera width [cm]          
static float ct_CameraWidth;  

//@: Pixel width [cm]           
static float ct_PixelWidth;   

//@: Number of mirrors          
static int ct_NMirrors = 0;   

//@: Number of pixels           
static int ct_NPixels;        

//!@}

/*!@"

  The following double-pointer is a 2-dimensional table with
  information about each mirror in the dish. The routine
  |read_ct_file()| will read this information from the file with the
  name given by the user in the E@parameters file@, in the command
  |ct_file|.  The information stored in this file (and in this table)
  depends on the type of telescope we are using.
  
  @"*/

//!@{

//@: Pointer to a table with the following info.:
static float **ct_data;       

/*
 *  TYPE=0  (CT1)
 *      i   s   rho   theta   x   y   z   thetan  phin  xn   yn   zn
 * 
 *       i : number of the mirror
 *       s : arc length [cm]
 *     rho : polar rho of the position of the center of the mirror [cm]
 *   theta : polar angle of the position of the center of the mirror [cm]
 *       x : x coordinate of the center of the mirror [cm]
 *       y : y coordinate of the center of the mirror [cm]
 *       z : z coordinate of the center of the mirror [cm]
 *  thetan : polar theta angle of the direction where the mirror points to
 *    phin : polar phi angle of the direction where the mirror points to
 *      xn : xn coordinate of the normal vector in the center (normalized)
 *      yn : yn coordinate of the normal vector in the center (normalized)
 *      zn : zn coordinate of the normal vector in the center (normalized)
 * 
 *  TYPE=1  (MAGIC)
 *      i  f   sx   sy   x   y   z   thetan  phin 
 * 
 *       i : number of the mirror
 *       f : focal distance of that mirror
 *      sx : curvilinear coordinate of mirror's center in X[cm]
 *      sy : curvilinear coordinate of mirror's center in X[cm]
 *       x : x coordinate of the center of the mirror [cm]
 *       y : y coordinate of the center of the mirror [cm]
 *       z : z coordinate of the center of the mirror [cm]
 *  thetan : polar theta angle of the direction where the mirror points to
 *    phin : polar phi angle of the direction where the mirror points to
 *      xn : xn coordinate of the normal vector in the center (normalized)
 *      yn : yn coordinate of the normal vector in the center (normalized)
 *      zn : zn coordinate of the normal vector in the center (normalized)
 */

//!@} 

/*!@"

  Here we define the global variables where the parameters of a
  parallel beam of light will be stored.

  @"*/

//!@{

static int   pb_ParallelBeam = FALSE;
static int   pb_ParallelBeamPM = FALSE;
static float pb_Theta, pb_Phi, pb_X, pb_Y; 
static float pb_LengthX, pb_LengthY, pb_NX, pb_NY;
static float pb_Scale;
static float pb_Height;
static char  pb_Filename[40];

//!@}

/*!@"

  We can analyze the data stored in the directories provided via the
  |data_paths| entry in the parameters file in one go, or in blocks of
  size |Block|, if this variable is a positive integer. This can be
  done through a shell script. The mechanism is the following:

  @itemize

  @- First, we read the parameters file, and wait for a file called
   |__DOIT| (written by the script when a new block of |Block| files
   is available in a loop.

  @- If this file exists, we remove it and create another file with
   the name |__WORKING| (in order to let the script to know that we
   are working), and process a block of |Block| files. After that, we
   will rename the file |__WORKING| to |__READY|.
   
  @- When the script sees a new block of |Block| files, it will write
   again a file called |__DOIT|, unless there exist a file called
   |__WORKING| (which means that the |reflector| is still working in
   the previous block).

  @- Each time a block is processed, is customary to the user to
  delete or not that block of files (or to save it to tape, ...).

  @- At the end, a file called |__DONE| is written telling us the
  number of blocks processed.

  @enditemize 

  @"*/

//!@{

static int Block=0;
int WORKING=FALSE;
int nfilesBlock=0;
ofstream obfile; 
long posdir;

//!@}

/*!@"

  We can read also the data from the standard input, and we could
  write data into the standard output.
  
  @"*/

//!@{

//@: boolean variable to control whether we read from STDIN or not
static int Data_From_STDIN = FALSE;

//@: boolean variable to control whether we write to STDOUT or not
static int Data_To_STDOUT  = FALSE;

//!@}

/*!@" 
  
  Next, we have two tables with data got from two files,
  |reflectivity.dat| and |axisdev.dat|, which contain information
  about the reflectivity of the mirrors, as a function of the
  wavelength @$\lambda@$, and about the (simulated) deviation of the
  mirrors' axes, with respect to the mathematical exact axes.

  @"*/

//!@{

// table of reflectivity for each mirror

//@: table with datapoints (wavelength,reflec.)
static float **Reflectivity;  

//@: number of datapoints
static int    nReflectivity;  

//@: table with deviations of the mirrors' normals
static float **AxisDeviation; 

//!@}

/*!@"

  We still define a table into where normal random numbers will be
  stored by the routine |rnormal(double *r, int n)|.
  
  @"*/

//!@{

//@: table of normal random numbers
static double NormalRandomNumbers[500];  

//!@}

/*!@" 

   This is a flag to change the E@verbosity@ of the output

   @"*/

//!@{

//@: flag to change the verbosity
int verbose = VERBOSE_DEFAULT;

//!@}

/*!@" 

  This option makes the telescope to point to a random position
  relative to the shower axis, with a maximum angular separation of
  |RANDOM_POINTING_MAX_SEPARATION| (defined in |reflector.h|.

  @"*/

//!@{

//@: random pointing for the CT?
int Random_Pointing = FALSE;

//@: random pointing for the CT?
int Random_Pointing_Isotropic = TRUE;

//@: number of times a shower is going to be processed
int nRepeat_Random;

//@: number of times a shower is already processed
int nRepeated;

//@: maximum random pointing distance
float Random_Pointing_MaxDist; // [radians]

//@: minimum random pointing distance
float Random_Pointing_MinDist; // [radians]

//!@}

//=---------------------------------------------------------------------
//!@subsection Main program.

/*!@"

  In the main program we basically follow the items expressed in the
  first section. Please, have a look into previous sections. I tried
  to make a code already well commented. Most things should be clear.
  Nevertheless, I'll try to comment again everything in following
  releases.

  @"*/

//!@{

//++++++++++++++++++++++++++++++++++++++++
// MAIN PROGRAM 
//----------------------------------------
                   
int
main(int argc, char **argv) 
{

  DIR *directory;             //@< directory of data
  struct dirent *de;          //@< directory entry

  char pathname[256];         //@< name of directory
  char cername[256];          //@< name of Cherenkov file
  char staname[256];          //@< name of Statistics file
  char outname[256];          //@< name of the output file
  char parname[256];          //@< name of parameters file

  ifstream cerfile;           //@< stream for Cherenkov file
  ifstream pmfile;            //@< stream for pixmap file
  ofstream outputfile;        //@< stream for output file
  ofstream pbfile;            //@< stream for parallel beam file

  COREventHeader evth;        //@< Event Header class (from CORSIKA)
  CORParticle photon;         //@< Particle (photon) class (from CORSIKA)
  CORStatfile stat;           //@< Statistics file class (from CORSIKA)

  MCEventHeader mcevth;       //@< Event Header class
  MCCphoton cphoton;          //@< Particle (photon) class

  float thetaCT, phiCT, xiCT; //@< parameters of a given shower
  float coreD, coreX, coreY;  //@< core position and distance
  float u, v, w;              //@< cosenos directores

  float r[3];                 //@< trayectory
  float x[3];                 //@< position of the photon in the ground

  float rCT[3];               //@< trayectory in the system of the CT
  float xCT[3];               //@< position of the photon in the ground (CT)
  float rm[3];                //@< trayectory in the system of a mirror
  float xmm[3];               //@< intermediate values
  float xm[3];                //@< position of photon in ground
  float xcut[3];              //@< location of the cut sphere-trayectory
  float xcutCT[3];            //@< location of the cut sphere-trayectory (CT)

  float rnor[3], rnorm;       //@< normal in that point

  float rrefl[3];             //@< reflected vector, from the mirror
  float rreflCT[3];           //@< reflected vector, from the CT
  float xcam[3];              //@< where the photon hits the camera plane

  float calpha;               //@< cos(alpha=angle incident/normal)
  float phi;                  //@< angle between photon and camera plane

  float a, b, c, t;           //@< intermediate variables

  float d;                    //@< minimum distance trayectory-mirror center

  float wl;                   //@< wavelength of the Cphoton
  float reflec;               //@< reflectivity for a Cphoton

  int ncer;                   //@< number of the shower;
  int np;                     //@< number of path
  int i, k;                   //@< simple counters
  int i_mirror=-1;            //@< number of a given mirror

  int nCphotons;              //@< number of a Cphotons written on output file

  float TR;                   //@< atmospheric transmitance
  int nbeforeTR, nafterTR;    //@< number of Cph. before and after transmission
  int num_cer_files;          //@< number of cer files;
  int max_num_cer_files;      //@< maximum number of cer files to read
  int f_ev, l_ev;             //@< first and last events of a range

  float lE, uE;              //@< lower and upper edges of energy range allowed

  float distmirr, distmirr2;  //@< distances used in MAGIC reflexion routine
  float sx, sy;
  float t1, t2;
  float pbx, pby, pbt, pbp;

  char line[200];
  
  int ch, errflg = 0;          //@< used by getopt

  /*!@'

    We start in the main program. First we (could) make some
    presentation, and follows the reading of the parameters file (now
    from the STDIN), the reading of the CT parameters file, and the
    creation of the output file, where the processed data will be
    stored.
    
  */

  //++
  //  START
  //--

  // make unbuffered output

  cout.setf ( ios::stdio );

  // parse command line options (see reflector.h)
  
  parname[0] = '\0';

  optarg = 0;
  while ( !errflg && ((ch = getopt(argc, argv, COMMAND_LINE_OPTIONS)) != -1) )
    switch (ch) {
    case 'f':
      strcpy(parname, optarg);
      break;
    case 'h':
      usage();
      break;
    default :
      errflg++;
    }
  
  // show help if error
  
  if ( errflg>0 )
    usage();

  // make some sort of presentation
  //   present();
  
  // read parameters from the stdin

  if ( strlen(parname) < 1 )
    readparam(0);
  else
    readparam(parname);

  // blocking-mode?

  Block = get_block();

  // reading data from STDIN?

  Data_From_STDIN = get_data_from_stdin();

  // writing data to STDOUT?

  Data_To_STDOUT = get_data_to_stdout();

  // make it verbosely?

  verbose = get_verbose();

  /*!@'
    
    @#### Parallel beam of light.
    
    In order to test the behaviour of the program, we can feed it with
    an artificial data file. This data file will be generated by the
    program itself and it will consist in a grid of photons, forming
    all together a parallel beam of light with certain
    characteristics. These characteristics will be given by the user
    in the |parameters file|, in the following form.
    
    parallel\_beam |<theta> <phi> <Xcen> <Ycen> <lenX> <lenY> <nX> <nY> <H>|
    
    or
    
    parallel\_beam\_pm |<filename> <scale> <H>|
    
    In the first form, the first two values give the arrival direction
    of the beam (zenith angle and azimuth) in degrees, and the last
    six values specify the geometry of the beam, in cm, and the height
    of production, also in cm.  In the second form, we use a pixmap
    file in PPM ASCII format, and scale it to create (for 0 degrees of
    zenith angle) a parallel beam of light.
    
  */

  //++
  // PARALLEL BEAM OF LIGHT
  //--

  // are we going to use a parallel beam of light?

  pb_ParallelBeam = is_parallel_beam();
  pb_ParallelBeamPM = is_parallel_beam_pm();

  if ( pb_ParallelBeam == TRUE ) {

    if ( pb_ParallelBeamPM == FALSE ) {

      // get parameters of the parallel beam of light

      get_parallel_beam(&pb_Theta, &pb_Phi, 
                        &pb_X, &pb_Y, 
                        &pb_LengthX, &pb_LengthY, 
                        &pb_NX, &pb_NY, &pb_Height);
      
      // generate artificial file(s)
      
      // a. cerfile
      
      sprintf( cername, "%s/cer999999", get_path_name(0) );
      
      pbfile.open( cername, ios::out );
      
      evth.fill ( 1., 1., 1000., 1., 1.e6, 0., 0., 0., pb_Theta, pb_Phi,
                  pb_X, pb_Y);
      
      evth.write( pbfile );
      
      for ( pbx = (pb_X - pb_LengthX/2.0); 
            pbx <= (pb_X + pb_LengthX/2.0);  
            pbx += (pb_LengthX/pb_NX) )
        
        for ( pby = (pb_Y - pb_LengthY/2.0); 
              pby <= (pb_Y + pb_LengthY/2.0);  
              pby += (pb_LengthY/pb_NY) ) {
          
          pbt = -atan2(sqrt(SQR(pbx)+SQR(pby)),pb_Height);
          pbp = atan2(pby, pbx);
          photon.fill(450.0, 
                      pbx, pby, 
                      -sin(pbt) * cos(pbp),
                      -sin(pbt) * sin(pbp),
                      0., pb_Height);
          
          photon.write( pbfile );
          
        }
      
      photon.fill( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
      
      photon.write( pbfile );
      
      pbfile.close();
      
      // b. statfile
      
      sprintf( staname, "%s/sta999999", get_path_name(0) );
      
      pbfile.open( staname );
      
      stat.write( pbfile );
      
      pbfile.close();

    } else {

      // get parameters of the parallel beam of light from PIXMAP

      strcpy(pb_Filename, get_parallel_beam_pm(&pb_Scale,&pb_Height));

      pmfile.open( pb_Filename, ios::out );
      pmfile.getline( line, 300 );
      
      do {      
        pmfile.getline( line, 300 );
      } while (line[0] == '#');

      sscanf( line, "%f %f", &pb_NX, &pb_NY );
      pb_LengthX = pb_NX * pb_Scale;
      pb_LengthY = pb_NY * pb_Scale;

      // generate artificial file(s)
      
      // a. cerfile
      
      sprintf( cername, "%s/cer999999", get_path_name(0) );
      
      pbfile.open( cername, ios::out );
      
      evth.fill ( 1., 1., 1000., 1., 1.e6, 0., 0., 0., 0., 0., 0., 0.);
      
      evth.write( pbfile );
      
      for ( pbx = (pb_X - pb_LengthX/2.0); 
            pbx <= (pb_X + pb_LengthX/2.0);  
            pbx += (pb_LengthX/pb_NX) )
        
        for ( pby = (pb_Y - pb_LengthY/2.0); 
              pby <= (pb_Y + pb_LengthY/2.0);  
              pby += (pb_LengthY/pb_NY) ) {
          
          pmfile >> i;

          if ( i < 1 ) 
            continue;

          pbt = -atan2(sqrt(SQR(pbx)+SQR(pby)),pb_Height);
          pbp = atan2(pby, pbx);
          photon.fill(450.0, 
                      pbx, pby, 
                      -sin(pbt) * cos(pbp),
                      -sin(pbt) * sin(pbp),
                      0., pb_Height);
          
          photon.write( pbfile );
          
        }
      
      photon.fill( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 );
      
      photon.write( pbfile );
      
      pbfile.close();
      
      // b. statfile
      
      sprintf( staname, "%s/sta999999", get_path_name(0) );
      
      pbfile.open( staname );
      
      stat.write( pbfile );
      
      pbfile.close();

      pmfile.close();

    }      

  } // YES parallel beam
  
  // read parameters from the stdin

  read_ct_file();

  // set all random numbers seeds

  setall( get_seeds(0), get_seeds(1) );
  
  // get name of the output file

  strcpy( outname, get_output_filename() );
  
  // open((re)create) output file
  
  if ( verbose >= VERBOSE_MINIMAL )
    log( SIGNATURE, "Openning/creating output file %s\n", outname );
  
  outputfile.open( outname );
  
  if ( outputfile.bad() ) 
    error( SIGNATURE, "Cannot open output file: %s\n", outname );
  
  // save signature to the output file

  if ( Data_To_STDOUT ) 
    cout.write( SIGNATURE, sizeof(SIGNATURE) );
  else
    outputfile.write( SIGNATURE, sizeof(SIGNATURE) );

  // get random pointing variables
  
  Random_Pointing = get_random_pointing( &Random_Pointing_MaxDist,
                                         &Random_Pointing_MinDist,
                                         &Random_Pointing_Isotropic );

  // generate a sort of log information

  if ( verbose >= VERBOSE_MINIMAL ) {
    log( SIGNATURE, "Random poi.:       %f-%f (%s)\n",
         DEG(Random_Pointing_MinDist), DEG(Random_Pointing_MaxDist),
         (Random_Pointing_Isotropic==TRUE)?"Isotropic":"Uniform" );
    log( SIGNATURE, "Atmospheric model: %s\n", get_atm_mod() );
    log( SIGNATURE, "Number of paths:   %d\n", get_num_of_paths() );
    log( SIGNATURE, "Output filename:   %s\n", outname );
    log( SIGNATURE, "CT def. filename:  %s\n", get_ct_filename() );
  }

  // get energy range

  get_energy_cuts( &lE, &uE );

  // get number of times which a shower is going to be used
  // at different pointing directions
  nRepeat_Random = get_repeat_random();   

  /*!@'

    After reading the parameters file (from STDIN), and the CT
    definition file, we start the analysis. Basically, there are three
    loops. The outermost loop is a loop in the E@data directories@ we
    introduced in the parameters file; the middle loop follows each
    |cer*| file in the directory; and the innermost loop gets each
    Cherenkov from these files.

  */

  // for each path of data files

  for (np=0; np<get_num_of_paths(); np++) {

    strcpy(pathname, get_path_name(np));
    
    // open directory 
      
    if ( verbose >= VERBOSE_MINIMAL )
      log( SIGNATURE, "Openning directory %s\n", pathname );
 
    directory = opendir(pathname);

    if ( directory == 0 ) 
      error( SIGNATURE, 
             "Cannot open directory %s\n", pathname );
    
    // write start-of-run mark
    
    if ( Data_To_STDOUT ) 
      cout.write( FLAG_START_OF_RUN, SIZE_OF_FLAGS );
    else
      outputfile.write( FLAG_START_OF_RUN, SIZE_OF_FLAGS );
    
    // trace the number of events (cer* files) 
    
    num_cer_files = 0;

    // get maximum number, first and last event

    max_num_cer_files = get_max_events();

    get_range_events(&f_ev, &l_ev);

    // if reading from disk, save position of handler in directory

    posdir = telldir( directory );

    if ( Data_From_STDIN ) {

      if ( verbose >= VERBOSE_MINIMAL )
        log(SIGNATURE, " * * * READING DATA FROM STDIN * * *\n");

      get_stdin_files(0, lE, uE, TRUE);

    }
    
    //--- LOOP ON FILES ---
    // for each directory entry (files), 
    // and(or) while still data has to be read 
    // this condition is:
    // while  ( ( there are files    AND 
    //            maximum number is not reached )
    //                     OR 
    //          ( reading from stdin   AND
    //            maximum number is not reached ) )

    nRepeated = 0;

    while ( (
             ((de = readdir( directory )) != 0) &&
             (num_cer_files < max_num_cer_files)
             )
            ||
            (
             ( Data_From_STDIN ) &&
             (num_cer_files < max_num_cer_files)
             ) ) {
	/*
      // increment the number of times this files is used
      nRepeated++;
	*/

      // if Block > 0, then we wait till a file __DOIT is present
      
      if ( Block > 0 ) {

        if ( ! WORKING ) {

          while ( ! WORKING ) {
            // sleep 10 minutes
            sleep( 60 );
            
            // look for the file __DOIT
            obfile.open("__DOIT");
            if ( ! obfile.bad() ) {
              WORKING = TRUE;
              obfile.close();
            }
          }
          
          obfile.open("__WORKING");
          remove("__DOIT");

          rewinddir( directory );
          seekdir( directory, posdir );
          
          nfilesBlock = 0;

          continue;
          
        } else {

          if ( nfilesBlock == Block ) {

            posdir = telldir( directory );
            obfile.close();
            obfile.open("__READY");
            obfile << nfilesBlock << " files/block, "
                   << ncer << " showers.\n";
            obfile.close();
            remove("__WORKING");
            WORKING = FALSE;

            continue;
          }

        }

      } // if Block

      if ( Data_From_STDIN ) {
        
        if ( verbose >= VERBOSE_MINIMAL )
          log(SIGNATURE, "Try to get data from STDIN\n");
        
        if ( ! get_stdin_files(num_cer_files) ) {
          if ( verbose >= VERBOSE_MINIMAL )
            log(SIGNATURE, "get_stdin_files returned FALSE.\n");
          if (cin.eof()) {
            num_cer_files = max_num_cer_files + 1;
            continue;
          }
        }
        
        ++num_cer_files;
        ncer = num_cer_files;

      } else {

        // skip removed files
        
        if ( de->d_ino == 0)
          continue;
        
        // keep only cer* files
        
        if ( strstr(de->d_name, "cer") != de->d_name )
          continue;
        
        if ( Block > 0 ) {
          ++nfilesBlock;
          obfile << nfilesBlock;
        }
        
        // get cer* file number (number of the shower)

        // increments the pointer in 3 to calculate the number
        
        ncer = atoi(de->d_name + 3);  
        
        if ( (ncer < f_ev) || (ncer > l_ev) )
          continue;
        
        // it is a real cer* file, and we want to read it
        
        ++num_cer_files;
        
        if ( verbose >= VERBOSE_NORMAL )
          log( SIGNATURE, "cerfile: %s\n", de->d_name );
        
      } // data from disk

      // Here we initialize the counters of Cherenkov photons at
      // different levels of the analysis.
      
      nCphotons = nbeforeTR = nafterTR = 0;
      
      // full names of the input files cer* and sta*
        
      sprintf(cername, "%s/cer%06d", pathname, ncer);
      sprintf(staname, "%s/sta%06d", pathname, ncer);

      // try to open cer* file
        
      if ( verbose >= VERBOSE_MAXIMAL )
        log( SIGNATURE, "Opening %s\n", cername );
        
      cerfile.open( cername );
        
      if ( cerfile.bad() ) 
        error( SIGNATURE, 
               "Cannot open input file: %s\n", cername );
        
      /*!@'

        Each shower has associated three files: E@ Cherenkov-photons
        file @ (|cer*|), E@ Particles file @ (|dat*|) and E@
        Statistics file @ (|sta*|). First we read the data from this
        last file, which is explained below, and then we read the E@
        Event-Header @ block from the |cer*| file.

      */
        
      // try to open the statistics file
        
      if ( verbose >= VERBOSE_MAXIMAL )
        log( SIGNATURE, "Opening %s\n", staname );
        
      stat.openfile ( staname );
        
      // reading data from this sta* file
        
      if ( verbose >= VERBOSE_MAXIMAL )
        log( SIGNATURE, "Reading %s\n", staname );
        
      stat.read();
        
      // read the Event Header from the cer* file
        
      evth.read( cerfile );
        
      if ( verbose >= VERBOSE_MAXIMAL ) {
        log( SIGNATURE, "EVENT HEADER:\n");
        evth.print();
      }
        
      // check energy
        
      if ( (evth.get_energy() < lE) || 
           (evth.get_energy() > uE) ) {
          
        // close files
        cerfile.close();
        stat.closefile();
          
        // skip it
        if ( verbose >= VERBOSE_MAXIMAL )
          log( SIGNATURE, "Bad energy.\n");
          
        continue;
      }
        
      // write start-of-event mark
        
      if ( Data_To_STDOUT ) 
        cout.write( FLAG_START_OF_EVENT, SIZE_OF_FLAGS );
      else
        outputfile.write( FLAG_START_OF_EVENT, SIZE_OF_FLAGS );
        
      // save information from the cer* to the mc-evth
        
      mcevth.transport( &evth );
        
      // save time interval in the mc-evth
        
      mcevth.put_times ( stat.get_tfirst(), stat.get_tlast() );
        
      // get direction where the CT is pointing to
      // (or, better, from when the shower is coming from)

      // first, put the values from the shower
      thetaCT = evth.get_theta();
      phiCT   = evth.get_phi();

      // then, may be we have to change them

      // do we want random pointing (around shower axis) ?
      if ( Random_Pointing == TRUE ) {

        // we do, then get a random position
        xiCT = get_new_ct_pointing(thetaCT, phiCT,
                                   Random_Pointing_MinDist,
                                   Random_Pointing_MaxDist,
                                   &thetaCT, &phiCT,
                                   Random_Pointing_Isotropic);

        xiCT = xiCT;   // this is to avoid "was set but not used" messages
        
        // save deviations in mc-evth
        mcevth.put_deviations (thetaCT - evth.get_theta(),
                               phiCT - evth.get_phi());

        if ( verbose >= VERBOSE_MAXIMAL )
          log( SIGNATURE, "NEW POINTING: (%f,%f)  ->  (%f,%f)\n",
               evth.get_theta(), evth.get_phi(), thetaCT, phiCT);

      } else {
        
        mcevth.put_deviations (0.0, 0.0);

        if ( get_fixed_target( &thetaCT, &phiCT ) == FALSE ) {
          thetaCT = evth.get_theta();
          phiCT   = evth.get_phi();
        }

      }
        
      // save event-header to the output file
        
      if ( Data_To_STDOUT ) 
        cout.write ( (char *)&mcevth, mcevth.mysize() );
      else
        mcevth.write( outputfile );
        
      if ( verbose >= VERBOSE_MAXIMAL )
        mcevth.print();

      // get core distance and position
        
      coreD = evth.get_core(&coreX, &coreY);
      coreD=coreD; // to avoid "was set but not used" messages
        
      // calculate matrices for changing of coordinate system from-to CT
      
      makeOmega( thetaCT, phiCT );
      makeOmegaI( thetaCT, phiCT );
      
      memcpy( OmegaCT, Omega, 9*sizeof(float) );
      memcpy( OmegaICT, OmegaI, 9*sizeof(float) );

      // read each and every single photon (particle) from cer* file
      
      while ( ! (cerfile.eof() || cerfile.bad()) ) {
        
        // read photon data from the file
        
        photon.read( cerfile );

        wl = photon.get_wl();
        
        if ( wl < 1.0 )
          break;
        
        // get director cosines in z-axis
        
        w = r[2] = photon.get_w();
        
        /*!@'

          @#### Atmospheric attenuation.

          The routine |atm()| calls the proper attenuation routine,
          and returns the transmission coefficient for that
          whavelength, height and zenith angle.

           */

        //++
        // FILTER No. 1: ATMOSPHERE
        //--

        ++nbeforeTR;

        // get transmitance of the atmosphere
          
        TR = atm( photon.get_wl(), photon.get_h(), acos(w) );  

        // passes the atmosphere?
          
        // if random > TR then goes to the TOP of the loop again
          
        if ( RandomNumber > TR ) 
          continue;              
          
        ++nafterTR;

        /*!@'

          @#### Reflectivity of the mirrors.

          We make a 3rd. order interpolation using Lagrange
          polynomials, in order to calculate the reflectivity of the
          mirror for that wavelength. Further developments will
          include also a incidence-angle dependence (which is not very
          important).

        */

        //++
        // FILTER No. 2: REFLECTIVITY R(lambda)
        //--

        // find data point to be used in Lagrange interpolation (-> k)

        FindLagrange(Reflectivity,k,wl);
          
        // if random > reflectivity then goes to the TOP of the loop again
          
        reflec = Lagrange(Reflectivity,k,wl);
          
        if ( RandomNumber > reflec )
          continue;

        /*!@'

          @#### Reflexion in the mirrors.

          The reflexion is made now is a somehow dirty way: if the
          value of the variable |ct\_Type| is 0, the telescope is CT1,
          and if the value is 1, the telescope is MAGIC. Due to the
          completly different geometries of the dishes and the
          mirrors, now we support two fully separated and different
          algorithms.

        */

        //++
        // REFLEXION
        //--

        // get director cosines x,y and save original trayectory
          
        u = r[0] = photon.get_u();
        v = r[1] = photon.get_v();
        u=u, v=v;         // to avoid "was set but not used" messages
        
        // get position in the ground
          
        x[0] = photon.get_x() - coreX;
        x[1] = photon.get_y() - coreY;
        x[2] = 0.0;
          
        if ( verbose >= VERBOSE_MAXIMAL ) {
          cout << '@' << '1' 
               << ' ' << ncer 
               << ' ' << t   
               << ' ' << x[0]
               << ' ' << x[1]
               << ' ' << x[2]
               << ' ' << r[0]
               << ' ' << r[1]
               << ' ' << r[2] 
               << ' ' << thetaCT 
               << ' ' << phiCT 
               << endl;
        }

        // change to the system of the CT
        
        applyMxV( OmegaCT, x, xCT );
        applyMxV( OmegaCT, r, rCT );        

        // before moving to the system of the mirror, for MAGIC, 
        // first we look whether the photon hits a mirror or not

        // calculate the intersection of the trayectory of the photon 
        // with the GLOBAL DISH !!!
        // we reproduce the calculation of the coefficients of the
        // second order polynomial in z (=xCT[2]), made with 
        // Mathematica 

        /*
         * In[1]:= parab:=z-(x^2+y^2)/(4F)
         *        par1=parab /. {x->x0+u/w(z-z0),y->y0+v/w(z-z0)}
         * 
         * Out[1]=
         *                  u (z - z0) 2         v (z - z0) 2
         *            (x0 + ----------)  + (y0 + ----------)
         *                      w                    w
         *        z - ---------------------------------------
         *                              4 F
         * 
         * In[2]:= CoefficientList[ExpandAll[par1*4F*w^2],z]
         * 
         * Out[2]=
         *            2   2     2   2
         *        {-(w  x0 ) - w  y0  + 2 u w x0 z0 + 
         *         
         *                         2   2    2   2
         *          2 v w y0 z0 - u  z0  - v  z0 , 
         *         
         *              2                            2
         *         4 F w  - 2 u w x0 - 2 v w y0 + 2 u  z0 + 
         *         
         *             2       2    2
         *          2 v  z0, -u  - v }
         */       
 
        // the z coordinate is calculated

        a = - SQR(rCT[0]) - SQR(rCT[1]);
        b = 4.0*ct_Focal_mean*SQR(rCT[2]) 
          - 2.0*rCT[0]*rCT[2]*xCT[0] - 2.0*rCT[1]*rCT[2]*xCT[1] 
          + 2.0*SQR(rCT[0])*xCT[2] + 2.0*SQR(rCT[1])*xCT[2];
        c = 2*rCT[0]*rCT[2]*x[0]*x[2] + 2*rCT[1]*rCT[2]*x[1]*x[2] 
          - SQR(rCT[2])*SQR(x[0]) - SQR(rCT[2])*SQR(x[1])
          - SQR(rCT[0])*SQR(x[2]) - SQR(rCT[1])*SQR(x[2]);

        if ( fabs(a) < 1.e-6 ) {

          // only one value

          xcut[2] = -c / b;

        } else {
          
          d = sqrt( b*b - 4.0*a*c );
          
          // two possible values for z
          
          t1 = (-b+d) / (2.0*a);
          t2 = (-b-d) / (2.0*a);

          // z must be the minimum of t1 and t2
          
          xcut[2] = (t1 < t2) ? t1 : t2;

        }
        
        // xcut[] is NOW the cut between the GLOBAL dish of MAGIC and
        // the trayectory of the photon

        xcut[0] = xCT[0] + rCT[0]/rCT[2]*(xcut[2]-xCT[2]);
        xcut[1] = xCT[1] + rCT[1]/rCT[2]*(xcut[2]-xCT[2]);

        if ( ct_Type == 1 ) {

          // ++ >>>>>>>>>> MAGIC <<<<<<<<<<
        
          // convert to Curvilinear distance over the parabolic dish
          
          sx = Lin2Curv( xcut[0] );
          sy = Lin2Curv( xcut[1] );
          
          // is it outside the dish?
          
          if ((fabs(sx) > 850.0) || (fabs(sy) > 850.0)) {
            //cout << "CONDICION 1 !" << endl << flush;
            //cout << '1';
            continue;
          }

          // -- >>>>>>>>>> MAGIC <<<<<<<<<<

        } // endif (ct_Type == 1)

        // calculate the mirror to be used

        distmirr = 1000000.;
        
        for (i=0; i<ct_NMirrors && distmirr>=ct_RMirror; ++i) {
          distmirr2 = sqrt(SQR(ct_data[i][CT_X] - xcut[0]) +
                           SQR(ct_data[i][CT_Y] - xcut[1]) +
                           SQR(ct_data[i][CT_Z] - xcut[2]));
          if (distmirr2 < distmirr) {
            i_mirror = i;
            distmirr = distmirr2;
          }
        }

        // the mirror to use is i_mirror (calculated several lines above)
        // check whether the photon is outside the nearest (this) mirror
          
        if ( ct_Type == 0 ) {

          // ++ >>>>>>>>>> CT1 <<<<<<<<<<
          if (distmirr > ct_RMirror) {
            //cout << "CONDICION 2 !" << endl << flush;
            //cout << '2';
            continue;
          }
          // -- >>>>>>>>>> CT1 <<<<<<<<<<
        
        } else { 
          
          // ++ >>>>>>>>>> MAGIC <<<<<<<<<<
          if ((fabs(ct_data[i_mirror][CT_SX] - sx) > ct_RMirror) ||
              (fabs(ct_data[i_mirror][CT_SY] - sy) > ct_RMirror)) {
            //cout << "CONDICION 2 !" << endl << flush;
            //cout << '2';
            continue;         
          }
          // -- >>>>>>>>>> MAGIC <<<<<<<<<<
            
        }
 
        // calculate matrices for the mirror
        
        makeOmega (-ct_data[i_mirror][CT_THETAN], 
                   ct_data[i_mirror][CT_PHIN]);
        makeOmegaI(-ct_data[i_mirror][CT_THETAN], 
                   ct_data[i_mirror][CT_PHIN]);
        
        // change to the system of the mirror
        
        // first translation...

        xmm[0] = xCT[0] - ct_data[i_mirror][CT_X];
        xmm[1] = xCT[1] - ct_data[i_mirror][CT_Y];
        xmm[2] = xCT[2] - ct_data[i_mirror][CT_Z];
        
        // ...then rotation

        applyMxV( Omega, xmm, xm );
        applyMxV( Omega, rCT, rm );
        
        // the vector rCT should be normalized, and
        // so the vector rm remains normalized as well, but, anyhow...
        
        rnorm = NORM( rm );
        rm[0] /= rnorm;
        rm[1] /= rnorm;
        rm[2] /= rnorm;

        // calculate the intersection of the trayectory of the photon 
        // with the mirror
        // we reproduce the calculation of the coefficients of the
        // second order polynomial in z (=xm[2]), made with 
        // Mathematica

        /*
         * In[1]:= esfera:=x^2+y^2+(z-R)^2-R^2;
         *         recta:={x->x0+u/w(z-z0),y->y0+v/w(z-z0)}
         * 
         * In[2]:= esfera
         * 
         *            2    2    2           2
         * Out[2]= -R  + x  + y  + (-R + z)
         * 
         * In[3]:= recta
         * 
         *                     u (z - z0)            v (z - z0)
         * Out[3]= {x -> x0 + ----------, y -> y0 + ----------}
         *                         w                     w
         * 
         * In[4]:= esf=esfera /. recta
         * 
         *           2           2         u (z - z0) 2         v (z - z0) 2
         * Out[4]= -R  + (-R + z)  + (x0 + ----------)  + (y0 + ----------)
         *                                      w                    w
         * 
         * In[5]:= coefs=CoefficientList[ExpandAll[esf],z]
         * 
         *                                               2   2    2   2
         *            2     2   2 u x0 z0   2 v y0 z0   u  z0    v  z0
         * Out[5]= {x0  + y0  - --------- - --------- + ------ + ------, 
         *                           w           w          2        2
         *                                                 w        w
         *  
         *                                  2         2          2    2
         *             2 u x0   2 v y0   2 u  z0   2 v  z0      u    v
         * >    -2 R + ------ + ------ - ------- - -------, 1 + -- + --}
         *               w        w         2         2          2    2
         *                                 w         w          w    w
         * In[6]:= Simplify[ExpandAll[coefs*w^2]]
         * 
         *           2    2     2                             2    2    2
         * Out[6]= {w  (x0  + y0 ) - 2 w (u x0 + v y0) z0 + (u  + v ) z0 ,
         *  
         *             2             2                            2    2    2
         * >    -2 (R w  - u w x0 + u  z0 + v (-(w y0) + v z0)), u  + v  + w }
         * 
         */
         
        // the z coordinate is calculated, using the coefficients
        // shown above

        a = SQR(rm[0]) + SQR(rm[1]) + SQR(rm[2]);
        if ( ct_Type == 0 )  {
          // CT1
          b = -2*(2.*ct_Focal_mean*SQR(rm[2]) 
                  - rm[0]*rm[2]*xm[0] 
                  + SQR(rm[0])*xm[2] 
                  + rm[1]*(-(rm[2]*xm[1]) + rm[1]*xm[2]));
        } else {
          // MAGIC
          b = -2*(2.*ct_data[i_mirror][CT_FOCAL]*SQR(rm[2]) 
                  - rm[0]*rm[2]*xm[0] 
                  + SQR(rm[0])*xm[2] 
                  + rm[1]*(-(rm[2]*xm[1]) + rm[1]*xm[2]));
        }
        c = (SQR(rm[2])*(SQR(xm[0]) + SQR(xm[1])) 
             - 2*rm[2]*(rm[0]*xm[0] + rm[1]*xm[1])*xm[2] + 
             (SQR(rm[0]) + SQR(rm[1]))*SQR(xm[2]));

        d = sqrt( b*b - 4.0*a*c );

        // two possible values for z

        t1 = (-b+d) / (2.0*a);
        t2 = (-b-d) / (2.0*a);

        // z must be the minimum of t1 and t2

        xcut[2] = (t1 < t2) ? t1 : t2;
        xcut[0] = xm[0] + rm[0]/rm[2]*(xcut[2]-xm[2]);
        xcut[1] = xm[1] + rm[1]/rm[2]*(xcut[2]-xm[2]);

        //++
        // BLACK SPOTS: If the photon hits the black spot, it's lost
        //--

        if ( sqrt(SQR(xcut[0]) + SQR(xcut[1])) < ct_BlackSpot_rad ) {
          //cout << "CONDITION 3!\n" << flush;
          //cout << '3';
          continue;
        }

        // if still we have the photon, continue with the reflexion
          
        // calculate normal vector in this point 
        // (and normalize, with the sign changed)

        rnor[0] = 2.0*xcut[0];
        rnor[1] = 2.0*xcut[1];
        rnor[2] = 2.0*(xcut[2] - 2.0*ct_Focal[i_mirror]);

        rnorm = -NORM( rnor );
        rnor[0] /= rnorm;
        rnor[1] /= rnorm;
        rnor[2] /= rnorm;

        // now, both "normal" vector and original trayectory are
        // normalized
        // just project the original vector in the normal, and 
        // take it as the "mean" position of the original and
        // the "reflected" vector
        // from this, we can calculate the "reflected" vector
        // calpha = cos(angle(rnor,rm))

        calpha = fabs(rnor[0]*rm[0] + rnor[1]*rm[1] + rnor[2]*rm[2]);
        
        // finally!!! we have the reflected trayectory of the photon

        rrefl[0] = 2.0*rnor[0]*calpha - rm[0];
        rrefl[1] = 2.0*rnor[1]*calpha - rm[1];
        rrefl[2] = 2.0*rnor[2]*calpha - rm[2];

        rnorm = NORM( rrefl );
        rrefl[0] /= rnorm;
        rrefl[1] /= rnorm;
        rrefl[2] /= rnorm;

        // let's go back to the coordinate system of the CT

        // first rotation...
        applyMxV( OmegaI, xcut, xcutCT);
        applyMxV( OmegaI, rrefl, rreflCT);

        // ...then translation
        xcutCT[0] += ct_data[i_mirror][CT_X];
        xcutCT[1] += ct_data[i_mirror][CT_Y];
        xcutCT[2] += ct_data[i_mirror][CT_Z];
        
        // calculate intersection of this trayectory and the camera plane
        // in the system of the CT, this plane is z = ct_Focal

        t = (ct_Focal_mean - xcutCT[2]) / rreflCT[2];

        xcam[0] = xcutCT[0] + rreflCT[0]*t;
        xcam[1] = xcutCT[1] + rreflCT[1]*t;
        xcam[2] = xcutCT[2] + rreflCT[2]*t;
        
        //++
        // AXIS DEVIATION: We introduce it here just as a first order 
        //   correction, by modifying the position of the reflected photon.
        //--

        xcam[0] += AxisDeviation[0][i_mirror];
        xcam[1] += AxisDeviation[1][i_mirror];
        
        //++
        // SMEARING: We apply the point spread function for the mirrors
        //--

        // get two N(0;1) random numbers

        rnormal( NormalRandomNumbers, 2 );

        // modify the Cphoton position in the camera

        xcam[0] += NormalRandomNumbers[0] * ct_PSpread_mean;
        xcam[1] += NormalRandomNumbers[1] * ct_PSpread_mean;

        // check whether the photon goes out of the camera

        if ( (SQR(xcam[0])+SQR(xcam[1])) > SQR(ct_CameraWidth) ) {
          continue;
        }

        //++
        // ANGLE OF INCIDENCE
        //--

        // calculate angle of incidence between tray. and camera plane
        // the camera plane is
        // 0 y + 0 y + z - ct_Focal = 0 => (A,B,C,D) = (0,0,1,-ct_Focal)
        // from Table 3.20 "Tasch. der Math."

        phi = asin(rreflCT[2]);  

        //++
        // TIMING
        //--

        // calculate the new time of the photon (in the camera)

        // initial time since first interaction

        t = photon.get_t();

        // substract path from the mirror till the ground, 'cos 
        // the photon actually hit the mirror!!

        
        t = t + ((( xm[2] > 0. ) ? -1.0 : +1.0) *
                 sqrt( SQR(xm[0] - xcut[0]) +
                       SQR(xm[1] - xcut[1]) +
                       SQR(xm[2] - xcut[2]) ) / Speed_of_Light_air_cmns);
                 
        // add path from the mirror till the camera

        t = t + sqrt( SQR(xcutCT[0] - xcam[0]) +
                      SQR(xcutCT[1] - xcam[1]) +
                      SQR(xcutCT[2] - xcam[2]) ) / Speed_of_Light_air_cmns;
        
        // save data in the photon variable

        cphoton.fill( photon.get_id(), 
                      xcam[0], xcam[1],
                      photon.get_u(), photon.get_v(),
                      photon.get_h(), t, phi);
          
        // show it

        if ( verbose >= VERBOSE_MAXIMAL ) {
          cout << '@' << '2' 
               << ' ' << ncer 
               << ' ' << xCT[0]
               << ' ' << xCT[1]
               << ' ' << xCT[2]
               << ' ' << rCT[0]
               << ' ' << rCT[1]
               << ' ' << rCT[2]
               << ' ' << xcut[0]
               << ' ' << xcut[1]
               << ' ' << xcut[2]
               << endl;
          cout << '@' << '3' 
               << ' ' << ncer 
               << ' ' << sx
               << ' ' << sy
               << ' ' << i_mirror
               << ' ' << ct_data[i_mirror][CT_SX]
               << ' ' << ct_data[i_mirror][CT_SY]
               << ' ' << ct_data[i_mirror][CT_SX] - sx
               << ' ' << ct_data[i_mirror][CT_SY] - sy 
               << endl;
          if ( pb_ParallelBeam == FALSE )  {
            cout << '@' << '4' 
                 << ' ' << ncer 
                 << ' ' << xcam[0] 
                 << ' ' << xcam[1] 
                 << ' ' << xcam[2] 
                 << ' ' << phi 
                 << ' ' << photon.get_t()-stat.get_tfirst() 
                 << ' ' << t-stat.get_tfirst() 
                 << endl << flush;
          } else {
            cout << '@' << '4' 
                 << ' ' << ncer 
                 << ' ' << xcam[0] 
                 << ' ' << xcam[1] 
                 << ' ' << xcam[2] 
                 << ' ' << phi 
                 << endl << flush;
          }
        }
                
        // write the photon data to the output file
        
        if ( Data_To_STDOUT ) 
          cout.write ( (char *)&cphoton, cphoton.mysize() );
        else
          cphoton.write( outputfile );

        // increase number of Cphotons written

        ++nCphotons;

      } // while still there are photons left

      if ( verbose >= VERBOSE_NORMAL )
        cout << nCphotons << ' '
             << nbeforeTR << ' '
             << nafterTR << ' '
             << endl << flush;
      
      // write end-of-event mark
      
      if ( Data_To_STDOUT ) 
        cout.write( FLAG_END_OF_EVENT, SIZE_OF_FLAGS );
      else
        outputfile.write( FLAG_END_OF_EVENT, SIZE_OF_FLAGS );

      // close files
        
      cerfile.close();
      stat.closefile();

      // show how many photons were written
          
      if ( verbose >= VERBOSE_MAXIMAL )
        log( SIGNATURE, "%d C-photons written.\n", nCphotons );

      // if we are reading data from STDIN, delete the last used files
      
      if ( Data_From_STDIN ) {
        
        unlink( cername );
        unlink( staname );
        
      }      
      
    } // while there are still data left
    
    // write end-of-run mark
    
    if ( Data_To_STDOUT ) 
      cout.write( FLAG_END_OF_RUN, SIZE_OF_FLAGS );
    else
      outputfile.write( FLAG_END_OF_RUN, SIZE_OF_FLAGS );
    
    // close directory
    
    closedir( directory );
    
  } // for each data directory 

  // write end-of-file mark
  
  if ( Data_To_STDOUT ) 
    cout.write( FLAG_END_OF_FILE, SIZE_OF_FLAGS );
  else
    outputfile.write( FLAG_END_OF_FILE, SIZE_OF_FLAGS );

  // close output file

  if ( verbose >= VERBOSE_MINIMAL ) 
    log( SIGNATURE, "Closing output file %s\n", outname );

  if ( ! Data_To_STDOUT ) 
    outputfile.close();

  // clean everything

  if ( verbose >= VERBOSE_MINIMAL ) 
    log( SIGNATURE, "Cleanning . . .\n" );

  clean();

  // program finished

  if ( verbose >= VERBOSE_MINIMAL ) 
    log( SIGNATURE, "Done.\n");

  return ( 0 );
}
//!@}

// @T \newpage

//=---------------------------------------------------------------------
// @subsection Functions definition

//!---------------------------------------------------------------------
// @name present
//
// @desc Make some presentation
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
present(void)
{
  cout << "##################################################\n"
       <<  SIGNATURE << '\n' << '\n'
       << "Simulation of the reflector\n"
       << "J C Gonzalez, May 1998\n"
       << "##################################################\n\n";
}
//!@}


//!---------------------------------------------------------------------
// @name usage
//
// @desc show help
//
// @date Tue Dec 15 16:23:30 MET 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
usage(void)
{
  present();
  cout << "\nusage ::\n\n"
       << "\t reflector "
       << " [ -@ paramfile ] "
       << " [ -h ] "
       << "\n\n or \n\n"
       << "\t reflector < paramfile"
       << "\n\n";
  exit(0);
}
//!@}


//!---------------------------------------------------------------------
// @name clean
//
// @desc basically, frees memory
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
clean(void)
{
  int i;

  // delete focals table

  delete [] ct_Focal;        
  ct_Focal = 0;

  // delete reflectivity table

  for (i=0; i<2; i++) {
    delete [] Reflectivity[i];
  }
  
  delete [] Reflectivity;    
  Reflectivity = 0;

  // delete mirrors' data table

  for (i=0; i<ct_NMirrors; i++) {
    delete [] ct_data[i];
  }
  
  delete [] ct_data;
  ct_data = 0;
}
//!@}


//!---------------------------------------------------------------------
// @name log                             
//                                   
// @desc function to send log information
//
// @var    funct  Name of the caller function
// @var    fmt    Format to be used (message)
// @var    ...    Other information to be shown
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
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
//!@}


//!---------------------------------------------------------------------
// @name logerr                             
//                                   
// @desc function to send log information to stderr
//
// @var    funct  Name of the caller function
// @var    fmt    Format to be used (message)
// @var    ...    Other information to be shown
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void
logerr(const char *funct, char *fmt, ...)
{
  va_list args;
  
  //  Display the name of the function that called error
  printf("[%s]: ", funct);
  
  // Display the remainder of the message
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}
//!@}


//!---------------------------------------------------------------------
// @name error                                                    
//                                                           
// @desc function to send an error message, and abort the program
//
// @var    funct  Name of the caller function
// @var    fmt    Format to be used (message)
// @var    ...    Other information to be shown
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
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
//!@}


//!---------------------------------------------------------------------
// @name makeOmega                                        
//                                                    
// @desc function to calculate the matrix Omega(theta,phi)
//
// @var    theta   Angle theta of the transformation 
// @var    phi     Angle phi of the transformation 
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void
makeOmega (float theta, float phi)
{
  static float ct, st, cp, sp;
  
  // shortcuts for cosine and sine of theta and phi
  ct = cos(theta);
  st = sin(theta);
  cp = cos(phi);
  sp = sin(phi);
  
  // save values in the array (see top of file)
  Omega[0][0] =  cp*ct;
  Omega[0][1] =  sp*ct; 
  Omega[0][2] = -st; 
     
  Omega[1][0] = -sp;
  Omega[1][1] =  cp;
  Omega[1][2] =  0;      

  Omega[2][0] =  cp*st;
  Omega[2][1] =  sp*st; 
  Omega[2][2] =  ct;         
}
//!@}


//!---------------------------------------------------------------------
// @name makeOmegaI                                         
//                                                      
// @desc function to calculate the matrix Omega-1(theta,phi)
//
// @var    theta   Angle theta of the transformation 
// @var    phi     Angle phi of the transformation 
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void
makeOmegaI(float theta, float phi)
{
  static float ct, st, cp, sp;
  
  // shortcuts for cosine and sine of theta and phi
  ct = cos(theta);
  st = sin(theta);
  cp = cos(phi);
  sp = sin(phi);
  
  // save values in the array (see top of file)
  OmegaI[0][0] =  cp*ct;
  OmegaI[0][1] = -sp;
  OmegaI[0][2] =  cp*st;     

  OmegaI[1][0] =  sp*ct;
  OmegaI[1][1] =  cp; 
  OmegaI[1][2] =  sp*st;

  OmegaI[2][0] = -st;   
  OmegaI[2][1] =  0; 
  OmegaI[2][2] =  ct;            
}
//!@}


//!---------------------------------------------------------------------
// @name applyMxv                                              
//                                                         
// @desc returns the vector v' such that v' = M x v
//
// @var    M       matrix of the transformation
// @var    v       vector to be multiplied
// @var    vi      resulting vector
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void
applyMxV(float M[3][3], float *V, float *Vp)
{
  Vp[0] = (M[0][0] * V[0] +
           M[0][1] * V[1] +
           M[0][2] * V[2]);
  Vp[1] = (M[1][0] * V[0] +
           M[1][1] * V[1] +
           M[1][2] * V[2]);
  Vp[2] = (M[2][0] * V[0] +
           M[2][1] * V[1] +
           M[2][2] * V[2]);
}
//!@}


//!---------------------------------------------------------------------
// @name read_ct_file           
//                          
// @desc read CT definition file
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void
read_ct_file(void)
{
  char line[LINE_MAX_LENGTH];    // line to get from the ctin
  char token[ITEM_MAX_LENGTH];   // a single token
  char fmirr[40];                // name of the binary version of mirrors data
  int i;                         // dummy counters

  if ( verbose >= VERBOSE_MINIMAL )
    log( "read_ct_file", "start.\n" );

  ifstream ctin ( get_ct_filename() );

  if ( ctin.bad() ) 
    error( "read_ct_file", 
           "Cannot open CT def. file: %s\n", get_ct_filename() );
  
  // loop till the "end" directive is reached

  while (!ctin.eof()) {          

    // get line from stdin

    ctin.getline(line, LINE_MAX_LENGTH);

    // look for each item at the beginning of the line

    for (i=0; i<=define_mirrors; i++) 
      if (strstr(line, CT_ITEM_NAMES[i]) == line)
        break;
    
    // if it is not a valid line, just ignore it

    if (i == define_mirrors+1) 
      continue;
    
    // case block for each directive

    switch ( i ) {

    case type:                // <type of telescope> (0:CT1 ¦ 1:MAGIC)
      
      // get focal distance

      sscanf(line, "%s %d", token, &ct_Type);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "<Type of Telescope>: %s\n", 
             ((ct_Type==0) ? "CT1" : "MAGIC") );

      break;

    case focal_distance:      // <focal distance> [cm]
      
      // get focal distance

      sscanf(line, "%s %f", token, &ct_Focal_mean);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "<Focal distance>: %f cm\n", ct_Focal_mean );

      break;

    case focal_std:           // s(focal distance) [cm]
      
      // get focal distance

      sscanf(line, "%s %f", token, &ct_Focal_std);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "s(Focal distance): %f cm\n", ct_Focal_std );

      break;

    case point_spread:        // <point spread> [cm]
      
      // get point spread

      sscanf(line, "%s %f", token, &ct_PSpread_mean);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "<Point spread>: %f cm\n", ct_PSpread_mean );

      break;

    case point_std:           // s(point spread) [cm]
      
      // get point spread

      sscanf(line, "%s %f", token, &ct_PSpread_std);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "s(Point spread): %f cm\n", ct_PSpread_std );

      break;

    case adjustment_dev:      // s(adjustment_dev) [cm]
      
      // get point spread

      sscanf(line, "%s %f", token, &ct_Adjustment_std);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "s(Adjustment): %f cm\n", ct_Adjustment_std );

      break;

    case black_spot:          // radius of the black spot in the center [cm]
      
      // get black spot radius

      sscanf(line, "%s %f", token, &ct_BlackSpot_rad);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "Radius of the black spots: %f cm\n", 
             ct_BlackSpot_rad);

      break;

    case r_mirror:            // radius of the mirrors [cm]
      
      // get radius of mirror

      sscanf(line, "%s %f", token, &ct_RMirror);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "Radii of the mirrors: %f cm\n", ct_RMirror );

      break;

    case n_mirrors:           // number of mirrors
      
      // get the name of the output_file from the line

      sscanf(line, "%s %d", token, &ct_NMirrors);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "Number of mirrors: %d\n", ct_NMirrors );

      break;

    case camera_width:        // camera width [cm]
      
      // get the name of the ct_file from the line

      sscanf(line, "%s %f", token, &ct_CameraWidth);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "Camera width: %f cm\n", ct_CameraWidth );

      break;

    case n_pixels:           // number of pixels
      
      // get the name of the output_file from the line

      sscanf(line, "%s %d", token, &ct_NPixels);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "Number of pixels: %d\n", ct_NPixels );

      break;

    case pixel_width:         // pixel width [cm]
      
      // get the name of the ct_file from the line

      sscanf(line, "%s %f", token, &ct_PixelWidth);

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "Pixel width: %f cm\n", ct_PixelWidth );

      break;

    case define_mirrors:      // read table with the parameters of the mirrors

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "Table of mirrors data:\n" );

      // check whether the number of mirrors was already set

      if ( ct_NMirrors == 0 )
        error( "read_ct_file", "NMirrors was not set.\n" );
      
      // allocate memory for paths list

      if ( verbose >= VERBOSE_MINIMAL )
        log( "read_ct_file", "Allocating memory for ct_data\n" );

      ct_data = new float*[ct_NMirrors];

      for (i=0; i<ct_NMirrors; i++) 
        ct_data[i] = new float[CT_NDATA];

      // read data

      // look for binary version of mirror data (faster to load)

      sprintf( fmirr, "%s.mirr", get_ct_filename() );

      ifstream ctmirr;
      ctmirr.open( fmirr );

      if ( ctmirr.bad() ) {

        // the file does not exist
        
        ctmirr.close(); // close not existing file

        ofstream ctmirr_write ( fmirr ); // open file to save the data

        if ( verbose >= VERBOSE_MINIMAL )
          log( "read_ct_file", "Reading mirrors data...\n" );

        // read ASCII data

        for (i=0; i<ct_NMirrors; i++) {
          //      for (j=0; j<CT_NDATA; j++) 

          ctin.getline(line, LINE_MAX_LENGTH);
          sscanf(line, "%f %f %f %f %f %f %f %f %f %f %f %f",
                 &ct_data[i][0],
                 &ct_data[i][1],
                 &ct_data[i][2],
                 &ct_data[i][3],
                 &ct_data[i][4],
                 &ct_data[i][5],
                 &ct_data[i][6],
                 &ct_data[i][7],
                 &ct_data[i][8],
                 &ct_data[i][9],
                 &ct_data[i][10],
                 &ct_data[i][11]);

          cout << '[' << i << ']' << flush;
        }
        cout << endl << flush;
        
        // save binary data and close output file
        
        for (i=0; i<ct_NMirrors; i++)
          ctmirr_write.write( (char*)ct_data[i], CT_NDATA*sizeof(float) ); 
        
        ctmirr_write.close();

      } else {

        // Excellent!
        // read binary data and close input file
        
        for (i=0; i<ct_NMirrors; i++)
          ctmirr.read( (char*)ct_data[i], CT_NDATA*sizeof(float) ); 

        ctmirr.close();

      }

      break;

    } // switch ( i ) 

  } // while (! is_end)

  // read reflectivity file

  read_reflectivity();

  // read axis deviations file

  read_axisdev();

  // read focals file

  read_focals(); 

  // end

  if ( verbose >= VERBOSE_MINIMAL )
    log( "read_ct_file", "done.\n" );
  return;
}
//!@}


//!---------------------------------------------------------------------
// @name dist_r_P                          
//                                     
// @desc distance straight line r - point P
// 
// @var a   coord. X of a fixed point of the straight line
// @var b   coord. Y of a fixed point of the straight line
// @var c   coord. Z of a fixed point of the straight line
// @var l   component X of a vector of the straight line
// @var m   component Y of a vector of the straight line
// @var n   component Z of a vector of the straight line
// @var x   coord. X of the point P
// @var y   coord. Y of the point P
// @var z   coord. Z of the point P
//
// @return  Returns the distance d(r,P) between the line and the point P
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
float 
dist_r_P(float a, float b, float c, 
         float l, float m, float n,
         float x, float y, float z)
{
  return (
          sqrt((SQR((a-x)*m-(b-y)*l) +
                SQR((b-y)*n-(c-z)*m) +
                SQR((c-z)*l-(a-x)*n))/
               (SQR(l)+SQR(m)+SQR(n))
               )
          );
}
//!@}


//!---------------------------------------------------------------------
// @name read_reflectivity                     
//                                         
// @desc read reflectivity data for the mirrors
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
read_reflectivity(void)
{
  ifstream reflfile;
  ofstream reflfile_write;
  int i, errors=0;
  char line[LINE_MAX_LENGTH];

  // try to open the file
  reflfile.open( REFLECTIVITY_FILE );
  
  // if it is wrong or does not exist:

  while ( reflfile.bad() ) {

    ++errors;

    if ( verbose >= VERBOSE_MINIMAL )
      log( "read_reflectivity", "Cannot open reflectivity file: %s\n", 
           REFLECTIVITY_FILE );

    if ( errors > 5 )
      error( "read_reflectivity", "Exiting.");

    // try to re-generate the file

    if ( verbose >= VERBOSE_MINIMAL )
      log( "read_reflectivity", "Generating file: %s\n", 
           REFLECTIVITY_FILE );

    // try to open the file
    reflfile_write.open( REFLECTIVITY_FILE );
  
    // write short comment in the beginning

    reflfile_write << "# reflectivity file" << endl
                   << "# reflectivity for each mirror in the frame" << endl
                   << "# J C Gonzalez, 1998" << endl
                   << "#" << endl;
    
    // write data
    
    // write number of data-points

    reflfile_write << "# number of datapoints" << endl
                   << 35 << endl;
    
    // write data-points

    reflfile_write << "# datapoints" << endl;

    for ( i=0; i<35; ++i ) 
      reflfile_write << 270.0+i*10.0 << ' ' <<  .9 << endl;
                  
    // close it and try to re-open

    reflfile_write.close();
    reflfile.close();
    reflfile.open( REFLECTIVITY_FILE );
  
  }
  
  if ( verbose >= VERBOSE_MINIMAL ) {
    if ( errors > 0 )
      log( "read_reflectivity", "Reading reflectivity file after %d fail%c\n", 
           errors, ((errors>1)?'s':' ') );
    else
      log( "read_reflectivity", "Reading reflectivity file . . .\n" );
  }

  // we set this to -1, in order to read in the first place the
  // number of data-points for the Reflectivity curve
  i = -1; 

  // scan the whole file

  while ( ! reflfile.eof() ) {          

    // get line from the file

    reflfile.getline(line, LINE_MAX_LENGTH);

    // skip if comment

    if ( *line == '#' )
      continue;

    // get the value

    if ( i < 0 ) {            // if it is the first number we read
      
      // read the number of datapoints

      sscanf(line, "%d", &nReflectivity);
      
      // allocate memory for Reflectivity table

      Reflectivity = new float * [2];
      Reflectivity[0] = new float[nReflectivity];
      Reflectivity[1] = new float[nReflectivity];

    } else {                  // if not, then it is a datapoint

      // get the datapoint (wavelength, reflectivity)

      sscanf(line, "%f %f", &Reflectivity[0][i], &Reflectivity[1][i]);

    }

    ++i;
        
  }

  // close it

  reflfile.close();
  
}
//!@}


//!---------------------------------------------------------------------
// @name read_axisdev                             
//                                            
// @desc read axis deviations data for the mirrors
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
read_axisdev(void)
{
  ifstream axisdevfile;
  ofstream axisdevfile_write;
  int i, errors=0;
  char line[LINE_MAX_LENGTH];

  // try to open the file
  axisdevfile.open( AXISDEVIATION_FILE );
  
  // if it is wrong or does not exist:

  while ( axisdevfile.bad() ) {

    ++errors;

    if ( verbose >= VERBOSE_MINIMAL )
      log( "read_axisdev", "Cannot open axis deviation file: %s\n", 
           AXISDEVIATION_FILE );

    if ( errors > 5 )
      error( "read_axisdev", "Exiting.");

    // try to re-generate the file

    if ( verbose >= VERBOSE_MINIMAL )
      log( "read_axisdev", "Generating file: %s\n", 
           AXISDEVIATION_FILE );

    // try to open the file
    axisdevfile_write.open( AXISDEVIATION_FILE );
  
    // write short comment in the beginning

    axisdevfile_write << "# axis deviation file" << endl
                      << "# axis deviation for each mirror" << endl
                      << "# J C Gonzalez, 1998" << endl
                      << "#" << endl;
    
    // write data
    
    for ( i=0; i<ct_NMirrors; ++i ) 
      axisdevfile_write << gennor(0.0,ct_Adjustment_std) 
                        << ' ' 
                        << gennor(0.0,ct_Adjustment_std) 
                        << endl;
    
    // close it and try to re-open

    axisdevfile_write.close();
    axisdevfile.close();
    axisdevfile.open( AXISDEVIATION_FILE );
  
  }
  
  if ( verbose >= VERBOSE_MINIMAL ) {
    if ( errors > 0 )
      log( "read_axisdev", "Reading axis deviation file after %d fail%c\n", 
           errors, ((errors>1)?'s':' ') );
    else
      log( "read_axisdev", "Reading axis deviation file . . .\n" );
  }

  //allocate memory for AxisDeviation table

  AxisDeviation = new float * [2];
  AxisDeviation[0] = new float[ct_NMirrors];
  AxisDeviation[1] = new float[ct_NMirrors];

  // scan the whole file

  i = 0; 

  while ( (! axisdevfile.eof()) && (i<ct_NMirrors) ) {          

    // get line from the file

    axisdevfile.getline(line, LINE_MAX_LENGTH);

    // skip if comment

    if ( *line == '#' )
      continue;

    // get the value (dx, dy)

    sscanf(line, "%f %f", &AxisDeviation[0][i], &AxisDeviation[1][i]);

    ++i;
    
  }

  // did i do it for all the mirrors ?
    
  if (i < ct_NMirrors) {
    error("read_axis_dev", "%s%s (%d < %d)\n",
          "Number of pairs in axisdev.dat file is",
          "\n\t\tsmaller than number of mirrors", i, ct_NMirrors);
  }
    
  // close it

  axisdevfile.close();
  
}
//!@}


//!---------------------------------------------------------------------
// @name read_focals                     
//                                   
// @desc read focals data for the mirrors
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void 
read_focals(void)
{
  ifstream focalfile;
  ofstream focalfile_write;
  int i, errors=0;
  char line[LINE_MAX_LENGTH];

  /*!@'
    
    Here we read the focals of each mirror, but only for CT1. For
    MAGIC we have already the Focals in the definition file. So, we
    copy these values to the |ct\_Focals| vector.

  */

  // >>>>>>>>>> MAGIC <<<<<<<<<<
  if ( ct_Type == 1) {

    // allocate memory for the focals table 
    ct_Focal = new float[ct_NMirrors];

    for (i=0; i<ct_NMirrors; ++i)
      ct_Focal[i] = ct_data[i][CT_FOCAL];
    
    return;

  }
  // ! >>>>>>>>>> MAGIC <<<<<<<<<<
  
  // try to open the file
  focalfile.open( FOCALS_FILE );
  
  // if it is wrong or does not exist:

  while ( focalfile.bad() ) {

    ++errors;

    if ( verbose >= VERBOSE_MINIMAL )
      log( "read_focals", "Cannot open focals file: %s\n", 
           FOCALS_FILE );

    if ( errors > 5 )
      error( "read_focals", "Exiting.");

    // try to re-generate the file

    if ( verbose >= VERBOSE_MINIMAL )
      log( "read_focals", "Generating file: %s\n", 
           FOCALS_FILE );

    // try to open the file
    focalfile_write.open( FOCALS_FILE );
  
    // write short comment in the beginning

    focalfile_write << "# focals file" << endl
                    << "# focals for each mirror in the frame" << endl
                    << "# J C Gonzalez, 1998" << endl;
        
    // write data

    rnormal( NormalRandomNumbers, ct_NMirrors+1 );
        
    for (i=0; i<ct_NMirrors; i++)
      focalfile_write << NormalRandomNumbers[i] * ct_Focal_std + ct_Focal_mean
                      << endl;
        
    // close it and try to re-open
        
    focalfile_write.close();
    focalfile.close();
    focalfile.open( FOCALS_FILE );
  
  }

  // allocate memory for the focals table

  ct_Focal = new float[ct_NMirrors]; 

  if ( verbose >= VERBOSE_MINIMAL ) {

    if ( errors > 0 )
      log( "read_focals", "Reading focals file after %d fail%c\n", 
           errors, ((errors>1) ? 's' : ' ') );

    else

      log( "read_focals", "Reading focals file . . .\n" );
  }

  // scan the whole file

  i = 0;

  while ( (! focalfile.eof()) && (i<ct_NMirrors) ) {          

    // get line from the file

    focalfile.getline(line, LINE_MAX_LENGTH);

    // skip if comment

    if ( *line == '#' )
      continue;

    // get the value

    sscanf(line, "%f", &ct_Focal[i]);

    ++i;
        
  }

  // did i do it for all the mirrors ?
    
  if (i < ct_NMirrors) {
    error("read_focals", "%s%s (%d < %d)\n",
          "Number of data in focals.dat file is",
          "\n\t\tsmaller than number of mirrors", i, ct_NMirrors);
  }
    
  // close it

  focalfile.close();
  
}
//!@}


//!---------------------------------------------------------------------
// @name rnormal                                   
//                                             
// @desc returns n(=2k) normaly distributed numbers
//
// @var *r   pointer to a vector where we write the numbers
// @var  n   how many numbers do we generate
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
void
rnormal(double *r, int n)
{

  double z1, z2;
  int i;

  for (i=0; i<n; i+=2) {

    z1 = RandomNumber;
    z2 = RandomNumber;
  
    r[i]   = sqrt(-2.0*log(z1)) * cos(2.0*M_PI*z2);
    r[i+1] = sqrt(-2.0*log(z1)) * sin(2.0*M_PI*z2);
        
  }

}
//!@}


//!---------------------------------------------------------------------
// @name isA                               
//                                             
// @desc returns TRUE(FALSE), if the flag is(is not) the given
//
// @var s1     String to be compared with s2
// @var s2     String in the form of a FLAG
//
// @return     TRUE: both strings match; FALSE: otherwise
//
// @date Wed Jul  8 15:25:39 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int 
isA( char * s1, const char * flag ) 
{
  return ( (strncmp((char *)s1, flag, 8)==0) ? TRUE : FALSE );
}
//!@}


//!---------------------------------------------------------------------
// @name Curv2Lin                             
//                                             
// @desc Curvilinear to Linear (Euclidean) distance
//
// @var s      Curvilinear distance over the parabolic shape
//
// @return     Radial distance from the axis of the paraboloid
//
// @date Wed Jul  8 15:25:39 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
float 
Curv2Lin(float s)
{
  float x;
  short i;

  x = s;
  for (i = 0; i < 4; i++)
    x = (s / 100.) / (1. + (float) 0.000144175317185 * x * x);

  return (x*100.);
}
//!@}


//!---------------------------------------------------------------------
// @name Lin2Curv                        
//                                             
// @desc Linear (Euclidean) to Curvilinear distance
//
// @var x      Radial distance from the axis of the paraboloid
//
// @return     Curvilinear distance over the parabolic shape
//
// @date Wed Jul  8 15:25:39 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
float 
Lin2Curv(float x)
{
  x /= 100.;
  return ((x + (float) 0.000144175317185 * x * x * x)*100.);
}
//!@}


//!---------------------------------------------------------------------
// @name get_stdin_files                       
//                                             
// @desc get data from stdin and create cerXXXXXX and staXXXXXX
//
// @var ncerf  number of next Cherenkov file cerXXXXXX to be created
// @var El     Lower limit for requested energy
// @var Eu     Upper limit for requested energy
// @var flag   Boolean flag: TRUE: initialize; FALSE: normal
//
// @return     TRUE: succesful;  FALSE: failure
//
// @date Tue Dec 15 15:03:40 MET 1998
//----------------------------------------------------------------------
// @function

//!@{ 
int 
get_stdin_files(int ncerf, float El, float Eu, int flag)
{
  static float *buffer;       // buffer for read-out of STDIN
  static COREventHeader *evth;// Event Header class (from CORSIKA)
  static float Elow, Eup;     // range for Energy
  static int reject;

  char *chkbuffer;

  char cername[256];          // output filename
  ofstream cerfile;           // output file (stream)
  char staname[256];          // output filename sta
  ofstream stafile;           // output file sta (stream)
  int bytes;
  float energy;

  /*!@'
    
    The main features of this function, as well as the algorithm,
    are taken from |corfilter|.

  */

  // if it's the first time, allocate buffers

  if ( flag == TRUE ) {

    if (verbose)
      log("get_stdin_files", "Allocating memory for buffer...\n");
    
    buffer = new float [BUFFER_LENGTH];
    evth = (COREventHeader*)buffer;

    Elow = El;
    Eup  = Eu;

    return ( TRUE ); 
  }

  // main loop
  
  while ( ! cin.eof() ) {

    // read buffer from STDIN

    cin.read( (char*) buffer, BUFFER_LENGTH * sizeof(float) );
    bytes = cin.gcount();

    if (bytes < (BUFFER_LENGTH * sizeof(float))) {
      if (verbose) 
        logerr("get_stdin_files", 
               "Can read only %d bytes, instead of required %d.\n",
               bytes, BUFFER_LENGTH * sizeof(float));
    }

    if (bytes == 0) 
      return ( FALSE );

    // check whether EVTH is there

    chkbuffer = (char*)buffer;
    if (strstr(chkbuffer,EVTH) == 0) {
      if (verbose) 
        logerr("get_stdin_files", "EVTH not found in this block\n");
      continue;
    } else {
      if (verbose) 
        logerr("get_stdin_files", "EVTH was FOUND in this block!!\n");
    }


    // show log
    if (verbose)
      logerr("get_stdin_files", " (E=%8.1f) Reading event %d... ",
             (float)evth->Etotal, (int)evth->EvtNumber);
    
    // CRITERIA
    energy = evth->Etotal;
    
    if ( (energy<Elow) || (energy>Eup) ) {
      if (verbose)
        log("REJECTED", "\n");
      reject = TRUE;        
    } else {                
      if (verbose)          
        log("ACCEPTED", "\n");
      reject = FALSE;
    }

    if ( !reject ) {

      // update no. of cer. file
      sprintf(cername, "%s/cer%06d", TMP_STDIN_DIR, ncerf+1);
      
      cerfile.open( cername );
      
      if ( verbose )
        log("get_stdin_files", " Writing event %d, will be %s\n",
            (int)evth->EvtNumber, cername);

      // write it
      cerfile.write( (char*)buffer, BUFFER_LENGTH * sizeof(float) );
      
    }

    // read blocks till new EVTH is found 
    // (now must be at the beginning of a block)

    if (verbose)
      log("get_stdin_files", " Reading photons...\n");

    bytes = 0;
    while ( TRUE ) {
      
      // read photon
      cin.read( (char*)buffer, 7 * sizeof(float) );

      // the last photon is reached when the EVTH is read, but
      // only when bytes % SIZE_OF_BLOCK == 0 
      // SIZE_OF_BLOCK = 22932
      // with this, we only execute strstr once each 819 photons

      // are we in a new block?

      if ((bytes % SIZE_OF_BLOCK) == 0) {
        // is this the last photon?
        if ( strstr(chkbuffer, EVTH) == chkbuffer )
          break;
      }

      // if not, write it to the cer-file
      if ( !reject ) 
        cerfile.write( (char*)buffer, 7 * sizeof(float) );

      bytes += 7 * sizeof(float);

    }

    if (verbose)
      log("get_stdin_files", " Finished: %d bytes\n", bytes);

    if ( !reject ) {
      // close cer-file
      cerfile.close();
    }
    
    // at this point, we must be reading sta-file
    
    // read the rest of the sta-file
    if (verbose)
      log("get_stdin_files", " Reading sta-data...\n");
    cin.read( (char*)buffer + 7*sizeof(float), 5956 - 7*sizeof(float));
    
    if ( !reject ) {

      // log
      if (verbose)
        log("get_stdin_files", " Saving sta-file...\n");
      
      // open file
      sprintf(staname, "%s/sta%06d", TMP_STDIN_DIR, ncerf+1);
      stafile.open( staname );
      
      // write it
      stafile.write( (char*)buffer, 5956 );
      
      // close sta-file
      stafile.close();

      // get out from here, return to main program
      return( TRUE );
      
    }

  } 

  return( TRUE );

}
//!@}


//!---------------------------------------------------------------------
// @name get_new_ct_pointing
//
// @desc returns new random position of CT around a given one
//
// @var theta     Theta (ZA) of the shower
// @var phi       Phi (AZ) of the shower
// @var range     Maximum allowed distance between CT and shower axis
// @var newtheta  Resulting Theta of the CT
// @var newphi    Resulting Phi of the CT
//
// @return        Angular distance between orig. and new directions
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
// @function

//!@{ 
static float
get_new_ct_pointing(float theta, float phi, float minang, float maxang,
                    float *newtheta, float *newphi,
                    int isotropic)
{
  static float distance;
  static float it, ip;
  static float sin_theta, cos_theta;
  static float sin_newtheta, cos_newtheta;
  static float sin_iphi, cos_iphi;
  static float iphi;
  static float range;

  // for the moment, we only simulate an uniform distribution,
  // since our theta distribution in the generation of events is
  // already uniform for hadrons, which are the main targets for
  // using this option

  range = maxang - minang;
  
  if (isotropic == TRUE)
    // :TODO:
    // hey, still I have to modify this to do it from angle minang to maxang!!
    it = acos(cos(range) + RandomNumber * (1 - cos(range)));
  else
    it = RandomNumber * range + minang;
  
  ip = RandomNumber * 2.0 * M_PI;
  
  if ( theta == 0.0 ) {

    *newtheta = it;
    *newphi = ip;

  } else {
    
    sin_theta = sin(theta);
    cos_theta = cos(theta);

    cos_newtheta = cos_theta*cos(it) + sin_theta*sin(it)*cos(ip);
    *newtheta = acos( cos_newtheta );
    sin_newtheta = sin( *newtheta );
    
    sin_iphi = sin(it)*sin(ip) / sin_newtheta;
    cos_iphi = (( cos(it) - cos_newtheta * cos_theta ) /
                ( sin_newtheta * sin_theta ));
    
    iphi = atan2( sin_iphi, cos_iphi );
    
    *newphi = phi + iphi;

  }

  return( it );
}
//!@}


//=------------------------------------------------------------
//!@subsection Log of this file.

//!@{
//
// $Log$
// Revision 1.22  2000/03/22  15:56:42  gonzalez
// *** empty log message ***
//
// Revision 1.21  2000/01/30  10:20:42  gonzalez
// Bug detected in uniformity of get_new_ct_pointing generated new directions: solved
//
// Revision 1.20  2000/01/27  10:47:54  gonzalez
// JAN2000-STABLE
//
// Revision 1.19  1999/11/19  20:52:31  gonzalez
// *** empty log message ***
//
// Revision 1.18  1999/10/05  11:11:12  gonzalez
// Sep.1999
//
// Revision 1.17  1999/03/24  16:33:01  gonzalez
// REFLECTOR 1.1: Release
//
// Revision 1.16  1999/01/21  16:03:44  gonzalez
// Only small modifications
//
// Revision 1.15  1999/01/19  18:07:16  gonzalez
// Bugs in STDIN-STDOUT version corrected.
//
// Revision 1.14  1999/01/14  17:35:47  gonzalez
// Both reading from STDIN (data_from_stdin) and
// writing to STDOUT (data_to_STDOUT) working.
//
// Revision 1.13  1999/01/13  12:41:12  gonzalez
// THIS IS A WORKING (last?) VERSION
//
// Revision 1.12  1998/12/17  16:26:09  gonzalez
// *************************************************
// *************************************************
//    WARNING:: Version 1.11 is completly wrong!!
// *************************************************
// *************************************************
//
// Revision 1.10  1998/12/15  10:47:22  gonzalez
// RELEASE-1.0
//
// Revision 1.9  1998/11/25  16:30:47  gonzalez
// Commit after inclusion of 'Blocking'
//
//!@}

//=EOF
