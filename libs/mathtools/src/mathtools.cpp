/******************************************************************************
 * File:    Reflector.cpp
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.reflector
 *
 * Version: 0.3
 *
 * Date:    2018/11/13
 *
 * Author:  J C Gonzalez
 *
 * Copyright (C) 2015-2018 by J C Gonzalez
 *_____________________________________________________________________________
 *
 * Topic: General Information
 *
 * Purpose:
 *   Implement ServiceMng class
 *
 * Created by:
 *   J C Gonzalez
 *
 * Status:
 *   Prototype
 *
 * Dependencies:
 *   none
 *
 * Files read / modified:
 *   none
 *
 * History:
 *   See <Changelog>
 *
 * About: License Conditions
 *   See <License>
 *
 ******************************************************************************/

#include "mathtools.h"

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

namespace MathTools {

template<typename T>
T sqr(T x) { return x * x; }

template<typename T>
T d2r(T x) { return x * M_PI / 180.; }

template<typename T>
T r2d(T x) { return x * 180. / M_PI; }

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
template<typename T>
void makeOmega(T (& Omega)[3][3] , T theta, T phi)
{
    static T ct, st, cp, sp;
  
    // shortcuts for cosine and sine of theta and phi
    ct = cos(theta);
    st = sin(theta);
    cp = cos(phi);
    sp = sin(phi);
    
    // save values in the array (see top of file)
    Omega[0][0] =  cp * ct;
    Omega[0][1] =  sp * ct; 
    Omega[0][2] = -st; 
    
    Omega[1][0] = -sp;
    Omega[1][1] =  cp;
    Omega[1][2] =  0;      
    
    Omega[2][0] =  cp * st;
    Omega[2][1] =  sp * st; 
    Omega[2][2] =  ct;         
}


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
template<typename T>
void makeOmegaI(T (& OmegaI)[3][3], T theta, T phi)
{
    static T ct, st, cp, sp;
  
    // shortcuts for cosine and sine of theta and phi
    ct = cos(theta);
    st = sin(theta);
    cp = cos(phi);
    sp = sin(phi);
    
    // save values in the array (see top of file)
    OmegaI[0][0] =  cp * ct;
    OmegaI[0][1] = -sp;
    OmegaI[0][2] =  cp * st;     
    
    OmegaI[1][0] =  sp * ct;
    OmegaI[1][1] =  cp; 
    OmegaI[1][2] =  sp * st;
    
    OmegaI[2][0] = -st;   
    OmegaI[2][1] =  0; 
    OmegaI[2][2] =  ct;            
}


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
template<typename T>
void applyMxV(T M[3][3], T *V, T *Vp)
{
    Vp[0] = ((M[0][0] * V[0]) +  (M[0][1] * V[1]) +  (M[0][2] * V[2]));
    Vp[1] = ((M[1][0] * V[0]) +  (M[1][1] * V[1]) +  (M[1][2] * V[2]));
    Vp[2] = ((M[2][0] * V[0]) +  (M[2][1] * V[1]) +  (M[2][2] * V[2]));
}


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
template<typename T>
T dist_r_P(T a, T b, T c,
           T l, T m, T n,
           T x, T y, T z)
{
    return ( sqrt((sqr<T>(((a - x) * m) - ((b - y) * l)) +
                   sqr<T>(((b - y) * n) - ((c - z) * m)) +
                   sqr<T>(((c - z) * l) - ((a - x) * n))) /
                  (sqr<T>(l) + sqr<T>(m) + sqr<T>(n)))
            );
}


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
template<typename T>
void rnormal(T *r, int n)
{
    T z1, z2;
    
    for (int i = 0; i < n; i+=2) {
        z1 = RandomNumber;
        z2 = RandomNumber;   
        r[i]   = sqrt(-2.0 * log(z1)) * cos(2.0 * M_PI * z2);
        r[i+1] = sqrt(-2.0 * log(z1)) * sin(2.0 * M_PI * z2);        
    }
}


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
float
Lin2Curv(float x)
{
  x /= 100.;
  return ((x + (float) 0.000144175317185 * x * x * x)*100.);
}


    // Explicit instantiations

    template float sqr<float>(float x);
    template float d2r<float>(float x);
    template float r2d<float>(float x);

    template void makeOmega<float>(float (& Omega)[3][3], float theta, float phi);
    template void makeOmegaI<float>(float (& OmegaI)[3][3], float theta, float phi);
    template void applyMxV<float>(float M[3][3], float *V, float *Vp);

    template float dist_r_P<float>(float a, float b, float c,
                                   float l, float m, float n,
                                   float x, float y, float z);

    template void rnormal<float>(float *r, int n);

    //-------------------------

    template double sqr<double>(double x);
    template double d2r<double>(double x);
    template double r2d<double>(double x);

    template void makeOmega<double>(double (& Omega)[3][3] , double theta, double phi);
    template void makeOmegaI<double>(double (& OmegaI)[3][3], double theta, double phi);
    template void applyMxV<double>(double M[3][3], double *V, double *Vp);

    template double dist_r_P<double>(double a, double b, double c,
                                     double l, double m, double n,
                                     double x, double y, double z);

    template void rnormal<double>(double *r, int n);

    //---------------------

    template int sqr<int>(int x);

} // namespace MathTools

//}
