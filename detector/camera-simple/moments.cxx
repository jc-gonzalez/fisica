//=//////////////////////////////////////////////////////////////////////
//=
//= moments            
//=
//= @file        moments.cxx
//= @desc        Calculation of image parameters
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

//# Source code of |moments.cxx|.

/*"

  This section describes briefly the source code for the file
  |moments.cxx|. All the defines it uses are located in the file
  |moments.h|.

**/

//## Includes and Global variables definition.

/*" 
  All the defines are located in the file |moments.h|.
**/
 
//{
#include "moments.h"

#define __DEBUG__
#undef  __DEBUG__
//}

//## Definition of global variables.

//{

static int npix;                 //@< number of pixels
static float *q;                 //@< charges in the pixels
static float xm, ym;             //@< centroid (used in moments and lenwid)

//@: structure with information about the image
static Moments_Info m;           

//@: structure with information about islands
static Islands_Info is;          

//@: structure with information about lenwid
static LenWid_Info  lw;          

//}

//## The function |moments()|.

//!-----------------------------------------------------------
// @name moments               
//                                             
// @desc calculate moments on the image
//
// @var  n           Number of pixels
// @var  *image      Vector of ph.e.s in pixels
// @var  **pix       Array with information about the pixels
// @var  plateScale  Plate scale for the CT in use
// @var  flag        1: initialize; other: normal 
//
// @return           Pointer to structure Moments_Info
//
// @date Mon Sep 14 15:22:44 MET DST 1998
//------------------------------------------------------------
// @function

//{
Moments_Info * 
moments( int n, float *image, float **pix, 
         float plateScale, int flag,
         int **pixneig, int *npixneig )
{
  register int i, j, k;

  float x, y;
  float x2m, xym, y2m;
  float x3m, x2ym, xy2m, y3m;
  float zz, zd, zu, zv, d, a, b;
  float ax, ay, unitx, unity, sigmaax;
  float sx2, sxy, sy2;
  float sx3, sx2y, sxy2, sy3;
  
  int (*compare) (const void *, const void *);

  if ( flag == 1 ) {
    q = new float[n];
    is.fi = new float[n];
    is.vislands = new float[n];
    is.islands = new int[n];
    is.isl = new int[n];
    for (i=1; i<n; ++i) {
      q[i] = is.fi[i] = is.vislands[i] = 0.0;
      is.islands[i] = is.isl[i] = 0;
    }
    return &m;
  } else {
    memcpy( q, image, sizeof(float) * n );
    /*
      for (i=1; i<n; ++i) 
      cout << q[i] << '\n';
      cout << endl << flush;
    */
  }

  // save number of pixels
  npix = n;
  
  // calculate sum of values
  xm = ym = 0.0;
  x2m = xym = y2m = 0.0;
  x3m = x2ym = xy2m = y3m = 0.0;
  m.charge = 0.0;
  
  for (i=0; i<npix; ++i)
    if ( q[i] > 0.0 ) {
      x = pix[i][0] * plateScale;
      y = pix[i][1] * plateScale;
      xm += x * q[i];
      ym += y * q[i];
      x2m += x * x * q[i];
      xym += x * y * q[i];
      y2m += y * y * q[i];
      x3m  += x * x * x * q[i];
      x2ym += x * x * y * q[i];
      xy2m += x * y * y * q[i]; 
      y3m  += y * y * y * q[i];
      m.charge += q[i];
    }

  
  //++++++++++++++++++++++++++++++++++++++++++++++++++ 
  // extremes and charges
  //--------------------------------------------------

  /*
   * The following block has been replaced by a new
   * algorithm (below), which takes as maximum position
   * the charged-weighted position of the maximum and all
   * its neighbors
   */

  //---- old routine ----------------------------------------
  //   for (i=0; i<10; ++i) 
  //     m.maxs[i] = 0.0;
  //
  //   for (i=0; i<npix; ++i) {
  //     if ( q[i] > m.maxs[0] ) {
  //       for (k=9; k>0; --k)
  //         m.maxs[k] = m.maxs[k-1];
  //       for (k=9; k>0; --k)
  //         m.nmaxs[k] = m.nmaxs[k-1];
  //       m.maxs[0] = q[i];
  //       m.nmaxs[0] = i;
  //     }
  //   }
  //
  //   // calculates weighted position of the maximum (6 pixels)
  //
  //   m.xmax = m.ymax = m.smax = 0.0;
  //
  //   for (i=0; i<6; ++i) {
  //     m.xmax += pix[m.nmaxs[i]][0] * q[m.nmaxs[i]];
  //     m.ymax += pix[m.nmaxs[i]][1] * q[m.nmaxs[i]];
  //     m.smax += q[m.nmaxs[i]];
  //   }

  //---- end of old routine ----------------------------------


  //---- new routine ----------------------------------------
  
  for (i=0; i<10; ++i) 
    m.maxs[i] = 0.0;
  
  for (i=0; i<397; ++i) {
    if ( q[i] > m.maxs[0] ) {
      for (k=9; k>0; --k)
        m.maxs[k] = m.maxs[k-1];
      for (k=9; k>0; --k)
        m.nmaxs[k] = m.nmaxs[k-1];
      m.maxs[0] = q[i];
      m.nmaxs[0] = i;
    }
  }
  
  // calculates weighted position of the maximum (6 pixels)

  m.xmax = m.ymax = m.smax = 0.0;

  for (i=0; i<npixneig[m.nmaxs[0]]; ++i) {
    j = pixneig[m.nmaxs[0]][i];
    if (j<npix) {
      m.xmax += pix[j][0] * q[j];
      m.ymax += pix[j][1] * q[j];
      m.smax += q[j];
    }
  }

  //---- end of new routine ----------------------------------
  
  if (m.smax==0.) m.smax=1.;
  if (m.charge==0.) m.charge=1.;

  m.xmax = m.xmax * plateScale / m.smax;
  m.ymax = m.ymax * plateScale / m.smax;

  // calculate concentrations with 2,3,4...10 pixels

  m.conc[0] = q[ m.nmaxs[0] ] + q[ m.nmaxs[1] ];

  for (i=2; i<10; ++i) 
    m.conc[i-1] = m.conc[i-2] + q[ m.nmaxs[i] ];

  for (i=0; i<9; ++i) 
    m.conc[i] /= m.charge;  

  
  //++++++++++++++++++++++++++++++++++++++++++++++++++ 
  // 1st moments
  //--------------------------------------------------

  xm /= m.charge;
  ym /= m.charge;

  m.m1x = xm;
  m.m1y = ym;

  
  //++++++++++++++++++++++++++++++++++++++++++++++++++
  // 2nd moments
  //--------------------------------------------------

  x2m /= m.charge;
  xym /= m.charge;
  y2m /= m.charge;

  // around the origin
  m.m2xx = x2m;
  m.m2xy = xym;
  m.m2yy = y2m;
  
  // around the mean
  sx2 = x2m - SQR(xm);
  sxy = xym - xm * ym;
  sy2 = y2m - SQR(ym);

  m.m2cxx = sx2;
  m.m2cxy = sxy;
  m.m2cyy = sy2;

  
  //++++++++++++++++++++++++++++++++++++++++++++++++++
  // 3rd moments
  //--------------------------------------------------

  x3m  /= m.charge;
  x2ym /= m.charge;
  xy2m /= m.charge;
  y3m  /= m.charge;

  // around the origin
  m.m3xxx = x3m;
  m.m3xxy = x2ym;
  m.m3xyy = xy2m;
  m.m3yyy = y3m;

  // around the mean
  sx3  = x3m  - 3 * x2m * xm + 2 * xm * xm * xm;
  sx2y = x2ym - 2 * xym * xm + 2 * xm * xm * ym - x2m * ym;
  sxy2 = xy2m - 2 * xym * ym + 2 * xm * ym * ym - y2m * xm;
  sy3  = y3m  - 3 * y2m * ym + 2 * ym * ym * ym;

  m.m3cxxx = x3m;
  m.m3cxxy = x2ym;
  m.m3cxyy = xy2m;
  m.m3cyyy = y3m;

  
  //++++++++++++++++++++++++++++++++++++++++++++++++++ 
  // hillas' parameters
  //--------------------------------------------------

  zd = sy2 - sx2;
  zz = sqrt( SQR(zd) + 4.*SQR( sxy ));;
  if ( (zz < 1.e-6) || (sxy == 0.) ) {
    m.dist = -1.;
    return &m;
  }
  zu = 1.0 + zd / zz;
  zv = 2.0 - zu;

  
  m.length = sqrt( fabs(sx2 + sy2 + zz) / 2. );
  m.width  = sqrt( fabs(sx2 + sy2 - zz) / 2. );
  m.dist   = sqrt( SQR(xm) + SQR(ym) );
  m.xdist  = sqrt( SQR(m.xmax) + SQR(m.ymax) );
  m.azw    = sqrt( fabs( SQR(xm)*y2m - 2.*xm*ym*xym + x2m*SQR(ym) ) ) / m.dist;
  m.miss   = sqrt( fabs( (SQR(xm)*zu + SQR(ym)*zv)/2. - (2.*sxy*xm*ym/zz) ) );
  m.alpha  = DEG( asin( m.miss/m.dist ) );
  m.beta   = atan(zz / ( 2. * sxy ));
 

  //++++++++++++++++++++++++++++++++++++++++++++++++++
  // asymetry
  //--------------------------------------------------
  
  unitx = sqrt(0.5*zv);
  unity = SGN( sxy )*sqrt(0.5*zu);

  if ( m.xdist > 0.0 ) {

    m.phi = acos((unitx*m.xmax + unity*m.ymax )/m.xdist);
  
    sigmaax = sx3*CUB(cos(m.phi)) + 3.0*sx2y*SQR(cos(m.phi))*sin(m.phi) +
      3.0*sxy2*cos(m.phi)*SQR(sin(m.phi)) + sy3*CUB(sin(m.phi));
    sigmaax = pow(fabs(sigmaax),0.3333333)*SGN(sigmaax);
    
    ax = sigmaax*unitx;
    ay = sigmaax*unity;
    m.asymx = (ax*m.xmax + ay*m.ymax)/(m.xdist*m.length*cos(m.phi));
    m.asymy = 0.0;

  } else {

    m.phi=-1000.0;
    m.asymx = -1000.0;
    m.asymy = -1000.0;

  }

  return &m;
}
//}

// @T \newpage

//## The function |islands()|.

//!-----------------------------------------------------------
// @name islands
//                                             
// @desc implementation of the "islands" algorithm
//
// @var  n             Number of pixels
// @var  f             Vector with the image (ph.e.s in pixels)
// @var  **pixneig     Array with indices of neighbour pixels
// @var  *npixneig     Vector with number of neighbours per pixel
// @var  cleanning     TRUE: remove spurious islands
// @var  ipixcut       Islands number cut
//
// @return           Pointer to structure Islands_Info
//
// @date Mon Sep 14 15:22:44 MET DST 1998
//------------------------------------------------------------
// @function

//{
Islands_Info *
islands( int n, float *f, int **pixneig, int *npixneig,
         int cleanning, int ipixcut)
{ 
  int i, j, k;
  int haschanged;
  int norder;
  int main_island = 0;
  int npix_main_island = 0;
 
  is.numisl = 0;

  memcpy( is.fi, f, sizeof(float) * n );

  // be aware: here we use the side effect of ++
  // there are two possibilities of using the operator ++:
  // 1) a = ++i  => is.first increments i, then evaluates expresion
  // 2) a = i++  => is.first evaluates expresion, then increments i
  // we INTENTIONALLY use the second form

  // ------------------------------------------------------------
  // ALGORITHM TO ISOLATE/DETECT ISLANDS
  // ------------------------------------------------------------
  // Copyleft (c) J.C.Gonzalez 1996-2000
  // Adapted from an algorithm designed by J.C.Fabero,
  //   Depto.Automatica, Univ.Complutense de Madrid
  // Thanks.
  // ------------------------------------------------------------

  // is.fi[i]       : charge in pixel i
  // is.isl[i]      : ordinal number for pixel i (or 0 if charge=0)
  // is.islands[k]  : number of pixels in island marked with number k
  // is.vislands[k] : total charge in island marked with number k
  // haschanged     : flag to detect a change in array 


  // (I) Numerate pixels with charge > 0

  j=1;
  for (i=0; i<n; ++i) 
    if ( is.fi[i]>0.0 ) {
      is.isl[i] = j;
      ++j;
    } else {
      is.isl[i] = 0;
    } 

  // (II) Give to all the pixels in one island the same number

  // stop when there are no changes in array

  haschanged = TRUE;
  while ( haschanged ) {

    haschanged = FALSE;

    // loop over all the pixels

    for (i=0; i<n; ++i) {
      
#ifdef __DEBUG__
      cout << "\nMOMENTS: " << i << " : ";
#endif // __DEBUG__

      // check only cells with order number > 0 (charge!=0)

      if ( is.isl[i] > 0 ) {

        // run over all the neighbours of the pixel i

        for (j=0; (j<npixneig[i]) && (pixneig[i][j]>-1); ++j) {
          
          // k is the order number of the j-th neighbour of pixel i
          k = pixneig[i][j];
          norder = is.isl[k];

#ifdef __DEBUG__
          cout << j << '[' << k << '{' << norder << "}] ";
#endif // __DEBUG__

          if ( norder > 0 ) {
            if ( is.isl[i] > is.isl[k] ) {
              is.isl[i] = is.isl[k];
              haschanged=TRUE;
            } 
          }
          
        }
      }
    }
  }
  
  // (III) Count num.pixels and sum charges for each island

  // initialize is.islands and is.vislands

  for (i=0; i<n; ++i)
    is.islands[i] = 0;

  for (i=0; i<n; ++i)
    is.vislands[i] = 0.0;

  // count islands

  for (i=0; i<n; ++i)
    if (is.isl[i]>0) {
      is.islands[is.isl[i]]++;
      is.vislands[is.isl[i]] += is.fi[i];
    }
  
  // (IV) Count islands

  for (i=0,j=0,is.numisl=0; i<n; ++i) {
    
    if ( is.islands[i] > 0) {
      j++;  

#ifdef __DEBUG__
      cout << '#' << j << ':' << is.islands[i] 
           << "  q=" << is.vislands[i]
           << endl;
#endif // __DEBUG__
      
      if (is.islands[i] > ipixcut)
        is.numisl++;
    }
    
  }

  // (V) Clean if required

#ifdef __DEBUG__
  cout << endl;
  for (i=0; i<n; ++i)
    cout << f[i] << ' ';
  cout << endl << flush;
#endif // __DEBUG__

  if ( cleanning ) {
    
    // cleanning image: pixcut = K 
    //   (any is.island with <=K pixels is removed

    for (i=0; i<n; ++i) {
      if ( is.islands[is.isl[i]] <= ipixcut ) 
        f[i] = 0.0;  
    }

    // removing every island but the main one
    
    for (i=0; i<n; ++i) {
      if ( is.islands[is.isl[i]] > npix_main_island ) {
        main_island = is.isl[i];
        npix_main_island = is.islands[ main_island ];
      } 
#ifdef __DEBUG__
      cout << main_island << ' ' << npix_main_island << ' '
           << ipixcut << endl << flush;
#endif // __DEBUG__
    }

    for (i=0; i<n; ++i) {
      if ( is.isl[i] != main_island )
        f[i] = 0.0;
    }
    
  }
  
#ifdef __DEBUG__
  for (i=0; i<n; ++i)
    cout << f[i] << ' ';
  cout << endl << flush;
#endif // __DEBUG__

  return &is;
}
//}


//## The function |lenwid()|.

//!-----------------------------------------------------------
// @name lenwid
//                                             
// @desc calculation of extended length and width params.
//
// @var  n           Number of pixels
// @var  *image      Vector of ph.e.s in pixels
// @var  **pix       Array with information about the pixels
// @var  plateScale  Plate scale for the CT in use
// @var  flag        1: initialize; other: normal 
//
// @return           Pointer to structure LenWid_Info
//
// @date Mon Sep 14 15:22:44 MET DST 1998
//------------------------------------------------------------
// @function

//{
LenWid_Info * 
lenwid( int n, float *image, float **pix, 
        float plateScale,
        float max_distance)
{
  register int i, j, k;
  float chi, phi;
  float cp, sp;
  float px1[2], px2[2], py1[2], py2[2];
  float x1, x2, y1, y2;
  int sign_of_dist;
  float a, b, c, d;
  float dist_to_axis, dist_to_axis2;
  float x, y;
  float wsum[4];
  float sum[4];
  float weight;
  float alpha;
  float fr, farea, tosum;

  static float radius;
  static float radius2;
  static int   bfirstcall = TRUE;

  float xcenter;
  float ycenter;
  
  // vector<float> lwvec[4];
  
  // calculate the radius of a circle with the same area of a pixel
  
  if ( bfirstcall == TRUE ) {

    radius2 = max_distance*max_distance*cos(DEG30)*3.0 / M_PI;
    radius = sqrt(radius2);

    radius *= plateScale;
    radius2 = SQR(radius);

    bfirstcall = FALSE;

  }
  
  /* @comment
     We have now an image in the camera. In this image we have
     defined two axes, Xe and Ye. Given the definition of alpha,
     we define phi, which is the angle of the rotation that should
     be applied to the original axis X and Y to get, together with
     a translation to the point (xm, ym), the new axes Xe and Ye.
     @endcomment */

  phi = m.beta;

  /* If the angle is phi, the rotation will be:
   *           / cos(phi)   sin(phi)\
   *  R(phi) = |                    |
   *           \-sin(phi)   cos(phi)/ 
   */

  cp = cos(phi);
  sp = sin(phi);

  /* The reference points for each axis will be px1,px2 and py1,py2
     We obtain these points by rotation and translation of the
     points [+-1000,0] and [0,+-1000] */

  /* Note! The rotation has to be R(-phi) */

  /*
   * Note! In order to use the center of mass, use (xm, ym)
   *       In order to use the maximum, use (m.xmax, m.ymax)
   */

  xcenter = xm;
  ycenter = ym;

  //xcenter = m.xmax;
  //ycenter = m.ymax;
  
  px1[0] =  cp*1000 + xcenter;
  px1[1] =  sp*1000 + ycenter;
  
  px2[0] = -cp*1000 + xcenter;
  px2[1] = -sp*1000 + ycenter;
  
  py1[0] = -sp*1000 + xcenter;
  py1[1] =  cp*1000 + ycenter;
  
  py2[0] =  sp*1000 + xcenter;
  py2[1] = -cp*1000 + ycenter;

  /* Now we have finally two points for each of the axes.
     We can now do, for each axis, and for each semi-plane
     it defines, the loop over the pixels */

  // Note that the possible values for sign_of_semiplane in the
  // next loops are precisely -1 and +1 

  for (i=0; i<4; ++i) {
    wsum[i] = 0.;
    sum[i] = 0.;
  }
  
  // first with the X, then with the Y

  for (k=1; k<3; ++k) {
    
    if ( k == 1) {
      x1 = px1[0];
      y1 = px1[1];
      x2 = px2[0];
      y2 = px2[1];
    } else {
      x1 = py1[0];
      y1 = py1[1];
      x2 = py2[0];
      y2 = py2[1];
    }
    
    // loop on pixels

    for ( i=0; i<n; ++i ) {
        
      weight = (float)image[i];
      if (weight < 1.0e-4)
        continue;

      // let's calculate the distance between the point and the axis
      
      x = pix[i][0] * plateScale;
      y = pix[i][1] * plateScale;
      
      a = (y2 - y1);
      b = (x1 - x2);
      c = (x1 * (y1-y2) + y1 * (x2 - x1));
      
      dist_to_axis = (a*x + b*y + c) / sqrt(a*a+b*b);
      sign_of_dist = (int)SGN( dist_to_axis );
      dist_to_axis2 = fabs(SQR(dist_to_axis));

      fr = fabs(dist_to_axis) / radius;

      // we have THREE cases:
      
      // (A)
      
      // if distance to the axis if larger than pixel diameter,
      // AND 
      // the SGN is + -> put the whole charge in the + bag
      
      if ( (fabs(dist_to_axis) > radius) &&
           (sign_of_dist == +1) ) {

        // here the sum
  
        j = k + sign_of_dist;

        wsum[j] += weight * dist_to_axis2;
        sum[j]  += weight;      

        // lwvec[j].push_back( dist_to_axis );
        
        if (weight < 0.0)
          cerr << "LENWID: weight = " << weight << endl << flush;

        /*
          cerr << "LENWID1: " 
          << i << ' '
          << dist_to_axis << ' '
          << sign_of_dist << ' '
          << weight << ' '
          << sum[0] << ' '<< sum[1] << ' '
          << sum[2] << ' '<< sum[3] << ' '
          << endl;
        */
        continue;

      }

      // (B)
        
      // if distance to the axis if larger than pixel diameter,
      // AND 
      // the SGN is - -> put the whole charge in the + bag
      
      if ( (fabs(dist_to_axis) > radius) &&
           (sign_of_dist == -1) ) {
          
        // here the sum
  
        j = k + sign_of_dist;

        wsum[j] += weight * dist_to_axis2;
        sum[j]  += weight;

        // lwvec[j].push_back( dist_to_axis );
        
        if (weight < 0.0)
          cerr << "LENWID: weight = " << weight << endl << flush;

        /*
          cerr << "LENWID2: " 
          << i << ' '
          << dist_to_axis << ' '
          << sign_of_dist << ' '
          << weight << ' '
          << sum[0] << ' '<< sum[1] << ' '
          << sum[2] << ' '<< sum[3] << ' '
          << endl;
        */
        continue;

      }
      
      // (C)
      // if we reach this point, that means that the center
      // of our pixel is too close to the axis, and we have
      // to feed it into the routine to check if the pixel
      // crosses the axis
      
      // ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE
      // simplified algorithm
      // assume the pixels are circular, and takes
      // the fraction of the surface lying on the semiplane
      // ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE
      
      // alpha
      alpha = fabs(acos( fr ));
      farea = sqrt(SQR(fr) - SQR(SQR(fr))) * alpha / M_PI;
      
      if ((farea > 1.0)||(farea < 0.0))
        cerr << "LENWID: farea = " << farea 
             << "  (" << alpha*180/M_PI << ")" << endl << flush;

      if (sign_of_dist == +1) {
  
        j = k + 1;
        wsum[j] += (1.0-farea) * weight * dist_to_axis2;
        tosum = (1.0-farea) * weight;
        sum[j]  += tosum;

        // lwvec[j].push_back( dist_to_axis );
        
        //if (tosum < 0.0)
        //  cerr << "LENWID31: tosum = " << tosum << endl << flush;

        j = k - 1;
        wsum[j] += farea * weight * dist_to_axis2;
        tosum = farea * weight;
        sum[j]  += tosum;

        // lwvec[j].push_back( dist_to_axis );
        
        //if (tosum < 0.0)
        //  cerr << "LENWID32: tosum = " << tosum << endl << flush;

      } else {

        j = k - 1;
        wsum[j] += (1.0-farea) * weight * dist_to_axis2;
        tosum = (1.0-farea) * weight;
        sum[j]  += tosum;

        // lwvec[j].push_back( dist_to_axis );
        
        //if (tosum < 0.0)
        //  cerr << "LENWID33: tosum = " << tosum << endl << flush;

        j = k + 1;
        wsum[j] += farea * weight * dist_to_axis2;
        tosum = farea * weight;
        sum[j]  += tosum;

        // lwvec[j].push_back( dist_to_axis );
        
        //if (tosum < 0.0)
        //  cerr << "LENWID34: tosum = " << tosum << endl << flush;

      }
      /*
        cerr << "LENWID3: " 
        << i << ' '
        << dist_to_axis << ' '
        << sign_of_dist << ' '
        << weight << ' '
        << sum[0] << ' ' << sum[1] << ' '
        << sum[2] << ' '<< sum[3] << '|'
        << fr << ' '
        << alpha << ' '
        << farea << ' '
        << endl;
      */
    } // foreach pixel pixels
    
  } // foreach axis

  
  lw.length1 = (sum[0] > 0.) ?  sqrt(wsum[0] / sum[0]) : -1;
  lw.width1  = (sum[1] > 0.) ?  sqrt(wsum[1] / sum[1]) : -1;
  lw.length2 = (sum[2] > 0.) ?  sqrt(wsum[2] / sum[2]) : -1;
  lw.width2  = (sum[3] > 0.) ?  sqrt(wsum[3] / sum[3]) : -1;
  
  /*
  for (i=0; i<4; ++i) {
    
    sort(lwvec[i].begin(), lwvec[i].end());
    j = lwvec[i].size();
    / *
    cerr << "LWV: " 
         << j << ' '
         << lwvec[i][0] << ' '
         << lwvec[i][j] << '\n';
    * /
    if ((j%2) == 1) {
      sum[i] = lwvec[i][j/2+1];
    } else {
      sum[i] = (lwvec[i][j/2] + lwvec[i][j/2+1]) / 2.0;
    }

    sum[i] = fabs(sum[i]);

  }

  lw.length1 = sum[0];
  lw.width1  = sum[1];
  lw.length2 = sum[2];
  lw.width2  = sum[3];
  
  */   
  

  /*
    cerr << "LENWID: " 
    << sum[0]<< ' '
    << sum[1]<< ' '
    << sum[2]<< ' '
    << sum[3]<< ' '
    << endl << flush;
  */
  return &lw;
}
//}


//## Auxiliary functions.

//!-----------------------------------------------------------
// @name crosspt
//                                             
// @desc calculate cross point of segments AB and CD
//
// @var ax         Coor. X of point A
// @var ay         Coor. Y of point A
// @var bx         Coor. X of point A
// @var by         Coor. Y of point A
// @var cx         Coor. X of point A
// @var cy         Coor. Y of point A
// @var dx         Coor. X of point A
// @var dy         Coor. Y of point A
// @var *pcrossx   Coor. X of cross point
// @var *pcrossy   Coor. Y of cross point
//
// @date Mon Mar  8 13:35:54 MET 1999
//------------------------------------------------------------
// @function

//{
void
crosspt( float ax, float ay,
         float bx, float by,
         float cx, float cy,
         float dx, float dy,
         float * pcrossx, float * pcrossy)
{
  float w, r;
  
  // the points A and B, and C and D define two segments (AB and CD)
  // the coordinates of these points are
  // A(ax,ay), B(bx,by), C(cx,cy), D(dx,dy)

  w=(bx-ax)*(dy-cy)-(by-ay)*(dx-cx);
  r=(ay-cy)*(dx-cx)-(ax-cx)*(dy-cy);

  *pcrossx = ax + r*(bx-ax)/w;
  *pcrossy = ay + r*(by-ay)/w;

}
//}

//=------------------------------------------------------------
//## Log of this file.

//{
//
// $Log$
// Revision 1.1.1.1  2000/06/07 21:03:49  gonzalez
// camera-simple created from camera-complex
//
// Revision 1.15  2000/05/29  16:01:27  gonzalez
// *** empty log message ***
//
// Revision 1.14  2000/02/20  11:38:39  gonzalez
// *** empty log message ***
//
// Revision 1.13  2000/01/27  10:48:51  gonzalez
// Trigger patterns + timing seem to work
//
// Revision 1.12  1999/12/03  17:18:09  gonzalez
// *** empty log message ***
//
// Revision 1.11  1999/11/25  16:43:01  gonzalez
// *** empty log message ***
//
// Revision 1.10  1999/11/25  07:48:15  gonzalez
// *** empty log message ***
//
// Revision 1.9  1999/11/17  13:32:04  gonzalez
// Nov.1999
//
// Revision 1.8  1999/10/26  09:39:12  gonzalez
// *** empty log message ***
//
// Revision 1.7  1999/10/08  08:00:02  gonzalez
// Bug in ISLANDS algorithm fixed
//
// Revision 1.6  1999/10/05  11:42:35  gonzalez
// Sep. 1999
//
// Revision 1.1  1999/03/08  10:04:06  gonzalez
// *** empty log message ***
//
// Revision 1.4  1999/03/02  09:56:14  gonzalez
// *** empty log message ***
//
// Revision 1.5  1999/03/15  14:59:10  gonzalez
// camera-1_1
//
//}

//=EOF
