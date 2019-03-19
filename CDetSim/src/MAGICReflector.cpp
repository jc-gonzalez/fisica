/******************************************************************************
 * File:    MAGICReflector.cpp
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.magicreflector
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
 *   Implement MAGICReflector class
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

#include "MAGICReflector.h"

thread_local UnifRnd magicreflector_unifUnit(0., 1.);
#define RandomNumber magicreflector_unifUnit()

//----------------------------------------------------------------------
// Constructor: MAGICReflector
//----------------------------------------------------------------------
MAGICReflector::MAGICReflector() 
{
}

//----------------------------------------------------------------------
// Destructor: ~MAGICReflector
//----------------------------------------------------------------------
MAGICReflector::~MAGICReflector()
{
}

//----------------------------------------------------------------------
// Method: reflect
//----------------------------------------------------------------------
bool MAGICReflector::reflect(CPhoton cph, point3D & xDish, point3D & xCam)
{
    // Atmospheric transmittance test
    if (!passedTransmittance(cph)) { return false; }
    
    // Mirrors reflectivity test
    if (!passedReflectivity(cph)) { return false; }
    
    // Reflection in mirrors
    point3D cphGround {cph.x - coreX, cph.y - coreY, 0.};
    vector3D orient {cph.u, cph.v, cph.w};

    return mirrorsReflection(cphGround, orient, cph.t, xDish, xCam);
}

//----------------------------------------------------------------------
// Method: mirrorsReflection
//----------------------------------------------------------------------
bool MAGICReflector::mirrorsReflection(point3D x, vector3D r, double timeFirstInt,
                                       point3D & xd, point3D & xr)
{
    static double normalRnd[2];
    
    // Pre-compute matrix to move to the main dish ref.system
    point3D  xCT = applyMxV(omegaCT, x);
    vector3D rCT = applyMxV(omegaCT, r);

    // Look for the intersection with the main dish (virtual reflection point)
    point3D xDish;
    if (!intersectionWithDish(x, xCT, rCT, xDish)) { return false; }

    // Look if reflection would have been completly outside the main dish
    double rx, ry, rz;
    std::tie(rx, ry, rz) = xDish;
    double sx = lin2curv(rx);
    double sy = lin2curv(ry);
    if ((fabs(sx) > ct_Radius) || (fabs(sy) > ct_Radius)) { return false; }

    // Search for the mirror element to use (if any)
    double distMirr;
    int i_mirror = findClosestMirror(xDish, distMirr);
    bool outOfMirror = ((fabs(ct_data[i_mirror][CT_SX] - sx) > ct_RMirror) ||
                        (fabs(ct_data[i_mirror][CT_SY] - sy) > ct_RMirror));
    if (outOfMirror) { return false; }

    // Compute matrices for the mirror
    double thetaMirr = ct_data[i_mirror][CT_THETAN];
    double phiMirr   = ct_data[i_mirror][CT_PHIN];
    
    matrix3D omegaMirr  = makeOmega(-d2r(thetaMirr), d2r(phiMirr));
    matrix3D omegaMirrI = makeOmega(-d2r(thetaMirr), d2r(phiMirr));

    // First translation...
    point3D xMirror {ct_data[i_mirror][CT_X],
                     ct_data[i_mirror][CT_Y],
                     ct_data[i_mirror][CT_Z]};
    point3D xmm = xCT - xMirror;
    // ... then rotation
    point3D  xm = applyMxV(omegaMirr, xmm);
    vector3D rm = applyMxV(omegaMirr, rCT);

    normalize(rm);

    // Compute intersection of the trajectory of the photon with the mirror
    point3D xCut = getIntersectionWithMirror(i_mirror, xm, rm);

    // Black Spot: If the photon hits the blsack spot, it's lost
    if (sqrt(sqr<double>(std::get<0>(xCut)) +
             sqr<double>(std::get<1>(xCut))) < ct_BlackSpot_rad ) { return false; }

    // Continue with the reflection

    // Calculate normal vector in this point
    point3D rnor = point3D {0., 0., 4. * ct_data[i_mirror][CT_FOCAL]} - xCut * 2.0;
    //rnor = point3D {0., 0., 4. * ct_data[i_mirror][CT_FOCAL]} - rnor;
    normalize(rnor);

    // Now, both "normal" vector and original trayectory are normalized.
    // Just project the original vector in the normal, and take it as
    // the "mean" position of the original and the "reflected" vector.
    // From this, we can calculate the "reflected" vector
    // calpha = cos(angle(rnor,rm))
    
    double calpha = fabs(dot(rnor, rm));
    point3D rrefl = (rnor * 2. * calpha) - rm;
    normalize(rrefl);

    // Let's go back to the coordinate system of the CT

    // First crotation...
    point3D xCutCT  = applyMxV(omegaMirrI, xCut);
    point3D rReflCT = applyMxV(omegaMirrI, rrefl);
    // ...then translation
    point3D xReflCT = xCutCT + xMirror;
    //normalize(rReflCT);

    // Calculate intersection of this trayectory and the camera plane
    // in the system of the CT, this plane is z = ct_Focal
    //std::cout << "============>" << xCut << ' ' << xcutCT << "   -   " << rrefl << ' ' << rreflCT << '\n';
    double s = (ct_Focal_mean - std::get<2>(xReflCT)) / std::get<2>(rReflCT);
    point3D xcam = xCutCT + rReflCT * s;

    // Axis deviation: we introduce it here just as a first order
    // correction, modifying the position of the reflected point
    xcam = xcam + point3D {axisDeviation[i_mirror][0],
                           axisDeviation[i_mirror][1],
                           0.};

    // Smearing: We apply the point spread function for the mirrors
    rnormal<double>(normalRnd, 2);
    xcam = xcam + point3D {normalRnd[0] * ct_PSpread_mean,
                           normalRnd[1] * ct_PSpread_mean,
                           0.};

    // Check if the photon is out of the camera
    if ((sqr<double>(std::get<0>(xcam)) +
         sqr<double>(std::get<1>(xcam))) > ct_CameraEdges2) { return false; } 

    // Angle of incidence
    // Calculate angle of incidence between tray. and camera plane the
    // camera plane is
    // 0 y + 0 y + z - ct_Focal = 0 => (A,B,C,D) = (0,0,1,-ct_Focal)
    // from Table 3.20 "Tasch. der Math."
    double psi = asin(std::get<2>(rReflCT));
    
    // Timing
    // t = adjust_time(t=timefirstint)
    // substract light-path from the mirror till the ground, 'cos 
    // the photon actually hit the mirror!!

    double factor = (std::get<2>(xm) > 0.) ? -1.0 : +1.0;
    double t = timeFirstInt + factor * norm(xm - xCut) / Phys::Speed_of_Light_air_cmns;
                 
    // and then add path from the mirror till the camera
    t = t + norm(xReflCT - xcam) / Phys::Speed_of_Light_air_cmns;

    xd = xDish;
    xr = xcam;
    
    return true;
}

//----------------------------------------------------------------------
// Method: intersectionWithDish
//----------------------------------------------------------------------
bool MAGICReflector::intersectionWithDish(point3D vx, point3D vxCT, vector3D vrCT,
                                          point3D & xDish)
{
    static const double Epsilon = 1.0e-12; //100. * DBL_EPSILON;
    
    /*
      Before moving to the system of the mirror, for MAGIC, 
      first we look whether the photon hits a mirror or not
      
      calculate the intersection of the trayectory of the photon 
      with the GLOBAL DISH !!!
      we reproduce the calculation of the coefficients of the
      second order polynomial in z (=vxCT[2]), made with 
      Mathematica 
      
      
      In[1]:= parab:=z-(x^2+y^2)/(4F)
             par1=parab /. {x->x0+u/w(z-z0),y->y0+v/w(z-z0)}
      
      Out[1]=
                       u (z - z0) 2         v (z - z0) 2
                 (x0 + ----------)  + (y0 + ----------)
                           w                    w
             z - ---------------------------------------
                                   4 F
      
      In[2]:= CoefficientList[ExpandAll[par1*4F*w^2],z]
      
      Out[2]=
                 2   2     2   2
             {-(w  x0 ) - w  y0  + 2 u w x0 z0 + 
              
                              2   2    2   2
               2 v w y0 z0 - u  z0  - v  z0 , 
              
                   2                            2
              4 F w  - 2 u w x0 - 2 v w y0 + 2 u  z0 + 
              
                  2       2    2
               2 v  z0, -u  - v }
    */

    double vx0, vx1, vx2;
    double vxCT0, vxCT1, vxCT2;
    double vrCT0, vrCT1, vrCT2;

    std::tie(vx0, vx1, vx2) = vx;
    std::tie(vxCT0, vxCT1, vxCT2) = vxCT;
    std::tie(vrCT0, vrCT1, vrCT2) = vrCT;

    double a = - sqr<double>(vrCT0) - sqr<double>(vrCT1);
    double b = (4. * ct_Focal_mean * sqr<double>(vrCT2) 
		- 2. * vrCT0 * vrCT2 * vxCT0 - 2. * vrCT1 * vrCT2 * vxCT1 
		+ 2. * sqr<double>(vrCT0) * vxCT2 + 2. * sqr<double>(vrCT1) * vxCT2);
    double c = (2. * vrCT0 * vrCT2 * vx0 * vx2 + 2. * vrCT1 * vrCT2 * vx1 * vx2 
		- sqr<double>(vrCT2) * sqr<double>(vx0) - sqr<double>(vrCT2) * sqr<double>(vx1)
		- sqr<double>(vrCT0) * sqr<double>(vx2) - sqr<double>(vrCT1) * sqr<double>(vx2));

    double zDish;

    //std::cout << vx << ' ' << a << ' ' << b << ' ' << c << ' ' << -c / b << '\n';
    
    if (fabs(a) < Epsilon) {
        zDish = -c / b;
    } else {
        double delta = b * b - 4. * a * c;
        if (delta < 0.) { return false; }
        double d = sqrt(delta);
        double t1 = (-b + d) / (2. * a);
        double t2 = (-b - d) / (2. * a);
        zDish = (t1 < t2) ? t1 : t2;
    }
    
    xDish = point3D { vxCT0 + (zDish - vxCT2) * vrCT0 / vrCT2,
                      vxCT1 + (zDish - vxCT2) * vrCT1 / vrCT2,
                      zDish };
    return true;
}

//----------------------------------------------------------------------
// Method: findClosestMirror
// Find the mirror element whose center is closest to the photon loc.
//----------------------------------------------------------------------
int MAGICReflector::findClosestMirror(point3D & xDish, double & distMirr)
{
    double distMirr_ = 1000000.;
    int i_mirror = 0;
    for (int i = 0; i < ct_NMirrors; ++i) {
        point3D rmirr {ct_data[i][CT_X], ct_data[i][CT_Y], ct_data[i][CT_Z]};
        point3D pmirr = rmirr - xDish;
        distMirr = norm(pmirr);
        if (distMirr < distMirr_) {
            distMirr_ = distMirr;
            i_mirror = i;
            if (distMirr_ < ct_RMirror) {
                i = ct_NMirrors;
            }
        }
    }
    return i_mirror;
}

//----------------------------------------------------------------------
// Method: getIntersectionWithMirror
// Compute the point of intersection of the trajectory of the photon
// with the mirror element
//----------------------------------------------------------------------
point3D MAGICReflector::getIntersectionWithMirror(int i, point3D vxm, vector3D vrm)
{
    // Calculate the intersection of the trayectory of the photon 
    // with the mirror
    // We reproduce the calculation of the coefficients of the
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

    double xm0, xm1, xm2;
    double rm0, rm1, rm2;
    std::tie(xm0, xm1, xm2) = vxm;
    std::tie(rm0, rm1, rm2) = vrm;
    
    double a = norm2(vrm);
    double b = -2. * (2. * ct_data[i][CT_FOCAL] * sqr<double>(rm2) 
                      - rm0 * rm2 * xm0 
                      + sqr<double>(rm0) * xm2 
                      + rm1 * (-(rm2 * xm1) + rm1 * xm2));
    double c = (sqr<double>(rm2) * (sqr<double>(xm0) + sqr<double>(xm1)) 
                - 2. * rm2 * (rm0 * xm0 + rm1 * xm1) * xm2 + 
                (sqr<double>(rm0) + sqr<double>(rm1)) * sqr<double>(xm2));
    double d = sqrt(b * b - 4. * a * c );

    // two possible values for z
    double t1 = (-b + d) / (2. * a);
    double t2 = (-b - d) / (2. * a);
    double z = (t1 < t2) ? t1 : t2;
    
    // z must be the minimum of t1 and t2
    return point3D {xm0 + (rm0 / rm2) * (z - xm2),
                    xm1 + (rm1 / rm2) * (z - xm2),
                    z};
}

double curv2lin(double s)
{
    double x = s;
    for (int i = 0; i < 4; i++) {
        x = (s * 0.01) / (1. + 0.000144175317185 * x * x);
    }
    return (x * 100.);
}

double lin2curv(double x)
{
  x *= 0.01;
  return ((x + 0.000144175317185 * x * x * x) * 100.);
}

//}
