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
// Method: setMirrorsFile
//----------------------------------------------------------------------
void MAGICReflector::setMirrorsFile(std::string fileName)
{
    // Read filename
    json::Parser cfgReader;
    json::Object content;
    assert(cfgReader.parseFile(fileName, content));
    mirrors = content["data"].asObject();

    // Pass config items to data members
    // Focal distances [cm]
    std::vector<float> ct_Focal;

    ct_Diameter = mirrors["diameter"]["value"].asFloat();
    ct_Radius = ct_Diameter * 0.5;
    
    ct_Focal_mean = mirrors["focal_distance"]["value"].asFloat();
    ct_Focal_std = mirrors["focal_std"]["value"].asFloat();

    ct_PSpread_mean = mirrors["point_spread_avg"]["value"].asFloat();
    ct_PSpread_std = mirrors["point_spread_std"]["value"].asFloat();

    ct_Adjustment_std = mirrors["adjustment_dev"]["value"].asFloat();
    ct_BlackSpot_rad = mirrors["black_spot"]["value"].asFloat();

    ct_NMirrors = mirrors["n_mirrors"]["value"].asInt();
    ct_RMirror = mirrors["r_mirror"]["value"].asFloat();

    ct_CameraWidth = mirrors["camera_width"]["value"].asFloat();
    ct_PixelWidth = mirrors["pixel_width"]["value"].asFloat();
    ct_CameraEdges2 = sqr(ct_CameraWidth * 0.5);
    
    ct_NPixels = mirrors["n_pixels"]["value"].asInt();

    ct_data = new double * [ct_NMirrors];
    for (int i = 0; i < ct_NMirrors; ++i) {
        ct_data[i] = new double [CT_NDATA];
        for (int j = 0; j < CT_NDATA; ++j) {
            ct_data[i][j] = mirrors["mirrors"]["value"][i][j].asFloat();
        }
    }

    // Reflectivity table
    std::string reflecFileName = mirrors["reflectivity"]["value"].asString();
    assert(cfgReader.parseFile(reflecFileName, content));
    nReflectivity = content["data"]["num_points"].asInt();
    reflectivity = new double * [nReflectivity];
    for (int i = 0; i < nReflectivity; ++i) {
        reflectivity[i] = new double [2];
        reflectivity[i][0] = content["data"]["reflectivity"][i][0].asFloat();
        reflectivity[i][1] = content["data"]["reflectivity"][i][1].asFloat();
    }
    
    // Table with deviations of the mirrors' normals
    std::string axisDevFileName = mirrors["axis_deviation"]["value"].asString();
    assert(cfgReader.parseFile(axisDevFileName, content));
    axisDeviation = new double * [ct_NMirrors];
    for (int i = 0; i < ct_NMirrors; ++i) {
        axisDeviation[i] = new double [2];
        axisDeviation[i][0] = content["data"]["axis_deviation"][i][0].asFloat();
        axisDeviation[i][1] = content["data"]["axis_deviation"][i][1].asFloat();
    }

}

//----------------------------------------------------------------------
// Method: reflect
//----------------------------------------------------------------------
bool MAGICReflector::reflect(CPhoton cph, point3d & xDish, point3d & xCam)
{
    // Atmospheric transmittance test
    if (!passedTransmittance(cph)) { return false; }
    
    // Mirrors reflectivity test
    if (!passedReflectivity(cph)) { return false; }
    
    // Reflection in mirrors
    point3d cphGround {cph.x - coreX, cph.y - coreY, 0.};
    vector3d orient {cph.u, cph.v, cph.w};

    return mirrorsReflection(cphGround, orient, cph.t, xDish, xCam);
}

//----------------------------------------------------------------------
// Method: mirrorsReflection
//----------------------------------------------------------------------
bool MAGICReflector::mirrorsReflection(point3d x, vector3d r, double timeFirstInt,
                                       point3d & xd, point3d & xr)
{
    static double normalRnd[2];
    
    // Pre-compute matrix to move to the main dish ref.system
    point3d  xCT = omegaCT * x;
    vector3d rCT = omegaCT * r;

    // Look for the intersection with the main dish (virtual reflection point)
    point3d xDish;
    if (!intersectionWithDish(x, xCT, rCT, xDish)) { return false; }

    // Look if reflection would have been completly outside the main dish
    double rx, ry, rz;
    rx = xDish.X, ry = xDish.Y, rz = xDish.Z;
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
    
    matrix3 omegaMirr; 
    matrix3 omegaMirrI; 

    omegaMirr.setRotationThetaPhi(-d2r(thetaMirr), d2r(phiMirr));
    omegaMirrI.setInverseRotationThetaPhi(-d2r(thetaMirr), d2r(phiMirr));
    
    // First translation...
    point3d xMirror {ct_data[i_mirror][CT_X],
                     ct_data[i_mirror][CT_Y],
                     ct_data[i_mirror][CT_Z]};
    point3d xmm = xCT - xMirror;
    // ... then rotation
    point3d  xm = omegaMirr * xmm;
    vector3d rm = omegaMirr * rCT;

    rm.normalize();

    // Compute intersection of the trajectory of the photon with the mirror
    point3d xCut = getIntersectionWithMirror(i_mirror, xm, rm);

    // Black Spot: If the photon hits the blsack spot, it's lost
    if (sqrt(sqr(xCut[0]) + sqr(xCut[1])) < ct_BlackSpot_rad ) { return false; }

    // Continue with the reflection

    // Calculate normal vector in this point
    point3d rnor = point3d {0., 0., 4. * ct_data[i_mirror][CT_FOCAL]} - xCut * 2.0;
    //rnor = point3d {0., 0., 4. * ct_data[i_mirror][CT_FOCAL]} - rnor;
    rnor.normalize();

    // Now, both "normal" vector and original trayectory are normalized.
    // Just project the original vector in the normal, and take it as
    // the "mean" position of the original and the "reflected" vector.
    // From this, we can calculate the "reflected" vector
    // calpha = cos(angle(rnor,rm))
    
    double calpha = fabs(rnor.dot(rm));
    point3d rrefl = (rnor * 2. * calpha) - rm;
    rrefl.normalize();

    // Let's go back to the coordinate system of the CT

    // First crotation...
    point3d xCutCT  = omegaMirrI * xCut;
    point3d rReflCT = omegaMirrI * rrefl;
    // ...then translation
    point3d xReflCT = xCutCT + xMirror;
    //rReflCT.normalize();

    // Calculate intersection of this trayectory and the camera plane
    // in the system of the CT, this plane is z = ct_Focal
    //std::cout << "============>" << xCut << ' ' << xcutCT << "   -   " << rrefl << ' ' << rreflCT << '\n';
    double s = (ct_Focal_mean - xReflCT[2]) / rReflCT[2];
    point3d xcam = xCutCT + rReflCT * s;

    // Axis deviation: we introduce it here just as a first order
    // correction, modifying the position of the reflected point
    xcam = xcam + point3d {axisDeviation[i_mirror][0],
                           axisDeviation[i_mirror][1],
                           0.};

    // Smearing: We apply the point spread function for the mirrors
    rnormal<double>(normalRnd, 2);
    xcam = xcam + point3d {normalRnd[0] * ct_PSpread_mean,
                           normalRnd[1] * ct_PSpread_mean,
                           0.};

    // Check if the photon is out of the camera
    if ((sqr(xcam[0]) + sqr(xcam[1])) > ct_CameraEdges2) { return false; } 

    // Angle of incidence
    // Calculate angle of incidence between tray. and camera plane the
    // camera plane is
    // 0 y + 0 y + z - ct_Focal = 0 => (A,B,C,D) = (0,0,1,-ct_Focal)
    // from Table 3.20 "Tasch. der Math."
    double psi = asin(rReflCT[2]);
    
    // Timing
    // t = adjust_time(t=timefirstint)
    // substract light-path from the mirror till the ground, 'cos 
    // the photon actually hit the mirror!!

    double factor = (xm[2] > 0.) ? -1.0 : +1.0;
    double t = timeFirstInt + factor * (xm - xCut).norm() / Phys::Speed_of_Light_air_cmns;
                 
    // and then add path from the mirror till the camera
    t = t + (xReflCT - xcam).norm() / Phys::Speed_of_Light_air_cmns;

    xd = xDish;
    xr = xcam;
    
    return true;
}

//----------------------------------------------------------------------
// Method: intersectionWithDish
//----------------------------------------------------------------------
bool MAGICReflector::intersectionWithDish(point3d vx, point3d vxCT, vector3d vrCT,
                                          point3d & xDish)
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
    /*
    double vx0, vx1, vx2;
    double vxCT0, vxCT1, vxCT2;
    double vrCT0, vrCT1, vrCT2;

    std::tie(vx0, vx1, vx2) = vx;
    std::tie(vxCT0, vxCT1, vxCT2) = vxCT;
    std::tie(vrCT0, vrCT1, vrCT2) = vrCT;
    */
    double a = - sqr(vrCT[0]) - sqr(vrCT[1]);
    double b = (4. * ct_Focal_mean * sqr(vrCT[2]) 
		- 2. * vrCT[0] * vrCT[2] * vxCT[0] - 2. * vrCT[1] * vrCT[2] * vxCT[1] 
		+ 2. * sqr(vrCT[0]) * vxCT[2] + 2. * sqr(vrCT[1]) * vxCT[2]);
    double c = (2. * vrCT[0] * vrCT[2] * vx[0] * vx[2] + 2. * vrCT[1] * vrCT[2] * vx[1] * vx[2] 
		- sqr(vrCT[2]) * sqr(vx[0]) - sqr(vrCT[2]) * sqr(vx[1])
		- sqr(vrCT[0]) * sqr(vx[2]) - sqr(vrCT[1]) * sqr(vx[2]));

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
    
    xDish = point3d { vxCT[0] + (zDish - vxCT[2]) * vrCT[0] / vrCT[2],
                      vxCT[1] + (zDish - vxCT[2]) * vrCT[1] / vrCT[2],
                      zDish };
    return true;
}

//----------------------------------------------------------------------
// Method: findClosestMirror
// Find the mirror element whose center is closest to the photon loc.
//----------------------------------------------------------------------
int MAGICReflector::findClosestMirror(point3d & xDish, double & distMirr)
{
    double distMirr_ = 1000000.;
    int i_mirror = 0;
    for (int i = 0; i < ct_NMirrors; ++i) {
        point3d rmirr {ct_data[i][CT_X], ct_data[i][CT_Y], ct_data[i][CT_Z]};
        point3d pmirr = rmirr - xDish;
        distMirr = pmirr.norm();
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
point3d MAGICReflector::getIntersectionWithMirror(int i, point3d vxm, vector3d vrm)
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
    /*
    double xm0, xm1, xm2;
    double rm0, rm1, rm2;
    std::tie(xm0, xm1, xm2) = vxm;
    std::tie(rm0, rm1, rm2) = vrm;
    */
    double a = vrm.norm2();
    double b = -2. * (2. * ct_data[i][CT_FOCAL] * sqr(vrm[2]) 
                      - vrm[0] * vrm[2] * vxm[0] 
                      + sqr(vrm[0]) * vxm[2] 
                      + vrm[1] * (-(vrm[2] * vxm[1]) + vrm[1] * vxm[2]));
    double c = (sqr(vrm[2]) * (sqr(vxm[0]) + sqr(vxm[1])) 
                - 2. * vrm[2] * (vrm[0] * vxm[0] + vrm[1] * vxm[1]) * vxm[2] + 
                (sqr(vrm[0]) + sqr(vrm[1])) * sqr(vxm[2]));
    double d = sqrt(b * b - 4. * a * c );

    // two possible values for z
    double t1 = (-b + d) / (2. * a);
    double t2 = (-b - d) / (2. * a);
    double z = (t1 < t2) ? t1 : t2;
    
    // z must be the minimum of t1 and t2
    return point3d {vxm[0] + (vrm[0] / vrm[2]) * (z - vxm[2]),
                    vxm[1] + (vrm[1] / vrm[2]) * (z - vxm[2]),
                    z};
}

double MAGICReflector::curv2lin(double s)
{
    double x = s;
    for (int i = 0; i < 4; i++) {
        x = (s * 0.01) / (1. + 0.000144175317185 * x * x);
    }
    return (x * 100.);
}

double MAGICReflector::lin2curv(double x)
{
  x *= 0.01;
  return ((x + 0.000144175317185 * x * x * x) * 100.);
}

//}
