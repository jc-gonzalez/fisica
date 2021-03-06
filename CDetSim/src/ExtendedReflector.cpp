/******************************************************************************
 * File:    ExtendedReflector.cpp
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
 *   Implement ExtendedReflector class
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

#include "ExtendedReflector.h"

#include "quaternions.h"
#include "surfaces.h"

thread_local UnifRnd extReflector_unifUnit(0., 1.);
#define RandomNumber extReflector_unifUnit()

//----------------------------------------------------------------------
// Constructor: ExtendedReflector
//----------------------------------------------------------------------
ExtendedReflector::ExtendedReflector()
{
}

//----------------------------------------------------------------------
// Destructor: ~ExtendedReflector
//----------------------------------------------------------------------
ExtendedReflector::~ExtendedReflector()
{
}

//----------------------------------------------------------------------
// Method: setMirrorsFile
//----------------------------------------------------------------------
void ExtendedReflector::setMirrorsFile(std::string fileName)
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

    ct_Elem_length = mirrors["element_length"]["value"].asFloat();
    ct_Elem_width = mirrors["element_width"]["value"].asFloat();

    ct_Focal_mean = mirrors["focal_distance"]["value"].asFloat();
    ct_Focal_std = mirrors["focal_std"]["value"].asFloat();

    ct_PSpread_mean = mirrors["point_spread_avg"]["value"].asFloat();
    ct_PSpread_std = mirrors["point_spread_std"]["value"].asFloat();

    ct_Adjustment_std = mirrors["adjustment_dev"]["value"].asFloat();
    ct_BlackSpot_rad = mirrors["black_spot"]["value"].asFloat();

    ct_CameraRadius = mirrors["camera_radius"]["value"].asFloat();
    ct_CameraRaised = mirrors["camera_raised"]["value"].asFloat();
    ct_CameraSize = mirrors["camera_size"]["value"].asFloat();

    ct_PixelWidth = mirrors["pixel_width"]["value"].asFloat();
    ct_NPixels = mirrors["n_pixels"]["value"].asInt();

    ct_data = new double * [ct_NMirrors];
    for (int i = 0; i < ct_NMirrors; ++i) {
        ct_data[i] = new double [CT_NDATA];
        for (int j = 0; j < CT_NDATA; ++j) {
            ct_data[i][j] = mirrors["mirrors"]["value"][i][j].asFloat();
        }
    }

    // Mirror info (in sep. file)
    std::string mirrorFileName = mirrors["mirror_info"]["value"].asString();
    assert(cfgReader.parseFile(mirrorFileName, content));
    nMirrorInfo = content["data"]["num_points"].asInt();
    mirrorInfo = new double * [nMirrorInfo];
    for (int i = 0; i < nMirrorInfo; ++i) {
        mirrorInfo[i] = new double [3];
        mirrorInfo[i][0] = content["data"]["mirror_info"][i][0].asFloat(); // xfrom
        mirrorInfo[i][1] = content["data"]["mirror_info"][i][1].asFloat(); // xto
        mirrorInfo[i][2] = content["data"]["mirror_info"][i][2].asFloat(); // focal
    }

    // Filling ringFocals vector
    RingData r;
    for (int i = 0; i < nMirrorInfo; ++i) {
        r.i = i;
        r.x1 = mirrorInfo[i][0];
        r.x2 = mirrorInfo[i][1];
        r.xn = (i == nMirrorInfo - 1) ? (r.x2 + 200.) : mirrorInfo[i + 1][0];
        r.f  = mirrorInfo[i][2];
        r.len = 2. * M_PI * (r.x1 + r.x2) * 0.5;
        r.nsec = int(r.len / (2. * ct_Elem_length));
        r.angSizeElem = r2d(2 * M_PI * (ct_Elem_length / r.len));
        r.angSep = (360. - r.nsec * r.angSizeElem) / r.nsec;
        ringFocals.push_back(r);
        std::cout << "#" << r.i << " ring from " << r.x1 << " up to "
                  << r.x2 << '(' << r.xn << ") with focal " << r.f << "   - "
                  << r.nsec << " elements, of ang. size " << r.angSizeElem << " deg. "
                  << " and separated " << r.angSep << " deg."
                  << '\n';
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

    // Setting up the different elements
    mainDish.set(point3d(coreX, coreY, ct_Center_height), ct_Diameter);
}

//----------------------------------------------------------------------
// Method: reflect
//----------------------------------------------------------------------
bool ExtendedReflector::reflect(CPhoton cph, point3d & xDish, point3d & xCam)
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
bool ExtendedReflector::mirrorsReflection(point3d x, vector3d r, double timeFirstInt,
                                       point3d & xd, point3d & xr)
{
    static double normalRnd[2];

    // Pre-compute matrix to move to the main dish ref.system
    point3d  xCT = x; //omegaCT * x;
    vector3d rCT = r; //omegaCT * r;

    line cphTrajectory;
    cphTrajectory.fromPointVector(xCT, rCT);
    double rhoCT = xCT.norm();
    /*
    xd = xCT;
    xr = xCT;
    return true;
    */
    // First, select the parabolic dish of all the family that is getting the
    // trajectory of the photon reflected
    RingData rng;
    rng.i = -1;
    for (auto & t: ringFocals) {
        if ((t.x1 < rhoCT) && (rhoCT < t.xn)) { rng = t; break; }
    }
    if (rng.i < 0) { return false; }
    /*
    std::cout << "RingData: "
              << rng.i << ' '
              << rng.x1 << ' ' 
              << rng.x2 << ' ' 
              << rng.xn << ' ' 
              << rng.f  << ' ' 
              << rng.len << ' ' 
              << rng.nsec << ' ' 
              << rng.angSizeElem << ' ' 
              << rng.angSep << '\n'; 
    */
    paraboloid dish(point3d(coreX, coreY, ct_Focal_mean - rng.f), rng.f);

    // Look for the intersection with the main dish (virtual reflection point)
    std::vector<point3d> pts;
    bool isThereIntersection = intersectionParaboloidLine(dish, cphTrajectory, pts);

    if (! isThereIntersection) { return false; }
    for (int i = pts.size() - 1; i >= 0; --i) {
        if (sqr(pts.at(i).X) + sqr(pts.at(i).Y) > sqr(ct_Radius)) {
            pts.erase(pts.begin() + i);
        }
    }
    if (pts.size() < 1) { return false; }

    point3d & xDish = pts.at(0);

    double rho = sqrt(sqr(xDish.X) + sqr(xDish.Y));

    if (rng.i > 0) {
        // Check if falls in an actual ring (not in the inter-ring)
        if (rho > (rng.x1 + ct_Elem_width)) { return false; }
        
        // Check that it falls in an actual element
        double angle = r2d(atan2(xDish.Y, xDish.X) + M_PI);
        double rmdr = fmod(angle, rng.angSizeElem + rng.angSep);
        //std::cout << xDish << " ; " << rho << " ; " << angle << " ; " << rmdr << '\n';
        if (rmdr > rng.angSizeElem) { return false; }
    } else {
        // Check if falls in an actual ring (not in the inter-ring)
        if (rho > rng.x2) { return false; }
    }
    
    vector3d v_in = xDish - cphTrajectory.o;
    quaternion q_in(v_in, 0.);
    vector3d v_axis {2. * xDish.X, 2. * xDish.Y, - 4. * dish.f}; // normal at the point
    v_axis.normalize();
    quaternion reflec_axis(v_axis, 0.);
    q_in.reflect(reflec_axis);
    vector3d v_out = q_in.getVector();
    /*
    // Reflect trayectory
    std::vector<point3d> pts2;
    line cphReflectedTrajectory;
    cphReflectedTrajectory.fromPointVector(xDish, v_out);
    bool isThereIntersection2 = intersectionCylinderLine(camera, cphReflectedTrajectory, pts2);
    if (!isThereIntersection2) { return false; } 

    if (pts2.size() > 1) {
        // remove the far one
        double d1 = pts2.at(0).distanceFrom(v_in);
        double d2 = pts2.at(1).distanceFrom(v_in);
        pts2.erase(pts2.begin() + ((d2 > d1) ? 1 : 0));
    }
    
    point3d & xcam = pts2.at(0);
    if ((xcam.Z < ct_CameraRaised) ||
        (xcam.Z > ct_CameraRaised + ct_CameraSize)) { return false; }
    */
    double lambda = (dish.f - xDish.Z) / v_out.Z;
    point3d xcam {xDish.X + lambda * v_out.X, xDish.Y + lambda * v_out.Y, dish.f};
    
    // Timing
    // t = adjust_time(t=timefirstint)
    // substract light-path from the mirror till the ground, 'cos
    // the photon actually hit the mirror!!
    /*
    double factor = (xm[2] > 0.) ? -1.0 : +1.0;
    double t = timeFirstInt + factor * (xm - xCut).norm() / Phys::Speed_of_Light_air_cmns;

    // and then add path from the mirror till the camera
    t = t + (xReflCT - xcam).norm() / Phys::Speed_of_Light_air_cmns;
    */

    xd = xDish;
    xr = xcam;

    return true;
}


//----------------------------------------------------------------------
// Method: findClosestMirror
// Find the mirror element whose center is closest to the photon loc.
//----------------------------------------------------------------------
int ExtendedReflector::findClosestMirror(point3d & xDish, double & distMirr)
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
point3d ExtendedReflector::getIntersectionWithMirror(int i, point3d vxm, vector3d vrm)
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

double ExtendedReflector::curv2lin(double s)
{
    double x = s;
    for (int i = 0; i < 4; i++) {
        x = (s * 0.01) / (1. + 0.000144175317185 * x * x);
    }
    return (x * 100.);
}

double ExtendedReflector::lin2curv(double x)
{
  x *= 0.01;
  return ((x + 0.000144175317185 * x * x * x) * 100.);
}

//}
