/******************************************************************************
 * File:    reflector.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.reflector
 *
 * Last update:  2.0
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
 *   Declare DockerMng class
 *
 * Created by:
 *   J C Gonzalez
 *
 * Status:
 *   Prototype
 *
 * Dependencies:
 *   Component
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

#ifndef REFLECTOR_H
#define REFLECTOR_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <cfloat>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------
#include "json.h"
#include "mathtools.h"

using namespace MathTools;

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------
#include "physconst.h"
#include "CPhoton.h"

#define CT_I       0

#define CT_S       1
#define CT_RHO     2
#define CT_THETA   3

#define CT_FOCAL   1
#define CT_SX      2
#define CT_SY      3

#define CT_X       4
#define CT_Y       5
#define CT_Z       6
#define CT_THETAN  7
#define CT_PHIN    8
#define CT_XC      9
#define CT_YC     10
#define CT_ZC     11

#define CT_NDATA  12

// Type: Mirrors definition
typedef json::Object MirrorSet;

double curv2lin(double s);
double lin2curv(double x);

//======================================================================
// Class: Reflector
//======================================================================
class Reflector {
public:
    Reflector();
    ~Reflector();

public:
    virtual void setMirrorsFile(std::string fileName);
    virtual void setCore(point3D core);
    virtual void setOrientation(double theta, double phi);

    virtual bool reflect(CPhoton cph, point3D & xDish, point3D & xCam);

protected:
    virtual double lagrange(double ** t, double x);
    
    virtual bool passedTransmittance(CPhoton & cph);
    virtual bool passedReflectivity(CPhoton & cph);
    virtual void applyAxisDeviation(CPhoton & cph);

protected:
    virtual bool mirrorsReflection(point3D x, vector3D r, double timeFirstInt,
                                   point3D & xd, point3D & xr) {}
    virtual bool intersectionWithDish(point3D vx, point3D vxCT, vector3D vrCT,
                                      point3D & xDish) {}
    virtual int findClosestMirror(point3D & xDish, double & distMirr) {}
    virtual point3D getIntersectionWithMirror(int i, point3D vxm, vector3D vrm) {}
    
protected:
    // Core location
    double coreX, coreY, coreD;

    // Orientation of CT
    double thetaCT, phiCT;

    // Matrices to change to the system where the optical axis is OZ
    double OmegaCT[3][3];

    // Matrices to change to the system where the optical axis is OZ (inverse)
    double OmegaICT[3][3];

    // Matrices to change the system of coordinates
    double Omega[3][3];

    // Matrices to change the system of coordinates (inverse)
    double OmegaI[3][3];

    // Focal distances [cm]
    std::vector<double> ct_Focal;

    // Diameter [cm]
    double ct_Diameter, ct_Radius;;

    // Mean Focal distances [cm]
    double ct_Focal_mean;

    // STDev. Focal distances [cm]
    double ct_Focal_std;

    // Mean Point Spread function [cm]
    double ct_PSpread_mean;

    // STDev. Point Spread function [cm]
    double ct_PSpread_std;

    // STDev. Adjustmente deviation [cm]
    double ct_Adjustment_std;

    // Radius of the Black Spot in mirror [cm]
    double ct_BlackSpot_rad;

    // Radius of one mirror [cm]
    double ct_RMirror;

    // Camera width [cm]
    double ct_CameraWidth;
    double ct_CameraEdges2;
    
    // Pixel width [cm]
    double ct_PixelWidth;

    // Number of mirrors
    int ct_NMirrors = 0;

    // Number of pixels
    int ct_NPixels;

    /* Mirrors information
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

    // Pointer to a table with the following info.:
    double **ct_data;

    // The mirror table has data obtained from this object
    MirrorSet mirrors;
    
    // Table with datapoints (wavelength,reflec.)
    double **reflectivity;

    // Number of datapoints
    int    nReflectivity;

    // Table with deviations of the mirrors' normals
    double **axisDeviation;

    // Table of normal random numbers
    double normalRandomNumbers[500];

    // Flag to change the verbosity
    int verbose;

    // Matrix to change to the system where the optical axis is OZ
    matrix3D omegaCT;

    // Matrix to change to the system where the optical axis is OZ (inverse)
    matrix3D omegaICT;
};


#endif  /* REFLECTOR_H */
