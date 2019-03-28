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

#ifndef ExpConeREFLECTOR_H
#define ExpConeREFLECTOR_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------
#include "surfaces.h"

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------
#include "Reflector.h"

// Type: Mirrors definition
typedef json::Object MirrorSet;

//======================================================================
// Class: ExpConeReflector
//======================================================================
class ExpConeReflector : public Reflector {
public:
    ExpConeReflector();
    ~ExpConeReflector();

public:
    virtual void setMirrorsFile(std::string fileName);

    virtual bool reflect(CPhoton cph, point3d & xDish, point3d & xCam);

protected:
    virtual bool mirrorsReflection(point3d x, vector3d r, double timeFirstInt,
                                   point3d & xd, point3d & xr);

    virtual int findClosestMirror(point3d & xDish, double & distMirr);
    virtual point3d getIntersectionWithMirror(int i, point3d vxm, vector3d vrm);    

private:
    double curv2lin(double s);
    double lin2curv(double x);

private:
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

    double ct_Center_height;
    double ct_Lower_section;
    double ct_Upper_section;
    double ct_CameraRadius;
    double ct_CameraRaised;
    double ct_CameraSize;
    double ct_CameraEdges2;
    
    // Pixel width [cm]
    double ct_PixelWidth;

    // Number of mirrors
    int ct_NMirrors = 0;

    // Number of pixels
    int ct_NPixels;

    // Main mirror, spherical
    paraboloid mainDish;

    // Camera system (cylinder)
    cone camera;
};


#endif  /* ExpConeREFLECTOR_H */
