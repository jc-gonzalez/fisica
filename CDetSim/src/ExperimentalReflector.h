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

#ifndef ExperimentalREFLECTOR_H
#define ExperimentalREFLECTOR_H

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

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------
#include "Reflector.h"

// Type: Mirrors definition
typedef json::Object MirrorSet;

//======================================================================
// Class: ExperimentalReflector
//======================================================================
class ExperimentalReflector : public Reflector {
public:
    ExperimentalReflector();
    ~ExperimentalReflector();

public:
    virtual void setMirrorsFile(std::string fileName);

    virtual bool reflect(CPhoton cph, point3D & xDish, point3D & xCam);

protected:
    virtual bool mirrorsReflection(point3D x, vector3D r, double timeFirstInt,
                                   point3D & xd, point3D & xr);
    virtual bool intersectionWithDish(point3D vx, point3D vxCT, vector3D vrCT,
                                      point3D & xDish);
    virtual int findClosestMirror(point3D & xDish, double & distMirr);
    virtual point3D getIntersectionWithMirror(int i, point3D vxm, vector3D vrm);    

private:
    double curv2lin(double s);
    double lin2curv(double x);

private:
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

};


#endif  /* ExperimentalREFLECTOR_H */
