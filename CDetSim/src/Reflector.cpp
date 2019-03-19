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
 *   Implement Reflector class
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

#include "Reflector.h"

thread_local UnifRnd reflector_unifUnit(0., 1.);
#define RandomNumber reflector_unifUnit()

//----------------------------------------------------------------------
// Constructor: Reflector
//----------------------------------------------------------------------
Reflector::Reflector() :
    coreX(0.), coreY(0.), coreD(0.), thetaCT(0.), phiCT(0.)
{
}

//----------------------------------------------------------------------
// Destructor: ~Reflector
//----------------------------------------------------------------------
Reflector::~Reflector()
{
}

//----------------------------------------------------------------------
// Method: setMirrorsFile
//----------------------------------------------------------------------
void Reflector::setMirrorsFile(std::string fileName)
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
    ct_CameraEdges2 = sqr<double>(ct_CameraWidth * 0.5);
    
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
// Method: setMirrorsFile
//----------------------------------------------------------------------
void Reflector::setCore(point3D core)
{
    std::tie(coreX, coreY, std::ignore) = core;
    coreD = norm(core);
}

//----------------------------------------------------------------------
// Method: setOrientation
//----------------------------------------------------------------------
void Reflector::setOrientation(double theta, double phi)
{
    thetaCT = theta, phiCT = phi;
    
    omegaCT  = makeOmega(d2r(theta), d2r(phi));
    omegaICT = makeOmegaI(d2r(theta), d2r(phi));    
}

//----------------------------------------------------------------------
// Method: reflect
//----------------------------------------------------------------------
bool Reflector::reflect(CPhoton cph, point3D & xDish, point3D & xCam)
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
// Method: lagrange
//----------------------------------------------------------------------
double Reflector::lagrange(double ** t, double x)
{
    int n = 0;
    while (t[n + 1][0] < x) { ++n; }
    
    return ((t[n][1] * ((x - t[n+1][0]) * (x - t[n+2][0])) / 
             ((t[n][0] - t[n+1][0]) * (t[n][0] - t[n+2][0])))  +  
            (t[n+1][1] * ((x - t[n][0]) * (x - t[n+2][0])) /   
             ((t[n+1][0] - t[n][0]) * (t[n+1][0] - t[n+2][0])))  +  
            (t[n+2][1] * ((x - t[n][0]) * (x - t[n+1][0])) /  
             ((t[n+2][0] - t[n][0]) * (t[n+2][0] - t[n+1][0]))));
}

//----------------------------------------------------------------------
// Method: passedTransmittance
//----------------------------------------------------------------------
bool Reflector::passedTransmittance(CPhoton & cph)
{
    return (RandomNumber < 0.9);  // atm(cph.wl, cph.h, acos(cph.w))
}

//----------------------------------------------------------------------
// Method: passedReflectivity
//----------------------------------------------------------------------
bool Reflector::passedReflectivity(CPhoton & cph)
{
    return (RandomNumber < lagrange(reflectivity, cph.wl));
}

//----------------------------------------------------------------------
// Method: applyAxisDeviation
//----------------------------------------------------------------------
void Reflector::applyAxisDeviation(CPhoton & cph)
{
    // return (unifUnit() < 0.9);
}


//}
