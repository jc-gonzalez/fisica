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
