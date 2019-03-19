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

#ifndef MAGICREFLECTOR_H
#define MAGICREFLECTOR_H

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
// Class: MAGICReflector
//======================================================================
class MAGICReflector : public Reflector {
public:
    MAGICReflector();
    ~MAGICReflector();

public:
    virtual bool reflect(CPhoton cph, point3D & xDish, point3D & xCam);

protected:
    virtual bool mirrorsReflection(point3D x, vector3D r, double timeFirstInt,
                                   point3D & xd, point3D & xr);
    virtual bool intersectionWithDish(point3D vx, point3D vxCT, vector3D vrCT,
                                      point3D & xDish);
    virtual int findClosestMirror(point3D & xDish, double & distMirr);
    virtual point3D getIntersectionWithMirror(int i, point3D vxm, vector3D vrm);    
};


#endif  /* MAGICREFLECTOR_H */
