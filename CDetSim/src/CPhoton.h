/******************************************************************************
 * File:    cphoton.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.cphoton
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

#ifndef CPHOTON_H
#define CPHOTON_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <fstream>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------

#include <tuple>

struct Point2D {
    double x, y;
};

struct Point3D {
    double x, y, z;
};

typedef std::tuple<double, double, double> point3D;

typedef std::tuple<double, double, double> vector3D;

//======================================================================
// Class: CPhoton
//======================================================================
class CPhoton {
public:
    CPhoton();
    virtual ~CPhoton();

public:
    bool read(std::ifstream & ifs);
    double getWavelength();

public:
    double wcode, wl, x, y, z, u, v, w, h, t;
};


#endif  /* CPHOTON_H */
