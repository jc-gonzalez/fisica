/******************************************************************************
 * File:    surfaces.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.surfaces
 *
 * Last update:  2.0
 *
 * Date:    2019/03/13
 *
 * Author:  J C Gonzalez
 *
 * Copyright (C) 2019 by J C Gonzalez
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

#ifndef SURFACES_H
#define SURFACES_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <iostream>
#include <vector>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   nones
//------------------------------------------------------------
#include "vectors.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

class sphere;
class cylinder;
class paraboloid;

//======================================================================
// Class: line
//======================================================================
class line {
public:
    point3d a, b, o;
    vector3d l;

    line();

    void fromTwoPoints(point3d x1 = point3d(0., 0., 0.),
                       point3d x2 = point3d(1., 1., 1.));
    void fromPointVector(point3d x = point3d(0., 0., 0.),
                         vector3d v = vector3d(1., 1., 1.));

    friend bool intersectionSphereLine(sphere & s, line & l, std::vector<point3d> & p);
    friend bool intersectionCylinderLine(cylinder & c, line & l, std::vector<point3d> & p);
    friend bool intersectionParaboloidLine(paraboloid & c, line & l, std::vector<point3d> & p);

    friend std::ostream& operator<<(std::ostream &io, const line &l);
};

//======================================================================
// Class: cylinder
//======================================================================
class cylinder {
public:
    double r;

    cylinder();
    cylinder(double radius);

    void set(double radius);

    friend bool intersectionCylinderLine(cylinder & c, line & l, std::vector<point3d> & p);

    friend std::ostream& operator<<(std::ostream &io, const cylinder &c);
};

//======================================================================
// Class: sphere
//======================================================================
class sphere {
public:
    point3d c;
    double r;

    sphere();
    sphere(point3d o, double radius);

    void set(point3d o, double radius);

    friend bool intersectionSphereLine(sphere & s, line & l, std::vector<point3d> & p);

    friend std::ostream& operator<<(std::ostream &io, const sphere &s);
};

//======================================================================
// Class: paraboloid
//======================================================================
class paraboloid {
public:
    point3d o;
    double f;

    paraboloid();
    paraboloid(point3d p, double foc);

    void set(point3d p, double foc);

    friend bool intersectionParaboloidLine(paraboloid & p, line & l, std::vector<point3d> & pts);

    friend std::ostream& operator<<(std::ostream &io, const paraboloid &p);
};

bool intersectionSphereLine(sphere & s, line & l, std::vector<point3d> & p);
bool intersectionCylinderLine(cylinder & cl, line & l, std::vector<point3d> & p);
bool intersectionParaboloidLine(paraboloid & c, line & l, std::vector<point3d> & p);

}

#endif // SURFACES_H
