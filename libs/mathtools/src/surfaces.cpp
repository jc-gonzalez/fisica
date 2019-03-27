/******************************************************************************
 * File:    surfaces.cpp
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

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <utility>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   nones
//------------------------------------------------------------
#include "surfaces.h"
#include "mathtools.h"
#include "comparisons.h"
#include "quaternions.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

class sphere;

//======================================================================
// Class: line
//======================================================================
line::line() {}

void line::fromTwoPoints(point3d x1, point3d x2) {
    a = x1, b = x2;
    o = a, l = (b-a).normalize();
}

void line::fromPointVector(point3d x, vector3d v) {
    o = x, l = v.normalize();
    a = x, b = a + l;
}

std::ostream& operator<<(std::ostream &io, const line &l) {
    io << "Line[" << l.a << "-" << l.b << ']';
    return io;
}

//======================================================================
// Class: cylinder
//======================================================================
cylinder::cylinder() {}
cylinder::cylinder(double radius) : r(radius) {}

void cylinder::set(double radius) { r = radius; }

std::ostream& operator<<(std::ostream &io, const cylinder &c) {
    io << "Cylinder[" << c.r << ']';
    return io;
}

//======================================================================
// Class: sphere
//======================================================================
sphere::sphere() {}
sphere::sphere(point3d o, double radius) : c(o), r(radius) {}

void sphere::set(point3d o, double radius) { c = o, r = radius; }

std::ostream& operator<<(std::ostream &io, const sphere &s) {
    io << "Sphere[" << s.c << "; " << s.r << ']';
    return io;
}

//======================================================================
// Class: paraboloid
//======================================================================
paraboloid::paraboloid() {}
paraboloid::paraboloid(point3d p, double foc) : o(p), f(foc) {}

void paraboloid::set(point3d p, double foc) { o = p; f = foc; }

std::ostream& operator<<(std::ostream &io, const paraboloid &p) {
    io << "Paraboloid[" << p.o << "; " << p.f << ']';
    return io;
}

//======================================================================
// Topic: Friend functions
//======================================================================

bool intersectionSphereLine(sphere & s, line & l, std::vector<point3d> & p)
{
    // The intersection points of the Sphere(c, r) and the Line(o, l) are:
    //     x = o + dl
    // where the valid d are the roots of the quadratic equation:
    //     a d^2 + b d + c = 0
    // where
    // a = l . l = ||l||^2
    // b = 2 [l.(o - c)]
    // c = (o - c).(o - c) - r^2 = ||o - c||^2 - r^2

    vector3d co = l.o - s.c;

    double a = l.l.norm2();
    double b = 2. * l.l.dot(co);
    double c = co.norm2() - sqr(s.r);

    double delta2 = b * b - 4. * a * c;
    if (delta2 < 0.) { return false; }
    double delta = sqrt(delta2);
    if (iszero(delta)) {
        // Only one point (tangent)
        p.clear();
        double d = -b / (2. * a);
        p.push_back(l.o + (l.l * d));
        return true;
    }
    // Two points
    p.clear();
    double d1 = (-b + delta) / (2. * a);
    double d2 = (-b - delta) / (2. * a);
    point3d p1 = l.o + (l.l * d1);
    point3d p2 = l.o + (l.l * d2);
    if (p2.Z < p1.Z) { std::swap(p1, p2); }
    p.push_back(p1);
    p.push_back(p2);
    return true;
}

bool intersectionCylinderLine(cylinder & cl, line & l, std::vector<point3d> & p)
{
    // The intersection points of the vertical Cylinder(r) and Line(o, l) are:
    //     x = o + dl
    // where the valid d are the roots of the quadratic equation:
    //     a d^2 + b d + c = 0
    // where
    // a = l1^2 + l2^2
    // b = 2 (o1l1 + o2l2)
    // c = (o1^2 + o2^2 - r^2)

    double a = sqr(l.l.X) + sqr(l.l.Y);
    double b = 2. * (l.o.X * l.l.X + l.o.Y * l.l.Y);
    double c = sqr(l.o.X) + sqr(l.o.Y) - sqr(cl.r);

    double delta2 = b * b - 4. * a * c;
    if (delta2 < 0.) { return false; }
    double delta = sqrt(delta2);
    if (iszero(delta)) {
        // Only one point (tangent)
        p.clear();
        double d = -b / (2. * a);
        p.push_back(l.o + (l.l * d));
        return true;
    }
    // Two points
    p.clear();
    double d1 = (-b + delta) / (2. * a);
    double d2 = (-b - delta) / (2. * a);
    point3d p1 = l.o + (l.l * d1);
    point3d p2 = l.o + (l.l * d2);
    //if (p2.Z < p1.Z) { std::swap(p1, p2); }
    p.push_back(p1);
    p.push_back(p2);
    return true;
}

bool intersectionParaboloidLine(paraboloid & p, line & l, std::vector<point3d> & pts)
{
    double a = sqr(l.l.X) + sqr(l.l.Y);
    double b = 2. * (l.o.X * l.l.X + l.o.Y * l.l.Y - p.f * l.l.Z);
    double c = sqr(l.o.X) + sqr(l.o.Y) - 2 * p.f * l.o.Z;
    
    double delta2 = b * b - 4. * a * c;
    if (delta2 < 0.) { return false; }
    double delta = sqrt(delta2);
    if (iszero(delta)) {
        // Only one point (tangent)
        pts.clear();
        double d = -b / (2. * a);
        pts.push_back(l.o + (l.l * d));
        return true;
    }
    // Two points
    pts.clear();
    double d1 = (-b + delta) / (2. * a);
    double d2 = (-b - delta) / (2. * a);
    point3d p1 = l.o + (l.l * d1);
    point3d p2 = l.o + (l.l * d2);
    //if (p2.Z < p1.Z) { std::swap(p1, p2); }
    pts.push_back(p1);
    pts.push_back(p2);
    return true;
}

}

