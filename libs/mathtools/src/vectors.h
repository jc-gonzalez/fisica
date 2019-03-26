/******************************************************************************
 * File:    vectors.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.vectors
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

#ifndef VECTORS_H
#define VECTORS_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <iostream>
#include <cmath>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   nones
//------------------------------------------------------------
#include "comparisons.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

//======================================================================
// Class: vector3d
//======================================================================
class vector3d {

public:
    vector3d();
    vector3d(double nx, double ny, double nz);
    explicit vector3d(double n);
    vector3d(const vector3d& other);

    // operators

    vector3d operator-() const;

    vector3d& operator=(const vector3d& other);

    vector3d operator+(const vector3d& other) const;
    vector3d& operator+=(const vector3d& other);
    vector3d operator+(const double val) const;
    vector3d& operator+=(const double val);

    vector3d operator-(const vector3d& other) const;
    vector3d& operator-=(const vector3d& other);
    vector3d operator-(const double val) const;
    vector3d& operator-=(const double val);

    vector3d operator*(const vector3d& other) const;
    vector3d& operator*=(const vector3d& other);
    vector3d operator*(const double v) const;
    vector3d& operator*=(const double v);

    vector3d operator/(const vector3d& other) const;
    vector3d& operator/=(const vector3d& other);
    vector3d operator/(const double v) const;
    vector3d& operator/=(const double v);

    bool operator<=(const vector3d&other) const;
    bool operator>=(const vector3d&other) const;
    bool operator<(const vector3d&other) const;
    bool operator>(const vector3d&other) const;

    bool operator==(const vector3d& other) const;
    bool operator!=(const vector3d& other) const;

    double & operator[](int i);
    const double & operator[](int i) const;

    // Functions

    bool isEqualTo(const vector3d& other, const double tolerance = Epsilon) const;

    vector3d& set(const double nx, const double ny, const double nz);
    vector3d& set(const vector3d& p);

    double norm2() const;
    double norm() const;

    double length2() const;
    double length() const;

    double dot(const vector3d& other) const;

    double distanceFrom(const vector3d& other) const;

    double distanceFrom2(const vector3d& other) const;

    vector3d cross(const vector3d& p) const;

    bool isBetweenPoints(const vector3d& begin, const vector3d& end) const;

    vector3d& normalize();

    vector3d& setLength(double newlength);

    vector3d& invert();

    void rotateXZBy(double degrees, const vector3d& center=vector3d());
    void rotateXYBy(double degrees, const vector3d& center=vector3d());
    void rotateYZBy(double degrees, const vector3d& center=vector3d());

    vector3d interpolate(const vector3d& other, double d) const;
    vector3d interpolate_quadratic(const vector3d& v2, const vector3d& v3, double d) const;
    vector3d& interpolate(const vector3d& a, const vector3d& b, double d);

    vector3d horizontalAngle() const;

    vector3d sphericalCoordinateAngles() const;

    vector3d rotationToDirection(const vector3d & forwards = vector3d(0, 0, 1)) const;

    void as4Values(double* array) const;
    void as3Values(double* array) const;

    // Allows cout << v
    friend std::ostream& operator<<(std::ostream &io, const vector3d &v);

    double X, Y, Z;
};

using point3d = vector3d;

}

#endif // VECTORS_H
