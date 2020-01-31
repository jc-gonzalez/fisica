/******************************************************************************
 * File:    quaternions.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.quaternions
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

#ifndef QUATERNIONS_H
#define QUATERNIONS_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <iostream>
using namespace std;

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
#include "vectors.h"
#include "matrix.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

//======================================================================
// Class: quaternion
//======================================================================
class quaternion {

public:
    double w, x, y, z;

    // Numerical constructor
    quaternion(const double &w, const double &x, const double &y, const double &z);
    quaternion(const double &x, const double &y, const double &z); // For 3-rotations
    quaternion(const double &r);
    quaternion();

    quaternion(const vector3d& vec, const double ww=0.);
    quaternion(const matrix4& mat);

    // Copy constructor and assignment
    quaternion(const quaternion &q);
    quaternion& operator=(const quaternion &q);
    quaternion& operator=(const matrix4& m);

    // Unary operators
    quaternion operator-() const;
    quaternion operator~() const; // Conjugate

    // Norm-squared. SQRT would have to be made generic to be used here
    double norm2() const;
    double norm() const;
    void normalize();

    // Inverts this quaternion
    quaternion& inverse();

    // Creates a matrix from this quaternion
    vector3d getVector() const;

    // Creates a matrix from this quaternion
    matrix4 getMatrix() const;

    void getMatrix(matrix4 &dest,
                   const vector3d &center=vector3d()) const;
    void getMatrixCenter(matrix4 &dest,
                         const vector3d &center,
                         const vector3d &translation) const;

    // Creates a matrix from this quaternion
    void getMatrix_transposed(matrix4 &dest) const;

    bool isEqualTo(const quaternion& other, const double tolerance) const;

    double dot( quaternion& q2) const;

    // set this quaternion to the result of the linear interpolation between two quaternions
    quaternion& lerp(quaternion q1, quaternion q2, double time);
    // set this quaternion to the result of the interpolation between two quaternions
    quaternion& slerp(quaternion q1, quaternion q2, double time, double threshold);

    // Sets new quaternion
    quaternion& set(double ww, double a, double b, double c);

    // sets new quaternion based on euler angles
    quaternion& set(double a, double b, double c);
    quaternion& set(vector3d v, double ww);

    // sets new quaternion based on other quaternion
    quaternion& set(const quaternion& quat);

    // In-place operators
    quaternion& operator+=(const double &r);
    quaternion& operator+=(const quaternion &q);

    quaternion& operator-=(const double &r);
    quaternion& operator-=(const quaternion &q);

    quaternion& operator*=(const double &r);
    quaternion& operator*=(const quaternion &q);

    quaternion& operator/=(const double &r);
    quaternion& operator/=(const quaternion &q);

    // Binary operators based on in-place operators
    quaternion operator+(const double &r) const;
    quaternion operator+(const quaternion &q) const;
    quaternion operator-(const double &r) const;
    quaternion operator-(const quaternion &q) const;
    quaternion operator*(const double &r) const;
    quaternion operator*(const quaternion &q) const;
    quaternion operator/(const double &r) const;
    quaternion operator/(const quaternion &q) const;

    // Comparison operators, as much as they make sense
    bool operator==(const quaternion &q) const;
    bool operator!=(const quaternion &q) const;

    // Reflection of the vector part (sets scalar to 0) w.r.t. to unit vector given
    // as a quaternion with 0 scalar part
    void reflect(quaternion q);

    // set quaternion to identity
    quaternion& makeIdentity();

    quaternion& fromAngleAxis (double angle, const vector3d& axis);
    void toAngleAxis (double &angle, vector3d& axis) const;
    void toEuler(vector3d& euler) const;

    quaternion& rotationFromTo(const vector3d& from, const vector3d& to);

    // The operators above allow quaternion op real. These allow real op quaternion.
    // Uses the above where appropriate.
    friend quaternion operator+(const double &r, const quaternion &q);
    friend quaternion operator-(const double &r, const quaternion &q);
    friend quaternion operator*(const double &r, const quaternion &q);
    friend quaternion operator/(const double &r, const quaternion &q);

    // Allows cout << q
    friend ostream& operator<<(ostream &io, const quaternion &q);

};

}

#endif // QUATERNIONS_H
