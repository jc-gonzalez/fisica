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

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   nones
//------------------------------------------------------------
#include "comparisons.h"
#include "angles.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

//======================================================================
// Class: vector3d
//======================================================================
class vector3d {

public:
    vector3d() : X(0), Y(0), Z(0) {}
    vector3d(double nx, double ny, double nz) : X(nx), Y(ny), Z(nz) {}
    explicit vector3d(double n) : X(n), Y(n), Z(n) {}
    vector3d(const vector3d& other) : X(other.X), Y(other.Y), Z(other.Z) {}

    // operators

    vector3d operator-() const
    { return vector3d(-X, -Y, -Z); }

    vector3d& operator=(const vector3d& other)
    { X = other.X; Y = other.Y; Z = other.Z; return *this; }

    vector3d operator+(const vector3d& other) const
    { return vector3d(X + other.X, Y + other.Y, Z + other.Z); }
    vector3d& operator+=(const vector3d& other)
    { X+=other.X; Y+=other.Y; Z+=other.Z; return *this; }
    vector3d operator+(const double val) const
    { return vector3d(X + val, Y + val, Z + val); }
    vector3d& operator+=(const double val)
    { X+=val; Y+=val; Z+=val; return *this; }

    vector3d operator-(const vector3d& other) const
    { return vector3d(X - other.X, Y - other.Y, Z - other.Z); }
    vector3d& operator-=(const vector3d& other)
    { X-=other.X; Y-=other.Y; Z-=other.Z; return *this; }
    vector3d operator-(const double val) const
    { return vector3d(X - val, Y - val, Z - val); }
    vector3d& operator-=(const double val)
    { X-=val; Y-=val; Z-=val; return *this; }

    vector3d operator*(const vector3d& other) const
    { return vector3d(X * other.X, Y * other.Y, Z * other.Z); }
    vector3d& operator*=(const vector3d& other)
    { X*=other.X; Y*=other.Y; Z*=other.Z; return *this; }
    vector3d operator*(const double v) const
    { return vector3d(X * v, Y * v, Z * v); }
    vector3d& operator*=(const double v)
    { X*=v; Y*=v; Z*=v; return *this; }

    vector3d operator/(const vector3d& other) const
    { return vector3d(X / other.X, Y / other.Y, Z / other.Z); }
    vector3d& operator/=(const vector3d& other)
    { X/=other.X; Y/=other.Y; Z/=other.Z; return *this; }
    vector3d operator/(const double v) const
    { double i=(double)1.0/v; return vector3d(X * i, Y * i, Z * i); }
    vector3d& operator/=(const double v)
    { double i=(double)1.0/v; X*=i; Y*=i; Z*=i; return *this; }

    bool operator<=(const vector3d&other) const
    {
        return  (X<other.X || equals(X, other.X)) ||
                (equals(X, other.X) && (Y<other.Y || equals(Y, other.Y))) ||
                (equals(X, other.X) && equals(Y, other.Y) && (Z<other.Z || equals(Z, other.Z)));
    }

    bool operator>=(const vector3d&other) const
    {
        return  (X>other.X || equals(X, other.X)) ||
                (equals(X, other.X) && (Y>other.Y || equals(Y, other.Y))) ||
                (equals(X, other.X) && equals(Y, other.Y) && (Z>other.Z || equals(Z, other.Z)));
    }

    bool operator<(const vector3d&other) const
    {
        return  (X<other.X && !equals(X, other.X)) ||
                (equals(X, other.X) && Y<other.Y && !equals(Y, other.Y)) ||
                (equals(X, other.X) && equals(Y, other.Y) && Z<other.Z && !equals(Z, other.Z));
    }

    bool operator>(const vector3d&other) const
    {
        return  (X>other.X && !equals(X, other.X)) ||
                (equals(X, other.X) && Y>other.Y && !equals(Y, other.Y)) ||
                (equals(X, other.X) && equals(Y, other.Y) && Z>other.Z && !equals(Z, other.Z));
    }

    bool operator==(const vector3d& other) const
    { return this->isEqualTo(other); }

    bool operator!=(const vector3d& other) const
    { return !this->isEqualTo(other); }

    double & operator[](int i) {
        if (i == 0) {return X;}
        else if (i == 1) {return Y;}
        else {return Z;}
    }

    const double & operator[](int i) const {
        if (i == 0) {return X;}
        else if (i == 1) {return Y;}
        else {return Z;}
    }

    // Functions

    bool isEqualTo(const vector3d& other, const double tolerance = Epsilon) const
    {
        return (equals(X, other.X, tolerance) &&
                equals(Y, other.Y, tolerance) &&
                equals(Z, other.Z, tolerance));
    }

    vector3d& set(const double nx, const double ny, const double nz)
    {X=nx; Y=ny; Z=nz; return *this;}
    vector3d& set(const vector3d& p)
    {X=p.X; Y=p.Y; Z=p.Z;return *this;}

    double norm2() const { return X*X + Y*Y + Z*Z; }
    double norm() const { return sqrt(norm2()); }

    double length2() const { return norm2(); }
    double length() const { return norm(); }

    double dot(const vector3d& other) const
    { return X*other.X + Y*other.Y + Z*other.Z; }

    double distanceFrom(const vector3d& other) const
    { return vector3d(X - other.X, Y - other.Y, Z - other.Z).length(); }

    double distanceFrom2(const vector3d& other) const
    { return vector3d(X - other.X, Y - other.Y, Z - other.Z).length2(); }

    vector3d cross(const vector3d& p) const
    { return vector3d(Y * p.Z - Z * p.Y, Z * p.X - X * p.Z, X * p.Y - Y * p.X); }

    bool isBetweenPoints(const vector3d& begin, const vector3d& end) const
    {
        const double f = (end - begin).length2();
        return (distanceFrom2(begin) <= f) && (distanceFrom2(end) <= f);
    }

    vector3d& normalize()
    {
        double length = norm2();
        if (length == 0 ) // this check isn't an optimization but prevents getting NAN in the sqrt.
            return *this;
        length = 1 / sqrt(length);

        X = (double)(X * length);
        Y = (double)(Y * length);
        Z = (double)(Z * length);
        return *this;
    }

    vector3d& setLength(double newlength)
    { normalize(); return (*this *= newlength); }

    vector3d& invert()
    { X *= -1, Y *= -1, Z *= -1; return *this; }

    void rotateXZBy(double degrees, const vector3d& center=vector3d())
    {
        degrees = d2r(degrees);
        double cs = cos(degrees);
        double sn = sin(degrees);
        X -= center.X;
        Z -= center.Z;
        set((double)(X*cs - Z*sn), Y, (double)(X*sn + Z*cs));
        X += center.X;
        Z += center.Z;
    }

    void rotateXYBy(double degrees, const vector3d& center=vector3d())
    {
        degrees = d2r(degrees);
        double cs = cos(degrees);
        double sn = sin(degrees);
        X -= center.X;
        Y -= center.Y;
        set((double)(X*cs - Y*sn), (double)(X*sn + Y*cs), Z);
        X += center.X;
        Y += center.Y;
    }

    void rotateYZBy(double degrees, const vector3d& center=vector3d())
    {
        degrees = d2r(degrees);
        double cs = cos(degrees);
        double sn = sin(degrees);
        Z -= center.Z;
        Y -= center.Y;
        set(X, (double)(Y*cs - Z*sn), (double)(Y*sn + Z*cs));
        Z += center.Z;
        Y += center.Y;
    }

    vector3d interpolate(const vector3d& other, double d) const
    {
        const double inv = 1.0 - d;
        return vector3d((double)(other.X*inv + X*d), (double)(other.Y*inv + Y*d), (double)(other.Z*inv + Z*d));
    }

    vector3d interpolate_quadratic(const vector3d& v2, const vector3d& v3, double d) const
    {
        // this*(1-d)*(1-d) + 2 * v2 * (1-d) + v3 * d * d;
        const double inv = (double) 1.0 - d;
        const double mul0 = inv * inv;
        const double mul1 = (double) 2.0 * d * inv;
        const double mul2 = d * d;

        return vector3d ((double)(X * mul0 + v2.X * mul1 + v3.X * mul2),
                (double)(Y * mul0 + v2.Y * mul1 + v3.Y * mul2),
                (double)(Z * mul0 + v2.Z * mul1 + v3.Z * mul2));
    }

    vector3d& interpolate(const vector3d& a, const vector3d& b, double d)
    {
        X = (double)((double)b.X + ( ( a.X - b.X ) * d ));
        Y = (double)((double)b.Y + ( ( a.Y - b.Y ) * d ));
        Z = (double)((double)b.Z + ( ( a.Z - b.Z ) * d ));
        return *this;
    }

    vector3d horizontalAngle() const
    {
        vector3d angle;

        const double tmp = r2d(atan2(X, Z));
        angle.Y = (double)tmp;

        if (angle.Y < 0)
            angle.Y += 360;
        if (angle.Y >= 360)
            angle.Y -= 360;

        const double z1 = sqrt(X*X + Z*Z);

        angle.X = r2d(atan2((double)z1, (double)Y)) - 90.0;

        if (angle.X < 0.) angle.X += 360;
        if (angle.X >= 360) angle.X -= 360;

        return angle;
    }

    vector3d sphericalCoordinateAngles() const
    {
        vector3d angle;
        const double length = X*X + Y*Y + Z*Z;

        if (length) {
            if (X!=0) {
                angle.Y = r2d(atan2(Z, X));
            } else if (Z<0) {
                angle.Y = 180.;
            }
            angle.X = r2d(acos(Y / length));
        }
        return angle;
    }

    vector3d rotationToDirection(const vector3d & forwards = vector3d(0, 0, 1)) const
    {
        const double cr = cos( d2r(X) );
        const double sr = sin( d2r(X) );
        const double cp = cos( d2r(Y) );
        const double sp = sin( d2r(Y) );
        const double cy = cos( d2r(Z) );
        const double sy = sin( d2r(Z) );

        const double srsp = sr*sp;
        const double crsp = cr*sp;

        const double pseudoMatrix[] = {
            ( cp*cy ), ( cp*sy ), ( -sp ),
            ( srsp*cy-cr*sy ), ( srsp*sy+cr*cy ), ( sr*cp ),
            ( crsp*cy+sr*sy ), ( crsp*sy-sr*cy ), ( cr*cp )};

        return vector3d( (double)(forwards.X * pseudoMatrix[0] +
                                  forwards.Y * pseudoMatrix[3] +
                                  forwards.Z * pseudoMatrix[6]),
                         (double)(forwards.X * pseudoMatrix[1] +
                                  forwards.Y * pseudoMatrix[4] +
                                  forwards.Z * pseudoMatrix[7]),
                         (double)(forwards.X * pseudoMatrix[2] +
                                  forwards.Y * pseudoMatrix[5] +
                                  forwards.Z * pseudoMatrix[8]));
    }

    void as4Values(double* array) const {
        array[0] = X;
        array[1] = Y;
        array[2] = Z;
        array[3] = 0;
    }

    void as3Values(double* array) const {
        array[0] = X;
        array[1] = Y;
        array[2] = Z;
    }

    // Allows cout << v
    friend ostream& operator<<(ostream &io, const vector3d &v) {
        io << '(' << v.X << ", " << v.Y << ", " << v.Z << ')';
        return io;
    }

    double X, Y, Z;
};

using point3d = vector3d;

}

#endif // VECTORS_H
