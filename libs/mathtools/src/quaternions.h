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

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------

//======================================================================
// Namespace: Quaternions
//======================================================================
namespace MathTools {

// Forward declarations

template<class T> class Quaternion;

template<class T> Quaternion<T> operator+(const T &r, const Quaternion<T> &q);
template<class T> Quaternion<T> operator-(const T &r, const Quaternion<T> &q);
template<class T> Quaternion<T> operator*(const T &r, const Quaternion<T> &q);
template<class T> Quaternion<T> operator/(const T &r, const Quaternion<T> &q);

template<class T> ostream& operator<<(ostream &io, const Quaternion<T> &q);

//======================================================================
// Template class: Quaternion
//======================================================================
template<class T = double>
class Quaternion
{
public:
    T w, x, y, z;

    // Numerical constructor
    Quaternion(const T &w, const T &x, const T &y, const T &z): w(w), x(x), y(y), z(z) {};
    Quaternion(const T &x, const T &y, const T &z): w(T()), x(x), y(y), z(z) {}; // For 3-rotations
    Quaternion(const T &r): w(r), x(T()), y(T()), z(T()) {};
    Quaternion(): w(T()), x(T()), y(T()), z(T()) {};

    // Copy constructor and assignment
    Quaternion(const Quaternion &q): w(q.w), x(q.x), y(q.y), z(q.z) {};
    Quaternion& operator=(const Quaternion &q) { w=q.w; x=q.x; y=q.y; z=q.z; return *this; }

    // Unary operators
    Quaternion operator-() const { return Quaternion(-w, -x, -y, -z); }
    Quaternion operator~() const { return Quaternion(w, -x, -y, -z); } // Conjugate

    // Norm-squared. SQRT would have to be made generic to be used here
    T normSquared() const { return w*w + x*x + y*y + z*z; }
    void normalise() {
        T norm2 = normSquared();
        w /= norm2;
        x /= norm2;
        y /= norm2;
        z /= norm2;
    }

    // In-place operators
    Quaternion& operator+=(const T &r)
    { w += r; return *this; }
    Quaternion& operator+=(const Quaternion &q)
    { w += q.w; x += q.x; y += q.y; z += q.z; return *this; }

    Quaternion& operator-=(const T &r)
    { w -= r; return *this; }
    Quaternion& operator-=(const Quaternion &q)
    { w -= q.w; x -= q.x; y -= q.y; z -= q.z; return *this; }

    Quaternion& operator*=(const T &r)
    { w *= r; x *= r; y *= r; z *= r; return *this; }
    Quaternion& operator*=(const Quaternion &q)
    {
     T oldW(w), oldX(x), oldY(y), oldZ(z);
     w = oldW*q.w - oldX*q.x - oldY*q.y - oldZ*q.z;
     x = oldW*q.x + oldX*q.w + oldY*q.z - oldZ*q.y;
     y = oldW*q.y + oldY*q.w + oldZ*q.x - oldX*q.z;
     z = oldW*q.z + oldZ*q.w + oldX*q.y - oldY*q.x;
     return *this;
    }

    Quaternion& operator/=(const T &r)
    { w /= r; x /= r; y /= r; z /= r; return *this; }

    Quaternion& operator/=(const Quaternion &q)
    {
     T oldW(w), oldX(x), oldY(y), oldZ(z), n(q.normSquared());
     w = (oldW*q.w + oldX*q.x + oldY*q.y + oldZ*q.z) / n;
     x = (oldX*q.w - oldW*q.x + oldY*q.z - oldZ*q.y) / n;
     y = (oldY*q.w - oldW*q.y + oldZ*q.x - oldX*q.z) / n;
     z = (oldZ*q.w - oldW*q.z + oldX*q.y - oldY*q.x) / n;
     return *this;
    }

    // Binary operators based on in-place operators
    Quaternion operator+(const T &r) const { return Quaternion(*this) += r; }
    Quaternion operator+(const Quaternion &q) const { return Quaternion(*this) += q; }
    Quaternion operator-(const T &r) const { return Quaternion(*this) -= r; }
    Quaternion operator-(const Quaternion &q) const { return Quaternion(*this) -= q; }
    Quaternion operator*(const T &r) const { return Quaternion(*this) *= r; }
    Quaternion operator*(const Quaternion &q) const { return Quaternion(*this) *= q; }
    Quaternion operator/(const T &r) const { return Quaternion(*this) /= r; }
    Quaternion operator/(const Quaternion &q) const { return Quaternion(*this) /= q; }

    // Comparison operators, as much as they make sense
    bool operator==(const Quaternion &q) const
    { return (w == q.w) && (x == q.x) && (y == q.y) && (z == q.z); }
    bool operator!=(const Quaternion &q) const { return !operator==(q); }

    // Reflection of the vector part (sets scalar to 0) w.r.t. to unit vector given
    // as a quaternion with 0 scalar part
    void reflect(Quaternion q)
    { Quaternion p(*this); p.w = 0; q.normalise(); *this = q*p*q; }
    /*
    // The operators above allow quaternion op real. These allow real op quaternion.
    // Uses the above where appropriate.
    friend Quaternion operator+(const T &r, const Quaternion &q);
    friend Quaternion operator-(const T &r, const Quaternion &q);
    friend Quaternion operator*(const T &r, const Quaternion &q);
    friend Quaternion operator/(const T &r, const Quaternion &q);

    // Allows cout << q
    friend ostream& operator<<(ostream &io, const Quaternion &q);
    */

    // The operators above allow quaternion op real. These allow real op quaternion.
    // Uses the above where appropriate.
    friend Quaternion operator+(const T &r, const Quaternion &q)
    { return q+r; }

    friend Quaternion operator-(const T &r, const Quaternion &q)
    { return Quaternion(r-q.w, q.x, q.y, q.z); }

    friend Quaternion operator*(const T &r, const Quaternion &q)
    { return q*r; }

    friend Quaternion operator/(const T &r, const Quaternion &q)
    {
        T n(q.normSquared());
        return Quaternion<T>(r*q.w/n, -r*q.x/n, -r*q.y/n, -r*q.z/n);
    }

    // Allows cout << q
    friend ostream& operator<<(ostream &io, const Quaternion &q)
    {
        io << q.w;
        (q.x < T()) ? (io << " - " << (-q.x) << "i") : (io << " + " << q.x << "i");
        (q.y < T()) ? (io << " - " << (-q.y) << "j") : (io << " + " << q.y << "j");
        (q.z < T()) ? (io << " - " << (-q.z) << "k") : (io << " + " << q.z << "k");
        return io;
    }

};

}

#endif // QUATERNIONS_H
