/******************************************************************************
 * File:    quaternions.cpp
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

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <cmath>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   nones
//------------------------------------------------------------
#include "quaternions.h"
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
quaternion::quaternion(const double &w, const double &x, const double &y,
                       const double &z)
    : w(w), x(x), y(y), z(z) {}
quaternion::quaternion(const double &x, const double &y, const double &z)
    : w(0.0), x(x), y(y), z(z) {}
quaternion::quaternion(const double &r)
    : w(r), x(0.0), y(0.0), z(0.0) {}
quaternion::quaternion()
    : w(0.0), x(0.0), y(0.0), z(0.0) {}

quaternion::quaternion(const vector3d& vec, const double ww)
{ set(ww, vec.X, vec.Y, vec.Z); }
quaternion::quaternion(const matrix4& mat)
{ (*this) = mat; }

// Copy constructor and assignment
quaternion::quaternion(const quaternion &q)
    : w(q.w), x(q.x), y(q.y), z(q.z) {};
quaternion& quaternion::operator=(const quaternion &q)
{ w=q.w; x=q.x; y=q.y; z=q.z; return *this; }
quaternion& quaternion::operator=(const matrix4& m) {
    const double diag = m[0] + m[5] + m[10] + 1;

    if (diag > 0.0) {
        const double scale = sqrt(diag) * 2.0; // get scale from diagonal
        // TODO: speed this up
        x = (m[6] - m[9]) / scale;
        y = (m[8] - m[2]) / scale;
        z = (m[1] - m[4]) / scale;
        w = 0.25 * scale;
    } else {
        if (m[0]>m[5] && m[0]>m[10]) {
            // 1st element of diag is greatest value
            // find scale according to 1st element, and double it
            const double scale = sqrt(1.0 + m[0] - m[5] - m[10]) * 2.0;
            // TODO: speed this up
            x = 0.25 * scale;
            y = (m[4] + m[1]) / scale;
            z = (m[2] + m[8]) / scale;
            w = (m[6] - m[9]) / scale;
        } else if (m[5]>m[10]) {
            // 2nd element of diag is greatest value
            // find scale according to 2nd element, and double it
            const double scale = sqrt(1.0 + m[5] - m[0] - m[10]) * 2.0;
            // TODO: speed this up
            x = (m[4] + m[1]) / scale;
            y = 0.25 * scale;
            z = (m[9] + m[6]) / scale;
            w = (m[8] - m[2]) / scale;
        } else {
            // 3rd element of diag is greatest value
            // find scale according to 3rd element, and double it
            const double scale = sqrt(1.0 + m[10] - m[0] - m[5]) * 2.0;
            // TODO: speed this up
            x = (m[8] + m[2]) / scale;
            y = (m[9] + m[6]) / scale;
            z = 0.25 * scale;
            w = (m[1] - m[4]) / scale;
        }
    }

    normalize();
    return *this;
}

// Unary operators
quaternion quaternion::operator-() const
{ return quaternion(-w, -x, -y, -z); }
quaternion quaternion::operator~() const
{ return quaternion(w, -x, -y, -z); } // Conjugate

// Norm-squared. SQRT would have to be made generic to be used here
double quaternion::norm2() const
{ return w*w + x*x + y*y + z*z; }
double quaternion::norm() const
{ return sqrt(norm2()); }

void quaternion::normalize() {
    double n = norm();
    w /= n, x /= n, y /= n, z /= n;
}

// Inverts this quaternion
quaternion& quaternion::inverse() {
    x = -x, y = -y, z = -z;
    return *this;
}

// Creates a matrix from this quaternion
vector3d quaternion::getVector() const {
    return vector3d(x, y, z);
}

// Creates a matrix from this quaternion
matrix4 quaternion::getMatrix() const {
    matrix4 m;
    getMatrix(m);
    return m;
}

void quaternion::getMatrix(matrix4 &dest,
                           const vector3d &center) const{
    dest[ 0] = 1.0 - 2.0*y*y - 2.0*z*z;
    dest[ 1] = 2.0*x*y + 2.0*z*w;
    dest[ 2] = 2.0*x*z - 2.0*y*w;
    dest[ 3] = 0.0;

    dest[ 4] = 2.0*x*y - 2.0*z*w;
    dest[ 5] = 1.0 - 2.0*x*x - 2.0*z*z;
    dest[ 6] = 2.0*z*y + 2.0*x*w;
    dest[ 7] = 0.0;

    dest[ 8] = 2.0*x*z + 2.0*y*w;
    dest[ 9] = 2.0*z*y - 2.0*x*w;
    dest[10] = 1.0 - 2.0*x*x - 2.0*y*y;
    dest[11] = 0.0;

    dest[12] = center.X;
    dest[13] = center.Y;
    dest[14] = center.Z;
    dest[15] = 1.0;
}

void quaternion::getMatrixCenter(matrix4 &dest,
                                 const vector3d &center,
                                 const vector3d &translation) const {
    dest[ 0] = 1.0 - 2.0*y*y - 2.0*z*z;
    dest[ 1] = 2.0*x*y + 2.0*z*w;
    dest[ 2] = 2.0*x*z - 2.0*y*w;
    dest[ 3] = 0.0;

    dest[ 4] = 2.0*x*y - 2.0*z*w;
    dest[ 5] = 1.0 - 2.0*x*x - 2.0*z*z;
    dest[ 6] = 2.0*z*y + 2.0*x*w;
    dest[ 7] = 0.0;

    dest[ 8] = 2.0*x*z + 2.0*y*w;
    dest[ 9] = 2.0*z*y - 2.0*x*w;
    dest[10] = 1.0 - 2.0*x*x - 2.0*y*y;
    dest[11] = 0.0;

    // dest.setRotationCenter ( center, translation );
}

// Creates a matrix from this quaternion
void quaternion::getMatrix_transposed(matrix4 &dest) const {
    dest[ 0] = 1.0 - 2.0*y*y - 2.0*z*z;
    dest[ 4] = 2.0*x*y + 2.0*z*w;
    dest[ 8] = 2.0*x*z - 2.0*y*w;
    dest[12] = 0.0;

    dest[ 1] = 2.0*x*y - 2.0*z*w;
    dest[ 5] = 1.0 - 2.0*x*x - 2.0*z*z;
    dest[ 9] = 2.0*z*y + 2.0*x*w;
    dest[13] = 0.0;

    dest[ 2] = 2.0*x*z + 2.0*y*w;
    dest[ 6] = 2.0*z*y - 2.0*x*w;
    dest[10] = 1.0 - 2.0*x*x - 2.0*y*y;
    dest[14] = 0.0;

    dest[ 3] = 0.0;
    dest[ 7] = 0.0;
    dest[11] = 0.0;
    dest[15] = 1.0;

    // dest.setDefinitelyIdentityMatrix(false);
}

bool quaternion::isEqualTo(const quaternion& other, const double tolerance) const {
    return (equals(x, other.x, tolerance) &&
            equals(y, other.y, tolerance) &&
            equals(z, other.z, tolerance) &&
            equals(w, other.w, tolerance));
}

double quaternion::dot( quaternion& q2) const {
    return (x * q2.x) + (y * q2.y) + (z * q2.z) + (w * q2.w);
}

// set this quaternion to the result of the linear interpolation between two quaternions
quaternion& quaternion::lerp(quaternion q1, quaternion q2, double time) {
    const double scale = 1.0 - time;
    return (*this = (q1*scale) + (q2*time));
}


// set this quaternion to the result of the interpolation between two quaternions
quaternion& quaternion::slerp(quaternion q1, quaternion q2, double time, double threshold)
{
    double angle = q1.dot(q2);

    // make sure we use the short rotation
    if (angle < 0.0) {
        q1 *= -1.0;
        angle *= -1.0;
    }

    if (angle <= (1-threshold)) { // spherical interpolation
        const double theta = acos(angle);
        const double invsintheta = 1.0 / sin(theta);
        const double scale = sin(theta * (1.0-time)) * invsintheta;
        const double invscale = sin(theta * time) * invsintheta;
        return (*this = (q1*scale) + (q2*invscale));
    } else { // linear interploation
        return lerp(q1,q2,time);
    }
}

// Sets new quaternion
quaternion& quaternion::set(double ww, double a, double b, double c) {
    w = ww, x = a, y = b, z = c;
    return *this;
}

// sets new quaternion based on euler angles
quaternion& quaternion::set(double a, double b, double c) {
    double angle;

    angle = a * 0.5;
    const double sr = sin(angle);
    const double cr = cos(angle);

    angle = b * 0.5;
    const double sp = sin(angle);
    const double cp = cos(angle);

    angle = c * 0.5;
    const double sy = sin(angle);
    const double cy = cos(angle);

    const double cpcy = cp * cy;
    const double spcy = sp * cy;
    const double cpsy = cp * sy;
    const double spsy = sp * sy;

    w = (double)(cr * cpcy + sr * spsy);
    x = (double)(sr * cpcy - cr * spsy);
    y = (double)(cr * spcy + sr * cpsy);
    z = (double)(cr * cpsy - sr * spcy);

    normalize();
    return *this;
}

quaternion& quaternion::set(vector3d v, double ww) {
    w = ww, x = v.X, y = v.Y, z = v.Z;
    return *this;
}

// sets new quaternion based on other quaternion
quaternion& quaternion::set(const quaternion& quat) {
    return (*this=quat);
}

// In-place operators
quaternion& quaternion::operator+=(const double &r)
{ w += r; return *this; }
quaternion& quaternion::operator+=(const quaternion &q)
{ w += q.w; x += q.x; y += q.y; z += q.z; return *this; }

quaternion& quaternion::operator-=(const double &r)
{ w -= r; return *this; }
quaternion& quaternion::operator-=(const quaternion &q)
{ w -= q.w; x -= q.x; y -= q.y; z -= q.z; return *this; }

quaternion& quaternion::operator*=(const double &r)
{ w *= r; x *= r; y *= r; z *= r; return *this; }
quaternion& quaternion::operator*=(const quaternion &q)
{
    double oldW(w), oldX(x), oldY(y), oldZ(z);
    w = oldW*q.w - oldX*q.x - oldY*q.y - oldZ*q.z;
    x = oldW*q.x + oldX*q.w + oldY*q.z - oldZ*q.y;
    y = oldW*q.y + oldY*q.w + oldZ*q.x - oldX*q.z;
    z = oldW*q.z + oldZ*q.w + oldX*q.y - oldY*q.x;
    return *this;
}

quaternion& quaternion::operator/=(const double &r)
{ w /= r; x /= r; y /= r; z /= r; return *this; }

quaternion& quaternion::operator/=(const quaternion &q)
{
    double oldW(w), oldX(x), oldY(y), oldZ(z), n(q.norm2());
    w = (oldW*q.w + oldX*q.x + oldY*q.y + oldZ*q.z) / n;
    x = (oldX*q.w - oldW*q.x + oldY*q.z - oldZ*q.y) / n;
    y = (oldY*q.w - oldW*q.y + oldZ*q.x - oldX*q.z) / n;
    z = (oldZ*q.w - oldW*q.z + oldX*q.y - oldY*q.x) / n;
    return *this;
}

// Binary operators based on in-place operators
quaternion quaternion::operator+(const double &r) const { return quaternion(*this) += r; }
quaternion quaternion::operator+(const quaternion &q) const { return quaternion(*this) += q; }
quaternion quaternion::operator-(const double &r) const { return quaternion(*this) -= r; }
quaternion quaternion::operator-(const quaternion &q) const { return quaternion(*this) -= q; }
quaternion quaternion::operator*(const double &r) const { return quaternion(*this) *= r; }
quaternion quaternion::operator*(const quaternion &q) const { return quaternion(*this) *= q; }
quaternion quaternion::operator/(const double &r) const { return quaternion(*this) /= r; }
quaternion quaternion::operator/(const quaternion &q) const { return quaternion(*this) /= q; }

// Comparison operators, as much as they make sense
bool quaternion::operator==(const quaternion &q) const
{ return (w == q.w) && (x == q.x) && (y == q.y) && (z == q.z); }
bool quaternion::operator!=(const quaternion &q) const { return !operator==(q); }

// Reflection of the vector part (sets scalar to 0) w.r.t. to unit vector given
// as a quaternion with 0 scalar part
void quaternion::reflect(quaternion q) {
    quaternion p(*this); p.w = 0; q.normalize(); *this = q*p*q;
}

// set quaternion to identity
quaternion& quaternion::makeIdentity() {
    w = 1.0, x = 0.0, y = 0.0, z = 0.0;
    return *this;
}

quaternion& quaternion::fromAngleAxis(double angle, const vector3d& axis) {
    const double fHalfAngle = 0.5*angle;
    const double fSin = sin(fHalfAngle);

    w = cos(fHalfAngle);
    x = fSin * axis[0];
    y = fSin * axis[1];
    z = fSin * axis[2];
    return *this;
}

void quaternion::toAngleAxis(double &angle, vector3d& axis) const {
    const double scale = sqrt(x*x + y*y + z*z);

    if (iszero(scale) || w > 1.0 || w < -1.0) {
        angle = 0.0;
        axis[0] = 0.0;
        axis[1] = 1.0;
        axis[2] = 0.0;
    } else {
        const double invscale = 1.0 / scale;
        angle = 2.0 * acos(w);
        axis[0] = x * invscale;
        axis[1] = y * invscale;
        axis[2] = z * invscale;
    }
}

void quaternion::toEuler(vector3d& euler) const {
    const double sqw = w*w;
    const double sqx = x*x;
    const double sqy = y*y;
    const double sqz = z*z;
    const double test = 2.0 * (y*w - x*z);

    if (equals(test, 1.0, 0.000001)) {
        // heading = rotation about z-axis
        euler[2] = (double) (-2.0*atan2(x, w));
        // bank = rotation about x-axis
        euler[0] = 0;
        // attitude = rotation about y-axis
        euler[1] = (double) (M_PI/2.0);
    } else if (equals(test, -1.0, 0.000001)) {
        // heading = rotation about z-axis
        euler[2] = (double) (2.0*atan2(x, w));
        // bank = rotation about x-axis
        euler[0] = 0;
        // attitude = rotation about y-axis
        euler[1] = (double) (M_PI/-2.0);
    } else {
        // heading = rotation about z-axis
        euler[2] = (double) atan2(2.0 * (x*y +z*w),(sqx - sqy - sqz + sqw));
        // bank = rotation about x-axis
        euler[0] = (double) atan2(2.0 * (y*z +x*w),(-sqx - sqy + sqz + sqw));
        // attitude = rotation about y-axis
        euler[1] = (double) asin( ((test < -1.0) ? -1.0 : ((test > 1.0) ? 1.0 : test)) );
    }
}

quaternion& quaternion::rotationFromTo(const vector3d& from, const vector3d& to) {
    // Based on Stan Melax's article in Game Programming Gems
    // Copy, since cannot modify local
    vector3d v0 = from;
    vector3d v1 = to;
    v0.normalize();
    v1.normalize();

    const double d = v0.dot(v1);
    if (d >= 1.0) { // If dot == 1, vectors are the same
        return makeIdentity();
    } else if (d <= -1.0) { // exactly opposite
        vector3d axis {1.0, 0.0, 0.0};
        axis = axis.cross(v0);
        if (axis.length() == 0) {
            axis.set(0.0, 1.0, 0.0);
            axis = axis.cross(v0);
        }
        // same as fromAngleAxis(PI, axis).normalize();
        set(axis, 0);
        normalize();
        return *this;
    }

    const double s = sqrt((1.0 + d) * 2.0); // optimize inv_sqrt
    const double invs = 1.0 / s;
    vector3d c = v0.cross(v1) * invs;
    set(c, s * 0.5);
    normalize();
    return *this;
}

// The operators above allow quaternion op real. These allow real op quaternion.
// Uses the above where appropriate.
quaternion operator+(const double &r, const quaternion &q)
{ return q+r; }

quaternion operator-(const double &r, const quaternion &q)
{ return quaternion(r-q.w, q.x, q.y, q.z); }

quaternion operator*(const double &r, const quaternion &q)
{ return q*r; }

quaternion operator/(const double &r, const quaternion &q)
{ double n(q.norm2());
  return quaternion(r*q.w/n, -r*q.x/n, -r*q.y/n, -r*q.z/n); }

// Allows cout << q
std::ostream& operator<<(std::ostream &io, const quaternion &q) {
    io << q.w;
    (q.x < 0.0) ? (io << " - " << (-q.x) << "i") : (io << " + " << q.x << "i");
    (q.y < 0.0) ? (io << " - " << (-q.y) << "j") : (io << " + " << q.y << "j");
    (q.z < 0.0) ? (io << " - " << (-q.z) << "k") : (io << " + " << q.z << "k");
    return io;
}

}

