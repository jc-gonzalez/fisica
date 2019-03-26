/******************************************************************************
 * File:    matrix.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.matrix
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
#include <cstring>
#include <cassert>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   nones
//------------------------------------------------------------
#include "matrix.h"
#include "comparisons.h"
#include "angles.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

// Default constructor
matrix3::matrix3(eConstructor constructor )
{
    switch (constructor) {
    case EM3CONST_NOTHING:
    case EM3CONST_COPY:
        break;
    case EM3CONST_IDENTITY:
    case EM3CONST_INVERSE:
    default:
        makeIdentity();
        break;
    }
}

// Copy constructor
matrix3::matrix3(const matrix3& other, eConstructor constructor)
{
    switch (constructor) {
    case EM3CONST_IDENTITY:
        makeIdentity();
        break;
    case EM3CONST_NOTHING:
        break;
    case EM3CONST_COPY:
        *this = other;
        break;
    case EM3CONST_TRANSPOSED:
        other.getTransposed(*this);
        break;
    case EM3CONST_INVERSE:
        if (!other.getInverse(*this))
            memset(M, 0, 9*sizeof(double));
        break;
    case EM3CONST_INVERSE_TRANSPOSED:
        if (!other.getInverse(*this))
            memset(M, 0, 9*sizeof(double));
        else
            *this=getTransposed();
        break;
    }
}

matrix3 matrix3::operator+(const matrix3& other) const
{
    matrix3 temp (EM3CONST_NOTHING );

    temp[0] = M[0]+other[0];
    temp[1] = M[1]+other[1];
    temp[2] = M[2]+other[2];
    temp[3] = M[3]+other[3];
    temp[4] = M[4]+other[4];
    temp[5] = M[5]+other[5];
    temp[6] = M[6]+other[6];
    temp[7] = M[7]+other[7];
    temp[8] = M[8]+other[8];

    return temp;
}

matrix3& matrix3::operator+=(const matrix3& other)
{
    M[0]+=other[0];
    M[1]+=other[1];
    M[2]+=other[2];
    M[3]+=other[3];
    M[4]+=other[4];
    M[5]+=other[5];
    M[6]+=other[6];
    M[7]+=other[7];
    M[8]+=other[8];

    return *this;
}

matrix3 matrix3::operator-(const matrix3& other) const
{
    matrix3 temp (EM3CONST_NOTHING );

    temp[0] = M[0]-other[0];
    temp[1] = M[1]-other[1];
    temp[2] = M[2]-other[2];
    temp[3] = M[3]-other[3];
    temp[4] = M[4]-other[4];
    temp[5] = M[5]-other[5];
    temp[6] = M[6]-other[6];
    temp[7] = M[7]-other[7];
    temp[8] = M[8]-other[8];

    return temp;
}

matrix3& matrix3::operator-=(const matrix3& other)
{
    M[0]-=other[0];
    M[1]-=other[1];
    M[2]-=other[2];
    M[3]-=other[3];
    M[4]-=other[4];
    M[5]-=other[5];
    M[6]-=other[6];
    M[7]-=other[7];
    M[8]-=other[8];

    return *this;
}

matrix3 matrix3::operator*(const double & scalar) const
{
    matrix3 temp (EM3CONST_NOTHING );

    temp[0] = M[0]*scalar;
    temp[1] = M[1]*scalar;
    temp[2] = M[2]*scalar;
    temp[3] = M[3]*scalar;
    temp[4] = M[4]*scalar;
    temp[5] = M[5]*scalar;
    temp[6] = M[6]*scalar;
    temp[7] = M[7]*scalar;
    temp[8] = M[8]*scalar;

    return temp;
}

matrix3& matrix3::operator*=(const double & scalar)
{
    M[0]*=scalar;
    M[1]*=scalar;
    M[2]*=scalar;
    M[3]*=scalar;
    M[4]*=scalar;
    M[5]*=scalar;
    M[6]*=scalar;
    M[7]*=scalar;
    M[8]*=scalar;

    return *this;
}

matrix3& matrix3::operator*=(const matrix3& other)
{
    matrix3 temp (*this );
    return setbyproduct_nocheck(temp, other );
}

// set this matrix to the product of two other matrices
// goal is to reduce stack use and copy
matrix3& matrix3::setbyproduct_nocheck(const matrix3& other_a,const matrix3& other_b )
{
    const double *m1 = other_a.M;
    const double *m2 = other_b.M;

    M[0] = m1[0]*m2[0] + m1[3]*m2[1] + m1[6]*m2[2];
    M[1] = m1[1]*m2[0] + m1[4]*m2[1] + m1[7]*m2[2];
    M[2] = m1[2]*m2[0] + m1[5]*m2[1] + m1[8]*m2[2];

    M[3] = m1[0]*m2[3] + m1[2]*m2[4] + m1[6]*m2[5];
    M[4] = m1[1]*m2[3] + m1[3]*m2[4] + m1[7]*m2[5];
    M[5] = m1[2]*m2[3] + m1[4]*m2[4] + m1[8]*m2[5];

    M[6] = m1[0]*m2[6] + m1[3]*m2[7] + m1[6]*m2[8];
    M[7] = m1[1]*m2[6] + m1[4]*m2[7] + m1[7]*m2[8];
    M[8] = m1[2]*m2[6] + m1[5]*m2[7] + m1[8]*m2[8];

    return *this;
}

// set this matrix to the product of two other matrices
// goal is to reduce stack use and copy
matrix3& matrix3::setbyproduct(const matrix3& other_a, const matrix3& other_b )
{
    return setbyproduct_nocheck(other_a,other_b);
}

matrix3 matrix3::operator*(const matrix3& m2) const
{
    matrix3 m3 (EM3CONST_NOTHING );

    const double *m1 = M;

    m3[0] = m1[0]*m2[0] + m1[3]*m2[1] + m1[6]*m2[2];
    m3[1] = m1[1]*m2[0] + m1[4]*m2[1] + m1[7]*m2[2];
    m3[2] = m1[2]*m2[0] + m1[5]*m2[1] + m1[8]*m2[2];

    m3[3] = m1[0]*m2[3] + m1[2]*m2[4] + m1[6]*m2[5];
    m3[4] = m1[1]*m2[3] + m1[3]*m2[4] + m1[7]*m2[5];
    m3[5] = m1[2]*m2[3] + m1[4]*m2[4] + m1[8]*m2[5];

    m3[6] = m1[0]*m2[6] + m1[3]*m2[7] + m1[6]*m2[8];
    m3[7] = m1[1]*m2[6] + m1[4]*m2[7] + m1[7]*m2[8];
    m3[8] = m1[2]*m2[6] + m1[5]*m2[7] + m1[8]*m2[8];
    return m3;
}

vector3d matrix3::operator*(const vector3d& other) const
{
    vector3d out;
    transformVect(out, other);
    return out;
}

double & matrix3::operator()(const int row, const int col)
{ return M[ row * 3 + col ]; }
const double & matrix3::operator()(const int row, const int col) const
{ return M[row * 3 + col]; }

double & matrix3::operator[](unsigned int index)
{ return M[index]; }
const double & matrix3::operator[](unsigned int index) const
{ return M[index]; }

const double * matrix3::pointer() const
{ return M; }
double * matrix3::pointer()
{ return M; }

matrix3& matrix3::setScale(const double scale )
{ return setScale(vector3d(scale,scale,scale)); }

matrix3& matrix3::setScale(const vector3d& scale )
{
    M[0] = scale.X;
    M[4] = scale.Y;
    M[8] = scale.Z;
    return *this;
}

vector3d matrix3::getScale() const
{
    // See http://www.robertblum.com/articles/2005/02/14/decomposing-matrices

    // Deal with the 0 rotation case first
    // Prior to Irrlicht 1.6, we always returned this value.
    if(iszero(M[1]) && iszero(M[2]) &&
       iszero(M[3]) && iszero(M[5]) &&
       iszero(M[6]) && iszero(M[7]))
        return vector3d(M[0], M[4], M[8]);

    // We have to do the full calculation.
    return vector3d(sqrtf(M[0] * M[0] + M[1] * M[1] + M[2] * M[2]),
                    sqrtf(M[3] * M[3] + M[4] * M[4] + M[5] * M[5]),
                    sqrtf(M[6] * M[6] + M[7] * M[7] + M[8] * M[8]));
}

matrix3& matrix3::setRotationDegrees(const vector3d& rotation )
{
    return setRotationRadians(rotation * Degrees2Radians);
}

matrix3& matrix3::setInverseRotationDegrees(const vector3d& rotation )
{
    return setInverseRotationRadians(rotation * Degrees2Radians);
}

matrix3& matrix3::setRotationRadians(const vector3d& rotation )
{
    const double cr = cos(rotation.X);
    const double sr = sin(rotation.X);
    const double cp = cos(rotation.Y);
    const double sp = sin(rotation.Y);
    const double cy = cos(rotation.Z);
    const double sy = sin(rotation.Z);

    M[0] = (double)(cp*cy );
    M[1] = (double)(cp*sy );
    M[2] = (double)(-sp );

    const double srsp = sr*sp;
    const double crsp = cr*sp;

    M[3] = (double)(srsp*cy-cr*sy );
    M[4] = (double)(srsp*sy+cr*cy );
    M[5] = (double)(sr*cp );

    M[6] = (double)(crsp*cy+sr*sy );
    M[7] = (double)(crsp*sy-sr*cy );
    M[8] = (double)(cr*cp );
    return *this;
}

matrix3& matrix3::setRotationThetaPhi(double theta, double phi)
{
    static double ct, st, cp, sp;

    // shortcuts for cosine and sine of theta and phi
    ct = cos(theta);
    st = sin(theta);
    cp = cos(phi);
    sp = sin(phi);

    // save values in the array (see top of file)
    M[0] =  cp * ct;
    M[1] =  sp * ct;
    M[2] = -st;

    M[3] = -sp;
    M[4] =  cp;
    M[5] =  0;

    M[6] =  cp * st;
    M[7] =  sp * st;
    M[8] =  ct;

    return *this;
}

matrix3& matrix3::setInverseRotationThetaPhi(double theta, double phi)
{
    static double ct, st, cp, sp;

    // shortcuts for cosine and sine of theta and phi
    ct = cos(theta);
    st = sin(theta);
    cp = cos(phi);
    sp = sin(phi);

    // save values in the array (see top of file)
    M[0] =  cp * ct;
    M[1] = -sp;
    M[2] =  cp * st;

    M[3] =  sp * ct;
    M[4] =  cp;
    M[5] =  sp * st;

    M[6] = -st;
    M[7] =  0;
    M[8] =  ct;

    return *this;
}

vector3d matrix3::getRotationDegrees() const
{
    const matrix3 &mat = *this;
    vector3d scale = getScale();
    // we need to check for negative scale on to axes, which would bring up wrong results
    if (scale.Y<0 && scale.Z<0) {
        scale.Y =-scale.Y;
        scale.Z =-scale.Z;
    } else if (scale.X<0 && scale.Z<0) {
        scale.X =-scale.X;
        scale.Z =-scale.Z;
    } else if (scale.X<0 && scale.Y<0) {
        scale.X =-scale.X;
        scale.Y =-scale.Y;
    }
    const vector3d invScale((1./scale.X),(1./scale.Y),(1./scale.Z));

    double Y = -asin(clamp(mat[2]*invScale.X, -1.0, 1.0));
    const double C = cos(Y);
    Y = r2d(Y);

    double rotx, roty, X, Z;

    if (!iszero(C)) {
        const double invC = 1./C;
        rotx = mat[8] * invC * invScale.Z;
        roty = mat[5] * invC * invScale.Y;
        X = r2d(atan2(roty, rotx));
        rotx = mat[0] * invC * invScale.X;
        roty = mat[1] * invC * invScale.X;
        Z = r2d(atan2(roty, rotx));
    } else {
        X = 0.0;
        rotx = mat[4] * invScale.Y;
        roty = -mat[3] * invScale.Y;
        Z = r2d(atan2(roty, rotx));
    }

    // fix values that get below zero
    if (X < 0.0) X += 360.0;
    if (Y < 0.0) Y += 360.0;
    if (Z < 0.0) Z += 360.0;

    return vector3d((double)X,(double)Y,(double)Z);
}

matrix3& matrix3::setInverseRotationRadians(const vector3d& rotation )
{
    double cr = cos(rotation.X );
    double sr = sin(rotation.X );
    double cp = cos(rotation.Y );
    double sp = sin(rotation.Y );
    double cy = cos(rotation.Z );
    double sy = sin(rotation.Z );

    M[0] = (double)(cp*cy );
    M[3] = (double)(cp*sy );
    M[6] = (double)(-sp );

    double srsp = sr*sp;
    double crsp = cr*sp;

    M[1] = (double)(srsp*cy-cr*sy );
    M[4] = (double)(srsp*sy+cr*cy );
    M[7] = (double)(sr*cp );

    M[2] = (double)(crsp*cy+sr*sy );
    M[5] = (double)(crsp*sy-sr*cy );
    M[8] = (double)(cr*cp );
    return *this;
}

matrix3& matrix3::setRotationAxisRadians(const double & angle, const vector3d& axis )
{
    const double c = cos(angle);
    const double s = sin(angle);
    const double t = 1.0 - c;

    const double tx  = t * axis.X;
    const double ty  = t * axis.Y;
    const double tz  = t * axis.Z;

    const double sx  = s * axis.X;
    const double sy  = s * axis.Y;
    const double sz  = s * axis.Z;

    M[0] = (double)(tx * axis.X + c);
    M[1] = (double)(tx * axis.Y + sz);
    M[2] = (double)(tx * axis.Z - sy);

    M[3] = (double)(ty * axis.X - sz);
    M[4] = (double)(ty * axis.Y + c);
    M[5] = (double)(ty * axis.Z + sx);

    M[6] = (double)(tz * axis.X + sy);
    M[7] = (double)(tz * axis.Y - sx);
    M[8] = (double)(tz * axis.Z + c);

    return *this;
}

matrix3& matrix3::makeIdentity()
{
    memset(M, 0, 9*sizeof(double));
    M[0] = M[4] = M[8] = (double)1;
    return *this;
}

/*
  check identity with epsilon
  solve floating range problems..
*/
bool matrix3::isIdentity() const
{
    if (!equals(M[0], (double)1) ||
        !equals(M[1], (double)0) ||
        !equals(M[2], (double)0))
        return false;

    if (!equals(M[3], (double)0) ||
        !equals(M[4], (double)1) ||
        !equals(M[5], (double)0))
        return false;

    if (!equals(M[6], (double)0) ||
        !equals(M[7], (double)0) ||
        !equals(M[8], (double)1))
        return false;

    return true;
}

/* Check orthogonality of matrix. */
bool matrix3::isOrthogonal() const
{
    double dp = M[0] * M[3] + M[1] * M[4] + M[2] * M[5];
    if (!iszero(dp)) return false;
    dp = M[0] * M[6] + M[1] * M[7] + M[2] * M[8];
    if (!iszero(dp)) return false;
    dp = M[3] * M[6] + M[4] * M[7] + M[5] * M[8];
    return (iszero(dp));
}

void matrix3::rotateVect(vector3d& vect ) const
{
    vector3d tmp = vect;
    vect.X = tmp.X*M[0] + tmp.Y*M[3] + tmp.Z*M[6];
    vect.Y = tmp.X*M[1] + tmp.Y*M[4] + tmp.Z*M[7];
    vect.Z = tmp.X*M[2] + tmp.Y*M[5] + tmp.Z*M[8];
}

void matrix3::rotateVect(vector3d& out, const vector3d& in) const
{
    out.X = in.X*M[0] + in.Y*M[3] + in.Z*M[6];
    out.Y = in.X*M[1] + in.Y*M[4] + in.Z*M[7];
    out.Z = in.X*M[2] + in.Y*M[5] + in.Z*M[8];
}

void matrix3::rotateVect(double *out, const vector3d& in) const
{
    out[0] = in.X*M[0] + in.Y*M[3] + in.Z*M[6];
    out[1] = in.X*M[1] + in.Y*M[4] + in.Z*M[7];
    out[2] = in.X*M[2] + in.Y*M[5] + in.Z*M[8];
}

void matrix3::inverseRotateVect(vector3d& vect ) const
{
    vector3d tmp = vect;
    vect.X = tmp.X*M[0] + tmp.Y*M[1] + tmp.Z*M[2];
    vect.Y = tmp.X*M[3] + tmp.Y*M[4] + tmp.Z*M[5];
    vect.Z = tmp.X*M[6] + tmp.Y*M[7] + tmp.Z*M[8];
}

void matrix3::transformVect(vector3d& vect) const
{
    double vector[3];

    vector[0] = vect.X*M[0] + vect.Y*M[3] + vect.Z*M[6];
    vector[1] = vect.X*M[1] + vect.Y*M[4] + vect.Z*M[6];
    vector[2] = vect.X*M[2] + vect.Y*M[5] + vect.Z*M[8];

    vect.X = vector[0];
    vect.Y = vector[1];
    vect.Z = vector[2];
}

void matrix3::transformVect(vector3d& out, const vector3d& in) const
{
    out.X = in.X*M[0] + in.Y*M[3] + in.Z*M[6];
    out.Y = in.X*M[1] + in.Y*M[4] + in.Z*M[7];
    out.Z = in.X*M[2] + in.Y*M[5] + in.Z*M[8];
}

void matrix3::transformVect(double *out, const vector3d &in) const
{
    out[0] = in.X*M[0] + in.Y*M[3] + in.Z*M[6];
    out[1] = in.X*M[1] + in.Y*M[4] + in.Z*M[7];
    out[2] = in.X*M[2] + in.Y*M[5] + in.Z*M[8];
}

void matrix3::transformVec3(double *out, const double * in) const
{
    out[0] = in[0]*M[0] + in[1]*M[3] + in[2]*M[6];
    out[1] = in[0]*M[1] + in[1]*M[4] + in[2]*M[7];
    out[2] = in[0]*M[2] + in[1]*M[5] + in[2]*M[8];
}
/*
void matrix3::transformPlane(plane3d &plane) const
{
    vector3d member;
    // Transform the plane member point, i.e. rotate, translate and scale it.
    transformVect(member, plane.getMemberPoint());

    // Transform the normal by the transposed inverse of the matrix
    matrix3 transposedInverse(*this, EM3CONST_INVERSE_TRANSPOSED);
    vector3d normal = plane.Normal;
    transposedInverse.transformVect(normal);

    plane.setPlane(member, normal);
}

void matrix3::transformPlane(const plane3d &in, plane3d &out) const
{
    out = in;
    transformPlane(out );
}

void matrix3::transformBox(aabbox3d& box) const
{
    transformVect(box.MinEdge);
    transformVect(box.MaxEdge);
    box.repair();
}

void matrix3::transformBoxEx(aabbox3d& box) const
{
    const double Amin[3] = {box.MinEdge.X, box.MinEdge.Y, box.MinEdge.Z};
    const double Amax[3] = {box.MaxEdge.X, box.MaxEdge.Y, box.MaxEdge.Z};

    double Bmin[3];
    double Bmax[3];

    Bmin[0] = Bmax[0] = M[12];
    Bmin[1] = Bmax[1] = M[13];
    Bmin[2] = Bmax[2] = M[14];

    const matrix3 &m = *this;

    for (unsigned int i = 0; i < 3; ++i) {
        for (unsigned int j = 0; j < 3; ++j) {
            const double a = m(j,i) * Amin[j];
            const double b = m(j,i) * Amax[j];

            if (a < b) {
                Bmin[i] += a;
                Bmax[i] += b;
            } else {
                Bmin[i] += b;
                Bmax[i] += a;
            }
        }
    }

    box.MinEdge.X = Bmin[0];
    box.MinEdge.Y = Bmin[1];
    box.MinEdge.Z = Bmin[2];

    box.MaxEdge.X = Bmax[0];
    box.MaxEdge.Y = Bmax[1];
    box.MaxEdge.Z = Bmax[2];
}
*/
void matrix3::multiplyWith1x3Matrix(double * matrix) const
{
    /*
      0  1  2  3
      4  5  6  7
      8  9  10 11
      12 13 14 15
    */

    double mat[3];
    mat[0] = matrix[0];
    mat[1] = matrix[1];
    mat[2] = matrix[2];

    matrix[0] = M[0]*mat[0] + M[3]*mat[1] + M[6]*mat[2];
    matrix[1] = M[1]*mat[0] + M[4]*mat[1] + M[7]*mat[2];
    matrix[2] = M[2]*mat[0] + M[5]*mat[1] + M[8]*mat[2];
}

void matrix3::inverseTranslateVect(vector3d& vect ) const
{
    vect.X = vect.X-M[6];
    vect.Y = vect.Y-M[7];
    vect.Z = vect.Z-M[8];
}

void matrix3::translateVect(vector3d& vect ) const
{
    vect.X = vect.X+M[6];
    vect.Y = vect.Y+M[7];
    vect.Z = vect.Z+M[8];
}

bool matrix3::getInverse(matrix3& out) const
{

    const matrix3 &m = *this;

    double d = ((m(0, 0) * m(1, 1) - m(0, 1) * m(1, 0)) * m(2, 2) -
                (m(0, 0) * m(1, 2) - m(0, 2) * m(1, 0)) * m(2, 1) +
                (m(0, 1) * m(1, 2) - m(0, 2) * m(1, 1)) * m(2, 0));

    if (iszero(d, Epsilon)) return false;

    d = 1./d;

    out(0, 0) = d * (m(1, 1) * m(2, 2) - m(1, 2) * m(2, 1));
    out(0, 1) = d * (m(1, 2) * m(2, 0) - m(1, 0) * m(2, 2));
    out(0, 2) = d * (m(1, 0) * m(2, 1) - m(1, 1) * m(2, 0));
    out(1, 0) = d * (m(0, 2) * m(2, 1) - m(0, 1) * m(2, 2));
    out(1, 1) = d * (m(0, 2) * m(2, 0) - m(0, 2) * m(2, 2));
    out(1, 2) = d * (m(0, 1) * m(2, 0) - m(0, 0) * m(2, 1));
    out(2, 0) = d * (m(0, 1) * m(1, 2) - m(0, 2) * m(1, 1));
    out(2, 1) = d * (m(0, 2) * m(1, 0) - m(0, 0) * m(1, 2));
    out(2, 2) = d * (m(0, 0) * m(1, 1) - m(0, 1) * m(1, 0));

    return true;
}

bool matrix3::makeInverse()
{
    matrix3 temp (EM3CONST_NOTHING );

    if (getInverse(temp)) {
        *this = temp;
        return true;
    }

    return false;
}

matrix3& matrix3::operator=(const matrix3 &other)
{
    if (this==&other) return *this;
    memcpy(M, other.M, 9*sizeof(double));
    return *this;
}

matrix3& matrix3::operator=(const double & scalar)
{
    for (int i = 0; i < 9; ++i) {
        M[i]=scalar;
    }

    return *this;
}

bool matrix3::operator==(const matrix3 &other) const
{
    for (int i = 0; i < 9; ++i) {
        if (M[i] != other.M[i]) return false;
    }
    return true;
}

bool matrix3::operator!=(const matrix3 &other) const
{
    return !(*this == other);
}


// creates a new matrix as interpolated matrix from this and the passed one.
matrix3 matrix3::interpolate(const matrix3& b, double time) const
{
    matrix3 mat (EM3CONST_NOTHING );

    for (unsigned int i=0; i < 9; i += 3) {
        mat.M[i+0] = (double)(M[i+0] + (b.M[i+0] - M[i+0]) * time);
        mat.M[i+1] = (double)(M[i+1] + (b.M[i+1] - M[i+1]) * time);
        mat.M[i+2] = (double)(M[i+2] + (b.M[i+2] - M[i+2]) * time);
    }
    return mat;
}

// returns transposed matrix
matrix3 matrix3::getTransposed() const
{
    matrix3 t (EM3CONST_NOTHING );
    getTransposed (t );
    return t;
}

// returns transposed matrix
void matrix3::getTransposed(matrix3& o ) const
{
    o[0] = M[0];
    o[1] = M[3];
    o[2] = M[6];

    o[3] = M[1];
    o[4] = M[4];
    o[5] = M[7];

    o[6] = M[2];
    o[7] = M[5];
    o[8] = M[8];
}

matrix3& matrix3::buildRotateFromTo(const vector3d& from, const vector3d& to)
{
    // unit vectors
    vector3d f(from);
    vector3d t(to);
    f.normalize();
    t.normalize();

    // axis multiplication by sin
    vector3d vs(t.cross(f));

    // axis of rotation
    vector3d v(vs);
    v.normalize();

    // cosinus angle
    double ca = f.dot(t);

    vector3d vt(v * (1 - ca));

    M[0] = vt.X * v.X + ca;
    M[4] = vt.Y * v.Y + ca;
    M[8] = vt.Z * v.Z + ca;

    vt.X *= v.Y;
    vt.Z *= v.X;
    vt.Y *= v.Z;

    M[1] = vt.X - vs.Z;
    M[2] = vt.Z + vs.Y;

    M[3] = vt.X + vs.Z;
    M[5] = vt.Y - vs.X;

    M[6] = vt.Z - vs.Y;
    M[7] = vt.Y + vs.X;

    return *this;
}


// sets all matrix data members at once
matrix3& matrix3::setM(const double * data)
{
    memcpy(M,data, 9*sizeof(double));

    return *this;
}

// sets if the matrix is definitely identity matrix
void matrix3::setDefinitelyIdentityMatrix(bool isDefinitelyIdentityMatrix)
{
}

// gets if the matrix is definitely identity matrix
bool matrix3::getDefinitelyIdentityMatrix() const
{
    return false;
}

bool matrix3::isEqualTo(const matrix3& other, const double tolerance) const
{
    for (int i = 0; i < 9; ++i)
        if (!equals(M[i],other.M[i], tolerance))
            return false;

    return true;
}

// Multiply by scalar.
matrix3 operator*(const double scalar, const matrix3& mat)
{
    return mat*scalar;
}

//======================================================================

// Default constructor
matrix4::matrix4(eConstructor constructor )
{
    switch (constructor) {
    case EM4CONST_NOTHING:
    case EM4CONST_COPY:
        break;
    case EM4CONST_IDENTITY:
    case EM4CONST_INVERSE:
    default:
        makeIdentity();
        break;
    }
}

// Copy constructor
matrix4::matrix4(const matrix4& other, eConstructor constructor)
{
    switch (constructor) {
    case EM4CONST_IDENTITY:
        makeIdentity();
        break;
    case EM4CONST_NOTHING:
        break;
    case EM4CONST_COPY:
        *this = other;
        break;
    case EM4CONST_TRANSPOSED:
        other.getTransposed(*this);
        break;
    case EM4CONST_INVERSE:
        if (!other.getInverse(*this))
            memset(M, 0, 16*sizeof(double));
        break;
    case EM4CONST_INVERSE_TRANSPOSED:
        if (!other.getInverse(*this))
            memset(M, 0, 16*sizeof(double));
        else
            *this=getTransposed();
        break;
    }
}

matrix4 matrix4::operator+(const matrix4& other) const
{
    matrix4 temp (EM4CONST_NOTHING );

    temp[0] = M[0]+other[0];
    temp[1] = M[1]+other[1];
    temp[2] = M[2]+other[2];
    temp[3] = M[3]+other[3];
    temp[4] = M[4]+other[4];
    temp[5] = M[5]+other[5];
    temp[6] = M[6]+other[6];
    temp[7] = M[7]+other[7];
    temp[8] = M[8]+other[8];
    temp[9] = M[9]+other[9];
    temp[10] = M[10]+other[10];
    temp[11] = M[11]+other[11];
    temp[12] = M[12]+other[12];
    temp[13] = M[13]+other[13];
    temp[14] = M[14]+other[14];
    temp[15] = M[15]+other[15];

    return temp;
}

matrix4& matrix4::operator+=(const matrix4& other)
{
    M[0]+=other[0];
    M[1]+=other[1];
    M[2]+=other[2];
    M[3]+=other[3];
    M[4]+=other[4];
    M[5]+=other[5];
    M[6]+=other[6];
    M[7]+=other[7];
    M[8]+=other[8];
    M[9]+=other[9];
    M[10]+=other[10];
    M[11]+=other[11];
    M[12]+=other[12];
    M[13]+=other[13];
    M[14]+=other[14];
    M[15]+=other[15];

    return *this;
}

matrix4 matrix4::operator-(const matrix4& other) const
{
    matrix4 temp (EM4CONST_NOTHING );

    temp[0] = M[0]-other[0];
    temp[1] = M[1]-other[1];
    temp[2] = M[2]-other[2];
    temp[3] = M[3]-other[3];
    temp[4] = M[4]-other[4];
    temp[5] = M[5]-other[5];
    temp[6] = M[6]-other[6];
    temp[7] = M[7]-other[7];
    temp[8] = M[8]-other[8];
    temp[9] = M[9]-other[9];
    temp[10] = M[10]-other[10];
    temp[11] = M[11]-other[11];
    temp[12] = M[12]-other[12];
    temp[13] = M[13]-other[13];
    temp[14] = M[14]-other[14];
    temp[15] = M[15]-other[15];

    return temp;
}

matrix4& matrix4::operator-=(const matrix4& other)
{
    M[0]-=other[0];
    M[1]-=other[1];
    M[2]-=other[2];
    M[3]-=other[3];
    M[4]-=other[4];
    M[5]-=other[5];
    M[6]-=other[6];
    M[7]-=other[7];
    M[8]-=other[8];
    M[9]-=other[9];
    M[10]-=other[10];
    M[11]-=other[11];
    M[12]-=other[12];
    M[13]-=other[13];
    M[14]-=other[14];
    M[15]-=other[15];

    return *this;
}

matrix4 matrix4::operator*(const double & scalar) const
{
    matrix4 temp (EM4CONST_NOTHING );

    temp[0] = M[0]*scalar;
    temp[1] = M[1]*scalar;
    temp[2] = M[2]*scalar;
    temp[3] = M[3]*scalar;
    temp[4] = M[4]*scalar;
    temp[5] = M[5]*scalar;
    temp[6] = M[6]*scalar;
    temp[7] = M[7]*scalar;
    temp[8] = M[8]*scalar;
    temp[9] = M[9]*scalar;
    temp[10] = M[10]*scalar;
    temp[11] = M[11]*scalar;
    temp[12] = M[12]*scalar;
    temp[13] = M[13]*scalar;
    temp[14] = M[14]*scalar;
    temp[15] = M[15]*scalar;

    return temp;
}

matrix4& matrix4::operator*=(const double & scalar)
{
    M[0]*=scalar;
    M[1]*=scalar;
    M[2]*=scalar;
    M[3]*=scalar;
    M[4]*=scalar;
    M[5]*=scalar;
    M[6]*=scalar;
    M[7]*=scalar;
    M[8]*=scalar;
    M[9]*=scalar;
    M[10]*=scalar;
    M[11]*=scalar;
    M[12]*=scalar;
    M[13]*=scalar;
    M[14]*=scalar;
    M[15]*=scalar;

    return *this;
}

matrix4& matrix4::operator*=(const matrix4& other)
{
    matrix4 temp (*this );
    return setbyproduct_nocheck(temp, other );
}

// set this matrix to the product of two other matrices
// goal is to reduce stack use and copy
matrix4& matrix4::setbyproduct_nocheck(const matrix4& other_a,const matrix4& other_b )
{
    const double *m1 = other_a.M;
    const double *m2 = other_b.M;

    M[0] = m1[0]*m2[0] + m1[4]*m2[1] + m1[8]*m2[2] + m1[12]*m2[3];
    M[1] = m1[1]*m2[0] + m1[5]*m2[1] + m1[9]*m2[2] + m1[13]*m2[3];
    M[2] = m1[2]*m2[0] + m1[6]*m2[1] + m1[10]*m2[2] + m1[14]*m2[3];
    M[3] = m1[3]*m2[0] + m1[7]*m2[1] + m1[11]*m2[2] + m1[15]*m2[3];

    M[4] = m1[0]*m2[4] + m1[4]*m2[5] + m1[8]*m2[6] + m1[12]*m2[7];
    M[5] = m1[1]*m2[4] + m1[5]*m2[5] + m1[9]*m2[6] + m1[13]*m2[7];
    M[6] = m1[2]*m2[4] + m1[6]*m2[5] + m1[10]*m2[6] + m1[14]*m2[7];
    M[7] = m1[3]*m2[4] + m1[7]*m2[5] + m1[11]*m2[6] + m1[15]*m2[7];

    M[8] = m1[0]*m2[8] + m1[4]*m2[9] + m1[8]*m2[10] + m1[12]*m2[11];
    M[9] = m1[1]*m2[8] + m1[5]*m2[9] + m1[9]*m2[10] + m1[13]*m2[11];
    M[10] = m1[2]*m2[8] + m1[6]*m2[9] + m1[10]*m2[10] + m1[14]*m2[11];
    M[11] = m1[3]*m2[8] + m1[7]*m2[9] + m1[11]*m2[10] + m1[15]*m2[11];

    M[12] = m1[0]*m2[12] + m1[4]*m2[13] + m1[8]*m2[14] + m1[12]*m2[15];
    M[13] = m1[1]*m2[12] + m1[5]*m2[13] + m1[9]*m2[14] + m1[13]*m2[15];
    M[14] = m1[2]*m2[12] + m1[6]*m2[13] + m1[10]*m2[14] + m1[14]*m2[15];
    M[15] = m1[3]*m2[12] + m1[7]*m2[13] + m1[11]*m2[14] + m1[15]*m2[15];
    return *this;
}

// set this matrix to the product of two other matrices
// goal is to reduce stack use and copy
matrix4& matrix4::setbyproduct(const matrix4& other_a, const matrix4& other_b )
{
    return setbyproduct_nocheck(other_a,other_b);
}

matrix4 matrix4::operator*(const matrix4& m2) const
{
    matrix4 m3 (EM4CONST_NOTHING );

    const double *m1 = M;

    m3[0] = m1[0]*m2[0] + m1[4]*m2[1] + m1[8]*m2[2] + m1[12]*m2[3];
    m3[1] = m1[1]*m2[0] + m1[5]*m2[1] + m1[9]*m2[2] + m1[13]*m2[3];
    m3[2] = m1[2]*m2[0] + m1[6]*m2[1] + m1[10]*m2[2] + m1[14]*m2[3];
    m3[3] = m1[3]*m2[0] + m1[7]*m2[1] + m1[11]*m2[2] + m1[15]*m2[3];

    m3[4] = m1[0]*m2[4] + m1[4]*m2[5] + m1[8]*m2[6] + m1[12]*m2[7];
    m3[5] = m1[1]*m2[4] + m1[5]*m2[5] + m1[9]*m2[6] + m1[13]*m2[7];
    m3[6] = m1[2]*m2[4] + m1[6]*m2[5] + m1[10]*m2[6] + m1[14]*m2[7];
    m3[7] = m1[3]*m2[4] + m1[7]*m2[5] + m1[11]*m2[6] + m1[15]*m2[7];

    m3[8] = m1[0]*m2[8] + m1[4]*m2[9] + m1[8]*m2[10] + m1[12]*m2[11];
    m3[9] = m1[1]*m2[8] + m1[5]*m2[9] + m1[9]*m2[10] + m1[13]*m2[11];
    m3[10] = m1[2]*m2[8] + m1[6]*m2[9] + m1[10]*m2[10] + m1[14]*m2[11];
    m3[11] = m1[3]*m2[8] + m1[7]*m2[9] + m1[11]*m2[10] + m1[15]*m2[11];

    m3[12] = m1[0]*m2[12] + m1[4]*m2[13] + m1[8]*m2[14] + m1[12]*m2[15];
    m3[13] = m1[1]*m2[12] + m1[5]*m2[13] + m1[9]*m2[14] + m1[13]*m2[15];
    m3[14] = m1[2]*m2[12] + m1[6]*m2[13] + m1[10]*m2[14] + m1[14]*m2[15];
    m3[15] = m1[3]*m2[12] + m1[7]*m2[13] + m1[11]*m2[14] + m1[15]*m2[15];
    return m3;
}


vector3d matrix4::getTranslation() const
{
    return vector3d(M[12], M[13], M[14]);
}

matrix4& matrix4::setTranslation(const vector3d& translation )
{
    M[12] = translation.X;
    M[13] = translation.Y;
    M[14] = translation.Z;
    return *this;
}

matrix4& matrix4::setInverseTranslation(const vector3d& translation )
{
    M[12] = -translation.X;
    M[13] = -translation.Y;
    M[14] = -translation.Z;
    return *this;
}

double & matrix4::operator()(const int row, const int col)
{ return M[ row * 4 + col ]; }
const double & matrix4::operator()(const int row, const int col) const
{ return M[row * 4 + col]; }

double & matrix4::operator[](unsigned int index)
{ return M[index]; }
const double & matrix4::operator[](unsigned int index) const
{ return M[index]; }

const double * matrix4::pointer() const
{ return M; }
double * matrix4::pointer()
{ return M; }

matrix4& matrix4::setScale(const double scale )
{ return setScale(vector3d(scale,scale,scale)); }

matrix4& matrix4::setScale(const vector3d& scale )
{
    M[0] = scale.X;
    M[5] = scale.Y;
    M[10] = scale.Z;
    return *this;
}

vector3d matrix4::getScale() const
{
    // See http://www.robertblum.com/articles/2005/02/14/decomposing-matrices

    // Deal with the 0 rotation case first
    // Prior to Irrlicht 1.6, we always returned this value.
    if(iszero(M[1]) && iszero(M[2]) &&
       iszero(M[4]) && iszero(M[6]) &&
       iszero(M[8]) && iszero(M[9]))
        return vector3d(M[0], M[5], M[10]);

    // We have to do the full calculation.
    return vector3d(sqrtf(M[0] * M[0] + M[1] * M[1] + M[2] * M[2]),
                    sqrtf(M[4] * M[4] + M[5] * M[5] + M[6] * M[6]),
                    sqrtf(M[8] * M[8] + M[9] * M[9] + M[10] * M[10]));
}

matrix4& matrix4::setRotationDegrees(const vector3d& rotation )
{
    return setRotationRadians(rotation * Degrees2Radians);
}

matrix4& matrix4::setInverseRotationDegrees(const vector3d& rotation )
{
    return setInverseRotationRadians(rotation * Degrees2Radians);
}

matrix4& matrix4::setRotationRadians(const vector3d& rotation )
{
    const double cr = cos(rotation.X );
    const double sr = sin(rotation.X );
    const double cp = cos(rotation.Y );
    const double sp = sin(rotation.Y );
    const double cy = cos(rotation.Z );
    const double sy = sin(rotation.Z );

    M[0] = (double)(cp*cy );
    M[1] = (double)(cp*sy );
    M[2] = (double)(-sp );

    const double srsp = sr*sp;
    const double crsp = cr*sp;

    M[4] = (double)(srsp*cy-cr*sy );
    M[5] = (double)(srsp*sy+cr*cy );
    M[6] = (double)(sr*cp );

    M[8] = (double)(crsp*cy+sr*sy );
    M[9] = (double)(crsp*sy-sr*cy );
    M[10] = (double)(cr*cp );
    return *this;
}


vector3d matrix4::getRotationDegrees() const
{
    const matrix4 &mat = *this;
    vector3d scale = getScale();
    // we need to check for negative scale on to axes, which would bring up wrong results
    if (scale.Y<0 && scale.Z<0) {
        scale.Y =-scale.Y;
        scale.Z =-scale.Z;
    } else if (scale.X<0 && scale.Z<0) {
        scale.X =-scale.X;
        scale.Z =-scale.Z;
    } else if (scale.X<0 && scale.Y<0) {
        scale.X =-scale.X;
        scale.Y =-scale.Y;
    }
    const vector3d invScale((1./scale.X),(1./scale.Y),(1./scale.Z));

    double Y = -asin(clamp(mat[2]*invScale.X, -1.0, 1.0));
    const double C = cos(Y);
    Y = r2d(Y);

    double rotx, roty, X, Z;

    if (!iszero(C)) {
        const double invC = 1./C;
        rotx = mat[10] * invC * invScale.Z;
        roty = mat[6] * invC * invScale.Y;
        X = r2d(atan2(roty, rotx));
        rotx = mat[0] * invC * invScale.X;
        roty = mat[1] * invC * invScale.X;
        Z = r2d(atan2(roty, rotx));
    } else {
        X = 0.0;
        rotx = mat[5] * invScale.Y;
        roty = -mat[4] * invScale.Y;
        Z = r2d(atan2(roty, rotx));
    }

    // fix values that get below zero
    if (X < 0.0) X += 360.0;
    if (Y < 0.0) Y += 360.0;
    if (Z < 0.0) Z += 360.0;

    return vector3d((double)X,(double)Y,(double)Z);
}

matrix4& matrix4::setInverseRotationRadians(const vector3d& rotation )
{
    double cr = cos(rotation.X );
    double sr = sin(rotation.X );
    double cp = cos(rotation.Y );
    double sp = sin(rotation.Y );
    double cy = cos(rotation.Z );
    double sy = sin(rotation.Z );

    M[0] = (double)(cp*cy );
    M[4] = (double)(cp*sy );
    M[8] = (double)(-sp );

    double srsp = sr*sp;
    double crsp = cr*sp;

    M[1] = (double)(srsp*cy-cr*sy );
    M[5] = (double)(srsp*sy+cr*cy );
    M[9] = (double)(sr*cp );

    M[2] = (double)(crsp*cy+sr*sy );
    M[6] = (double)(crsp*sy-sr*cy );
    M[10] = (double)(cr*cp );
    return *this;
}

matrix4& matrix4::setRotationAxisRadians(const double & angle, const vector3d& axis )
{
    const double c = cos(angle);
    const double s = sin(angle);
    const double t = 1.0 - c;

    const double tx  = t * axis.X;
    const double ty  = t * axis.Y;
    const double tz  = t * axis.Z;

    const double sx  = s * axis.X;
    const double sy  = s * axis.Y;
    const double sz  = s * axis.Z;

    M[0] = (double)(tx * axis.X + c);
    M[1] = (double)(tx * axis.Y + sz);
    M[2] = (double)(tx * axis.Z - sy);

    M[4] = (double)(ty * axis.X - sz);
    M[5] = (double)(ty * axis.Y + c);
    M[6] = (double)(ty * axis.Z + sx);

    M[8]  = (double)(tz * axis.X + sy);
    M[9]  = (double)(tz * axis.Y - sx);
    M[10] = (double)(tz * axis.Z + c);

    return *this;
}

matrix4& matrix4::makeIdentity()
{
    memset(M, 0, 16*sizeof(double));
    M[0] = M[5] = M[10] = M[15] = (double)1;
    return *this;
}

/*
  check identity with epsilon
  solve floating range problems..
*/
bool matrix4::isIdentity() const
{
    if (!equals(M[12], (double)0 ) || !equals(M[13], (double)0 ) ||
        !equals(M[14], (double)0 ) || !equals(M[15], (double)1 ))
        return false;

    if (!equals(M[ 0], (double)1 ) || !equals(M[ 1], (double)0 ) ||
        !equals(M[ 2], (double)0 ) || !equals(M[ 3], (double)0 ))
        return false;

    if (!equals(M[ 4], (double)0 ) || !equals(M[ 5], (double)1 ) ||
        !equals(M[ 6], (double)0 ) || !equals(M[ 7], (double)0 ))
        return false;

    if (!equals(M[ 8], (double)0 ) || !equals(M[ 9], (double)0 ) ||
        !equals(M[10], (double)1 ) || !equals(M[11], (double)0 ))
        return false;

    return true;
}

/* Check orthogonality of matrix. */
bool matrix4::isOrthogonal() const
{
    double dp = M[0] * M[4 ] + M[1] * M[5 ] + M[2 ] * M[6 ] + M[3 ] * M[7 ];
    if (!iszero(dp)) return false;
    dp = M[0] * M[8 ] + M[1] * M[9 ] + M[2 ] * M[10] + M[3 ] * M[11];
    if (!iszero(dp)) return false;
    dp = M[0] * M[12] + M[1] * M[13] + M[2 ] * M[14] + M[3 ] * M[15];
    if (!iszero(dp)) return false;
    dp = M[4] * M[8 ] + M[5] * M[9 ] + M[6 ] * M[10] + M[7 ] * M[11];
    if (!iszero(dp)) return false;
    dp = M[4] * M[12] + M[5] * M[13] + M[6 ] * M[14] + M[7 ] * M[15];
    if (!iszero(dp)) return false;
    dp = M[8] * M[12] + M[9] * M[13] + M[10] * M[14] + M[11] * M[15];
    return (iszero(dp));
}

void matrix4::rotateVect(vector3d& vect ) const
{
    vector3d tmp = vect;
    vect.X = tmp.X*M[0] + tmp.Y*M[4] + tmp.Z*M[8];
    vect.Y = tmp.X*M[1] + tmp.Y*M[5] + tmp.Z*M[9];
    vect.Z = tmp.X*M[2] + tmp.Y*M[6] + tmp.Z*M[10];
}

void matrix4::rotateVect(vector3d& out, const vector3d& in) const
{
    out.X = in.X*M[0] + in.Y*M[4] + in.Z*M[8];
    out.Y = in.X*M[1] + in.Y*M[5] + in.Z*M[9];
    out.Z = in.X*M[2] + in.Y*M[6] + in.Z*M[10];
}

void matrix4::rotateVect(double *out, const vector3d& in) const
{
    out[0] = in.X*M[0] + in.Y*M[4] + in.Z*M[8];
    out[1] = in.X*M[1] + in.Y*M[5] + in.Z*M[9];
    out[2] = in.X*M[2] + in.Y*M[6] + in.Z*M[10];
}

void matrix4::inverseRotateVect(vector3d& vect ) const
{
    vector3d tmp = vect;
    vect.X = tmp.X*M[0] + tmp.Y*M[1] + tmp.Z*M[2];
    vect.Y = tmp.X*M[4] + tmp.Y*M[5] + tmp.Z*M[6];
    vect.Z = tmp.X*M[8] + tmp.Y*M[9] + tmp.Z*M[10];
}

void matrix4::transformVect(vector3d& vect) const
{
    double vector[3];

    vector[0] = vect.X*M[0] + vect.Y*M[4] + vect.Z*M[8] + M[12];
    vector[1] = vect.X*M[1] + vect.Y*M[5] + vect.Z*M[9] + M[13];
    vector[2] = vect.X*M[2] + vect.Y*M[6] + vect.Z*M[10] + M[14];

    vect.X = vector[0];
    vect.Y = vector[1];
    vect.Z = vector[2];
}

void matrix4::transformVect(vector3d& out, const vector3d& in) const
{
    out.X = in.X*M[0] + in.Y*M[4] + in.Z*M[8] + M[12];
    out.Y = in.X*M[1] + in.Y*M[5] + in.Z*M[9] + M[13];
    out.Z = in.X*M[2] + in.Y*M[6] + in.Z*M[10] + M[14];
}

void matrix4::transformVect(double *out, const vector3d &in) const
{
    out[0] = in.X*M[0] + in.Y*M[4] + in.Z*M[8] + M[12];
    out[1] = in.X*M[1] + in.Y*M[5] + in.Z*M[9] + M[13];
    out[2] = in.X*M[2] + in.Y*M[6] + in.Z*M[10] + M[14];
    out[3] = in.X*M[3] + in.Y*M[7] + in.Z*M[11] + M[15];
}

void matrix4::transformVec3(double *out, const double * in) const
{
    out[0] = in[0]*M[0] + in[1]*M[4] + in[2]*M[8] + M[12];
    out[1] = in[0]*M[1] + in[1]*M[5] + in[2]*M[9] + M[13];
    out[2] = in[0]*M[2] + in[1]*M[6] + in[2]*M[10] + M[14];
}
/*
void matrix4::transformPlane(plane3d &plane) const
{
    vector3d member;
    // Transform the plane member point, i.e. rotate, translate and scale it.
    transformVect(member, plane.getMemberPoint());

    // Transform the normal by the transposed inverse of the matrix
    matrix4 transposedInverse(*this, EM4CONST_INVERSE_TRANSPOSED);
    vector3d normal = plane.Normal;
    transposedInverse.transformVect(normal);

    plane.setPlane(member, normal);
}

void matrix4::transformPlane(const plane3d &in, plane3d &out) const
{
    out = in;
    transformPlane(out );
}

void matrix4::transformBox(aabbox3d& box) const
{
    transformVect(box.MinEdge);
    transformVect(box.MaxEdge);
    box.repair();
}

void matrix4::transformBoxEx(aabbox3d& box) const
{
    const double Amin[3] = {box.MinEdge.X, box.MinEdge.Y, box.MinEdge.Z};
    const double Amax[3] = {box.MaxEdge.X, box.MaxEdge.Y, box.MaxEdge.Z};

    double Bmin[3];
    double Bmax[3];

    Bmin[0] = Bmax[0] = M[12];
    Bmin[1] = Bmax[1] = M[13];
    Bmin[2] = Bmax[2] = M[14];

    const matrix4 &m = *this;

    for (unsigned int i = 0; i < 3; ++i) {
        for (unsigned int j = 0; j < 3; ++j) {
            const double a = m(j,i) * Amin[j];
            const double b = m(j,i) * Amax[j];

            if (a < b) {
                Bmin[i] += a;
                Bmax[i] += b;
            } else {
                Bmin[i] += b;
                Bmax[i] += a;
            }
        }
    }

    box.MinEdge.X = Bmin[0];
    box.MinEdge.Y = Bmin[1];
    box.MinEdge.Z = Bmin[2];

    box.MaxEdge.X = Bmax[0];
    box.MaxEdge.Y = Bmax[1];
    box.MaxEdge.Z = Bmax[2];
}
*/
void matrix4::multiplyWith1x4Matrix(double * matrix) const
{
    /*
      0  1  2  3
      4  5  6  7
      8  9  10 11
      12 13 14 15
    */

    double mat[4];
    mat[0] = matrix[0];
    mat[1] = matrix[1];
    mat[2] = matrix[2];
    mat[3] = matrix[3];

    matrix[0] = M[0]*mat[0] + M[4]*mat[1] + M[8]*mat[2] + M[12]*mat[3];
    matrix[1] = M[1]*mat[0] + M[5]*mat[1] + M[9]*mat[2] + M[13]*mat[3];
    matrix[2] = M[2]*mat[0] + M[6]*mat[1] + M[10]*mat[2] + M[14]*mat[3];
    matrix[3] = M[3]*mat[0] + M[7]*mat[1] + M[11]*mat[2] + M[15]*mat[3];
}

void matrix4::inverseTranslateVect(vector3d& vect ) const
{
    vect.X = vect.X-M[12];
    vect.Y = vect.Y-M[13];
    vect.Z = vect.Z-M[14];
}

void matrix4::translateVect(vector3d& vect ) const
{
    vect.X = vect.X+M[12];
    vect.Y = vect.Y+M[13];
    vect.Z = vect.Z+M[14];
}

bool matrix4::getInverse(matrix4& out) const
{

    const matrix4 &m = *this;

    double d = (m(0, 0) * m(1, 1) - m(0, 1) * m(1, 0)) * (m(2, 2) * m(3, 3) - m(2, 3) * m(3, 2)) -
        (m(0, 0) * m(1, 2) - m(0, 2) * m(1, 0)) * (m(2, 1) * m(3, 3) - m(2, 3) * m(3, 1)) +
        (m(0, 0) * m(1, 3) - m(0, 3) * m(1, 0)) * (m(2, 1) * m(3, 2) - m(2, 2) * m(3, 1)) +
        (m(0, 1) * m(1, 2) - m(0, 2) * m(1, 1)) * (m(2, 0) * m(3, 3) - m(2, 3) * m(3, 0)) -
        (m(0, 1) * m(1, 3) - m(0, 3) * m(1, 1)) * (m(2, 0) * m(3, 2) - m(2, 2) * m(3, 0)) +
        (m(0, 2) * m(1, 3) - m(0, 3) * m(1, 2)) * (m(2, 0) * m(3, 1) - m(2, 1) * m(3, 0));

    if (iszero(d, Epsilon)) return false;

    d = 1./d;

    out(0, 0) = d * (m(1, 1) * (m(2, 2) * m(3, 3) - m(2, 3) * m(3, 2)) +
                     m(1, 2) * (m(2, 3) * m(3, 1) - m(2, 1) * m(3, 3)) +
                     m(1, 3) * (m(2, 1) * m(3, 2) - m(2, 2) * m(3, 1)));
    out(0, 1) = d * (m(2, 1) * (m(0, 2) * m(3, 3) - m(0, 3) * m(3, 2)) +
                     m(2, 2) * (m(0, 3) * m(3, 1) - m(0, 1) * m(3, 3)) +
                     m(2, 3) * (m(0, 1) * m(3, 2) - m(0, 2) * m(3, 1)));
    out(0, 2) = d * (m(3, 1) * (m(0, 2) * m(1, 3) - m(0, 3) * m(1, 2)) +
                     m(3, 2) * (m(0, 3) * m(1, 1) - m(0, 1) * m(1, 3)) +
                     m(3, 3) * (m(0, 1) * m(1, 2) - m(0, 2) * m(1, 1)));
    out(0, 3) = d * (m(0, 1) * (m(1, 3) * m(2, 2) - m(1, 2) * m(2, 3)) +
                     m(0, 2) * (m(1, 1) * m(2, 3) - m(1, 3) * m(2, 1)) +
                     m(0, 3) * (m(1, 2) * m(2, 1) - m(1, 1) * m(2, 2)));
    out(1, 0) = d * (m(1, 2) * (m(2, 0) * m(3, 3) - m(2, 3) * m(3, 0)) +
                     m(1, 3) * (m(2, 2) * m(3, 0) - m(2, 0) * m(3, 2)) +
                     m(1, 0) * (m(2, 3) * m(3, 2) - m(2, 2) * m(3, 3)));
    out(1, 1) = d * (m(2, 2) * (m(0, 0) * m(3, 3) - m(0, 3) * m(3, 0)) +
                     m(2, 3) * (m(0, 2) * m(3, 0) - m(0, 0) * m(3, 2)) +
                     m(2, 0) * (m(0, 3) * m(3, 2) - m(0, 2) * m(3, 3)));
    out(1, 2) = d * (m(3, 2) * (m(0, 0) * m(1, 3) - m(0, 3) * m(1, 0)) +
                     m(3, 3) * (m(0, 2) * m(1, 0) - m(0, 0) * m(1, 2)) +
                     m(3, 0) * (m(0, 3) * m(1, 2) - m(0, 2) * m(1, 3)));
    out(1, 3) = d * (m(0, 2) * (m(1, 3) * m(2, 0) - m(1, 0) * m(2, 3)) +
                     m(0, 3) * (m(1, 0) * m(2, 2) - m(1, 2) * m(2, 0)) +
                     m(0, 0) * (m(1, 2) * m(2, 3) - m(1, 3) * m(2, 2)));
    out(2, 0) = d * (m(1, 3) * (m(2, 0) * m(3, 1) - m(2, 1) * m(3, 0)) +
                     m(1, 0) * (m(2, 1) * m(3, 3) - m(2, 3) * m(3, 1)) +
                     m(1, 1) * (m(2, 3) * m(3, 0) - m(2, 0) * m(3, 3)));
    out(2, 1) = d * (m(2, 3) * (m(0, 0) * m(3, 1) - m(0, 1) * m(3, 0)) +
                     m(2, 0) * (m(0, 1) * m(3, 3) - m(0, 3) * m(3, 1)) +
                     m(2, 1) * (m(0, 3) * m(3, 0) - m(0, 0) * m(3, 3)));
    out(2, 2) = d * (m(3, 3) * (m(0, 0) * m(1, 1) - m(0, 1) * m(1, 0)) +
                     m(3, 0) * (m(0, 1) * m(1, 3) - m(0, 3) * m(1, 1)) +
                     m(3, 1) * (m(0, 3) * m(1, 0) - m(0, 0) * m(1, 3)));
    out(2, 3) = d * (m(0, 3) * (m(1, 1) * m(2, 0) - m(1, 0) * m(2, 1)) +
                     m(0, 0) * (m(1, 3) * m(2, 1) - m(1, 1) * m(2, 3)) +
                     m(0, 1) * (m(1, 0) * m(2, 3) - m(1, 3) * m(2, 0)));
    out(3, 0) = d * (m(1, 0) * (m(2, 2) * m(3, 1) - m(2, 1) * m(3, 2)) +
                     m(1, 1) * (m(2, 0) * m(3, 2) - m(2, 2) * m(3, 0)) +
                     m(1, 2) * (m(2, 1) * m(3, 0) - m(2, 0) * m(3, 1)));
    out(3, 1) = d * (m(2, 0) * (m(0, 2) * m(3, 1) - m(0, 1) * m(3, 2)) +
                     m(2, 1) * (m(0, 0) * m(3, 2) - m(0, 2) * m(3, 0)) +
                     m(2, 2) * (m(0, 1) * m(3, 0) - m(0, 0) * m(3, 1)));
    out(3, 2) = d * (m(3, 0) * (m(0, 2) * m(1, 1) - m(0, 1) * m(1, 2)) +
                     m(3, 1) * (m(0, 0) * m(1, 2) - m(0, 2) * m(1, 0)) +
                     m(3, 2) * (m(0, 1) * m(1, 0) - m(0, 0) * m(1, 1)));
    out(3, 3) = d * (m(0, 0) * (m(1, 1) * m(2, 2) - m(1, 2) * m(2, 1)) +
                     m(0, 1) * (m(1, 2) * m(2, 0) - m(1, 0) * m(2, 2)) +
                     m(0, 2) * (m(1, 0) * m(2, 1) - m(1, 1) * m(2, 0)));

    return true;
}

bool matrix4::getInversePrimitive (matrix4& out ) const
{
    out.M[0 ] = M[0];
    out.M[1 ] = M[4];
    out.M[2 ] = M[8];
    out.M[3 ] = 0;

    out.M[4 ] = M[1];
    out.M[5 ] = M[5];
    out.M[6 ] = M[9];
    out.M[7 ] = 0;

    out.M[8 ] = M[2];
    out.M[9 ] = M[6];
    out.M[10] = M[10];
    out.M[11] = 0;

    out.M[12] = (double)-(M[12]*M[0] + M[13]*M[1] + M[14]*M[2]);
    out.M[13] = (double)-(M[12]*M[4] + M[13]*M[5] + M[14]*M[6]);
    out.M[14] = (double)-(M[12]*M[8] + M[13]*M[9] + M[14]*M[10]);
    out.M[15] = 1;

    return true;
}

bool matrix4::makeInverse()
{
    matrix4 temp (EM4CONST_NOTHING );

    if (getInverse(temp)) {
        *this = temp;
        return true;
    }

    return false;
}

matrix4& matrix4::operator=(const matrix4 &other)
{
    if (this==&other) return *this;
    memcpy(M, other.M, 16*sizeof(double));
    return *this;
}

matrix4& matrix4::operator=(const double & scalar)
{
    for (int i = 0; i < 16; ++i) {
        M[i]=scalar;
    }

    return *this;
}

bool matrix4::operator==(const matrix4 &other) const
{
    for (int i = 0; i < 16; ++i) {
        if (M[i] != other.M[i]) return false;
    }
    return true;
}

bool matrix4::operator!=(const matrix4 &other) const
{
    return !(*this == other);
}

// Builds a right-handed perspective projection matrix based on a field of view
matrix4& matrix4::buildProjectionMatrixPerspectiveFovRH(double fieldOfViewRadians, double aspectRatio,
                                                        double zNear, double zFar)
{
    const double h = 1./tan(fieldOfViewRadians*0.5);
    assert(aspectRatio != 0.f); //divide by zero
    const double w = static_cast<double>(h / aspectRatio);

    assert(zNear != zFar); //divide by zero
    M[0] = w;
    M[1] = 0;
    M[2] = 0;
    M[3] = 0;

    M[4] = 0;
    M[5] = (double)h;
    M[6] = 0;
    M[7] = 0;

    M[8] = 0;
    M[9] = 0;
    M[10] = (double)(zFar/(zNear-zFar)); // DirectX version
    //      M[10] = (double)(zFar+zNear/(zNear-zFar)); // OpenGL version
    M[11] = -1;

    M[12] = 0;
    M[13] = 0;
    M[14] = (double)(zNear*zFar/(zNear-zFar)); // DirectX version
    //      M[14] = (double)(2.0f*zNear*zFar/(zNear-zFar)); // OpenGL version
    M[15] = 0;

    return *this;
}

// Builds a left-handed perspective projection matrix based on a field of view
matrix4& matrix4::buildProjectionMatrixPerspectiveFovLH(double fieldOfViewRadians, double aspectRatio,
                                                        double zNear, double zFar)
{
    const double h = 1./tan(fieldOfViewRadians*0.5);
    assert(aspectRatio != 0.f); //divide by zero
    const double w = static_cast<double>(h / aspectRatio);

    assert(zNear != zFar); //divide by zero

    M[0] = w;
    M[1] = 0;
    M[2] = 0;
    M[3] = 0;

    M[4] = 0;
    M[5] = (double)h;
    M[6] = 0;
    M[7] = 0;

    M[8] = 0;
    M[9] = 0;
    M[10] = (double)(zFar/(zFar-zNear));
    M[11] = 1;

    M[12] = 0;
    M[13] = 0;
    M[14] = (double)(-zNear*zFar/(zFar-zNear));
    M[15] = 0;

    return *this;
}

// Builds a left-handed perspective projection matrix based on a field of view, with far plane culling at infinity
matrix4& matrix4::buildProjectionMatrixPerspectiveFovInfinityLH(double fieldOfViewRadians, double aspectRatio,
                                                                double zNear, double epsilon)
{
    const double h = 1./tan(fieldOfViewRadians*0.5);
    assert(aspectRatio != 0.f); //divide by zero
    const double w = static_cast<double>(h / aspectRatio);

    M[0] = w;
    M[1] = 0;
    M[2] = 0;
    M[3] = 0;

    M[4] = 0;
    M[5] = (double)h;
    M[6] = 0;
    M[7] = 0;

    M[8] = 0;
    M[9] = 0;
    M[10] = (double)(1.f-epsilon);
    M[11] = 1;

    M[12] = 0;
    M[13] = 0;
    M[14] = (double)(zNear*(epsilon-1.f));
    M[15] = 0;

    return *this;
}

// Builds a left-handed orthogonal projection matrix.
matrix4& matrix4::buildProjectionMatrixOrthoLH(double widthOfViewVolume, double heightOfViewVolume,
                                               double zNear, double zFar)
{
    assert(widthOfViewVolume != 0.f); //divide by zero
    assert(heightOfViewVolume != 0.f); //divide by zero
    assert(zNear != zFar); //divide by zero

    M[0] = (double)(2/widthOfViewVolume);
    M[1] = 0;
    M[2] = 0;
    M[3] = 0;

    M[4] = 0;
    M[5] = (double)(2/heightOfViewVolume);
    M[6] = 0;
    M[7] = 0;

    M[8] = 0;
    M[9] = 0;
    M[10] = (double)(1/(zFar-zNear));
    M[11] = 0;

    M[12] = 0;
    M[13] = 0;
    M[14] = (double)(zNear/(zNear-zFar));
    M[15] = 1;

    return *this;
}

// Builds a right-handed orthogonal projection matrix.
matrix4& matrix4::buildProjectionMatrixOrthoRH(double widthOfViewVolume, double heightOfViewVolume, double zNear, double zFar)
{
    assert(widthOfViewVolume != 0.f); //divide by zero
    assert(heightOfViewVolume != 0.f); //divide by zero
    assert(zNear != zFar); //divide by zero

    M[0] = (double)(2/widthOfViewVolume);
    M[1] = 0;
    M[2] = 0;
    M[3] = 0;

    M[4] = 0;
    M[5] = (double)(2/heightOfViewVolume);
    M[6] = 0;
    M[7] = 0;

    M[8] = 0;
    M[9] = 0;
    M[10] = (double)(1/(zNear-zFar));
    M[11] = 0;

    M[12] = 0;
    M[13] = 0;
    M[14] = (double)(zNear/(zNear-zFar));
    M[15] = 1;

    return *this;
}

// Builds a right-handed perspective projection matrix.
matrix4& matrix4::buildProjectionMatrixPerspectiveRH(double widthOfViewVolume, double heightOfViewVolume, double zNear, double zFar)
{
    assert(widthOfViewVolume != 0.f); //divide by zero
    assert(heightOfViewVolume != 0.f); //divide by zero
    assert(zNear != zFar); //divide by zero

    M[0] = (double)(2*zNear/widthOfViewVolume);
    M[1] = 0;
    M[2] = 0;
    M[3] = 0;

    M[4] = 0;
    M[5] = (double)(2*zNear/heightOfViewVolume);
    M[6] = 0;
    M[7] = 0;

    M[8] = 0;
    M[9] = 0;
    M[10] = (double)(zFar/(zNear-zFar));
    M[11] = -1;

    M[12] = 0;
    M[13] = 0;
    M[14] = (double)(zNear*zFar/(zNear-zFar));
    M[15] = 0;

    return *this;
}

// Builds a left-handed perspective projection matrix.
matrix4& matrix4::buildProjectionMatrixPerspectiveLH(double widthOfViewVolume, double heightOfViewVolume, double zNear, double zFar)
{
    assert(widthOfViewVolume != 0.f); //divide by zero
    assert(heightOfViewVolume != 0.f); //divide by zero
    assert(zNear != zFar); //divide by zero

    M[0] = (double)(2*zNear/widthOfViewVolume);
    M[1] = 0;
    M[2] = 0;
    M[3] = 0;

    M[4] = 0;
    M[5] = (double)(2*zNear/heightOfViewVolume);
    M[6] = 0;
    M[7] = 0;

    M[8] = 0;
    M[9] = 0;
    M[10] = (double)(zFar/(zFar-zNear));
    M[11] = 1;

    M[12] = 0;
    M[13] = 0;
    M[14] = (double)(zNear*zFar/(zNear-zFar));
    M[15] = 0;
    return *this;
}
/*
// Builds a matrix that flattens geometry into a plane.
matrix4& matrix4::buildShadowMatrix(const vector3d& light, plane3d plane, double point)
{
    plane.Normal.normalize();
    const double d = plane.Normal.dot(light);

    M[ 0] = (double)(-plane.Normal.X * light.X + d);
    M[ 1] = (double)(-plane.Normal.X * light.Y);
    M[ 2] = (double)(-plane.Normal.X * light.Z);
    M[ 3] = (double)(-plane.Normal.X * point);

    M[ 4] = (double)(-plane.Normal.Y * light.X);
    M[ 5] = (double)(-plane.Normal.Y * light.Y + d);
    M[ 6] = (double)(-plane.Normal.Y * light.Z);
    M[ 7] = (double)(-plane.Normal.Y * point);

    M[ 8] = (double)(-plane.Normal.Z * light.X);
    M[ 9] = (double)(-plane.Normal.Z * light.Y);
    M[10] = (double)(-plane.Normal.Z * light.Z + d);
    M[11] = (double)(-plane.Normal.Z * point);

    M[12] = (double)(-plane.D * light.X);
    M[13] = (double)(-plane.D * light.Y);
    M[14] = (double)(-plane.D * light.Z);
    M[15] = (double)(-plane.D * point + d);
    return *this;
}
*/
// Builds a left-handed look-at matrix.
matrix4& matrix4::buildCameraLookAtMatrixLH(const vector3d& position,
                                            const vector3d& target,
                                            const vector3d& upVector)
{
    vector3d zaxis = target - position;
    zaxis.normalize();

    vector3d xaxis = upVector.cross(zaxis);
    xaxis.normalize();

    vector3d yaxis = zaxis.cross(xaxis);

    M[0] = (double)xaxis.X;
    M[1] = (double)yaxis.X;
    M[2] = (double)zaxis.X;
    M[3] = 0;

    M[4] = (double)xaxis.Y;
    M[5] = (double)yaxis.Y;
    M[6] = (double)zaxis.Y;
    M[7] = 0;

    M[8] = (double)xaxis.Z;
    M[9] = (double)yaxis.Z;
    M[10] = (double)zaxis.Z;
    M[11] = 0;

    M[12] = (double)-xaxis.dot(position);
    M[13] = (double)-yaxis.dot(position);
    M[14] = (double)-zaxis.dot(position);
    M[15] = 1;
    return *this;
}

// Builds a right-handed look-at matrix.
matrix4& matrix4::buildCameraLookAtMatrixRH(
                                            const vector3d& position,
                                            const vector3d& target,
                                            const vector3d& upVector)
{
    vector3d zaxis = position - target;
    zaxis.normalize();

    vector3d xaxis = upVector.cross(zaxis);
    xaxis.normalize();

    vector3d yaxis = zaxis.cross(xaxis);

    M[0] = (double)xaxis.X;
    M[1] = (double)yaxis.X;
    M[2] = (double)zaxis.X;
    M[3] = 0;

    M[4] = (double)xaxis.Y;
    M[5] = (double)yaxis.Y;
    M[6] = (double)zaxis.Y;
    M[7] = 0;

    M[8] = (double)xaxis.Z;
    M[9] = (double)yaxis.Z;
    M[10] = (double)zaxis.Z;
    M[11] = 0;

    M[12] = (double)-xaxis.dot(position);
    M[13] = (double)-yaxis.dot(position);
    M[14] = (double)-zaxis.dot(position);
    M[15] = 1;
    return *this;
}

// creates a new matrix as interpolated matrix from this and the passed one.
matrix4 matrix4::interpolate(const matrix4& b, double time) const
{
    matrix4 mat (EM4CONST_NOTHING );

    for (unsigned int i=0; i < 16; i += 4) {
        mat.M[i+0] = (double)(M[i+0] + (b.M[i+0] - M[i+0] ) * time);
        mat.M[i+1] = (double)(M[i+1] + (b.M[i+1] - M[i+1] ) * time);
        mat.M[i+2] = (double)(M[i+2] + (b.M[i+2] - M[i+2] ) * time);
        mat.M[i+3] = (double)(M[i+3] + (b.M[i+3] - M[i+3] ) * time);
    }
    return mat;
}

// returns transposed matrix
matrix4 matrix4::getTransposed() const
{
    matrix4 t (EM4CONST_NOTHING );
    getTransposed (t );
    return t;
}

// returns transposed matrix
void matrix4::getTransposed(matrix4& o ) const
{
    o[ 0] = M[ 0];
    o[ 1] = M[ 4];
    o[ 2] = M[ 8];
    o[ 3] = M[12];

    o[ 4] = M[ 1];
    o[ 5] = M[ 5];
    o[ 6] = M[ 9];
    o[ 7] = M[13];

    o[ 8] = M[ 2];
    o[ 9] = M[ 6];
    o[10] = M[10];
    o[11] = M[14];

    o[12] = M[ 3];
    o[13] = M[ 7];
    o[14] = M[11];
    o[15] = M[15];
}
/*
// used to scale <-1,-1><1,1> to viewport
matrix4& matrix4::buildNDCToDCMatrix(const rect<int>& viewport, double zScale)
{
    const double scaleX = (viewport.getWidth() - 0.75f ) * 0.5f;
    const double scaleY = -(viewport.getHeight() - 0.75f ) * 0.5f;

    const double dx = -0.5f + ((viewport.UpperLeftCorner.X + viewport.LowerRightCorner.X ) * 0.5f );
    const double dy = -0.5f + ((viewport.UpperLeftCorner.Y + viewport.LowerRightCorner.Y ) * 0.5f );

    makeIdentity();
    M[12] = (double)dx;
    M[13] = (double)dy;
    return setScale(vector3d((double)scaleX, (double)scaleY, (double)zScale));
}
*/
matrix4& matrix4::buildRotateFromTo(const vector3d& from, const vector3d& to)
{
    // unit vectors
    vector3d f(from);
    vector3d t(to);
    f.normalize();
    t.normalize();

    // axis multiplication by sin
    vector3d vs(t.cross(f));

    // axis of rotation
    vector3d v(vs);
    v.normalize();

    // cosinus angle
    double ca = f.dot(t);

    vector3d vt(v * (1 - ca));

    M[0] = vt.X * v.X + ca;
    M[5] = vt.Y * v.Y + ca;
    M[10] = vt.Z * v.Z + ca;

    vt.X *= v.Y;
    vt.Z *= v.X;
    vt.Y *= v.Z;

    M[1] = vt.X - vs.Z;
    M[2] = vt.Z + vs.Y;
    M[3] = 0;

    M[4] = vt.X + vs.Z;
    M[6] = vt.Y - vs.X;
    M[7] = 0;

    M[8] = vt.Z - vs.Y;
    M[9] = vt.Y + vs.X;
    M[11] = 0;

    M[12] = 0;
    M[13] = 0;
    M[14] = 0;
    M[15] = 1;

    return *this;
}

void matrix4::buildAxisAlignedBillboard(const vector3d& camPos,
                                        const vector3d& center,
                                        const vector3d& translation,
                                        const vector3d& axis,
                                        const vector3d& from)
{
    // axis of rotation
    vector3d up = axis;
    up.normalize();
    const vector3d forward = (camPos - center).normalize();
    const vector3d right = up.cross(forward).normalize();

    // correct look vector
    const vector3d look = right.cross(up);

    // rotate from to
    // axis multiplication by sin
    const vector3d vs = look.cross(from);

    // cosinus angle
    const double ca = from.dot(look);

    vector3d vt(up * (1.f - ca));

    M[0] = static_cast<double>(vt.X * up.X + ca);
    M[5] = static_cast<double>(vt.Y * up.Y + ca);
    M[10] = static_cast<double>(vt.Z * up.Z + ca);

    vt.X *= up.Y;
    vt.Z *= up.X;
    vt.Y *= up.Z;

    M[1] = static_cast<double>(vt.X - vs.Z);
    M[2] = static_cast<double>(vt.Z + vs.Y);
    M[3] = 0;

    M[4] = static_cast<double>(vt.X + vs.Z);
    M[6] = static_cast<double>(vt.Y - vs.X);
    M[7] = 0;

    M[8] = static_cast<double>(vt.Z - vs.Y);
    M[9] = static_cast<double>(vt.Y + vs.X);
    M[11] = 0;

    setRotationCenter(center, translation);
}

void matrix4::setRotationCenter(const vector3d& center, const vector3d& translation)
{
    M[12] = -M[0]*center.X - M[4]*center.Y - M[8]*center.Z + (center.X - translation.X );
    M[13] = -M[1]*center.X - M[5]*center.Y - M[9]*center.Z + (center.Y - translation.Y );
    M[14] = -M[2]*center.X - M[6]*center.Y - M[10]*center.Z + (center.Z - translation.Z );
    M[15] = (double) 1.0;
}
/*
matrix4& matrix4::buildTextureTransform(double rotateRad,
                                         const vector2df &rotatecenter,
                                         const vector2df &translate,
                                         const vector2df &scale)
{
    const double c = cosf(rotateRad);
    const double s = sinf(rotateRad);

    M[0] = (double)(c * scale.X);
    M[1] = (double)(s * scale.Y);
    M[2] = 0;
    M[3] = 0;

    M[4] = (double)(-s * scale.X);
    M[5] = (double)(c * scale.Y);
    M[6] = 0;
    M[7] = 0;

    M[8] = (double)(c * scale.X * rotatecenter.X + -s * rotatecenter.Y + translate.X);
    M[9] = (double)(s * scale.Y * rotatecenter.X +  c * rotatecenter.Y + translate.Y);
    M[10] = 1;
    M[11] = 0;

    M[12] = 0;
    M[13] = 0;
    M[14] = 0;
    M[15] = 1;
    return *this;
}

// rotate about z axis, center (0.5, 0.5 )
matrix4& matrix4::setTextureRotationCenter(double rotateRad )
{
    const double c = cosf(rotateRad);
    const double s = sinf(rotateRad);
    M[0] = (double)c;
    M[1] = (double)s;

    M[4] = (double)-s;
    M[5] = (double)c;

    M[8] = (double)(0.5f * (s - c) + 0.5f);
    M[9] = (double)(-0.5f * (s + c) + 0.5f);

    return *this;
}

matrix4& matrix4::setTextureTranslate (double x, double y )
{
    M[8] = (double)x;
    M[9] = (double)y;

    return *this;
}

matrix4& matrix4::setTextureTranslateTransposed (double x, double y )
{
    M[2] = (double)x;
    M[6] = (double)y;

    return *this;
}

matrix4& matrix4::setTextureScale (double sx, double sy )
{
    M[0] = (double)sx;
    M[5] = (double)sy;
    return *this;
}

matrix4& matrix4::setTextureScaleCenter(double sx, double sy )
{
    M[0] = (double)sx;
    M[5] = (double)sy;
    M[8] = (double)(0.5f - 0.5f * sx);
    M[9] = (double)(0.5f - 0.5f * sy);

    return *this;
}
*/
// sets all matrix data members at once
matrix4& matrix4::setM(const double * data)
{
    memcpy(M,data, 16*sizeof(double));

    return *this;
}

// sets if the matrix is definitely identity matrix
void matrix4::setDefinitelyIdentityMatrix(bool isDefinitelyIdentityMatrix)
{
}

// gets if the matrix is definitely identity matrix
bool matrix4::getDefinitelyIdentityMatrix() const
{
    return false;
}

bool matrix4::isEqualTo(const matrix4& other, const double tolerance) const
{
    for (int i = 0; i < 16; ++i)
        if (!equals(M[i],other.M[i], tolerance))
            return false;

    return true;
}

// Multiply by scalar.
matrix4 operator*(const double scalar, const matrix4& mat)
{
    return mat*scalar;
}

const matrix3 IdentityMatrix3(matrix3::EM3CONST_IDENTITY);
const matrix4 IdentityMatrix4(matrix4::EM4CONST_IDENTITY);

}

