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

#ifndef MATRIX_H
#define MATRIX_H

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
#include "vectors.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

//======================================================================
// Class: matrix3
//======================================================================
class matrix3 {

public:

    enum eConstructor { EM3CONST_NOTHING = 0,
                        EM3CONST_COPY,
                        EM3CONST_IDENTITY,
                        EM3CONST_TRANSPOSED,
                        EM3CONST_INVERSE,
                        EM3CONST_INVERSE_TRANSPOSED };

    matrix3(eConstructor constructor = EM3CONST_IDENTITY );
    matrix3(const matrix3& other, eConstructor constructor = EM3CONST_COPY);

    double & operator()(const int row, const int col);
    const double & operator()(const int row, const int col) const;

    double & operator[](unsigned int index);
    const double & operator[](unsigned int index) const;

    matrix3& operator=(const matrix3 &other);
    matrix3& operator=(const double & scalar);

    const double * pointer() const;
    double * pointer();

    bool operator==(const matrix3 &other) const;
    bool operator!=(const matrix3 &other) const;

    matrix3 operator+(const matrix3& other) const;
    matrix3& operator+=(const matrix3& other);

    matrix3 operator-(const matrix3& other) const;
    matrix3& operator-=(const matrix3& other);

    matrix3& setbyproduct(const matrix3& other_a,const matrix3& other_b );
    matrix3& setbyproduct_nocheck(const matrix3& other_a,const matrix3& other_b );

    matrix3 operator*(const matrix3& other) const;
    matrix3& operator*=(const matrix3& other);
    matrix3 operator*(const double & scalar) const;
    matrix3& operator*=(const double & scalar);

    vector3d operator*(const vector3d& other) const;

    matrix3& makeIdentity();
    bool isIdentity() const;
    bool isOrthogonal() const;

    matrix3& setTranslation(const vector3d& translation );
    vector3d getTranslation() const;
    matrix3& setInverseTranslation(const vector3d& translation );

    matrix3& setRotationThetaPhi(double theta, double phi);
    matrix3& setInverseRotationThetaPhi(double theta, double phi);

    matrix3& setRotationRadians(const vector3d& rotation );
    matrix3& setRotationDegrees(const vector3d& rotation );
    vector3d getRotationDegrees() const;

    matrix3& setInverseRotationRadians(const vector3d& rotation );
    matrix3& setInverseRotationDegrees(const vector3d& rotation );
    matrix3& setRotationAxisRadians(const double & angle, const vector3d& axis);

    matrix3& setScale(const vector3d& scale );
    matrix3& setScale(const double scale );
    vector3d getScale() const;

    void inverseTranslateVect(vector3d& vect ) const;
    void inverseRotateVect(vector3d& vect ) const;
    void rotateVect(vector3d& vect ) const;
    void rotateVect(vector3d& out, const vector3d& in) const;
    void rotateVect(double *out,const vector3d &in) const;

    void transformVect(vector3d& vect) const;
    void transformVect(vector3d& out, const vector3d& in ) const;
    void transformVect(double *out,const vector3d &in) const;
    void transformVec3(double *out, const double * in) const;

    void translateVect(vector3d& vect ) const;
    /*
    void transformPlane(plane3d &plane) const;
    void transformPlane(const plane3d &in, plane3d &out) const;
    void transformBox(aabbox3d& box) const;
    void transformBoxEx(aabbox3d& box) const;
    */
    void multiplyWith1x3Matrix(double * matrix) const;

    bool makeInverse();

    bool getInversePrimitive (matrix3& out ) const;
    bool getInverse(matrix3& out) const;

    matrix3 interpolate(const matrix3& b, double time) const;

    matrix3 getTransposed() const;
    void getTransposed(matrix3& dest ) const;

    matrix3& buildRotateFromTo(const vector3d& from, const vector3d& to);

    matrix3& setM(const double * data);

    void setDefinitelyIdentityMatrix(bool isDefinitelyIdentityMatrix);
    bool getDefinitelyIdentityMatrix() const;

    bool isEqualTo(const matrix3& other, const double tolerance = Epsilon) const;

private:
    double M[9];
};


//======================================================================
// Class: matrix4
//======================================================================
class matrix4 {

public:

    enum eConstructor { EM4CONST_NOTHING = 0,
                        EM4CONST_COPY,
                        EM4CONST_IDENTITY,
                        EM4CONST_TRANSPOSED,
                        EM4CONST_INVERSE,
                        EM4CONST_INVERSE_TRANSPOSED };

    matrix4(eConstructor constructor = EM4CONST_IDENTITY );
    matrix4(const matrix4& other, eConstructor constructor = EM4CONST_COPY);

    double & operator()(const int row, const int col);
    const double & operator()(const int row, const int col) const;

    double & operator[](unsigned int index);
    const double & operator[](unsigned int index) const;

    matrix4& operator=(const matrix4 &other);
    matrix4& operator=(const double & scalar);

    const double * pointer() const;
    double * pointer();

    bool operator==(const matrix4 &other) const;
    bool operator!=(const matrix4 &other) const;

    matrix4 operator+(const matrix4& other) const;
    matrix4& operator+=(const matrix4& other);

    matrix4 operator-(const matrix4& other) const;
    matrix4& operator-=(const matrix4& other);

    matrix4& setbyproduct(const matrix4& other_a,const matrix4& other_b );
    matrix4& setbyproduct_nocheck(const matrix4& other_a,const matrix4& other_b );

    matrix4 operator*(const matrix4& other) const;
    matrix4& operator*=(const matrix4& other);
    matrix4 operator*(const double & scalar) const;
    matrix4& operator*=(const double & scalar);

    matrix4& makeIdentity();
    bool isIdentity() const;
    bool isOrthogonal() const;

    matrix4& setTranslation(const vector3d& translation );
    vector3d getTranslation() const;
    matrix4& setInverseTranslation(const vector3d& translation );

    matrix4& setRotationRadians(const vector3d& rotation );
    matrix4& setRotationDegrees(const vector3d& rotation );
    vector3d getRotationDegrees() const;

    matrix4& setInverseRotationRadians(const vector3d& rotation );
    matrix4& setInverseRotationDegrees(const vector3d& rotation );
    matrix4& setRotationAxisRadians(const double & angle, const vector3d& axis);

    matrix4& setScale(const vector3d& scale );
    matrix4& setScale(const double scale );
    vector3d getScale() const;

    void inverseTranslateVect(vector3d& vect ) const;
    void inverseRotateVect(vector3d& vect ) const;
    void rotateVect(vector3d& vect ) const;
    void rotateVect(vector3d& out, const vector3d& in) const;
    void rotateVect(double *out,const vector3d &in) const;

    void transformVect(vector3d& vect) const;
    void transformVect(vector3d& out, const vector3d& in ) const;
    void transformVect(double *out,const vector3d &in) const;
    void transformVec3(double *out, const double * in) const;

    void translateVect(vector3d& vect ) const;
    /*
    void transformPlane(plane3d &plane) const;
    void transformPlane(const plane3d &in, plane3d &out) const;
    void transformBox(aabbox3d& box) const;
    void transformBoxEx(aabbox3d& box) const;
    */
    void multiplyWith1x4Matrix(double * matrix) const;

    bool makeInverse();

    bool getInversePrimitive (matrix4& out ) const;
    bool getInverse(matrix4& out) const;

    matrix4& buildProjectionMatrixPerspectiveFovRH(double fieldOfViewRadians, double aspectRatio, double zNear, double zFar);
    matrix4& buildProjectionMatrixPerspectiveFovLH(double fieldOfViewRadians, double aspectRatio, double zNear, double zFar);
    matrix4& buildProjectionMatrixPerspectiveFovInfinityLH(double fieldOfViewRadians, double aspectRatio, double zNear, double epsilon=0);
    matrix4& buildProjectionMatrixPerspectiveRH(double widthOfViewVolume, double heightOfViewVolume, double zNear, double zFar);
    matrix4& buildProjectionMatrixPerspectiveLH(double widthOfViewVolume, double heightOfViewVolume, double zNear, double zFar);
    matrix4& buildProjectionMatrixOrthoLH(double widthOfViewVolume, double heightOfViewVolume, double zNear, double zFar);
    matrix4& buildProjectionMatrixOrthoRH(double widthOfViewVolume, double heightOfViewVolume, double zNear, double zFar);

    matrix4& buildCameraLookAtMatrixLH(const vector3d& position,
                                       const vector3d& target,
                                       const vector3d& upVector);

    matrix4& buildCameraLookAtMatrixRH(const vector3d& position,
                                       const vector3d& target,
                                       const vector3d& upVector);
    /*
    matrix4& buildShadowMatrix(const vector3d& light, plane3d plane, double point=1.0f);

    matrix4& buildNDCToDCMatrix(const rect<int>& area, double zScale);
    */
    matrix4 interpolate(const matrix4& b, double time) const;

    matrix4 getTransposed() const;
    void getTransposed(matrix4& dest ) const;

    matrix4& buildRotateFromTo(const vector3d& from, const vector3d& to);
    void setRotationCenter(const vector3d& center, const vector3d& translate);

    void buildAxisAlignedBillboard(const vector3d& camPos,
                                   const vector3d& center,
                                   const vector3d& translation,
                                   const vector3d& axis,
                                   const vector3d& from);

    /*
      construct 2D Texture transformations
      rotate about center, scale, and transform.

    matrix4& buildTextureTransform(double rotateRad,
                                    const vector2df &rotatecenter,
                                    const vector2df &translate,
                                    const vector2df &scale);

    matrix4& setTextureRotationCenter(double radAngle );
    matrix4& setTextureTranslate(double x, double y );
    matrix4& setTextureTranslateTransposed(double x, double y );
    matrix4& setTextureScale(double sx, double sy );
    matrix4& setTextureScaleCenter(double sx, double sy );
    */
    matrix4& setM(const double * data);

    void setDefinitelyIdentityMatrix(bool isDefinitelyIdentityMatrix);
    bool getDefinitelyIdentityMatrix() const;

    bool isEqualTo(const matrix4& other, const double tolerance = Epsilon) const;

private:
    double M[16];
};

}

#endif // MATRIX_H
