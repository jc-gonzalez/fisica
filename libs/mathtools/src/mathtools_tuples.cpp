/******************************************************************************
 * File:    Reflector.cpp
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.reflector
 *
 * Version: 0.3
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
 *   Implement ServiceMng class
 *
 * Created by:
 *   J C Gonzalez
 *
 * Status:
 *   Prototype
 *
 * Dependencies:
 *   none
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

#include "mathtools.h"

namespace MathTools {

vector3D operator+(vector3D a, vector3D b)
{
    return std::make_tuple(std::get<0>(a) + std::get<0>(b),
			   std::get<1>(a) + std::get<1>(b),
			   std::get<2>(a) + std::get<2>(b));
}

vector3D operator-(vector3D a, vector3D b)
{
    return std::make_tuple(std::get<0>(a) - std::get<0>(b),
			   std::get<1>(a) - std::get<1>(b),
			   std::get<2>(a) - std::get<2>(b));
}

vector3D operator*(vector3D a, double x)
{
    return std::make_tuple(std::get<0>(a) * x,
			   std::get<1>(a) * x,
			   std::get<2>(a) * x);
}

vector3D operator/(vector3D a, double x)
{
    return std::make_tuple(std::get<0>(a) / x,
			   std::get<1>(a) / x,
			   std::get<2>(a) / x);
}

double dot(vector3D a, vector3D b)
{
    return (std::get<0>(a) * std::get<0>(b) +
	    std::get<1>(a) * std::get<1>(b) +
	    std::get<2>(a) * std::get<2>(b));
}

vector3D cross(vector3D a, vector3D b)
{
    return std::make_tuple(0., 0., 0.);
}

double norm2(vector3D v)
{
    double x, y, z;
    std::tie(x, y, z) = v;
    return norm2<double>(x, y, z);
}

double norm(vector3D v)
{
    double x, y, z;
    std::tie(x, y, z) = v;
    return norm<double>(x, y, z);
}

void normalize(vector3D & v)
{
    v = v / norm(v);
}
    
//!---------------------------------------------------------------------
// @name makeOmega
//
// @desc function to calculate the matrix Omega(theta,phi)
//
// @var    theta   Angle theta of the transformation
// @var    phi     Angle phi of the transformation
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
matrix3D makeOmega(double theta, double phi)
{
    static double ct, st, cp, sp;
  
    // shortcuts for cosine and sine of theta and phi
    ct = cos(theta);
    st = sin(theta);
    cp = cos(phi);
    sp = sin(phi);
    
    // save values in the array (see top of file)
    return std::tuple<std::tuple<double, double, double>,
		      std::tuple<double, double, double>,
		      std::tuple<double, double, double>>
	(std::tuple<double, double, double>(cp * ct, sp * ct, -st), 
	 std::tuple<double, double, double>(-sp,     cp,      0.),
	 std::tuple<double, double, double>(cp * st, sp * st, ct));
}

//!---------------------------------------------------------------------
// @name makeOmegaI
//
// @desc function to calculate the matrix Omega-1(theta,phi)
//
// @var    theta   Angle theta of the transformation
// @var    phi     Angle phi of the transformation
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
matrix3D makeOmegaI(double theta, double phi)
{
    static double ct, st, cp, sp;
  
    // shortcuts for cosine and sine of theta and phi
    ct = cos(theta);
    st = sin(theta);
    cp = cos(phi);
    sp = sin(phi);

    
    // save values in the array (see top of file)
    return std::tuple<std::tuple<double, double, double>,
		      std::tuple<double, double, double>,
		      std::tuple<double, double, double>>
	(std::tuple<double, double, double>(cp * ct, -sp,  cp * st), 
	 std::tuple<double, double, double>(sp * ct,  cp,  sp * st),
	 std::tuple<double, double, double>(  -st,    0,     ct   ));
}

//!---------------------------------------------------------------------
// @name applyMxv
//
// @desc returns the vector v' such that v' = M x v
//
// @var    M       matrix of the transformation
// @var    v       vector to be multiplied
// @var    vi      resulting vector
//
// @date Sat Jun 27 05:58:56 MET DST 1998
//----------------------------------------------------------------------
vector3D applyMxV(matrix3D M, vector3D & V)
{
    std::tuple<double, double, double> M0, M1, M2;
    double M00, M01, M02, M10, M11, M12, M20, M21, M22;
    double V0, V1, V2;
    std::tie(M0, M1, M2) = M;
    std::tie(M00, M01, M02) = M0;
    std::tie(M10, M11, M12) = M1;
    std::tie(M20, M21, M22) = M2;
    std::tie(V0, V1, V2) = V;

    return vector3D
	(((M00 * V0) +  (M01 * V1) +  (M02 * V2)),
	 ((M10 * V0) +  (M11 * V1) +  (M12 * V2)),
	 ((M20 * V0) +  (M21 * V1) +  (M22 * V2)));
}

std::ostream & operator<<(std::ostream & os, vector3D & v)
{
    double x, y, z;
    std::tie(x, y, z) = v;
    os << '(' << x << ", " << y << ", " << z << ')';
    return os;
}

} // namespace MathTools

//}

