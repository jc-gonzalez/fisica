/******************************************************************************
 * File:    mathtools.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.mathtools
 *
 * Last update:  2.0
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

#ifndef MATHTOOLS_H
#define MATHTOOLS_H

#include <cmath>
#include <cstdlib>
#include <tuple>

#include <random>
#include <chrono>
#include <thread>

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
//   none
//------------------------------------------------------------

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

    //#define RandomNumber drand48()

    class UnifRnd {
    public:
	std::random_device                     rd;
	std::mt19937                           mt;
	std::uniform_real_distribution<double> unif;

	UnifRnd(double a, double b) : rd{}, mt{rd()},  unif{a, b} {}

	double operator()() { return unif(mt); }
    };
    
    template<typename T>
    T sqr(T x);

    template<typename T>
    T d2r(T x);

    template<typename T>
    T r2d(T x);

    template<typename T>
    T norm2(T x, T y, T z);
    
    template<typename T>
    T norm(T x, T y, T z);
    
    template<typename T>
    void makeOmega(T (& Omega)[3][3] , T theta, T phi);

    template<typename T>
    void makeOmegaI(T (& OmegaI)[3][3], T theta, T phi);

    template<typename T>
    void applyMxV(T M[3][3], T *V, T *Vp);

    template<typename T>
    void rnormal(T *r, int n);

    template<typename T>
    T dist_r_P(T a, T b, T c,
               T u, T v, T w,
               T x, T y, T z);


    typedef std::tuple<double, double, double>  point3D;
    typedef std::tuple<double, double, double>  vector3D;
    
    typedef std::tuple<std::tuple<double, double, double>,
		       std::tuple<double, double, double>,
		       std::tuple<double, double, double>> matrix3D;

    vector3D operator+(vector3D a, vector3D b);
    vector3D operator-(vector3D a, vector3D b);
    double dot(vector3D a, vector3D b);
    vector3D cross(vector3D a, vector3D b);
    
    double norm2(vector3D v);
    double norm(vector3D v);
    matrix3D makeOmega(double theta, double phi);
    matrix3D makeOmegaI(double theta, double phi);
    vector3D applyMxV(matrix3D M, vector3D & V);

    std::ostream & operator<<(std::ostream & os, vector3D & v);

}


#endif  /* MATHTOOLS_H */
