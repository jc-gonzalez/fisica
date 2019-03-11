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

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------

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

#define RandomNumber drand48()

    template<typename T>
    T sqr(T x);

    template<typename T>
    T d2r(T x);

    template<typename T>
    T r2d(T x);

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


}


#endif  /* MATHTOOLS_H */
