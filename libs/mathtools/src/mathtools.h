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

    double sqr(double x);
    float sqr(float x);
    int sqr(int x);
    
    double d2r(double x);
    float d2r(float x);
    
    double r2d(double x);
    float r2d(float x);
    
    void makeOmega(float (& Omega)[3][3] , float theta, float phi);
    void makeOmegaI(float (& OmegaI)[3][3], float theta, float phi);
    void applyMxV(float M[3][3], float *V, float *Vp);
    void rnormal(double *r, int n);
    float dist_r_P(float a, float b, float c,
                   float u, float v, float w,
                   float x, float y, float z);

}


#endif  /* MATHTOOLS_H */
