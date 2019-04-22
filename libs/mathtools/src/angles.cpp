/******************************************************************************
 * File:    angles.cpp
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.angles
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
#include "angles.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

const double Radians2Degrees = M_PI / 180.;
const double Degrees2Radians = 180. / M_PI;

const double Deg360 = (2.*M_PI);       // 2 Pi
const double Deg180 = (M_PI);          //   Pi
const double Deg120 = (2.*M_PI/3.);    // 2 Pi / 3
const double Deg90  = (M_PI/2.);       //   Pi / 2
const double Deg60  = (M_PI/3.);       //   Pi / 3
const double Deg30  = (M_PI/6.);       //   Pi / 6

double d2r(double x) { return x * Radians2Degrees; }
double r2d(double x) { return x * Degrees2Radians; }

}

