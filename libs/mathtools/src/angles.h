/******************************************************************************
 * File:    angles.h
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

#ifndef ANGLES_H
#define ANGLES_H

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
//   nones
//------------------------------------------------------------

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

    extern const double Radians2Degrees;
    extern const double Degrees2Radians;

    extern const double Deg360;    // 2 Pi
    extern const double Deg180;    //   Pi
    extern const double Deg120;    // 2 Pi / 3
    extern const double Deg90;     //   Pi / 2
    extern const double Deg60;     //   Pi / 3
    extern const double Deg30;     //   Pi / 6

    double d2r(double x);
    double r2d(double x);

}

#endif // ANGLES_H
