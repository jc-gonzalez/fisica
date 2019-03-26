/******************************************************************************
 * File:    comparisons.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.comparisons
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

#ifndef COMPARISONS_H
#define COMPARISONS_H

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

    //----------------------------------------------------------------------
    // Functions for comparison with tolerances
    //----------------------------------------------------------------------

    extern const double Epsilon;
    extern const double StdEpsilon;

    // Implements relative method - do not use for comparing with zero
    // Use this most of the time, tolerance needs to be meaningful in your context
    bool isApproximatelyEqual(double a, double b, double tolerance = StdEpsilon);

    // Supply tolerance that is meaningful in your context
    // For example, default tolerance may not work if you are comparing double with float
    bool isApproximatelyZero(double a, double tolerance = StdEpsilon);

    // Use this when you want to be on safe side
    // For example, don't start rover unless signal is above 1
    bool isDefinitelyLessThan(double a, double b, double tolerance = StdEpsilon);
    bool isDefinitelyGreaterThan(double a, double b, double tolerance = StdEpsilon);

    // implements ULP (Units-in-the-Last-Place) method
    // Use this when you are only concerned about floating point precision issue
    // For example, if you want to see if a is 1.0 by checking if its within
    // 10 closest representable floating point numbers around 1.0.
    bool isWithinPrecisionInterval(double a, double b, unsigned int interval_size = 1);

    bool equals(double a, double b, const double tolerance = Epsilon);
    bool iszero(double a, const double tolerance = Epsilon);
    bool clamp(double x, double a, double b, const double tolerance = Epsilon);

}

#endif // COMPARISONS_H
