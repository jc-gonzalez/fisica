/******************************************************************************
 * File:    comparisons.cpp
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
#include "comparisons.h"

//======================================================================
// Namespace: MathTools
//======================================================================
namespace MathTools {

//----------------------------------------------------------------------
// Functions for comparison with tolerances
//----------------------------------------------------------------------

const double Epsilon = 1.0e-6;
const double StdEpsilon = std::numeric_limits<double>::epsilon();

// Implements relative method - do not use for comparing with zero
// Use this most of the time, tolerance needs to be meaningful in your context
bool isApproximatelyEqual(double a, double b, double tolerance)
{
    double diff = std::fabs(a - b);
    if (diff <= tolerance) { return true; }
    if (diff < std::fmax(std::fabs(a), std::fabs(b)) * tolerance) { return true; }
    return false;
}

// Supply tolerance that is meaningful in your context
// For example, default tolerance may not work if you are comparing double with float
bool isApproximatelyZero(double a, double tolerance)
{
    return (std::fabs(a) <= tolerance);
}

// Use this when you want to be on safe side
// For example, don't start rover unless signal is above 1
bool isDefinitelyLessThan(double a, double b, double tolerance)
{
    double diff = a - b;
    if (diff < tolerance) { return true; }
    if (diff < std::fmax(std::fabs(a), std::fabs(b)) * tolerance) { return true; }
    return false;
}

bool isDefinitelyGreaterThan(double a, double b, double tolerance)
{
    double diff = a - b;
    if (diff > tolerance) { return true; }
    if (diff > std::fmax(std::fabs(a), std::fabs(b)) * tolerance) { return true; }
    return false;
}

// implements ULP (Units-in-the-Last-Place) method
// Use this when you are only concerned about floating point precision issue
// For example, if you want to see if a is 1.0 by checking if its within
// 10 closest representable floating point numbers around 1.0.
bool isWithinPrecisionInterval(double a, double b, unsigned int interval_size)
{
    double min_a = a - (a - std::nextafter(a, std::numeric_limits<double>::lowest())) * interval_size;
    double max_a = a + (std::nextafter(a, std::numeric_limits<double>::max()) - a) * interval_size;
    return min_a <= b && max_a >= b;
}

bool equals(double a, double b, const double tolerance)
{
    return isApproximatelyEqual(a, b, tolerance);
}

bool iszero(double a, const double tolerance)
{
    return isApproximatelyZero(a, tolerance);
}

bool clamp(double x, double a, double b, const double tolerance)
{
    return ((isDefinitelyLessThan(x, a, tolerance)) ? a :
            (isDefinitelyGreaterThan(x, b, tolerance)) ? b : x);
}

}

