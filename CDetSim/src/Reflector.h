/******************************************************************************
 * File:    reflector.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.reflector
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

#ifndef REFLECTOR_H
#define REFLECTOR_H

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
#include "physconst.h"
#include "json.h"

// Type: Mirrors definition
typedef json::Object MirrorSet;

//======================================================================
// Class: Reflector
//======================================================================
class Reflector {
public:
    Reflector();
    ~Reflector();

public:
    void setMirrorsFile(std::string fileName);
    
private:
    // Matrices to change to the system where the optical axis is OZ
    double OmegaCT[3][3];

    // Matrices to change to the system where the optical axis is OZ (inverse)
    double OmegaICT[3][3];

    // Matrices to change the system of coordinates
    double Omega[3][3];

    // Matrices to change the system of coordinates (inverse)
    double OmegaI[3][3];

    // Focal distances [cm]
    std::vector<double> ct_Focal;

    // Mean Focal distances [cm]
    double ct_Focal_mean;

    // STDev. Focal distances [cm]
    double ct_Focal_std;

    // Mean Point Spread function [cm]
    double ct_PSpread_mean;

    // STDev. Point Spread function [cm]
    double ct_PSpread_std;

    // STDev. Adjustmente deviation [cm]
    double ct_Adjustment_std;

    // Radius of the Black Spot in mirror [cm]
    double ct_BlackSpot_rad;

    // Radius of one mirror [cm]
    double ct_RMirror;

    // Camera width [cm]
    double ct_CameraWidth;

    // Pixel width [cm]
    double ct_PixelWidth;

    // Number of mirrors
    int ct_NMirrors = 0;

    // Number of pixels
    int ct_NPixels;

    /* Mirrors information
     *  TYPE=1  (MAGIC)
     *      i  f   sx   sy   x   y   z   thetan  phin
     *
     *       i : number of the mirror
     *       f : focal distance of that mirror
     *      sx : curvilinear coordinate of mirror's center in X[cm]
     *      sy : curvilinear coordinate of mirror's center in X[cm]
     *       x : x coordinate of the center of the mirror [cm]
     *       y : y coordinate of the center of the mirror [cm]
     *       z : z coordinate of the center of the mirror [cm]
     *  thetan : polar theta angle of the direction where the mirror points to
     *    phin : polar phi angle of the direction where the mirror points to
     *      xn : xn coordinate of the normal vector in the center (normalized)
     *      yn : yn coordinate of the normal vector in the center (normalized)
     *      zn : zn coordinate of the normal vector in the center (normalized)
     */

    // Pointer to a table with the following info.:
    double **ct_data;

    // The mirror table has data obtained from this object
    MirrorSet mirrors;
    
    // Table with datapoints (wavelength,reflec.)
    double **reflectivity;

    // Number of datapoints
    int    nReflectivity;

    // Table with deviations of the mirrors' normals
    double **axisDeviation;

    // Table of normal random numbers
    double normalRandomNumbers[500];

    // Flag to change the verbosity
    int verbose;

};


#endif  /* REFLECTOR_H */
