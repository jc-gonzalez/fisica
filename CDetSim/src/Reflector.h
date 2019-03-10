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

//======================================================================
// Class: Reflector
//======================================================================
class Reflector {
public:
    Reflector();

private:
    //@: matrices to change to the system where the optical axis is OZ
    float OmegaCT[3][3];

    //@: matrices to change to the system where the optical axis is OZ (inverse)
    float OmegaICT[3][3];

    //@: matrices to change the system of coordinates
    float Omega[3][3];

    //@: matrices to change the system of coordinates (inverse)
    float OmegaI[3][3];

    //@: Focal distances [cm]
    float *ct_Focal;

    //@: Mean Focal distances [cm]
    float ct_Focal_mean;

    //@: STDev. Focal distances [cm]
    float ct_Focal_std;

    //@: Mean Point Spread function [cm]
    float ct_PSpread_mean;

    //@: STDev. Point Spread function [cm]
    float ct_PSpread_std;

    //@: STDev. Adjustmente deviation [cm]
    float ct_Adjustment_std;

    //@: Radius of the Black Spot in mirror [cm]
    float ct_BlackSpot_rad;

    //@: Radius of one mirror [cm]
    float ct_RMirror;

    //@: Camera width [cm]
    float ct_CameraWidth;

    //@: Pixel width [cm]
    float ct_PixelWidth;

    //@: Number of mirrors
    int ct_NMirrors = 0;

    //@: Number of pixels
    int ct_NPixels;

    /*!@"

      The following double-pointer is a 2-dimensional table with
      information about each mirror in the dish. The routine
      |read_ct_file()| will read this information from the file with the
      name given by the user in the E@parameters file@, in the command
      |ct_file|.  The information stored in this file (and in this table)
      depends on the type of telescope we are using.

      @"*/

    //!@{

    /*
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

    //@: Pointer to a table with the following info.:
    float **ct_data;


    /*!@"

      Next, we have two tables with data got from two files,
      |reflectivity.dat| and |axisdev.dat|, which contain information
      about the reflectivity of the mirrors, as a function of the
      wavelength @$\lambda@$, and about the (simulated) deviation of the
      mirrors' axes, with respect to the mathematical exact axes.

      @"*/

    //!@{

    // table of reflectivity for each mirror

    //@: table with datapoints (wavelength,reflec.)
    float **Reflectivity;

    //@: number of datapoints
    int    nReflectivity;

    //@: table with deviations of the mirrors' normals
    float **AxisDeviation;

    //!@}

    /*!@"

      We still define a table into where normal random numbers will be
      stored by the routine |rnormal(double *r, int n)|.

      @"*/

    //!@{

    //@: table of normal random numbers
    double NormalRandomNumbers[500];

    //!@}

    /*!@"

      This is a flag to change the E@verbosity@ of the output

      @"*/

    //!@{

    //@: flag to change the verbosity
    int verbose;

    /*!@"

      This option makes the telescope to point to a random position
      relative to the shower axis, with a maximum angular separation of
      |RANDOM_POINTING_MAX_SEPARATION| (defined in |reflector.h|.

      @"*/

    //!@{

    //@: random pointing for the CT?
    int Random_Pointing;

    //@: random pointing for the CT?
    int Random_Pointing_Isotropic;

    //@: number of times a shower is going to be processed
    int nRepeat_Random;

    //@: number of times a shower is already processed
    int nRepeated;

    //@: maximum random pointing distance
    float Random_Pointing_MaxDist; // [radians]

    //@: minimum random pointing distance
    float Random_Pointing_MinDist; // [radians]

    //!@}

};


#endif  /* REFLECTOR_H */
