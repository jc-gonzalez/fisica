/******************************************************************************
 * File:    moments.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.moments
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

#ifndef MOMENTS_H
#define MOMENTS_H

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
// Class: Moments
//======================================================================
class Moments {
public:
    Moments() {}

    void calculate(int n, double *image, double **pix,
                   double plateScale, bool initialize = false,
                   int **pixneig = nullptr, int *npixneig = nullptr);
public:
    int npix;                              // number of pixels
    double *q;                             // charges in the pixels
    double xm, ym;                         // centroid (used in moments and lenwid)

    // moments
    double m1x, m1y;                       // first moments (mean)
    double m2xx, m2xy, m2yy;               // second moments (around origin)
    double m2cxx, m2cxy, m2cyy;            // second moments (around mean)
    double m3xxx, m3xxy, m3xyy, m3yyy;     // third moments (around origin)
    double m3cxxx, m3cxxy, m3cxyy, m3cyyy; // third moments (around mean)
    
    // charges
    double charge;                         // total charge in the image
    double xmax, ymax;                     // position of the maximum
    double smax;                           // charge of the block of maximum
    double maxs[10];                       // charges of the first 10 max.
    int nmaxs[10];                         // number of pixels of 10 max.
    
    // parameters of the image
    double length, width, dist, xdist, azw, miss, alpha, conc[9], beta;
    double phi, asymx, asymy;
};


#endif  /* MOMENTS_H */
