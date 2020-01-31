/******************************************************************************
 * File:    PixelsSet.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.PixelsSet
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
 *   Declare PixelsSet class
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

#ifndef PIXELSSET_H
#define PIXELSSET_H

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
#include "histogram.h"
using MathTools::Histogram;

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------

#define PIX_ARRAY_SIDE       80
#define PIX_ARRAY_HALF_SIDE  40


//============================================================
// Class: PixelsSet
//============================================================
class PixelsSet {
public:
    PixelsSet(int npix, double wid, int nring1, int nring2=0);

public:
    void read_pixels(void);
    void create_pixels(void);

    bool pixels_are_neig(int pix1, int pix2);
    int pixel_bit_mask(int i, double q0, Histogram<double> **d, int b);

    void hex2coord (int ki, int kj, int kk, double &x, double &y);
    void coord2hex (double cx, double cy, int &ki, int &kj, int &kk);

    void ijk2xy (double di, double dj, double dk,
                 double &x, double &y);
    void xy2ijk (double cx, double cy,
                 double &di, double &dj, double &dk);

    int pixel_id (double x, double y);

private:
    //@: table for IJ(K) system
    int pixels[PIX_ARRAY_SIDE][PIX_ARRAY_SIDE];

    //@: coordinates x,y for each pixel
    double **pixary;

    //@: indexes of pixels neighbours of a given one
    int **pixneig;

    //@: number of neighbours a pixel have
    int *npixneig;

    //@: contents of the pixels (ph.e.)
    double **fnpix;

    //@: contents of the pixels (ph.e.) after cleanning
    double *fnpixclean;

    // The following double-pointer is a 2-dimensional table with the
    // Quantum Efficiency @$QE@$ of each pixel in the camera, as a function
    // of the wavelength @$\lambda@$. The routine |read_pixels()| will read
    // also this information from the file |qe.dat|.

    // Pointer to a table with QE, number of datapoints, and wavelengths

    //@: table of QE
    double ***QE;

    //@: number of datapoints for the QE curve
    int pointsQE;

    //@: table of QE
    double *QElambda;

    //@: Pixel width [cm]
    double ct_PixelWidth;

    //@: ct_PixelWidth_corner_2_corner = ct_PixelWidth / cos(60)
    double ct_PixelWidth_corner_2_corner;

    //@: ct_PixelWidth_corner_2_corner / 2
    double ct_PixelWidth_corner_2_corner_half;

    //@: Number of rings of small pixels
    int ct_NRings_small;

    //@: Number of rings of big pixels
    int ct_NRings_big;

    //@: Number of small pixels
    int ct_NPixels_small;

    //@: Number of gap pixels
    int ct_NPixels_gap;

    //@: Number of big pixels
    int ct_NPixels_big;

    //@: Number (total) of pixels
    int ct_NPixels;

    //@: Number of big pixels in the first sector of first ring
    int ct_NBig1;

    //@: ct_Apot = ct_PixelWidth / 2
    double ct_Apot;

    //@: ct_2Apot = 2 * ct_Apot = ct_PixelWidth
    double ct_2Apot;
};

#endif // PIXELSSET_H
