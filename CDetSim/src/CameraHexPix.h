/******************************************************************************
 * File:    CameraHexPix.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.CameraHexPix
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
 *   Declare CameraHexPix class
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

#ifndef CAMERAHEXPIX_H
#define CAMERAHEXPIX_H

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
#include "Camera.h"

//============================================================
// Class: CameraHexPix
//============================================================
class CameraHexPix : public Camera {
public:
    CameraHexPix(double pw, double camRad);
    virtual ~CameraHexPix() {}

public:
    virtual void read_pixels(void);
    virtual void create_pixels(void);

    virtual bool pixels_are_neig(int pix1, int pix2);

    virtual int pixel_id(double x, double y);

public:
    void setCameraRadius(double r);

    //int pixel_bit_mask(int i, double q0, Histogram<double> **d, int b);

    void coord2hex(double cx, double cy, int &ki, int &kj, int &kk);
    void hex2coord(int ki, int kj, int kk, double &x, double &y);
    void ijk2xy(double ki, double kj, double kk,
                double &x, double &y);
    void xy2ijk(double cx, double cy,
                double &di, double &dj, double &dk);

public:
    // Camera radius [cm]
    double cameraRadius;

    double pixelWidth_corner_2_corner;
    double pixelWidth_corner_2_corner_half;
    double apot;
};

#endif // CAMERAHEXPIX_H
