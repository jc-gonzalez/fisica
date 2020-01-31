/******************************************************************************
 * File:    Camera.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.Camera
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
 *   Declare Camera class
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

#ifndef CAMERA_H
#define CAMERA_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <vector>
#include <string>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------

#define SIN60   0.866025403784439
#define COS60   0.500000000000000
#define COS30   SIN60
#define SIN30   COS60

#define QE_FILE               "qe.dat"
#define PIXELS_FILE           "pixels.dat"

//============================================================
// Class: Camera
//============================================================
class Camera {
public:
    Camera()
        : pixelWidth(0.),
          nPixels(0) {}
    virtual ~Camera() {}

public:
    virtual void read_pixels(void) = 0;
    virtual void create_pixels(void) = 0;

    virtual bool pixels_are_neig(int pix1, int pix2) = 0;

    virtual int pixel_id (double x, double y) = 0;

    virtual void setPixelsFileName(std::string s = std::string(PIXELS_FILE)) {
        pixelsFileName = s;
    }

    virtual void setQEFileName(std::string s = std::string(QE_FILE)) {
        qeFileName = s;
    }

    struct IJPoint {
        int i;
        int j;
    };

    struct XYPoint {
        double x;
        double y;
    };

    struct QEatWl {
        double wl;
        double qe;
    };

protected:
    //@: table for IJ(K) system
    std::vector<IJPoint> pixels;

    //@: coordinates x,y for each pixel
    std::vector<XYPoint> pixary;

    //@: indexes of pixels neighbours of a given one
    std::vector< std::vector<int> > pixneig;

    //@: contents of the pixels (ph.e.)
    std::vector< std::vector<double> > fnpix;

    //@: contents of the pixels (ph.e.) after cleanning
    std::vector<double> fnpixclean;

    //@: tables of QE for all pixels
    std::vector< std::vector<QEatWl> > qe;

    //@: Pixel width [cm] - 2 . aphotem for hex. pixels
    double pixelWidth;

    //@: Number (total) of pixels
    int nPixels;

    // Pixels file
    std::string pixelsFileName;

    // QE File
    std::string qeFileName;
};

#endif // CAMERA_H
