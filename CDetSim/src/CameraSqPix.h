/******************************************************************************
 * File:    CameraSqPix.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.CameraSqPix
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
 *   Declare CameraSqPix class
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

#ifndef CAMERASQPIX_H
#define CAMERASQPIX_H

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
// Class: CameraSqPix
//============================================================
class CameraSqPix : public Camera {
public:
    CameraSqPix() {}
    virtual ~CameraSqPix() {}

public:
    virtual void read_pixels(void);
    virtual void create_pixels(void);

    virtual bool pixels_are_neig(int pix1, int pix2);

    virtual int pixel_id(double x, double y);
};

#endif // CAMERASQPIX_H
