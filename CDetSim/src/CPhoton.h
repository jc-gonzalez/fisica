/******************************************************************************
 * File:    cphoton.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.cphoton
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

#ifndef CPHOTON_H
#define CPHOTON_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <fstream>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------
#include "InputDataSource.h"

//======================================================================
// Class: CPhoton
//======================================================================
class CPhoton : public InputDataSource {
public:
    CPhoton();
    virtual ~CPhoton();

public:
    bool read(std::ifstream & ifs);
    double getWavelength();

public:
    double w, wl, x, y, u, v, h, t;
};


#endif  /* CPHOTON_H */
