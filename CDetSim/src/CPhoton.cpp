/******************************************************************************
 * File:    cphoton.cpp
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
 *   Implement CPhoton class
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

#include "CPhoton.h"

#include <cmath>

//----------------------------------------------------------------------
// Constructor: CPhoton
//----------------------------------------------------------------------
CPhoton::CPhoton() {}

//----------------------------------------------------------------------
// Destructor: CPhoton
//----------------------------------------------------------------------
CPhoton::~CPhoton() {}

//----------------------------------------------------------------------
// Method: read
// Reads the Cherenlov photon data from file
//----------------------------------------------------------------------
bool CPhoton::read(std::ifstream & ifs)
{
    float buffer[7];
    ifs.read((char *)(buffer), 7 * sizeof( float ) );   // 
    if (ifs.gcount() > 0) {
	w = buffer[0];
	x = buffer[1];
	y = buffer[2];
	u = buffer[3];
	v = buffer[4];
	h = buffer[5];
	t = buffer[6];
	if (w < 1.) { return false; }
	wl = getWavelength();
	return true;
    } else {
	return false;
    }
}
	
//----------------------------------------------------------------------
// Method: getWavelength
// Returns Cherenkov photons until the input source is exhausted
//----------------------------------------------------------------------
double CPhoton::getWavelength()
{ 
    return ( (w > 1.0) ? w - 1000.*((int)floor(w/1000.)) : 0.0 ); 
}

