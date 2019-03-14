/******************************************************************************
 * File:    cerphotonssource.cpp
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.cerphotonssource
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
 *   Implement CerPhotonsSource class
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

#include "CerPhotonsSource.h"

#include <iostream>

//----------------------------------------------------------------------
// Constructor: CerPhotonsSource
//----------------------------------------------------------------------
CerPhotonsSource::CerPhotonsSource() {}

//----------------------------------------------------------------------
// Destructor: CerPhotonsSource
//----------------------------------------------------------------------
CerPhotonsSource::~CerPhotonsSource() {}

//----------------------------------------------------------------------
// Method: openFile
// Open the iFile-th file in the pool
//----------------------------------------------------------------------
bool CerPhotonsSource::openFile(int iFile)
{
    static const int EvtHeaderLength = 273; // 4-byte words

    ifs.clear();
    std::cout << "    . . . opening " << iFile << "-th file "
	      << inputFiles.at(iFile) << '\n';
    ifs.open(inputFiles.at(iFile),
	     std::ifstream::in | std::ifstream::binary);
    if (! ifs.good()) { return false; }
    isFileOpen = true;

    // Read header
    float buffer[EvtHeaderLength];
    ifs.read((char*)(buffer), EvtHeaderLength * sizeof(float));

    thetaEvt = double(buffer[10]);
    phiEvt   = double(buffer[11]);

    // We are dealing with 1 single core
    int nCore = 0;
    coreEvtX = double(buffer[98 + nCore]);
    coreEvtY = double(buffer[118 + nCore]);

    return true;
}
	
//----------------------------------------------------------------------
// Method: getCore
// Returns Cherenkov photons until the input source is exhausted
//----------------------------------------------------------------------
Point2D CerPhotonsSource::getCore()
{
    return Point2D { coreEvtX, coreEvtY };
}
	
//----------------------------------------------------------------------
// Method: getOrientation
// Returns Cherenkov photons until the input source is exhausted
//----------------------------------------------------------------------
std::tuple<double, double> CerPhotonsSource::getOrientation()
{
    return std::tuple<double, double>(thetaEvt, phiEvt);
}
	
//----------------------------------------------------------------------
// Method: getNextCPhoton
// Returns Cherenkov photons until the input source is exhausted
//----------------------------------------------------------------------
bool CerPhotonsSource::getNextCPhoton(CPhoton & cph, bool & isNewSet)
{
    isNewSet = false;
    while (!cph.read(ifs)) {
	if (!openNextFile()) { return false; }
	isNewSet = true;
    }
    return true;
}

