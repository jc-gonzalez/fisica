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
    static const int WordSize = sizeof(float);

    ifs.clear();
    std::cout << "    . . . opening " << iFile << "-th file "
	      << inputFiles.at(iFile) << '\n';
    ifs.open(inputFiles.at(iFile),
	     std::ifstream::in | std::ifstream::binary);
    if (! ifs.good()) { return false; }
    isFileOpen = true;

    // Read header
    char buffer[EvtHeaderLength * WordSize];
    ifs.read(buffer, EvtHeaderLength * WordSize);

    thetaEvt = double(*(float*)(buffer + 10 * WordSize));
    phiEvt   = double(*(float*)(buffer + 11 * WordSize));

    // We are dealing with 1 single core
    int nCore = 0;
    coreEvtX = double(*(float*)(buffer + (98 + nCore) * WordSize));
    coreEvtY = double(*(float*)(buffer + (118 + nCore) * WordSize));

    // Get primary energy
    primaryEnergy = double(*(float*)(buffer + 3 * WordSize));;
    
    return true;
}
	
//----------------------------------------------------------------------
// Method: getCore
// Returns Cherenkov photons until the input source is exhausted
//----------------------------------------------------------------------
point3d CerPhotonsSource::getCore()
{
    return point3d { coreEvtX, coreEvtY, 0.0 };
}
	
//----------------------------------------------------------------------
// Method: getPrimaryEnergy
// Returns primary energy
//----------------------------------------------------------------------
double CerPhotonsSource::getPrimaryEnergy()
{
    return primaryEnergy;
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

//----------------------------------------------------------------------
// Method: endProcessingCurrentFile
// Go to end of the current file
//----------------------------------------------------------------------
void CerPhotonsSource::endProcessingCurrentFile()
{
    ifs.seekg(0, std::ios::end);
}

