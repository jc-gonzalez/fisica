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
    
    ifs.open(inputFiles.at(iFile));
    if (! ifs.good()) { return false; }

    // Read header
    float buffer[EvtHeaderLength];
    ifs.read((char*)(buffer), EvtHeaderLength * sizeof(float));
    
    return true;
}
	
//----------------------------------------------------------------------
// Method: getNextCPhoton
// Returns Cherenkov photons until the input source is exhausted
//----------------------------------------------------------------------
bool CerPhotonsSource::getNextCPhoton(CPhoton & cph)
{
    while (!cph.read(ifs)) {
	if (!openNextFile()) { return false; }
    }
    return true;
}

