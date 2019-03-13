/******************************************************************************
 * File:    inputdatasource.cpp
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.inputdatasource
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
 *   Implement InputDataSource class
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

#include "InputDataSource.h"

//----------------------------------------------------------------------
// Constructor: InputDataSource
//----------------------------------------------------------------------
InputDataSource::InputDataSource() :
    iFile(-1), isFileOpen(false)
{
}

//----------------------------------------------------------------------
// Destructor: InputDataSource
//----------------------------------------------------------------------
InputDataSource::~InputDataSource()
{
}

//----------------------------------------------------------------------
// Method: appendFiles
// Appends a list of input files to the input files pool
//----------------------------------------------------------------------
void InputDataSource::appendFiles(std::vector<std::string> files)
{
    inputFiles.insert(inputFiles.end(),
		      files.begin(), files.end());
}

//----------------------------------------------------------------------
// Method: openFile
// Open the iFile-th file in the pool
//----------------------------------------------------------------------
bool InputDataSource::openFile(int iFile)
{
    ifs.open(inputFiles.at(iFile));

    if (! ifs.good()) { return false; }

    return true;
}

//----------------------------------------------------------------------
// Method: openNextFile
// Closes the currently opened file (if any), and opens the next one,
// if there are files remaining in the pool
//----------------------------------------------------------------------
bool InputDataSource::openNextFile()
{
    if (isFileOpen) { ifs.close(); }

    iFile++;
    if (iFile >= inputFiles.size()) { return false; }

    return openFile(iFile);
}

