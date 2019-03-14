/******************************************************************************
 * File:    inputdatasource.h
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

#ifndef INPUTDATASOURCE_H
#define INPUTDATASOURCE_H

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <string>
#include <vector>
#include <fstream>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------

//======================================================================
// Class: InputDataSource
//======================================================================
class InputDataSource {
public:
    InputDataSource();
    virtual ~InputDataSource();

    void appendFiles(std::vector<std::string> files);
    bool openNextFile();
    int currentFileIndex();
    virtual bool openFile(int);
    
protected:

    bool isFileOpen;
    std::ifstream ifs;
    int iFile;
    std::vector<std::string> inputFiles;
};


#endif  /* INPUTDATASOURCE_H */
