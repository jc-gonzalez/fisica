/******************************************************************************
 * File:    outputdatahandler.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.outputdatahandler
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

#ifndef OUTPUTDATAHANDLER_H
#define OUTPUTDATAHANDLER_H

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

//======================================================================
// Class: OutputDataHandler
//======================================================================
class OutputDataHandler {
public:
    // Singleton getter
    static OutputDataHandler& getInstance();

    // Methods to block
    OutputDataHandler(const OutputDataHandler& arg) = delete; // Copy constructor
    OutputDataHandler(const OutputDataHandler&& arg) = delete;  // Move constructor
    OutputDataHandler& operator=(const OutputDataHandler& arg) = delete; // Assignment operator
    OutputDataHandler& operator=(const OutputDataHandler&& arg) = delete; // Move operator

private:
    OutputDataHandler();
    virtual ~OutputDataHandler();
};


#endif  /* OUTPUTDATAHANDLER_H */