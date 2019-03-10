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
    // Singleton getter
    static InputDataSource& getInstance();

    // Methods to block
    InputDataSource(const InputDataSource& arg) = delete; // Copy constructor
    InputDataSource(const InputDataSource&& arg) = delete;  // Move constructor
    InputDataSource& operator=(const InputDataSource& arg) = delete; // Assignment operator
    InputDataSource& operator=(const InputDataSource&& arg) = delete; // Move operator

private:
    InputDataSource();
    virtual ~InputDataSource();
};


#endif  /* INPUTDATASOURCE_H */