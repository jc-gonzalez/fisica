/******************************************************************************
 * File:    main.cpp
 *          This file is part of the CDetSim project
 *
 * Domain:  CDetSim.main
 *
 * Version:  2.0
 *
 * Date:    2018/07/01
 *
 * Author:   J C Gonzalez
 *
 * Copyright (C) 2018-2019 J C Gonzalez
 *_____________________________________________________________________________
 *
 * Topic: General Information
 *
 * Purpose:
 *   Main entry for CDetSim application
 *
 * Created by:
 *   J C Gonzalez
 *
 * Status:
 *   Prototype
 *
 * Dependencies:
 *   none
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

#include <iostream>
#include <cmath>
#include <cstdlib>

#include "Simulator.h"


//----------------------------------------------------------------------
// Function: main
// Main function, entry point
//----------------------------------------------------------------------
int main(int argc, char * argv[]) 
{
    Simulator & sim = Simulator::getInstance();
    sim.run(argc, argv);

    return 0;
}
