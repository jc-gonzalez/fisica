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
// Define options for arguments parser
//
// The fields are:
//  optletter, optname, type, helpstr, [NO_ARG|REQ_ARG|OPT_ARG]
//
// Each option will have the corresponding variable in the
// structure Args, of the type specified.  The argument, if
// present/required, is set ot the type of the variable. In case
// of bool, several options are checked. See the code.
//----------------------------------------------------------------------
#define ARGS_CMDNAME "cdetsim"
#define ARGS_DESC "Program to run the CT simulation"
#define ARGS_OPTIONS_LIST                                              \
    T('c', config_file, string, "Configuration file", REQ_ARG)         \
    T('x', corex, double, "X postion of core in ground", OPT_ARG)      \
    T('y', corey, double, "X postion of core in ground", OPT_ARG)
//----------------------------------------------------------------------
#include "parse_args.h"

//----------------------------------------------------------------------
// Function: main
// Main function, entry point
//----------------------------------------------------------------------
int main(int argc, char * argv[]) 
{
    // Parse command line
    int exitCode = EXIT_SUCCESS;

    Args a;
    if (! parseArgs(argc, argv, a)) { return EXIT_FAILURE; }

    // Create simulator, and launch
    Simulator & sim = Simulator::getInstance();
    sim.run(a.config_file);

    return 0;
}
