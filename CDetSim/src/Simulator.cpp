/******************************************************************************
 * File:    simulator.cpp
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.simulator
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
 *   Implement Simulator class
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

#include "Simulator.h"

#include <iostream>
#include <cassert>
#include <dirent.h>

//----------------------------------------------------------------------
// Constructor: Simulator
//----------------------------------------------------------------------
Simulator::Simulator() :
    verbLevel(VERB_INFO),
    definedFixedTarget(false),
    definedEventRange(false),
    definedSkipRange(false),
    definedMaxEvents(false),
    definedEnergyCut(false)
{
}

//----------------------------------------------------------------------
// Destructor: Simulator
//----------------------------------------------------------------------
Simulator::~Simulator() {}

//----------------------------------------------------------------------
// Method: readConfiguration
// Read the configuration file
//----------------------------------------------------------------------
void Simulator::readConfiguration(std::string fileName)
{

    // Read filename
    json::Parser cfgReader;
    assert(cfgReader.parseFile(fileName, cfgSim));

    // Pass config items to data members
    json::Object && cfgData = cfgSim["data"].asObject();

    if (cfgData.exists("verbose_level")) {
        verbLevel = cfgSim["verbose_level"].asInt();
    }

    if (cfgData.exists("fixed_target")) {
        fixedTargetTheta = cfgSim["fixed_target"][0].asFloat();
        fixedTargetPhi   = cfgSim["fixed_target"][1].asFloat();
        definedFixedTarget = true;
    }

    if (cfgData.exists("range_events")) {
        minEvtId = cfgSim["range_events"][0].asInt();
        maxEvtId = cfgSim["range_events"][1].asInt();
        definedEventRange = true;
    }

    if (cfgData.exists("skip")) {
        minSkipEvtNum = cfgSim["skip"][0].asInt();
        maxSkipEvtNum = cfgSim["skip"][1].asInt();
        definedSkipRange = true;
    }

    if (cfgData.exists("max_events")) {
        maxEvtNum = cfgSim["max_events"].asInt();
        definedMaxEvents = true;
    }

    if (cfgData.exists("energy_cut")) {
        minEnergy = cfgSim["energy_cut"][0].asFloat();
        maxEnergy = cfgSim["energy_cut"][1].asFloat();
        definedEnergyCut = true;
    }

    std::cerr << cfgData["reflector_file"].asString() << '\n';
    std::cerr << cfgData["output_file"].asString() << '\n';
    std::cerr << cfgData["atm_model"].asString() << '\n';

    /*
    reflectorFile = cfgData["reflector_file"].asString();
    outputFile = cfgData["output_file"].asString();
    atmModel = cfgData["atm_model"].asString();

    // Get data files (cer*) for all input paths
    for (auto s : cfgData["data_paths"].asArray()) {
        DIR *dir;
        struct dirent *ent;
        if ((dir = opendir(s.asString().c_str())) != NULL) {
            // Get all files in the directory
            while ((ent = readdir(dir)) != NULL) {
                // Check that it means the criteria
                const char * name = ent->d_name;
                if (((name[0] == 'c') || (name[0] == 'C')) &&
                    ((name[1] == 'e') || (name[1] == 'E')) &&
                    ((name[2] == 'r') || (name[3] == 'R'))) {
                    inputFiles.push_back(std::string(name));
                }
            }
            closedir(dir);
        } else {
            // Could not open directory
            perror("opendir");
        }
    }
    */
}

//----------------------------------------------------------------------
// Method: run
// Execute the simulation
//----------------------------------------------------------------------
void Simulator::run()
{

}

//----------------------------------------------------------------------
// Method: run
// Execute the simulation
//----------------------------------------------------------------------
Simulator& Simulator::getInstance()
{
    static Simulator instance;
    return instance;
}

const int Simulator::VERB_DBG = 3;
const int Simulator::VERB_INFO = 2;
const int Simulator::VERB_MINIMAL = 1;
const int Simulator::VERB_QUIET = 0;
