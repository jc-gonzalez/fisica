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
#include <algorithm>

#include <dirent.h>

#include "str.h"
#include "mathtools.h"

#include "ExperimentalReflector.h"
#include "CerPhotonsSource.h"


//----------------------------------------------------------------------
// Constructor: Simulator
//----------------------------------------------------------------------
Simulator::Simulator() :
    verbLevel(VERB_INFO),
    definedFixedTarget(false),
    definedEventRange(false),
    definedSkipRange(false),
    definedMaxEvents(false),
    definedEnergyCut(false),
    definedCoreOffset(false),
    coreOffset({0., .0, 0.})
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
    json::Object && cfg = cfgSim["data"].asObject();

    if (cfg.exists("verbose_level")) {
        verbLevel = cfg["verbose_level"].asInt();
    }

    if (cfg.exists("fixed_target")) {
        fixedTargetTheta = cfg["fixed_target"][0].asFloat();
        fixedTargetPhi   = cfg["fixed_target"][1].asFloat();
        definedFixedTarget = true;
    }

    coreOffset = point3D {0., 0., 0.};
    if (cfg.exists("core_offset")) {
        coreOffset = point3D {cfg["core_offset"][0].asFloat(),
                              cfg["core_offset"][1].asFloat(),
                              0.0};
        definedCoreOffset = true;
    }

    if (cfg.exists("range_events")) {
        minEvtId = cfg["range_events"][0].asInt();
        maxEvtId = cfg["range_events"][1].asInt();
        definedEventRange = true;
    }

    if (cfg.exists("skip")) {
        minSkipEvtNum = cfg["skip"][0].asInt();
        maxSkipEvtNum = cfg["skip"][1].asInt();
        definedSkipRange = true;
    }

    if (cfg.exists("max_events")) {
        maxEvtNum = cfg["max_events"].asInt();
        definedMaxEvents = true;
    }

    if (cfg.exists("energy_cut")) {
        minEnergy = cfg["energy_cut"][0].asFloat();
        maxEnergy = cfg["energy_cut"][1].asFloat();
        definedEnergyCut = true;
    }

    std::cerr << cfg["reflector_file"].asString() << '\n'
	      << cfg["output_file"].asString() << '\n'
	      << cfg["atm_model"].asString() << '\n';

    reflectorFile = subEnvVars(cfg["reflector_file"].asString());
    outputFile    = subEnvVars(cfg["output_file"].asString());
    atmModel      = subEnvVars(cfg["atm_model"].asString());

    // Get data files (cer*) for all input paths
    for (auto s : cfg["data_paths"].asArray()) {
	std::string ss = subEnvVars(s.asString());
        DIR *dir;
        struct dirent *ent;
        if ((dir = opendir(ss.c_str())) != NULL) {
            // Get all files in the directory
            while ((ent = readdir(dir)) != NULL) {
                // Check that it means the criteria
                const char * name = ent->d_name;
                if (((name[0] == 'c') || (name[0] == 'C')) &&
                    ((name[1] == 'e') || (name[1] == 'E')) &&
                    ((name[2] == 'r') || (name[3] == 'R'))) {
                    inputFiles.push_back(ss + "/" + std::string(name));
		    std::cerr << ss << "/" << name << '\n';
                }
            }
            closedir(dir);
        } else {
            // Could not open directory
            perror("opendir");
        }
    }
    
    std::sort(inputFiles.begin(), inputFiles.end());
}

//----------------------------------------------------------------------
// Method: showConfiguration
// Display the configuration file
//----------------------------------------------------------------------
void Simulator::showConfiguration()
{
    std::cout << "==== Simulation Configuration Parameters ===="
	      << "=============================================\n";
    std::cout << "Verbosity Level ...: " << verbLevel << '\n';
    std::cout << "Fixed Target ......: ";
    if (definedFixedTarget) {
	std::cout << "(\u03b8, \u03d5) = (" << fixedTargetTheta << ", " << fixedTargetPhi << ")\n";
    } else {
	std::cout << "OFF\n";
    }
    std::cout << "Event range .......: ";
    if (definedEventRange) {
	std::cout << "from " << minEvtId << " to " << maxEvtId << '\n';
    } else {
	std::cout << "OFF\n";
    }
    std::cout << "Skip Event range ..: ";
    if (definedSkipRange) {
	std::cout << "from " << minSkipEvtNum << " to " << maxSkipEvtNum << '\n';
    } else {
	std::cout << "OFF\n";
    }
    std::cout << "Max. Events .......: ";
    if (definedMaxEvents) {
	std::cout << "anlize up to " << maxEvtNum << " events\n";
    } else {
	std::cout << "OFF\n";
    }
    std::cout << "Core offset .......: " << coreOffset << '\n';
    std::cout << "Energy range ......: ";
    if (definedEnergyCut) {
	std::cout << "[" << minEnergy << ", " << maxEnergy << "]\n";
    } else {
	std::cout << "OFF\n";
    }
    std::cerr << "Reflector file ....: " << reflectorFile << '\n'
	      << "Output file .......: " << outputFile << '\n'
	      << "Atmospheric model .: " << atmModel << '\n';

    std::cerr << "Input files .......: " << inputFiles.at(0) << '\n';
    for (int i = 1; i < inputFiles.size(); ++i) {
	std::cerr << "                     " << inputFiles.at(i) << '\n';
    }
    std::cout << "============================================="
	      << "=============================================\n";
}

//----------------------------------------------------------------------
// Method: subEnvVars
// Substitute environmental variables in the config. strings
//----------------------------------------------------------------------
std::string Simulator::subEnvVars(std::string s)
{
    char * home = getenv("HOME");
    
    return str::replaceAll(s, "$HOME", home);
}

//----------------------------------------------------------------------
// Method: run
// Execute the simulation
//----------------------------------------------------------------------
void Simulator::run()
{
    // Define reflector
    Reflector * reflector = new ExperimentalReflector;
    reflector->setMirrorsFile(reflectorFile);
    if (definedFixedTarget) {
	reflector->setOrientation(fixedTargetTheta, fixedTargetPhi);
    }
	
    // Define input data source
    CerPhotonsSource cphFiles;
    cphFiles.appendFiles(inputFiles);

    // Start loop
    CPhoton cph;
    bool isNewFile;
    
    point3D core;
    double theta, phi;
    int i;
    
    while (cphFiles.getNextCPhoton(cph, isNewFile)) {

	if (isNewFile) {
            i = cphFiles.currentFileIndex() + 1;

            // Check is this file is to be skipped
	    if (definedMaxEvents && (i > maxEvtNum)) { break; }

            if (definedEventRange && ((i < minEvtId) || (maxEvtId < i))) {
                cphFiles.endProcessingCurrentFile();
                continue;
            }

            if (definedSkipRange && ((minSkipEvtNum <= i) && (i <= maxSkipEvtNum))) {
                cphFiles.endProcessingCurrentFile();
                continue;
            }

            std::cout << i << ' ' << minEvtId << "-" << maxEvtId << ':';
	    core = cphFiles.getCore() + coreOffset;
            primaryEnergy = cphFiles.getPrimaryEnergy();
	    std::tie(theta, phi) = cphFiles.getOrientation();
	    std::cout << "% ------------ New core at " << core
                      << ", primary energy is "
                      << primaryEnergy << " GeV\n";
	    
	    reflector->setCore(coreOffset);
	    if (!definedFixedTarget) {
		reflector->setOrientation(theta, phi);
	    }
	}

	point3D xd, xc;
	if (reflector->reflect(cph, xd, xc)) {
	    std::cout << i << ' '
                      << cph.wl << ' ' << cph.x << ' ' << cph.y << ' '
		      << cph.u << ' ' << cph.v << ' ' << cph.w << ' '
		      << cph.h << ' ' << cph.t << ' '
		      << xd << ' ' << xc << '\n';
	}
    }
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
