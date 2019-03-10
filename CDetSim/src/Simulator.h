/******************************************************************************
 * File:    simulator.h
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
 *   Declare Simulator class
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

#ifndef SIMULATOR_H
#define SIMULATOR_H

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
#include "json.h"

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------

//======================================================================
// Class: Simulator
//======================================================================
class Simulator {
public:
    // Singleton getter
    static Simulator& getInstance();

    // Methods to block
    Simulator(const Simulator& arg) = delete; // Copy constructor
    Simulator(const Simulator&& arg) = delete;  // Move constructor
    Simulator& operator=(const Simulator& arg) = delete; // Assignment operator
    Simulator& operator=(const Simulator&& arg) = delete; // Move operator

private:
    Simulator();
    virtual ~Simulator();

public:
    void readConfiguration(std::string fileName);
    void run();

private:
    json::Object cfgSim;

    int verbLevel;
    int maxEvtNum;
    double fixedTargetTheta, fixedTargetPhi;
    double minEvtId, maxEvtId;
    double minSkipEvtNum, maxSkipEvtNum;
    double minEnergy, maxEnergy;

    bool definedFixedTarget;
    bool definedEventRange;
    bool definedSkipRange;
    bool definedMaxEvents;
    bool definedEnergyCut;

    std::string reflectorFile;
    std::string outputFile;
    std::string atmModel;

    std::vector<std::string> inputFiles;

    static const int VERB_DBG, VERB_INFO, VERB_MINIMAL, VERB_QUIET;
};


#endif  /* SIMULATOR_H */