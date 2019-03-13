/******************************************************************************
 * File:    physconst.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.physconst
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

#ifndef PHYSCONST_H
#define PHYSCONST_H

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
// Namespace: Physconst
//======================================================================
namespace Phys {

    // Speed of Light in vacuum, in m/s
    const double Speed_of_Light_vacuum = 299792458.0;  // EXACT!!
    const double Speed_of_Light_air = Speed_of_Light_vacuum / 1.000293;
    
    // Speed of Light in vacuum, in cm/ns
    const double Speed_of_Light_vacuum_cmns = Speed_of_Light_vacuum / 1.0e7;
    const double Speed_of_Light_air_cmns = Speed_of_Light_air / 1.0e7;
    
}

#endif  /* PHYSCONST_H */
