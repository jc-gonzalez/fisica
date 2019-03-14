/******************************************************************************
 * File:    cerphotonssource.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.cerphotonssource
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

#ifndef CERPHOTONSSOURCE_H
#define CERPHOTONSSOURCE_H

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
#include "InputDataSource.h"
#include "CPhoton.h"

//======================================================================
// Class: CerPhotonsSource
//======================================================================
class CerPhotonsSource : public InputDataSource {
public:
    CerPhotonsSource();
    virtual ~CerPhotonsSource();

    virtual bool openFile(int iFile);
    bool getNextCPhoton(CPhoton & cph, bool & isNewSet);
    Point2D getCore();
    std::tuple<double, double> getOrientation();
    //void getCore(double & x, double & y);
    //void getOrientation(double & theta, double & phi);

private:
    double thetaEvt, phiEvt;
    double coreEvtX, coreEvtY;
};


#endif  /* CERPHOTONSSOURCE_H */
