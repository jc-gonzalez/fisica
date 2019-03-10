/******************************************************************************
 * File:    qdtrephdl.cpp
 *          This file is part of QLA Processing Framework
 *
 * Domain:  QPF.libQPF.qdtrephdl
 *
 * Version:  2.0
 *
 * Date:    2015/07/01
 *
 * Author:   J C Gonzalez
 *
 * Copyright (C) 2015-2018 Euclid SOC Team @ ESAC
 *_____________________________________________________________________________
 *
 * Topic: General Information
 *
 * Purpose:
 *   Implement QDTReportHandler class
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

#include "qdtrephdl.h"

#include <fstream>
#include <iostream>

////////////////////////////////////////////////////////////////////////////
// Namespace: QPF
// -----------------------
//
// Library namespace
////////////////////////////////////////////////////////////////////////////
// namespace QPF {

//----------------------------------------------------------------------
// Method: getIssues
// Retrieves from the data the list of issues found
//----------------------------------------------------------------------
bool QDTReportHandler::getIssues(std::vector<Alert*> & issues)
{
    // Loop on all the products in the report (normally, only 1)
    json::Object::iterator prodIt = data.asObject().begin();
    while (prodIt != data.asObject().end()) {

        json::Value & p = prodIt->second;
        std::string product = prodIt->second.asString();
        //std::cerr << product << '\n';

        // Loop on all the CCDs
        json::Object::iterator ccdIt = p.asObject().begin();
        while (ccdIt != p.asObject().end()) {

            json::Value & c = ccdIt->second;
            std::string ccdSet = ccdIt->second.asString();
            //std::cerr << '\t' << ccdSet << '\n';

            if (ccdSet.compare(0, 3, "CCD") == 0) {
                // Loop on all the quadrant
                json::Object::iterator quadIt = c.asObject().begin();
                json::Object::iterator quadItEnd = c.asObject().end();
                quadItEnd--;
                while (quadIt != quadItEnd) {

                    json::Value & q = quadIt->second;
                    std::string quadrant = quadIt->second.asString();
                    //std::cerr << "\t\t" << quadrant << '\n';

                    // Loop on all the diagnostics for the quadrant
                    json::Object::iterator diagIt = q.asObject()["diagnostics"].asObject().begin();
                    while (diagIt != q.asObject()["diagnostics"].asObject().end()) {

                        std::string diagnostic = diagIt->second.asString();
                        //std::cerr << "\t\t\t" << diagnostic << '\n';

                        std::string location = (product + "." + ccdSet + "." +
                                                quadrant + "." + diagnostic);

                        checkDiagnostic(diagIt, location, issues);

                        ++diagIt; // next diagnostic
                    }

                    ++quadIt; // next quadrant
                }
            }

            // Loop on all the diagnostics for the entire CCD or Detector
            json::Object::iterator diagIt = c.asObject()["diagnostics"].asObject().begin();
            while (diagIt != c.asObject()["diagnostics"].asObject().end()) {

                std::string diagnostic = diagIt->second.asString();
                //std::cerr << "\t\t\t" << diagnostic << '\n';

                std::string location = (product + "." + ccdSet + "." +
                                        diagnostic);

                checkDiagnostic(diagIt, location, issues);

                ++diagIt; // next diagnostic
            }

            ++ccdIt; // next CCD
        }

        ++prodIt; // next product
    }

    return true;
}

void QDTReportHandler::checkDiagnostic(json::Object::iterator it,
                                       std::string & location,
                                       std::vector<Alert*> & issues)
{
    Alert::Messages msgs;

    std::stringstream ss("");
    
    json::Value & d = it->second;
    std::cerr << d.asObject()["outcome"].asString();
    if (d.asObject()["result"].asObject()["outcome"].asString() == "Warning") {
        msgs.push_back("Messsages:");
        json::Object::iterator mIt;
        for (auto & v : d.asObject()["result"].asObject()["messages"].asArray()) {
            msgs.push_back(v.asString());
        }

        msgs.push_back("Values:");
	ss.str("");
	ss << d.asObject()["values"];
        msgs.push_back(ss.str());

        Alert * alert = new Alert(Alert::Diagnostics,
                                  Alert::Warning,
                                  Alert::Diagnostic,
                                  location,
                                  "",
                                  0);
        alert->setMessages(msgs);
        alert->setFile(fileName);
        issues.push_back(alert);
    }
}

// }
