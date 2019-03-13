/******************************************************************************
 * File:    Reflector.cpp
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.reflector
 *
 * Version: 0.3
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
 *   Implement Reflector class
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

#include "Reflector.h"

#include "mathtools.h"
#include "json.h"

#define CT_I       0

#define CT_S       1
#define CT_RHO     2
#define CT_THETA   3

#define CT_FOCAL   1
#define CT_SX      2
#define CT_SY      3

#define CT_X       4
#define CT_Y       5
#define CT_Z       6
#define CT_THETAN  7
#define CT_PHIN    8
#define CT_XC      9
#define CT_YC     10
#define CT_ZC     11

#define CT_NDATA  12

Reflector::Reflector()
{
}

Reflector::~Reflector()
{
}

void Reflector::setMirrorsFile(std::string fileName)
{
    // Read filename
    json::Parser cfgReader;
    json::Object content;
    assert(cfgReader.parseFile(fileName, content));
    mirrors = content["data"].asObject();

    // Pass config items to data members
    // Focal distances [cm]
    std::vector<float> ct_Focal;

    ct_Focal_mean = mirrors["focal_distance"]["value"].asFloat();
    ct_Focal_std = mirrors["focal_std"]["value"].asFloat();

    ct_PSpread_mean = mirrors["point_spread_avg"]["value"].asFloat();
    ct_PSpread_std = mirrors["point_spread_std"]["value"].asFloat();

    ct_Adjustment_std = mirrors["adjustment_dev"]["value"].asFloat();
    ct_BlackSpot_rad = mirrors["black_spot"]["value"].asFloat();

    ct_NMirrors = mirrors["n_mirrors"]["value"].asInt();
    ct_RMirror = mirrors["r_mirror"]["value"].asFloat();

    ct_CameraWidth = mirrors["camera_width"]["value"].asFloat();
    ct_PixelWidth = mirrors["pixel_width"]["value"].asFloat();

    ct_NPixels = mirrors["n_pixels"]["value"].asInt();

    ct_data = new double * [ct_NMirrors];
    for (int i = 0; i < ct_NMirrors; ++i) {
	ct_data[i] = new double [CT_NDATA];
	for (int j = 0; j < CT_NDATA; ++j) {
	    ct_data[i][j] = mirrors["mirrors"]["value"][i][j].asFloat();
	}
    }

    // Reflectivity table
    std::string reflecFileName = mirrors["reflectivity"]["value"].asString();
    assert(cfgReader.parseFile(reflecFileName, content));
    nReflectivity = content["data"]["num_points"].asInt();
    reflectivity = new double * [nReflectivity];
    for (int i = 0; i < nReflectivity; ++i) {
	reflectivity[i] = new double [2];
	reflectivity[i][0] = content["data"]["reflectivity"][0].asFloat();
	reflectivity[i][1] = content["data"]["reflectivity"][1].asFloat();
    }

    // Table with deviations of the mirrors' normals
    std::string axisDevFileName = mirrors["axis_deviation"]["value"].asString();
    assert(cfgReader.parseFile(axisDevFileName, content));
    axisDeviation = new double * [ct_NMirrors];
    for (int i = 0; i < ct_NMirrors; ++i) {
	axisDeviation[i] = new double [2];
	axisDeviation[i][0] = content["data"]["axis_deviation"][0].asFloat();
	axisDeviation[i][1] = content["data"]["axis_deviation"][1].asFloat();
    }

}

/*
Reflector::Reflector(std::string name) : reflectorFileName(name)
{
}

void Reflector::open(std::string name, int mode)
{
    reflectorFileName = name;
    this->open(mode);
}

void Reflector::open(int mode)
{
    reflector_open_file(&fptr, reflectorFileName.c_str(), mode, &status);
}

void Reflector::readHeader()
{
    reflector_get_hdrspace(fptr, &numOfKeys, NULL, &status);
}

void Reflector::dumpHeader()
{
    char card[FLEN_CARD];

    int n = getNumOfHDUs();
    for (int hdu = 1; hdu <= n; ++hdu) {
        moveToHDU(hdu);
        printf("HDU #%d - %d of %d - %d\n", hdu, currHDU, numOfHDUs, currHDUType);
        readHeader();

        for (int k = 1; k < numOfKeys; k++)  {
            reflector_read_record(fptr, k, card, &status); // read keyword 
            printf("%s\n", card);
        }
        printf("END\n\n");  // terminate listing with END 
    }
}

void Reflector::close()
{
    reflector_close_file(fptr, &status);

    if (status) {
        reflector_report_error(stderr, status);
        exit(status);
    }
}

int Reflector::getNumOfHDUs()
{
    reflector_get_num_hdus(fptr, &numOfHDUs, &status);
    return numOfHDUs;
}

bool Reflector::moveToHDU(int n)
{
    if (n > numOfHDUs) { n = numOfHDUs; }
    if (n < 1) { n = 1; }
    reflector_movabs_hdu(fptr, n, &currHDUType, &status);
    currHDU = n;
    return true;
}

bool Reflector::moveToNextHDU()
{
    if (currHDU < numOfHDUs) { return moveToHDU(currHDU + 1); }
    else { return false; }
}

bool Reflector::moveToPrevHDU()
{
    if (currHDU > 1) { return moveToHDU(currHDU - 1); }
    else { return false; }
}

int Reflector::getHDUType()
{
    return currHDUType;
}

void Reflector::getImageSize(int & dataType, std::vector<int> & axes)
{
    int bitpix;
    reflector_get_img_type(fptr, &bitpix, &status);

    int naxis;
    reflector_get_img_dim(fptr, &naxis, &status);

    long * naxes = new long [naxis];
    reflector_get_img_size(fptr, naxis, naxes, &status);

    dataType = bitpix;
    for (int i = 0; i < naxis; ++i) { axes.push_back(naxes[i]); }

    delete [] naxes;
}

void Reflector::getImage(unsigned short * img)
{
    int bitpix;
    reflector_get_img_type(fptr, &bitpix, &status);

    int naxis;
    reflector_get_img_dim(fptr, &naxis, &status);

    long * naxes = new long [naxis];
    reflector_get_img_size(fptr, naxis, naxes, &status);

    long null_value = 0;
    int any_null;
    long fpixel[2] = {1, 1};
    long lpixel[2];
    long inc[2] = {1, 1};
    lpixel[0] = naxes[0];
    lpixel[1] = naxes[1];

    reflector_read_subset(fptr, TUSHORT, fpixel, lpixel, inc, 
                     &null_value, img, &any_null, &status);

    delete [] naxes;
}
*/
//}
