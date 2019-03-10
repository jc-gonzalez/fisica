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
 *   Implement ServiceMng class
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

Reflector::Reflector()
{
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
