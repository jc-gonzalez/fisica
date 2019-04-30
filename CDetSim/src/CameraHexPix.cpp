/******************************************************************************
 * File:    CameraHexPix.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.CameraHexPix
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
 *   Declare CameraHexPix class
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

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <fstream>
#include <iostream>
#include <cmath>
#include <algorithm>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------
#include "mathtools.h"
#include "angles.h"
#include "json.h"

using namespace MathTools;

#define nint std::rint

thread_local UnifRnd camera_unifUnit(0., 1.);
#define RandomNumber camera_unifUnit()

thread_local NormalRnd     camera_normalUnit(0., 1.);
#define RandomNormalNumber camera_normalUnit()

#define INITIAL_QE_FILE "qe-initial.json"

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------
#include "CameraHexPix.h"

//============================================================
// Group: Macro definitions
//============================================================

#define LINE_MAX_LENGTH     400

#define Push_Pixel(x, y, i, j) do {                                     \
        pixels.push_back( IJPoint({i, j}) );                            \
        pixary.push_back( XYPoint({x, y}) ); } while(0)

#define NumPixelsInRing(r) (6 * r)

#define logMsg(a, s) std::cerr << '[' << a << "] " << s << std::endl;

//!-----------------------------------------------------------
// Constructor
//------------------------------------------------------------
CameraHexPix::CameraHexPix(double pw, double camRad)
{
    pixelWidth = pw;
    pixelWidth_corner_2_corner = pixelWidth / COS60;
    pixelWidth_corner_2_corner_half = pixelWidth_corner_2_corner * 0.5;

    apot = pixelWidth * 0.5;

    setCameraRadius(camRad);
}

//!-----------------------------------------------------------
// Method: setCameraRadius
// Param:
//   r :   Radius [cm]
//------------------------------------------------------------
void CameraHexPix::setCameraRadius(double r)
{
    cameraRadius = r;
}

//!-----------------------------------------------------------
// @name read_pixels
//
// @desc read pixels data
//
// @date Fri Mar 12 16:33:34 MET 1999
//------------------------------------------------------------
void CameraHexPix::read_pixels(void)
{
    //----------------------------------------
    // Pixels file
    //----------------------------------------
    std::ifstream pixfile;

    // try to open the file
    logMsg("read_pixels", "Openning the file " << pixelsFileName);

    pixfile.open(pixelsFileName);

    // if it is wrong or does not exist, go away
    if ( ! pixfile.good() ) {

        logMsg("read_pixels", "============================================================");
        logMsg("read_pixels", "Cannot open " << pixelsFileName);
        logMsg("read_pixels", "Trying to create it, and get the data. . .");

        pixfile.close();

        create_pixels();

        logMsg("read_pixels", "Succeded.");
        logMsg("read_pixels", "============================================================");

    } else {

        // read file
        logMsg("read_pixels", "Reading pixels data . . .");

        json::Object jCamera;
        json::Parser jParser;
        jParser.parseFile(pixelsFileName, jCamera);

        json::Object && hdr  = jCamera["header"].asObject();
        json::Object && data = jCamera["data"].asObject();

        nPixels      = data["num_pixels"].asInt();
        pixelWidth   = jCamera["data"].asObject()["pixel_width"].asFloat();
        cameraRadius = data["camera_radius"].asFloat();

        logMsg("read_pixels", "nPixels:      " << nPixels);
        logMsg("read_pixels", "pixelWidth:   " << pixelWidth);
        logMsg("read_pixels", "cameraRadius: " << cameraRadius);

        json::Array && jpixij = data["pixels_ij"].asArray();
        json::Array && jpixxy = data["pixels_xy"].asArray();

        double x, y;
        int ki, kj;
        QEatWl qept;

        /*
        for (auto & p : jpix) {
            json::Array && ijCoord = p.asObject()["ij"].asArray();
            json::Array && xyCoord = p.asObject()["xy"].asArray();
            x  = xyCoord[0].asFloat();
            y  = xyCoord[1].asFloat();
            ki = ijCoord[0].asInt();
            kj = ijCoord[1].asInt();

            Push_Pixel(x, y, ki, kj);
            logMsg("read_pixels", "Pixel:  " << ki << ' ' << kj << " - " << x << ' ' << y);
            }*/

        int npix = jpixij.size();
        logMsg("read_pixels", "Retrieving pixel (" << npix << ") data . . .");

        int nqe = data["qe"].asArray()[0].asArray().size();

        for (int k = 0; k < npix; ++k) {
            json::Array && pij = jpixij[k].asArray();
            json::Array && pxy = jpixxy[k].asArray();
            x  = pxy[0].asFloat();
            y  = pxy[1].asFloat();
            ki = pij[0].asInt();
            kj = pij[1].asInt();

            Push_Pixel(x, y, ki, kj);
            //logMsg("read_pixels", "Pixel:  " << ki << ' ' << kj << " - " << x << ' ' << y);

            // Create QE table
            json::Array && jqePixelTable = data["qe"].asArray()[k].asArray();
            std::vector<QEatWl> qePixelTable;
            for (int q = 0; q < nqe; ++q) {
                json::Array && jqePixelPt = jqePixelTable[q].asArray();
                qept.wl = jqePixelPt[0].asFloat();
                qept.qe = jqePixelPt[1].asFloat();
                qePixelTable.push_back(qept);
                std::cerr << k << "  - " << q << " : " << qept.wl << ' ' << qept.qe << '\n';
            }
            qe.push_back(qePixelTable);

        }
    }

    // end
    logMsg("read_pixels", "Done.\n");

}

//!-----------------------------------------------------------
// @name create_pixels
//
// @desc create pixels data file
//
// @date Fri Mar 12 16:33:34 MET 1999
//------------------------------------------------------------
void CameraHexPix::create_pixels(void)
{
    // Generate coordinates of pixels
    //
    // The pixels are created from the center, then following rings in
    // counter-clock-wise, until no pixel has its center inside the circular
    // camera (specified by its radius)

    std::vector<int> zeroInt {0, 0};
    std::vector<double> zeroDbl {0., 0.};

    json::Array  jPixelIJ(zeroInt);
    json::Array  jPixelXY(zeroDbl);
    json::Array  jPixelsIJ;
    json::Array  jPixelsXY;

    Push_Pixel(0., 0., 0, 0);
    int n = 1;

    jPixelsIJ.append(jPixelIJ);
    jPixelsXY.append(jPixelXY);

    int i, j, k;
    int ki, kj, kk, kstatic;
    double x, y, r, camRad2;
    camRad2 = cameraRadius * cameraRadius;

    // Read initial QE table
    json::Parser parser;
    json::Object jQEfile;
    assert(parser.parseFile(INITIAL_QE_FILE, jQEfile));
    int nqe = jQEfile["data"].asObject()["num_values"].asInt();
    json::Array && jQE = jQEfile["data"].asObject()["qe"].asArray();

    json::Array jQEtable;
    QEatWl qept;

    int iring = 1;
    int numPixInRing, ntot, nhalf;
    do {
        ntot  = NumPixelsInRing(iring);
        nhalf = ntot/2;
        numPixInRing = 0;

        ki = iring;
        kj = -ki;
        kk = 0;
        kstatic = 0;

        int * iki = new int [ ntot ];
        int * ikj = new int [ ntot ];
        int * ikk = new int [ ntot ];

        for (k = 0; k < nhalf; k++) {
            if ( kstatic > iring ) ki--;
            kstatic++;
            iki[k] = ki;
            iki[k+nhalf]  = -iki[k];
        }

        for (k = 1; k < ntot; k++) { ikj[ntot-k] = -iki[k]; }

        ikj[0] = -iki[0];

        for (k = 0; k < ntot; k++) { ikk[k] = -(iki[k]+ikj[k]); }

        for (k = 0; k < ntot; k++) {
            ki = iki[k];
            kj = ikj[k];
            kk = ikk[k];

            hex2coord(ki, kj, kk, x, y);

            r = x * x + y * y;
            if (r < camRad2) {
                // Still in the camera
                Push_Pixel(x, y, ki, kj);

                // Create location and position info
                jPixelIJ[0] = ki, jPixelIJ[1] = kj;
                jPixelXY[0] = x,  jPixelXY[1] = y;
                jPixelsIJ.append(jPixelIJ);
                jPixelsXY.append(jPixelXY);

                // Create QE table
                json::Array jqePixelTable;
                std::vector<QEatWl> qePixelTable;
                for (int q = 0; q < nqe; ++q) {
                    qept.wl = jQE[q].asArray()[0].asFloat();
                    double qeMean = jQE[q].asArray()[1].asFloat();
                    double qeStd  = sqrt(qeMean * 0.1);
                    qept.qe = std::max(RandomNormalNumber * qeStd + qeMean, 0.);
                    qePixelTable.push_back(qept);

                    json::Array jqept;
                    jqept.append(qept.wl);
                    jqept.append(qept.qe);
                    jqePixelTable.append(jqept);
                }
                jQEtable.append(jqePixelTable);
                qe.push_back(qePixelTable);

                ++numPixInRing;
            }
        }

        delete [] iki;
        delete [] ikj;
        delete [] ikk;

        n += numPixInRing;
        ++iring;

    } while (numPixInRing > 0);

    // look: shorcuts are defined in file camera.h

    std::ofstream pixfile;

    // open new file
    logMsg("create_pixels", "Creating the file " << pixelsFileName);

    pixfile.open(pixelsFileName);
    //pixfile << "#------------------------------------------------------------\n"
    //        << "# " << pixelsFileName << " -- Pixel IDs + coordinates\n"
    //        << "#------------------------------------------------------------\n#\n";

    json::Object jData;
    jData.append("pixel_width", pixelWidth);
    jData.append("num_pixels", n);
    jData.append("camera_radius", cameraRadius);
    jData.append("max_ring", iring - 1);
    jData.append("pixels_ij", jPixelsIJ);
    jData.append("pixels_xy", jPixelsXY);
    jData.append("qe", jQEtable);

    json::Object jHdr;
    jHdr.append("file", std::string("Camera pixels definition file"));
    jHdr.append("version", std::string("0.1"));

    json::Object jCameraObj;
    jCameraObj.append("header", jHdr);
    jCameraObj.append("data", jData);

    json::setFloatFormat(json::FIX, 2);
    json::enableFormattedOutput("    ", 3);
    pixfile << jCameraObj << '\n' << std::flush;
    json::disableFormattedOutput();
    pixfile.close();
}

//!-----------------------------------------------------------
// @name pixel_id
//
// @desc returns the pixel number (ID) of pixel from any point inside
//
// @var x         Coord.X of point in the camera
// @var y         Coord.Y of point in the camera
// @return        pixel ID
//
// @date
//------------------------------------------------------------
int CameraHexPix::pixel_id(double x, double y)
{/*
    static int ki, kj, kk;
    static int n;
    static int j;
    static int p[3];

    static double a = pixelWidth_corner_2_corner;
    static double b = pixelWidth;
    static double c = 1. - 1./sqrt(3.);
    static double xx, yy;

    coord2hex( x, y, ki, kj, kk );

    n = -1;

    ki += PIX_ARRAY_HALF_SIDE;
    kj += PIX_ARRAY_HALF_SIDE;

    if ((ki < 0) || (ki>=PIX_ARRAY_SIDE) ||
        (kj < 0) || (kj>=PIX_ARRAY_SIDE))
        return n; // -1

    n = pixels[ki][kj];

    // we can have now 3 cases:
    // a) n==1
    //    the position (x,y) is outside any pixel
    //    the identification is direct
    // b) n in [0,max_num_pixel)
    //    the position (x,y) belongs to pixel n;
    //    the identification is direct
    // c) n > 1000
    //    the number n is in the form AAABBBCCC, where AAA, BBB
    //    and CCC are pixel IDs; we must test these three pixels
    //    the identification is more difficult
    //    this happens (under assumption of uniform light in the
    //    camera) about 10% of the times.

    // if n>1000  ==> this small hexagon belongs to more than
    //                one pixel (at least to one big pixel)
    if (n > 1000) {

        // identify the pixels where to look at
        p[2] = n % 1000;
        n /=1000;
        p[1] = n % 1000;
        n /=1000;
        p[0] = n;

        // look at each of those pixels
        n = -1;
        for (j=0; j < 3; j++) {

            if (p[j] > 0) {
                xx = x - pixary[ p[j] ][0];
                yy = y - pixary[ p[j] ][1];

                // look whether position (xx,yy) is inside a big hexagon
                if (((-b <= xx) && (xx <= 0.)
                     && ((-c * xx - a) <= yy) && (yy <= ( c * xx + a))) ||
                    ((0. <  xx) && (xx <= b )
                     && (( c * xx - a) <= yy) && (yy <= (-c * xx + a))) ) {
                    n = p[j];
                    break;
                }

            }

        }

    }

    return n;  // if n==-1  ==> outside any pixel
 */
    return 0;
}

//!-----------------------------------------------------------
// @name pixels_are_neig
//
// @desc check whether two pixels are neighbours
//
// @var pix1      Number of the first pixel
// @var pix2      Number of the second pixel
// @return        TRUE: both pixels are neighbours; FALSE: oth.
//
// @date Wed Sep  9 17:58:37 MET DST 1998
//------------------------------------------------------------
bool CameraHexPix::pixels_are_neig(int pix1, int pix2)
{
    return (sqrt(sqr<double>(pixary[pix1].x - pixary[pix2].x) +
                 sqr<double>(pixary[pix1].y - pixary[pix2].y)) <= pixelWidth_corner_2_corner);
}
/*
//!-----------------------------------------------------------
// @name pixel_bit_mask
//
// @desc calculates the bit mask for a given pixel
//
// @var i     Number of the pixel
// @var q0    Threshold in num.ph.e-s per pixel
// @var d     Pointer to the array of discriminator histograms
// @var b     Bin in time where we look for coincidences
//
// @return        Bit mask in the form of an integer
//
// @date Wed Jan 19 14:06:52 MET 2000
//------------------------------------------------------------
int CameraHexPix::pixel_bit_mask(int i, double q0, Histogram<double> **d, int b)
{
    static int triggerBits;
    static int bit;

    int j;

#define DISCRIMINATOR(x,y) (d[x])->get_content(y)

    triggerBits = (DISCRIMINATOR(i,b) > 0.5) ? 1 : 0;

    for ( j=0 ; j < npixneig[i] && pixneig[i][j]>-1; ++j ) {

        if ( DISCRIMINATOR(pixneig[i][j],b) > 0.5 ) {

            if ( pixary[pixneig[i][j]][0] > pixary[i][0] ) {

                if ( nint(pixary[pixneig[i][j]][1]*10.0) >
                     nint(pixary[i][1]*10.0) )
                    bit = 2;
                else if ( nint(pixary[pixneig[i][j]][1]*10.0) <
                          nint(pixary[i][1]*10.0) )
                    bit = 6;
                else
                    bit = 1;

            } else {

                if ( nint(pixary[pixneig[i][j]][1]*10.0) >
                     nint(pixary[i][1]*10.0) )
                    bit = 3;
                else if ( nint(pixary[pixneig[i][j]][1]*10.0) <
                          nint(pixary[i][1]*10.0) )
                    bit = 5;
                else
                    bit = 4;

            }

            triggerBits |= (1 << bit);

        }

    }

    return triggerBits;
} */

//!-----------------------------------------------------------
// @name coord2hex
//
// @desc returns the coordinates (i,j,k) of pixel for point (cx,cy)
//
// @var cx        Coord.X of point in the camera
// @var cy        Coord.Y of point in the camera
// @var ki        Reference to the x' coordinate i
// @var kj        Reference to the y' coordinate j
// @var kk        Reference to the z' coordinate k
//
// @date
//------------------------------------------------------------
void CameraHexPix::coord2hex(double cx, double cy, int &ki, int &kj, int &kk)
{
    double dx, dy, dz;
    double rx, ry, rz;
    double ax, ay, az;
    int   s;

    xy2ijk (cx, cy, dx, dy, dz);

    rx = std::round(dx);
    ry = std::round(dy);
    rz = std::round(dz);

    ki = int(rx);
    kj = int(ry);
    kk = int(rz);

    s = ki + kj + kk;
    if (s) {
        ax = fabs(rx-dx);
        ay = fabs(ry-dy);
        az = fabs(rz-dz);

        if ((ax >= ay) && (ax >= az)) { ki -= s; }
        else if ((ay >= ax) && (ay >= az)) { kj -= s; }
        else { kk -= s; }
    }
}

//!-----------------------------------------------------------
// @name hex2coord
//
// @desc returns the coordinates (x,y) of the center of a pixel (i,j,k)
//
// @var ki        Coord.X' of pixel in the camera
// @var kj        Coord.Y' of pixel in the camera
// @var kk        Coord.Z' of pixel in the camera
// @var cx        Reference to the x coordinate
// @var cy        Reference to the y coordinate
//
// @date
//------------------------------------------------------------
void CameraHexPix::hex2coord(int ki, int kj, int kk, double &x, double &y)
{
    static const double unit = pixelWidth_corner_2_corner_half;

    x = (ki * COS30 - kj * COS30) * unit;
    y = ((ki + kj) * SIN30 - kk) * unit;
}

//!-----------------------------------------------------------
// @name ijk2xy
//
// @desc returns the coordinates (x,y) corresponding to any (i,j,k)
//
// @var ki        Coord.X' of pixel in the camera
// @var kj        Coord.Y' of pixel in the camera
// @var kk        Coord.Z' of pixel in the camera
// @var cx        Reference to the x coordinate
// @var cy        Reference to the y coordinate
//
// @date
//------------------------------------------------------------
void CameraHexPix::ijk2xy(double ki, double kj, double kk,
                       double &x, double &y)
{
    static const double unit = pixelWidth_corner_2_corner_half;

    x = (ki * COS30 - kj * COS30) * unit;
    y = ((ki + kj) * SIN30 - kk) * unit;
}

//!-----------------------------------------------------------
// @name xy2ijk
//
// @desc returns the coordinates (x,y) corresponding to any (x,y)
//
// @var cx        Coord.X of point in the camera
// @var cy        Coord.Y of point in the camera
// @var di        Reference to the x' coordinate i
// @var dj        Reference to the y' coordinate j
// @var dk        Reference to the z' coordinate k
//
// @date
//------------------------------------------------------------
void CameraHexPix::xy2ijk(double cx, double cy,
                       double &di, double &dj, double &dk)
{
    double x, y;
    static const double unit = 1.5 * pixelWidth_corner_2_corner_half;

    x = (  cx * COS30 + cy * SIN30);
    y = (- cx * SIN30 + cy * COS30);

    di = x / unit;
    dj = (- x * SIN30 + y * COS30) / unit;
    dk = (- x * SIN30 - y * COS30) / unit;
}
