/******************************************************************************
 * File:    PixelsSet.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.PixelsSet
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
 *   Declare PixelsSet class
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

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------
#include "mathtools.h"
#include "angles.h"

using MathTools::sqr;
using MathTools::cube;
using MathTools::d2r;
using MathTools::sgn;
using MathTools::Deg30;

#define nint std::rint

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------
#include "PixelsSet.h"

//============================================================
// Group: Macro definitions
//============================================================

#define LINE_MAX_LENGTH     400

#define QE_FILE               "qe.dat"
#define PIXELS_FILE           "pixels.dat"
#define SINGLE_PHE_SPEC_FILE  "sphes.dat"

#define Make_Pixel(n, x, y, i, j, k)                                    \
    { pki[n] = i, pkj[n] = j, pkk[n] = k, px[n] = x, py[n] = y; }

#define Map_Pixel(i, j, n)                                              \
    { pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE] = n; }

#define Map_Multiple_Pixel(i, j, n) \
    { \
        int p = int(pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE]); \
        p = (p<0)? 0 : p;                                               \
        int p2 = (p/1000)%1000, p3 = p/1000000;                         \
        if (p3==0) { p += 1000000 * n;}                                 \
        else if (p2==0) { p += 1000 * n;}                               \
        else { p += n; }                                                \
        pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE] = p; \
    }

#define SumNumPixelInRings(r)   ((3 * r * (r+1)) + 1)
#define FirstInRing(r)          (SumNumPixelInRings(r-1))
#define NumPixelsInRing(r)      (6 * r)
#define FirstInRingSector(r, s) (SumNumPixelInRings(r-1) + (s * r))

// number of the 1st. pixel of a sector s in a ring r (central pixel: ring=0)
#define FIRST_PIXEL(r,s)   ( ((r)>0) ? (3*(r)*((r)-1) + (r)*(s) + 1) : 0 )

// number of the pixels include in a camera of r pixels
#define NUMBER_PIXELS(r)   ( ((r)>0) ? FIRST_PIXEL((r)+1,0) : 1 )

#define SIN60   0.866025403784439
#define COS60   0.500000000000000
#define COS30   SIN60
#define SIN30   COS60

#define logMsg(a, s) std::cerr << '[' << a << "] " << s << std::endl;

//!-----------------------------------------------------------
// Constructor
//------------------------------------------------------------
PixelsSet::PixelsSet(int npix, double wid, int nring1, int nring2)
    : ct_NPixels(npix),
      ct_PixelWidth(wid),
      ct_NRings_small(nring1),
      ct_NRings_big(nring2)
{
    ct_PixelWidth_corner_2_corner = ct_PixelWidth / COS60;
    ct_PixelWidth_corner_2_corner_half = ct_PixelWidth_corner_2_corner * 0.5;

    ct_Apot = ct_PixelWidth * 0.5;
    ct_2Apot = ct_PixelWidth;
}

//!-----------------------------------------------------------
// @name read_pixels
//
// @desc read pixels data
//
// @date Fri Mar 12 16:33:34 MET 1999
//------------------------------------------------------------
void PixelsSet::read_pixels(void)
{
    std::ifstream pixfile, qefile;
    char line[LINE_MAX_LENGTH];
    int n, i, j, k;
    double x, y, qe;

    //------------------------------------------------------------
    // first, pixels' coordinates

    // initialize pixel numbers
    for ( i=0; i < PIX_ARRAY_SIDE; ++i ) {
        for ( j=0; j < PIX_ARRAY_SIDE; ++j ) {
            pixels[i][j] = -1;
        }
    }

    pixary = new double* [2*ct_NPixels];
    for ( i=0; i < 2*ct_NPixels; ++i ) pixary[i] = new double[2];

    pixneig = new int* [ct_NPixels];
    for ( i=0; i < ct_NPixels; ++i ) {
        pixneig[i] = new int[6];
        for ( j=0; j < 6; ++j ) {
            pixneig[i][j] = -1;
        }
    }

    npixneig = new int[ct_NPixels];
    for ( i=0; i < ct_NPixels; ++i ) npixneig[i] = 0;

    // try to open the file
    logMsg("read_pixels", "Openning the file " << PIXELS_FILE);

    pixfile.open( PIXELS_FILE );

    // if it is wrong or does not exist, go away
    if ( ! pixfile.good() ) {

        logMsg("read_pixels", "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*");
        logMsg("read_pixels", "Cannot open " << PIXELS_FILE);
        logMsg("read_pixels", "Trying to create it, and get the data. . .");

        pixfile.close();

        create_pixels();

        logMsg("read_pixels", "Succeded.");
        logMsg("read_pixels", "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*");

    } else {

        // read file
        logMsg("read_pixels", "Reading data . . .");

        n=0;
        while ( ! pixfile.eof() ) {

            // get line from the file
            pixfile.getline(line, LINE_MAX_LENGTH);

            // skip if comment
            if ( *line == '#' )
                continue;

            // get the value (dx, dy)
            sscanf(line, "%d %d %d %d %lf %lf", &n, &i, &j, &k, &x, &y);

            if (n < 0)
                break;

            // we read the coordinates (i,j,k), but the 'k' is thrown away
            // (the three coord. are not independent, i+j+k=0)

            // WARNING!! The pixel coordinates are normalized to a
            // hypothetical camera with Pixel Diameter = 1
            // In order to use the right coordinates, we must multiply them
            // by ct_PixelWidth

            x *= ct_PixelWidth;
            y *= ct_PixelWidth;

            if (n < 1000) {

                // save the values to the array
                pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE] = n-1;
                pixary[n-1][0] = x;
                pixary[n-1][1] = y;

            } else {

                // save the values to the array
                pixels[i+PIX_ARRAY_HALF_SIDE][j+PIX_ARRAY_HALF_SIDE] = n;

            }

        }

        // close file
        pixfile.close();

    }


    // calculate tables of neighbours
    for ( n=0 ; n < ct_NPixels ; ++n ) {
#ifdef __DEBUG__
        std::cout << "Para el pixel " << n << ": ";
#endif // ! __DEBUG__
        for ( i=n+1 ; (i < ct_NPixels)&&(npixneig[n] < 6) ; ++i) {
            if ( pixels_are_neig(n,i) ) {
                pixneig[n][npixneig[n]] = i;
                pixneig[i][npixneig[i]] = n;
#ifdef __DEBUG__
                cout << i << ' ';
#endif // ! __DEBUG__
                ++npixneig[n];
                ++npixneig[i];
            }
        }
#ifdef __DEBUG__
        std::cout << std::endl << std::flush;
#endif // ! __DEBUG__
    }

#ifdef __DEBUG__
    for ( n=0 ; n < ct_NPixels ; ++n ) {
        std::cout << n << ':';
        for ( j=0; j<npixneig[n]; ++j)
            std:: cout << ' ' << pixneig[n][j];
        std::cout << std::endl << std::flush;
    }
#endif // __DEBUG__

    //------------------------------------------------------------
    // second, pixels' QE

    // try to open the file
    logMsg("read_pixels", "Openning the file " << QE_FILE);

    qefile.open( QE_FILE );

    // if it is wrong or does not exist, go away
    if ( ! qefile.good() ) {
        logMsg( "read_pixels", "Cannot open " << QE_FILE );
        exit(1);
    }

    // read file
    logMsg("read_pixels", "Reading QE data . . .");

    i=-1;

    while ( ! qefile.eof() ) {

        // get line from the file
        qefile.getline(line, LINE_MAX_LENGTH);

        // skip if comment
        if ( *line == '#' )
            continue;

        // if it is the first valid value, it is the number of QE data points
        if ( i < 0 ) {

            // get the number of datapoints
            sscanf(line, "%d", &pointsQE);

            // allocate memory for the table of QEs
            QE = new double ** [ct_NPixels];

            for ( i=0; i < ct_NPixels; ++i ) {
                QE[i] = new double * [2];
                QE[i][0] = new double[pointsQE];
                QE[i][1] = new double[pointsQE];
            }

            QElambda = new double [pointsQE];

            for ( i=0; i < pointsQE; ++i ) {
                qefile.getline(line, LINE_MAX_LENGTH);
                sscanf(line, "%lf", &QElambda[i]);
            }

            i=0;

            continue;
        }

        // get the values (num-pixel, num-datapoint, QE-value)
        sscanf(line, "%d %d %lf", &i, &j, &qe);

        if ( ((i-1) < ct_NPixels) && ((i-1) > -1) &&
             ((j-1) < pointsQE)   && ((j-1) > -1) ) {
            QE[i-1][0][j-1] = QElambda[j-1];
            QE[i-1][1][j-1] = qe;
        }

    }

    // close file
    qefile.close();

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
void PixelsSet::create_pixels(void)
{

    // look: shorcuts are defined in file camera.h

    std::ofstream pixfile;
    char line[LINE_MAX_LENGTH];

    int i, j, k;
    int ki, kj, kk, kstatic;

    double x, y, dx, dy;
    int ntot, nhalf;

    double fi, fj, fk;
    double nx, ny;

    double  d1 = ct_PixelWidth_corner_2_corner_half;
    double cd1 = d1 * COS30;
    double sd1 = d1 * SIN30;

    double  d2 = 1.2*ct_PixelWidth_corner_2_corner_half;
    double cd2 = d2 * COS30;
    double sd2 = d2 * SIN30;

    int n=0;

    double * pki = new double [ ct_NPixels ];
    double * pkj = new double [ ct_NPixels ];
    double * pkk = new double [ ct_NPixels ];
    double * px  = new double [ ct_NPixels ];
    double * py  = new double [ ct_NPixels ];

    //------------------------------------------------------------

    for (i=0; i < PIX_ARRAY_SIDE; i++)
        for (j=0; j < PIX_ARRAY_SIDE; j++)
            pixels[i][j] = -1;

    // open new file
    logMsg("create_pixels", "Creating the file " << PIXELS_FILE);

    pixfile.open( PIXELS_FILE );
    pixfile.width(8);

    pixfile << "#--------------------------------------------------\n";
    pixfile << "# pixels.dat -- pixels IDs + coordinates\n";
    pixfile << "#\n";
    pixfile << "# Kopyleft (K) 2000 J C Gonzalez\n";
    pixfile << "# Automatocally generated by camera program\n";
    pixfile << "#--------------------------------------------------\n#\n";

    // (i) generate coordinates of small, central pixels
    //------------------------------------------------------------

    pixfile << "#-- small pixels ----------------------------------\n";

    Make_Pixel(0, 0., 0., 0, 0, 0);
    Map_Pixel(0, 0, 0);

    pixfile << 1 << 0 << 0 << 0 << 0. << 0. << std::endl;

    dx = ct_PixelWidth / 2.0;
    dy = ct_PixelWidth_corner_2_corner_half * 1.5;

    n++;
    for ( int iring=1; iring < ct_NRings_small+1; iring++ ) {

        assert( n == FirstInRing(iring) );

        ntot  = NumPixelsInRing(iring);
        nhalf = ntot/2;

        ki = iring;
        kj = -ki;
        kk = 0;
        kstatic = 0;

        int * iki = new int [ ntot ];
        int * ikj = new int [ ntot ];
        int * ikk = new int [ ntot ];

        for (k=0; k < nhalf; k++) {

            if ( kstatic > iring ) ki--;

            kstatic++;

            iki[k] = ki;

            iki[k+nhalf]  = -iki[k];
        }

        for (k=1; k < ntot; k++) {
            ikj[ntot-k] = -iki[k];
        }
        ikj[0] = -iki[0];

        for (k=0; k < ntot; k++) {
            ikk[k] = -(iki[k]+ikj[k]);
        }

        for (k=0; k < ntot; k++) {

            ki = iki[k];
            kj = ikj[k];
            kk = ikk[k];

            hex2coord(ki, kj, kk, x, y);

            pixfile << n+1 << ki << kj << kk << x << y << std::endl;

            Make_Pixel( n, x, y, ki, kj, kk);
            Map_Pixel(ki, kj, n);

            n++;
        }

        delete [] iki;
        delete [] ikj;
        delete [] ikk;

    }

    // (ii) generate coordinates for gap pixels
    //------------------------------------------------------------

    pixfile << "#-- gap pixels ------------------------------------\n";

    // loop on number of gap pixels each corner
    for (i=1; i < ct_NRings_big; i++) {

        int inc = i*2;

        k = FirstInRingSector(ct_NRings_small,0);
        pki[n] = pki[k] + inc;
        pkj[n] = pkj[k] - inc;
        pkk[n] = pkk[k];

        n++;

        k = FirstInRingSector(ct_NRings_small,1);
        pki[n] = pki[k] + inc;
        pkj[n] = pkj[k];
        pkk[n] = pkk[k] - inc;

        n++;

        k = FirstInRingSector(ct_NRings_small,2);
        pki[n] = pki[k];
        pkj[n] = pkj[k] + inc;
        pkk[n] = pkk[k] - inc;

        n++;

        for (j=0; j < 3; j++) {
            pki[n] = -pki[n-3];
            pkj[n] = -pkj[n-3];
            pkk[n] = -pkk[n-3];
            n++;
        }

    }

    for (k=0; k < ct_NPixels_gap; k++) {

        n = ct_NPixels_small + k;
        ki = int( pki[n] );
        kj = int( pkj[n] );
        kk = int( pkk[n] );

        hex2coord(ki, kj, kk, x, y);

        px[n] = x;
        py[n] = y;

        pixfile << n+1 << ki << kj << kk << x << y << std::endl;

        Map_Pixel(ki, kj, n);


    }

    // (iii) generate big pixels coordinates
    //------------------------------------------------------------

    pixfile << "#-- big pixels ------------------------------------\n";

    fi = fj = fk = 0.;
    n = ct_NPixels_small+ct_NPixels_gap;

    k = FirstInRingSector(ct_NRings_small,0);

    // loop on ring of big pixels
    for (i=0; i < ct_NRings_big; i++) {

        x = px[k] + 2 * (i+1) * ct_PixelWidth;
        y = py[k] - ct_PixelWidth_corner_2_corner_half;

        for (j=0; j < (ct_NBig1+i); j++) {

            x -= ct_PixelWidth;
            y += 3. * ct_PixelWidth_corner_2_corner_half;

            xy2ijk( x, y, fi, fj, fk);
            Make_Pixel( n,                 x,  y, fi, fj, fk );

            ijk2xy(-fj,-fk,-fi, nx, ny);
            Make_Pixel( n+  (ct_NBig1+i), nx, ny,-fj,-fk,-fi );

            ijk2xy( fk, fi, fj, nx, ny);
            Make_Pixel( n+2*(ct_NBig1+i), nx, ny, fk, fi, fj );

            ijk2xy(-fi,-fj,-fk, nx, ny);
            Make_Pixel( n+3*(ct_NBig1+i), nx, ny,-fi,-fj,-fk );

            ijk2xy( fj, fk, fi, nx, ny);
            Make_Pixel( n+4*(ct_NBig1+i), nx, ny, fj, fk, fi );

            ijk2xy(-fk,-fi,-fj, nx, ny);
            Make_Pixel( n+5*(ct_NBig1+i), nx, ny,-fk,-fi,-fj );

            n++;

        }

        n+=5*(ct_NBig1+i);

    }

    k=0;

    n = ct_NPixels_small+ct_NPixels_gap;

    for (i=0; i < ct_NRings_big; i++) {

        for (j=0; j < 6*(ct_NBig1+i); j++) {

            fi = pki[n];
            fj = pkj[n];
            fk = pkk[n];
            x  = px[n];
            y  = py[n];

            pixfile << n+1 << fi << fj << fk << x << y << std::endl;

            if ((j/(ct_NBig1+i))%2 == 0) {

                // map also the three small hexagons inside the big one

                coord2hex( x    , y+d1 , ki, kj, kk);
                Map_Pixel(ki, kj, n);
                pixfile << n+1 << ki << kj << kk << x << y << std::endl;

                coord2hex( x-cd1, y-sd1, ki, kj, kk);
                Map_Pixel(ki, kj, n);
                pixfile << n+1 << ki << kj << kk << x << y << std::endl;

                coord2hex( x+cd1, y-sd1, ki, kj, kk);
                Map_Pixel(ki, kj, n);
                pixfile << n+1 << ki << kj << kk << x << y << std::endl;

                // map then the pieces of small hexagons shared between big ones

                coord2hex( x    , y-d2 , ki, kj, kk);
                Map_Multiple_Pixel(ki, kj, n);

                coord2hex( x+cd2, y+sd2, ki, kj, kk);
                Map_Multiple_Pixel(ki, kj, n);

                coord2hex( x-cd2, y+sd2, ki, kj, kk);
                Map_Multiple_Pixel(ki, kj, n);

            } else {

                // map also the three small hexagons inside the big one

                coord2hex( x    , y-d1 , ki, kj, kk);
                Map_Pixel(ki, kj, n);
                pixfile << n+1 << ki << kj << kk << x << y << std::endl;

                coord2hex( x+cd1, y+sd1, ki, kj, kk);
                Map_Pixel(ki, kj, n);
                pixfile << n+1 << ki << kj << kk << x << y << std::endl;

                coord2hex( x-cd1, y+sd1, ki, kj, kk);
                Map_Pixel(ki, kj, n);
                pixfile << n+1 << ki << kj << kk << x << y << std::endl;

                // map then the pieces of small hexagons shared between big ones

                coord2hex( x    , y+d2 , ki, kj, kk);
                Map_Multiple_Pixel(ki, kj, n);

                coord2hex( x-cd2, y-sd2, ki, kj, kk);
                Map_Multiple_Pixel(ki, kj, n);

                coord2hex( x+cd2, y-sd2, ki, kj, kk);
                Map_Multiple_Pixel(ki, kj, n);

            }

            n++;

        }

    }

    for (i=0; i < PIX_ARRAY_SIDE; i++) {
        for (j=0; j < PIX_ARRAY_SIDE; j++) {
            n = pixels[i][j];
            if (n>1000) {
                pixfile << n << i << j << -i-j << 0. << 0. << std::endl;
            } else {
                pixary[n][0] = px[n];
                pixary[n][1] = py[n];
            }
        }
    }

    pixfile << -1 << "\n# EOF\n" << std::flush;

    pixfile.close();

    delete [] pki;
    delete [] pkj;
    delete [] pkk;
    delete [] px;
    delete [] py;

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
int PixelsSet::pixel_id(double x, double y)
{
    static int ki, kj, kk;
    static int n;
    static int j;
    static int p[3];

    static double a = ct_PixelWidth_corner_2_corner;
    static double b = ct_PixelWidth;
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
bool PixelsSet::pixels_are_neig(int pix1, int pix2)
{
    return (sqrt(sqr(pixary[pix1][0] - pixary[pix2][0]) +
                 sqr(pixary[pix1][1] - pixary[pix2][1])) <= ct_PixelWidth_corner_2_corner);
}

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
int PixelsSet::pixel_bit_mask(int i, double q0, Histogram<double> **d, int b)
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
}

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
void PixelsSet::coord2hex(double cx, double cy, int &ki, int &kj, int &kk)
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
void PixelsSet::hex2coord(int ki, int kj, int kk, double &x, double &y)
{
    static const double unit = ct_PixelWidth_corner_2_corner_half;

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
void PixelsSet::ijk2xy(double ki, double kj, double kk,
                       double &x, double &y)
{
    static const double unit = ct_PixelWidth_corner_2_corner_half;

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
void PixelsSet::xy2ijk(double cx, double cy,
                       double &di, double &dj, double &dk)
{
    double x, y;
    static const double unit = 1.5 * ct_PixelWidth_corner_2_corner_half;

    x = (  cx * COS30 + cy * SIN30);
    y = (- cx * SIN30 + cy * COS30);

    di = x / unit;
    dj = (- x * SIN30 + y * COS30) / unit;
    dk = (- x * SIN30 - y * COS30) / unit;
}
