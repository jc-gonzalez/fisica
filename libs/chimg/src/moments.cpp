/******************************************************************************
 * File:    moments.h
 *          This file is part of the Cherenkov Detector Simulation library
 *
 * Domain:  cherdetsim.moments
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

//============================================================
// Group: External Dependencies
//============================================================

//------------------------------------------------------------
// Topic: System headers
//   none
//------------------------------------------------------------
#include <cstring>
#include <cmath>

//------------------------------------------------------------
// Topic: External packages
//   none
//------------------------------------------------------------

//------------------------------------------------------------
// Topic: Project headers
//   none
//------------------------------------------------------------
#include "moments.h"

#include "mathtools.h"

using namespace MathTools;

//!-----------------------------------------------------------
// @name moments
//
// @desc calculate moments on the image
//
// @var  n	     Number of pixels
// @var  *image      Vector of ph.e.s in pixels
// @var  **pix	     Array with information about the pixels
// @var  plateScale  Plate scale for the CT in use
// @var  flag	     1: initialize; other: normal
//
// @return	     Pointer to structure Moments_Info
//
// @date Mon Sep 14 15:22:44 MET DST 1998
//------------------------------------------------------------
void Moments::calculate(int n, double *image, double **pix,
                        double plateScale, bool initialize,
                        int **pixneig, int *npixneig)
{
    Moments & m = *this;
    
    int i, j, k;

    double x, y;
    double x2m, xym, y2m;
    double x3m, x2ym, xy2m, y3m;
    double zz, zd, zu, zv, d, a, b;
    double ax, ay, unitx, unity, sigmaax;
    double sx2, sxy, sy2;
    double sx3, sx2y, sxy2, sy3;

    //int (*compare) (const void *, const void *);

    if (initialize) {
        q = new double[n];
        for (int i = 0; i < n; ++i) { q[i] = 0.0; }
        return;
    } else {
        memcpy(q, image, sizeof(double) * n);
    }

    // save number of pixels
    npix = n;

    // calculate sum of values
    xm = ym = 0.0;
    x2m = xym = y2m = 0.0;
    x3m = x2ym = xy2m = y3m = 0.0;
    m.charge = 0.0;

    for (i = 0; i < npix; ++i) {
        if (q[i] > 0.0) {
            x = pix[i][0] * plateScale;
            y = pix[i][1] * plateScale;
            xm += x * q[i];
            ym += y * q[i];
            x2m += x * x * q[i];
            xym += x * y * q[i];
            y2m += y * y * q[i];
            x3m  += x * x * x * q[i];
            x2ym += x * x * y * q[i];
            xy2m += x * y * y * q[i];
            y3m  += y * y * y * q[i];
            m.charge += q[i];
        }
    }

    //++++++++++++++++++++++++++++++++++++++++++++++++++
    // extremes and charges
    //--------------------------------------------------

    /*
     * The following block has been replaced by a new
     * algorithm (below), which takes as maximum position
     * the charged-weighted position of the maximum and all
     * its neighbors
     */

    for (i = 0; i < 10; ++i) { m.maxs[i] = 0.0; }

    for (i = 0; i < npix; ++i) {
        if (q[i] > m.maxs[0]) {
            for (k = 9; k > 0; --k) { m.maxs[k] = m.maxs[k-1]; }
            for (k = 9; k > 0; --k) { m.nmaxs[k] = m.nmaxs[k-1]; }
            m.maxs[0] = q[i];
            m.nmaxs[0] = i;
        }
    }

    // calculates weighted position of the maximum (6 pixels)

    m.xmax = m.ymax = m.smax = 0.0;

    if (pixneig != nullptr) {
        //---- old routine ----------------------------------------
        for (i=0; i < 6; ++i) {
            m.xmax += pix[m.nmaxs[i]][0] * q[m.nmaxs[i]];
            m.ymax += pix[m.nmaxs[i]][1] * q[m.nmaxs[i]];
            m.smax += q[m.nmaxs[i]];
        }
    } else {
        //---- new routine ----------------------------------------
        for (i=0; i<npixneig[m.nmaxs[0]]; ++i) {
            j = pixneig[m.nmaxs[0]][i];
            if (j<npix) {
                m.xmax += pix[j][0] * q[j];
                m.ymax += pix[j][1] * q[j];
                m.smax += q[j];
            }
        }
    }

    if (m.smax == 0.) m.smax=1.;
    if (m.charge == 0.) m.charge=1.;

    m.xmax = m.xmax * plateScale / m.smax;
    m.ymax = m.ymax * plateScale / m.smax;

    // calculate concentrations with 2,3,4...10 pixels
    m.conc[0] = q[m.nmaxs[0]] + q[m.nmaxs[1]];

    for (i = 2; i < 10; ++i) { m.conc[i-1] = m.conc[i-2] + q[ m.nmaxs[i] ]; }
    for (i = 0; i < 9; ++i) { m.conc[i] /= m.charge; }

    //++++++++++++++++++++++++++++++++++++++++++++++++++
    // 1st moments
    //--------------------------------------------------

    xm /= m.charge;
    ym /= m.charge;

    m.m1x = xm;
    m.m1y = ym;

    //++++++++++++++++++++++++++++++++++++++++++++++++++
    // 2nd moments
    //--------------------------------------------------

    x2m /= m.charge;
    xym /= m.charge;
    y2m /= m.charge;

    // around the origin
    m.m2xx = x2m;
    m.m2xy = xym;
    m.m2yy = y2m;

    // around the mean
    sx2 = x2m - sqr<double>(xm);
    sxy = xym - xm * ym;
    sy2 = y2m - sqr<double>(ym);

    m.m2cxx = sx2;
    m.m2cxy = sxy;
    m.m2cyy = sy2;

    //++++++++++++++++++++++++++++++++++++++++++++++++++
    // 3rd moments
    //--------------------------------------------------

    x3m  /= m.charge;
    x2ym /= m.charge;
    xy2m /= m.charge;
    y3m  /= m.charge;

    // around the origin
    m.m3xxx = x3m;
    m.m3xxy = x2ym;
    m.m3xyy = xy2m;
    m.m3yyy = y3m;

    // around the mean
    sx3  = x3m  - 3 * x2m * xm + 2 * xm * xm * xm;
    sx2y = x2ym - 2 * xym * xm + 2 * xm * xm * ym - x2m * ym;
    sxy2 = xy2m - 2 * xym * ym + 2 * xm * ym * ym - y2m * xm;
    sy3  = y3m  - 3 * y2m * ym + 2 * ym * ym * ym;

    m.m3cxxx = x3m;
    m.m3cxxy = x2ym;
    m.m3cxyy = xy2m;
    m.m3cyyy = y3m;

    //++++++++++++++++++++++++++++++++++++++++++++++++++
    // hillas' parameters
    //--------------------------------------------------

    zd = sy2 - sx2;
    zz = sqrt( sqr<double>(zd) + 4. *sqr<double>( sxy ));;
    if ((zz < 1.e-6) || (sxy == 0.)) {
        m.dist = -1.;
        return;
    }
    zu = 1.0 + zd / zz;
    zv = 2.0 - zu;


    m.length = sqrt( fabs(sx2 + sy2 + zz) / 2. );
    m.width  = sqrt( fabs(sx2 + sy2 - zz) / 2. );
    m.dist   = sqrt( sqr<double>(xm) + sqr<double>(ym) );
    m.xdist  = sqrt( sqr<double>(m.xmax) + sqr<double>(m.ymax) );
    m.azw    = sqrt( fabs( sqr<double>(xm) * y2m - 2. * xm * ym * xym + x2m * sqr<double>(ym) ) ) / m.dist;
    m.miss   = sqrt( fabs( (sqr<double>(xm) * zu + sqr<double>(ym) * zv) / 2. - (2. * sxy * xm * ym / zz) ) );
    m.alpha  = d2r(asin(m.miss / m.dist));
    m.beta   = atan(zz / ( 2. * sxy ));

    //++++++++++++++++++++++++++++++++++++++++++++++++++
    // asymetry
    //--------------------------------------------------

    unitx = sqrt(0.5 * zv);
    unity = sgn<double>(sxy) * sqrt(0.5 * zu);

    if ( m.xdist > 0.0 ) {

        m.phi = acos((unitx * m.xmax + unity * m.ymax ) / m.xdist);

        sigmaax = (sx3 * cube<double>(cos(m.phi)) +
                   3.0 * sx2y * sqr<double>(cos(m.phi)) * sin(m.phi) +
                   3.0 * sxy2 * cos(m.phi) * sqr<double>(sin(m.phi)) +
                   sy3 * cube<double>(sin(m.phi)));
        sigmaax = pow(fabs(sigmaax),0.3333333) * sgn<double>(sigmaax);

        ax = sigmaax * unitx;
        ay = sigmaax * unity;
        
        m.asymx = (ax * m.xmax + ay * m.ymax) / (m.xdist * m.length * cos(m.phi));
        m.asymy = 0.0;

    } else {

        m.phi   = -1000.0;
        m.asymx = -1000.0;
        m.asymy = -1000.0;

    }

    return;
}

