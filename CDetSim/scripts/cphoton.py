#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""EAS_Query class from package eas

This module incorporates almost without modification the code provided
by the ESDC Euclid team to store some content in a folder/file in the
user VOSpace account.

Usage:
    The sequence of commands to perform a query would be
     1. Create the VOSpace_Push object
     2. Call the ``save_to_file`` method to store something in a
        VOSpace folder/file

    Please, have a look at the file ``query_and_save_to_vospace.py'' script for
    an example.  This example can be executed with::

        $ python query_and_save_to_vospace.py

"""

VERSION = '0.1.3'

__author__ = "jcgonzalez" # Refactoring from ESDC Euclid Team code
__version__ = VERSION
__email__ = "josec.glez@gmail.com"
__status__ = "Prototype" # Prototype | Development | Production

from struct import *
from math import cos, sin, acos, asin, atan2, sqrt, fabs, pi

import json
import pprint

import numpy as np
from numpy import array, dot
from numpy.linalg import norm

from matplotlib.image import NonUniformImage
import matplotlib.pyplot as plt

# Speed of Light in vacuum, in m/s
Speed_of_Light_vacuum = 299792458.0  # EXACT!!
Speed_of_Light_air = Speed_of_Light_vacuum / 1.000293

# Speed of Light in vacuum, in cm/ns
Speed_of_Light_vacuum_cmns = Speed_of_Light_vacuum / 1.0e7
Speed_of_Light_air_cmns = Speed_of_Light_air / 1.0e7

# MAGIC Telescope - Mirror definition fields
#     i  f   sx   sy   x   y   z   thetan  phin 
#
#      i : number of the mirror
#      f : focal distance of that mirror
#     sx : curvilinear coordinate of mirror's center in X[cm]
#     sy : curvilinear coordinate of mirror's center in X[cm]
#      x : x coordinate of the center of the mirror [cm]
#      y : y coordinate of the center of the mirror [cm]
#      z : z coordinate of the center of the mirror [cm]
# thetan : polar theta angle of the direction where the mirror points to
#   phin : polar phi angle of the direction where the mirror points to
#     xn : xn coordinate of the normal vector in the center (normalized)
#     yn : yn coordinate of the normal vector in the center (normalized)
#     zn : zn coordinate of the normal vector in the center (normalized)

CT_I      =  0
CT_S      =  1
CT_RHO    =  2
CT_THETA  =  3
CT_FOCAL  =  1
CT_SX     =  2
CT_SY     =  3           
CT_X      =  4
CT_Y      =  5
CT_Z      =  6
CT_THETAN =  7
CT_PHIN   =  8
CT_XC     =  9
CT_YC     = 10
CT_ZC     = 11

CT_NDATA  = 12

# Pre-defined filenames / values

ReflectivityFile  = './reflectivity.json'
FocalsFile        = 'focals.dat'
AxisDeviationFile = './axis_deviation.json'
MAGICDefFile      = './magic.def.json'

# Read MAGIC definition file
try:
    with open(MAGICDefFile, "r") as magicF:
        MAGICDef = json.load(magicF)
except:
    print('Could not open MAGIC Telescope definition file {}'.format(MAGICDefFile))
    
# Read reflectivity table
try:
    with open(ReflectivityFile, "r") as fh:
        reflectivityData = json.load(fh)
except:
    print('Could not open reflectivity table {}'.format(ReflectivityFile))
    
# Read mirrors axis deviation table
try:
    with open(AxisDeviationFile, "r") as fh:
        axisDevData = json.load(fh)
except:
    print('Could not open axis deviation table {}'.format(AxisDeviationFile))
    
EvtH = { 'words': 273 }

evthSize = EvtH['words']
cphotonSize = 7
wordSize = 4

Epsilon = 1.0e-6

UseFixedTarget = False
FixedTargetTheta, FixedTargetPhi = (0., 0.)

MainFocal = 1700.0

reflectivity = reflectivityData["data"]["reflectivity"]

mirrorData = MAGICDef["data"]["mirrors"]["value"]
ct_Radius = MAGICDef["data"]["diameter"]["value"] / 2.
ct_RMirror = MAGICDef["data"]["r_mirror"]["value"]
ct_BlackSpot = MAGICDef["data"]["black_spot"]["value"]
ct_PSpread_avg = MAGICDef["data"]["point_spread_avg"]["value"]
ct_PSpread_std = MAGICDef["data"]["point_spread_std"]["value"]
ct_CameraWidth = MAGICDef["data"]["camera_width"]["value"]

ct_BlackSpot2 = ct_BlackSpot * ct_BlackSpot
ct_CameraEdges2 = (ct_CameraWidth / 2.) ** 2

axisDev = axisDevData["data"]["axis_deviation"]

omegaCT     = np.zeros((3,3))
omegaCT_inv = np.zeros((3,3))


def rnd():
    return np.random.random_sample()

def lin2curv(x):
    xx = x / 100.
    return ((xx + 0.000144175317185 * xx * xx * xx) * 100.)

def get_new_ct_pointing(theta, phi, minang, maxang, isotropic=True):
    """
    For the moment, we only simulate an uniform distribution,
    since our theta distribution in the generation of events is
    already uniform for hadrons, which are the main targets for
    using this option
    
    :param theta: 
    :param phi: 
    :param minang: 
    :param maxang: 
    :param isotropic: 
    :return: 
    """
    
    range = maxang - minang;
    
    if isotropic:
        # TODO: hey, still I have to modify this to do it from angle minang to maxang!!
        it = acos(cos(range) + rnd() * (1 - cos(range)))
    else:
        it = rnd() * range + minang
    
    ip = rnd() * 2.0 * pi;
    
    if theta < Epsilon:
        newtheta = it;
        newphi = ip;
    else:
        sin_theta = sin(theta)
        cos_theta = cos(theta)
        
        cos_newtheta = cos_theta * cos(it) + sin_theta * sin(it) * cos(ip)
        newtheta = acos(cos_newtheta)
        sin_newtheta = sin(newtheta)
        
        sin_iphi = (sin(it) * sin(ip)) / sin_newtheta
        cos_iphi = ((cos(it) - cos_newtheta * cos_theta) /
                    (sin_newtheta * sin_theta))
    
        iphi = atan2(sin_iphi, cos_iphi)
    
        newphi = phi + iphi

    return (newtheta, newphi, it)

def get_fixed_target():
    """
    """
    return FixedTargetTheta, FixedTargetPhi

def get_core(evth, ncore=0):
    """
    According to the documentation, the n-th core position is at 
    words (x, y) => (98 + n, 118 + n), with 1st core being n=1, and
    number of words starting at 1.  So, for 1st core this means
    1-based words (99, 119), that is 0-based (98, 118).  
    So, if we count the number of cores from 0, ncore=0:.., then:
    """
    x, y = (evth[98 + ncore], evth[118 + ncore])
    return (x, y, sqrt((x * x) + (y * y)))

def omega(theta, phi):
    """
    """
    # shortcuts for cosine and sine of theta and phi
    ct, st = (cos(theta), sin(theta))
    cp, sp = (cos(phi), sin(phi))
    
    # save values in the array (see top of file)
    o = array(((cp*ct, sp*ct, -st),
               (-sp, cp, 0.0),
               (cp*st, sp*st, ct)))
    return o #np.transpose(o)

def omega_inv(theta, phi):
    """
    """
    # shortcuts for cosine and sine of theta and phi
    ct, st = (cos(theta), sin(theta))
    cp, sp = (cos(phi), sin(phi))
    
    # save values in the array (see top of file)
    o_inv = array(((cp*ct, -sp, cp*st),
                   (sp*ct, cp, sp*st),
                   (-st, 0, ct)))
    return o_inv #np.transpose(o_inv)

def applyMxV(M, V):
    return np.matmul(M, V)

def sqr(x):
    return x ** 2

def normalize(n):
    return n / norm(n)

def lagrange(t, x):
    """
    Formula for Lagrange interpolation of 3rd. order
    x: value to be interpolated
    t: table(2xN), table[0]: abscissas, table[1]: ordinates
    n: higher value of abscissas, such that t[0][n] <= x
    """
    n = 0
    while (t[n + 1][0] < x): n = n + 1
    
    return ((t[n][1] * ((x - t[n+1][0]) * (x - t[n+2][0])) / 
             ((t[n][0] - t[n+1][0]) * (t[n][0] - t[n+2][0])))  +  
            (t[n+1][1] * ((x - t[n][0]) * (x - t[n+2][0])) /   
             ((t[n+1][0] - t[n][0]) * (t[n+1][0] - t[n+2][0])))  +  
            (t[n+2][1] * ((x - t[n][0]) * (x - t[n+1][0])) /  
             ((t[n+2][0] - t[n][0]) * (t[n+2][0] - t[n+1][0]))))

def passed_transmittance(wl, h, w):
    """
    Test: Atmopsphere transmittance (currently, just 90% for all wl)
    """
    return rnd() < 0.9  # atm(wl, h, acos(w))

def passed_reflectivity(wl):
    """
    Test: Mirror reflectivity
    """
    return rnd() < lagrange(reflectivity, wl)

def apply_axis_deviation(x, i):
    """
    AXIS DEVIATION
    We introduce it here just as a first order 
    correction, by modifying the position of the reflected photon.        
    """
    return x + array((axisDev[i][0], axisDev[i][1], 0.))

def apply_semaring(x):
    """
    SMEARING
    We apply the point spread function for the mirrors
    """
    # get two N(0;1) random numbers to modify the Cphoton position in the camera
    rndSmear = np.random.normal(loc=ct_PSpread_avg, scale=ct_PSpread_std, size=2)
    return x + array((rndSmear[0], rndSmear[1], 0.))

def get_intersection_with_dish(vx, vxCT, vrCT):
    """
    Calculate intersection with big dish
    """
    # Before moving to the system of the mirror, for MAGIC, 
    # first we look whether the photon hits a mirror or not
    # 
    # calculate the intersection of the trayectory of the photon 
    # with the GLOBAL DISH !!!
    # we reproduce the calculation of the coefficients of the
    # second order polynomial in z (=vxCT[2]), made with 
    # Mathematica 
    # 
    # 
    # In[1]:= parab:=z-(x^2+y^2)/(4F)
    #        par1=parab /. {x->x0+u/w(z-z0),y->y0+v/w(z-z0)}
    # 
    # Out[1]=
    #                  u (z - z0) 2         v (z - z0) 2
    #            (x0 + ----------)  + (y0 + ----------)
    #                      w                    w
    #        z - ---------------------------------------
    #                              4 F
    # 
    # In[2]:= CoefficientList[ExpandAll[par1*4F*w^2],z]
    # 
    # Out[2]=
    #            2   2     2   2
    #        {-(w  x0 ) - w  y0  + 2 u w x0 z0 + 
    #         
    #                         2   2    2   2
    #          2 v w y0 z0 - u  z0  - v  z0 , 
    #         
    #              2                            2
    #         4 F w  - 2 u w x0 - 2 v w y0 + 2 u  z0 + 
    #         
    #             2       2    2
    #          2 v  z0, -u  - v }
                
    a = - sqr(vrCT[0]) - sqr(vrCT[1])
    b = (4. * MainFocal * sqr(vrCT[2]) 
         - 2. * vrCT[0] * vrCT[2] * vxCT[0]
         - 2. * vrCT[1] * vrCT[2] * vxCT[1] 
         + 2. * sqr(vrCT[0]) * vxCT[2]
         + 2. * sqr(vrCT[1]) * vxCT[2])
    c = (2. * vrCT[0] * vrCT[2] * vx[0] * vx[2] 
         + 2. * vrCT[1] * vrCT[2] * vx[1] * vx[2] 
         - sqr(vrCT[2]) * sqr(vx[0]) - sqr(vrCT[2]) * sqr(vx[1])
         - sqr(vrCT[0]) * sqr(vx[2]) - sqr(vrCT[1]) * sqr(vx[2]))

    # the z coordinate is calculated
    if fabs(a) < Epsilon:
        # Only one value
        zDish = -c / b
    else:
        delta = b * b - 4. * a * c
        if delta < 0.:
            return None
        d = sqrt(delta)
        t1 = (-b + d) / (2. * a)
        t2 = (-b - d) / (2. * a)
        zDish = t1 if t1 < t2 else t2

    xDish = vxCT[0] + (zDish - vxCT[2]) * vrCT[0] / vrCT[2]
    yDish = vxCT[1] + (zDish - vxCT[2]) * vrCT[1] / vrCT[2]

    print('@ {} {} {} {} {}'.format(vx[0], vx[1], xDish, yDish, zDish))
    return array((xDish, yDish, zDish))

def get_intersection_with_mirror(i, vxm, vrm):
    """
    Calculate the intersection of the trayectory of the photon 
    with the mirrors
    """
    
    # we reproduce the calculation of the coefficients of the
    # second order polynomial in z (=xm[2]), made with 
    # Mathematica

    # 
    # In[1]:= esfera:=x^2+y^2+(z-R)^2-R^2;
    #         recta:={x->x0+u/w(z-z0),y->y0+v/w(z-z0)}
    # 
    # In[2]:= esfera
    # 
    #            2    2    2           2
    # Out[2]= -R  + x  + y  + (-R + z)
    # 
    # In[3]:= recta
    # 
    #                     u (z - z0)            v (z - z0)
    # Out[3]= {x -> x0 + ----------, y -> y0 + ----------}
    #                         w                     w
    # 
    # In[4]:= esf=esfera /. recta
    # 
    #           2           2         u (z - z0) 2         v (z - z0) 2
    # Out[4]= -R  + (-R + z)  + (x0 + ----------)  + (y0 + ----------)
    #                                      w                    w
    # 
    # In[5]:= coefs=CoefficientList[ExpandAll[esf],z]
    # 
    #                                               2   2    2   2
    #            2     2   2 u x0 z0   2 v y0 z0   u  z0    v  z0
    # Out[5]= {x0  + y0  - --------- - --------- + ------ + ------, 
    #                           w           w          2        2
    #                                                 w        w
    #  
    #                                  2         2          2    2
    #             2 u x0   2 v y0   2 u  z0   2 v  z0      u    v
    # >    -2 R + ------ + ------ - ------- - -------, 1 + -- + --}
    #               w        w         2         2          2    2
    #                                 w         w          w    w
    # In[6]:= Simplify[ExpandAll[coefs*w^2]]
    # 
    #           2    2     2                             2    2    2
    # Out[6]= {w  (x0  + y0 ) - 2 w (u x0 + v y0) z0 + (u  + v ) z0 ,
    #  
    #             2             2                            2    2    2
    # >    -2 (R w  - u w x0 + u  z0 + v (-(w y0) + v z0)), u  + v  + w }
    #
    
    # the z coordinate is calculated, using the coefficients
    # shown above

    a = sqr(vrm[0]) + sqr(vrm[1]) + sqr(vrm[2])
    b = -2 * (2. * mirrorData[i][CT_FOCAL] * sqr(vrm[2]) 
              - vrm[0] * vrm[2] * vxm[0] 
              + sqr(vrm[0]) * vxm[2] 
              + vrm[1] * (-(vrm[2] * vxm[1]) + vrm[1] * vxm[2]))
    c = (sqr(vrm[2]) * (sqr(vxm[0]) + sqr(vxm[1])) 
         - 2 * vrm[2] * (vrm[0] * vxm[0] + vrm[1] * vxm[1]) * vxm[2] + 
         (sqr(vrm[0]) + sqr(vrm[1])) * sqr(vxm[2]))
    
    d = sqrt( b * b - 4.0 * a * c )

    # two possible values for z
    t1 = (-b + d) / (2.0 * a)
    t2 = (-b - d) / (2.0 * a)

    # z must be the minimum of t1 and t2
    zCut = t1 if (t1 < t2) else t2
    xCut = vxm[0] + (zCut - vxm[2]) * vrm[0] / vrm[2]
    yCut = vxm[1] + (zCut - vxm[2]) * vrm[1] / vrm[2]

    return array((xCut, yCut, zCut))

def mirrors_reflection(photonloc, orient, timefirstint):
    """
    Reflection in the mirrors
    """
    global omegaCT, omegaCT_inv
    global mirrorData, ct_RMirror

    result = [False, None]
    
    r_ = array(orient)
    x_ = array(photonloc)

    xCT_ = applyMxV(omegaCT, x_)
    rCT_ = applyMxV(omegaCT, r_)

    #-- Calculate intersection with big dish
    xDish_ = get_intersection_with_dish(vx=x_, vxCT=xCT_, vrCT=rCT_)
    if xDish_ is None:
        return result
    
    #-- Convert to curvilinear coordinates to see if outside dish
    sx, sy = (lin2curv(xDish_[0]), lin2curv(xDish_[1]))
    if (fabs(sx) > 850.) or (fabs(sy) > 850.):
        return result

    #-- Search for the mirror element to use
    distMirr = 1000000.
    i_mirror = 0
    i = 0
    for m in mirrorData:
        distMirr2 = norm(array((m[CT_X] - xDish_[0],
                                m[CT_Y] - xDish_[1],
                                m[CT_Z] - xDish_[2])))
        if distMirr2 < distMirr:
            distMirr = distMirr2
            i_mirror = i
        if distMirr < ct_RMirror:
            break
        i = i + 1

    if (fabs(mirrorData[i_mirror][CT_SX] - sx) > ct_RMirror) or \
       (fabs(mirrorData[i_mirror][CT_SY] - sy) > ct_RMirror):
       return result

    # Compute matrices for the mirror
    thetaMirr = mirrorData[i_mirror][CT_THETAN]
    phiMirr   = mirrorData[i_mirror][CT_PHIN]
    
    omegaMirr     = omega(-thetaMirr, phiMirr)
    omegaMirr_inv = omega_inv(-thetaMirr, phiMirr)

    # First translation ...
    xMirror_ = array((mirrorData[i_mirror][CT_X],
                      mirrorData[i_mirror][CT_Y],
                      mirrorData[i_mirror][CT_Z]))
    xmm_ = xCT_ - xMirror_

    # ... then rotation
    xm_ = applyMxV(omegaMirr, xmm_)
    rm_ = applyMxV(omegaMirr, rCT_)

    # rm should be normalized, as rCT_ was normalized, but still...
    rm_ = normalize(rm_)

    # calculate the intersection of the trayectory of the photon 
    # with the mirror
    xCut_ = get_intersection_with_mirror(i=i_mirror, vxm=xm_, vrm=rm_)

    # BLACK SPOTS: If the photon hits the black spot, it's lost
    if sqr(xCut_[0]) + sqr(xCut_[1]) < ct_BlackSpot2:
        return result

    # if still we have the photon, continue with the reflexion
          
    # calculate normal vector in this point 
    # (and normalize, with the sign changed)
    rnor_ = array((2.0 * xCut_[0],
                   2.0 * xCut_[1],
                   2.0 * (xCut_[2] - 2.0 * mirrorData[i_mirror][CT_FOCAL])))
    rnor_ = - normalize(rnor_)
    
    # now, both "normal" vector and original trayectory are
    # normalized
    # just project the original vector in the normal, and 
    # take it as the "mean" position of the original and
    # the "reflected" vector
    # from this, we can calculate the "reflected" vector
    # calpha = cos(angle(rnor_,rm))

    calpha = fabs(dot(rnor_, rm_))
    
    # finally!!! we have the reflected trayectory of the photon
    rRefl_ = 2.0 * calpha * rnor_ - rm_
    rRefl_ = normalize(rRefl_)

    # Let's go back to the coordinate system of the CT

    # first rotation...
    xCutCT_  = applyMxV(omegaMirr_inv, xCut_);
    rReflCT_ = applyMxV(omegaMirr_inv, rRefl_);

    # ...then translation
    xReflCT_ = xCutCT_ + xMirror_
    
    # calculate intersection of this trayectory and the camera plane
    # in the system of the CT, this plane is z = ct_Focal
    
    s = (MainFocal - xReflCT_[2]) / rReflCT_[2];
    
    xCam_ = xReflCT_ + rReflCT_ * s
    
    #-- AXIS DEVIATION
    #xCam_ = apply_axis_deviation(xCam_, i_mirror)

    #-- SMEARING
    #xCam_ = apply_semaring(xCam_)

    #-- CAMERA LIMITS
    #if sqr(xCam_[0]) + sqr(xCam_[1]) > ct_CameraEdges2:
    #    return result

    #-- ANGLE OF INCIDENCE
    # calculate angle of incidence between tray. and camera plane
    # the camera plane is
    # 0 y + 0 y + z - ct_Focal = 0 => (A,B,C,D) = (0,0,1,-ct_Focal)
    # from Table 3.20 "Tasch. der Math."
    phi = asin(rReflCT_[2])

    #-- TIMING
    #t = adjust_time(t=timefirstint)
    # substract light-path from the mirror till the ground, 'cos 
    # the photon actually hit the mirror!!

    factor = -1.0 if (xm_[2] > 0.) else +1.0
    t = timefirstint + factor * norm(xm_ - xCut_) / Speed_of_Light_air_cmns
                 
    # and then add path from the mirror till the camera
    t = t + norm(xReflCT_ - xCam_) / Speed_of_Light_air_cmns

    return [True, {"x_dish": xDish_, "x_cam": xCam_, "t": t}] 

def processCerFile(k, fb, newcorex=None, newcorey=None):
    """
    Process a single cer* file
    :return:
    """
    global omegaCT, omegaCT_inv

    #---- Read Event Header
    evth = unpack('{}f'.format(evthSize), fb.read(evthSize * wordSize))
    #print(evth)

    thetaEvtH, phiEvtH = evth[10:12]
    
    thetaCT, phiCT = get_fixed_target() if UseFixedTarget else (thetaEvtH, phiEvtH)
    coreX, coreY, coreD = get_core(evth)
    #print(coreX, coreY, coreD)
    if newcorex:
        coreX = newcorex
    if newcorey:
        coreY = newcorey
    
    #---- Build rotation matrices
    
    omegaCT = omega(thetaCT, phiCT)
    omegaCT_inv = omega_inv(thetaCT, phiCT)

    #---- Read Cherenkov photons from file

    wl = 999.
    i = 0

    jwl = []
    jDx = []
    jDy = []
    jDz = []
    jCx = []
    jCy = []
    jCz = []
    jt = []

    print('% ', coreX, coreY)
    
    while wl > 0.5:
        cphotonData = fb.read(cphotonSize * wordSize)
        
        i = i + 1
        wl, x, y, u, v, t, h = unpack('{}f'.format(cphotonSize), cphotonData)
        w = sqrt(1.0 - u ** 2 - v ** 2)
        
        #continue
        
        if wl < 1.:
            continue

        wl = wl - 101000.

        #-- Transmittance check
        #if not passed_transmittance(wl, h, w): continue

        #-- Reflectivity check
        #if not passed_reflectivity(wl): continue

        #-- Reflection in the mirrors
        photonloc = (x - coreX, y - coreY, 0.0)
        orient = (u, v, w)
        
        result = mirrors_reflection(photonloc=photonloc,
                                    orient=orient,
                                    timefirstint=t)
        
        if not result[0]:
            continue

        photonDish = result[1]["x_dish"]
        photonCam = result[1]["x_cam"]
        delta_t = result[1]["t"]

        jwl.append(wl)
        jDx.append(photonDish[0])
        jDy.append(photonDish[1])
        jDz.append(photonDish[2])
        jCx.append(photonCam[0])
        jCy.append(photonCam[1])
        jCz.append(photonCam[2])
        jt.append(delta_t)
        
        #print('{} {}   {}    {} {} {}   {} {} {}    {}'
        #      .format(k, i, wl,
        #              photonDish[0], photonDish[1], photonDish[2],
        #              photonCam[0], photonCam[1], photonCam[2],
        #              delta_t))

    return {"core": [coreX, coreY], "wl": jwl,
            "dish": {"x": jDx, "y": jDy, "z": jDz},
            "cam": {"x": jCx, "y": jCy, "z": jCz},
            "deltat": jt}

def processSetOfCerFiles(files):
    """
    Process a list of cer* files throught the reflector simulator
    :return:
    """
    results = {}
    
    k = 0
    for f in files:
        k = k + 10
        for j in range(4,5):
            w = k + j
            with open(f, 'rb') as fb:
                result = processCerFile(k=w, fb=fb, newcorex=j*2500.)
                results[str(w)] = {"file": f, "data": result}

    with open("results.json", "w") as outjson:
        json.dump(results, outjson)


def main():
    """
    Run the simulation of the reflection of the atmospheric showers
    in the MAGIC telescope
    """

    files = [
        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000001'
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000002',
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000003',
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000004',
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000005',
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000006',
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000007',
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000008',
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000009',
#        '/home/jcgonzalez/JC1/fisica.git/detector/run/4/in/cer000010'
    ]

    processSetOfCerFiles(files)
    
    
                
if __name__ == '__main__':
    main()
    
