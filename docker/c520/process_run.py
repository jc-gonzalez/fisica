#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
"""

VERSION = '0.1.3'

__author__ = "jcgonzalez" # Refactoring from ESDC Euclid Team code
__version__ = VERSION
__email__ = "josec.glez@gmail.com"
__status__ = "Prototype" # Prototype | Development | Production

from struct import *
from math import cos, sin, acos, asin, atan2, sqrt, fabs, pi

import os, sys
import json
import pprint
import re
import pickle

import numpy as np
from numpy import array, dot
from numpy.linalg import norm

from matplotlib.image import NonUniformImage
import matplotlib.pyplot as plt

D2R = 180. / pi

EvtH = { 'words': 273 }

evthSize = EvtH['words']
cphotonSize = 7
wordSize = 4


def rnd():
    return np.random.random_sample()

def get_file_size(file):
    st = os.stat(file)
    return st.st_size

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

def get_primary(evth):
    return evth[2]

def get_energy(evth):
    return evth[3]

def get_height_first(evth):
    return evth[6]

def get_direction(evth):
    thetaEvtH, phiEvtH = evth[10:12]
    return thetaEvtH * D2R, phiEvtH * D2R

def printHeader():
    print('{:4s} {:3s} {:9s} {:6s} {:8s} {:7s} {:7s} {:8s} {:5s} {:5s}'
          .format('#Shw', 'Pri', 'Size', 'Energy', 'Height',
                  'CoreX', 'CoreY', 'CoreD', 'Theta', 'Phi'))

def store1(arr, x, b, mx):
    if x > mx or x < 0.: return        
    bx = int(x // b)
    a = arr[bx]
    arr[bx] = a + 1

def store2(arr, x, y, b, o):
    bx = int(x // b) + o
    by = int(y // b) + o
    oo = 2 * o
    if (bx > -1) and (bx < oo):
        if (by > -1) and (by < oo):
            a = arr[bx][by]
            arr[bx][by] = a + 1

def processCerFile(k, fb, newcorex=None, newcorey=None, sz=0):
    """
    Process a single cer* file
    :return:
    """
    try:
        with open('cer_data.pickle', 'rb') as fp:
            # The protocol version used is detected automatically, so we do not
            # have to specify it.
            cer_data = pickle.load(fp)
    except:
        cer_data = {
            'shower_info': [],
            'xhist1': np.zeros((1000,1000)),
            'xhist2': np.zeros((500,500)),
            'xhist5': np.zeros((200,200)),
            'xhist10': np.zeros((100,100)),
            'xhist20': np.zeros((50,50)),
            'xhist50': np.zeros((20,20)),
            'rhist1': np.zeros((500,)),
            'rhist2': np.zeros((250,)),
            'rhist5': np.zeros((100,)),
            'rhist10': np.zeros((50,)),
            'ahist1': np.zeros((90,360)),
            'ahist2': np.zeros((45,180)),
            'ahist5': np.zeros((18,72)),
            'ahist10': np.zeros((9,36))
        }

    #---- Read Event Header
    evth = unpack('{}f'.format(evthSize), fb.read(evthSize * wordSize))
    #print(evth)

    primary = get_primary(evth)
    energy = get_energy(evth)
    height = get_height_first(evth)
    thetaEvtH, phiEvtH = get_direction(evth)
    coreX, coreY, coreD = get_core(evth)

    print('{:4d} {:3d} {:9d} {:6.1f} {:8.1f} {:7.1f} {:7.1f} {:8.1f} {:5.1f} {:5.1f}'
          .format(k, int(primary), sz, energy, height,
                  coreX, coreY, coreD, thetaEvtH, phiEvtH))

    cer_data['shower_info'].append([k, int(primary), sz, energy,
                                    height, coreX, coreY, coreD,
                                    thetaEvtH, phiEvtH])
    
    #---- Read Cherenkov photons from file

    wl = 999.
    i = 0

    mnu = 99999.
    mxu = -1.
    mnv = 99999.
    mxv = -1.


    while wl > 0.5:
        cphotonData = fb.read(cphotonSize * wordSize)
        
        i = i + 1
        wl, x, y, u, v, t, h = unpack('{}f'.format(cphotonSize), cphotonData)
        w = sqrt(1.0 - u ** 2 - v ** 2)
        
        mnu = min(mnu, u)
        mxu = max(mxu, u)
        
        mnv = min(mnv, v)
        mxv = max(mxv, v)

        if wl < 1.:
            continue

        wl = wl - 101000.

        #print('{} {}   {:.2f}    {:.2f} {:.2f}    {:.6f} {:.6f} {:.6f}    {:.8f} {:.2f}'
        #      .format(k, i, wl, x, y, u, v, w, t, h))

        r = sqrt(x ** 2 + y ** 2)
        
        store2(cer_data['xhist1'],  x, y,  100., 500)
        store2(cer_data['xhist2'],  x, y,  200., 250)
        store2(cer_data['xhist5'],  x, y,  500., 100)
        store2(cer_data['xhist10'], x, y, 1000.,  50)
        store2(cer_data['xhist20'], x, y, 2000.,  25)
        store2(cer_data['xhist50'], x, y, 5000.,  10)

        store1(cer_data['rhist1'],  r,  100., 50000.)
        store1(cer_data['rhist2'],  r,  200., 50000.)
        store1(cer_data['rhist5'],  r,  500., 50000.)
        store1(cer_data['rhist10'], r, 1000., 50000.)

        phi = atan2(v, u) * 180. / 3.1415926585
        theta = acos(w) * 180 / 3.141592
        
        store2(cer_data['ahist1'],  theta, phi,  1., 0)
        store2(cer_data['ahist2'],  theta, phi,  2., 0)
        store2(cer_data['ahist5'],  theta, phi,  5., 0)
        store2(cer_data['ahist10'], theta, phi, 10., 0)

    with open('cer_data.pickle', 'wb') as fp:
        pickle.dump(cer_data, fp, protocol=pickle.HIGHEST_PROTOCOL)

    #pprint.pprint(cer_data)
    #print('u: [{}, {}],  v: [{}, {}]'.format(mnu, mxu, mnv, mxv))

def processSetOfCerFiles(files):
    """
    Process a list of cer* files throught the reflector simulator
    :return:
    """
    printHeader()
    
    k = 0
    for f in files:
        k = k + 1
        sz = get_file_size(f)
        with open(f, 'rb') as fb:
            processCerFile(k, fb, sz=sz)

def processLogFile(logFile):
    """
    Process the log file
    :return:
    """
    try:
        with open('log_data.pickle', 'rb') as fp:
            # The protocol version used is detected automatically, so we do not
            # have to specify it.
            log_data = pickle.load(fp)
    except:
        log_data = {
            'first_int': [],
            'cer_phot_elec': [],
            'cer_phot_had': [],
            'long_fit': [],
            'long_dist': {
                'depth': np.zeros((794,)),
                'gammas': np.zeros((794,)),
                'positrons': np.zeros((794,)),
                'electrons': np.zeros((794,)),
                'mu_plus': np.zeros((794,)),
                'mu_minus': np.zeros((794,)),
                'hadrons': np.zeros((794,)),
                'charged': np.zeros((794,)),
                'nuclei': np.zeros((794,))
            }
        }

    print('Processing {} . . . '.format(logFile))
    with open(logFile) as fl:

        for line in fl:

            if 'FIRST INTERACTION AT' in line:
                value = re.split(' +', line.strip())[3]
                log_data['first_int'].append(value)

            if 'CERENKOV PH. FROM ELECTRONS =' in line:
                values = re.split(' +', line.strip())
                log_data['cer_phot_elec'].append(values[5])
                log_data['cer_phot_had'].append(values[11])

            if 'FIT OF THE CURVE' in line:
                line = fl.readline()
                params = re.split(' +', fl.readline().strip())[2:]
                chi2 = re.split(' +', fl.readline().strip())[2]
                avdev = re.split(' +', fl.readline().strip())[5]
                long_fit = {'params': params, 'chi2': chi2, 'avdev': avdev}
                log_data['long_fit'].append(long_fit)

            if '-- LONGITUDINAL DISTRIBUTION IN STEPS' in line:
                depth     = np.zeros(794)
                gammas    = log_data['long_dist']['gammas']
                positrons = log_data['long_dist']['positrons']
                electrons = log_data['long_dist']['electrons']
                mu_plus   = log_data['long_dist']['mu_plus']
                mu_minus  = log_data['long_dist']['mu_minus']
                hadrons   = log_data['long_dist']['hadrons']
                charged   = log_data['long_dist']['charged']
                nuclei    = log_data['long_dist']['nuclei']

                line = fl.readline()
                for dpth in range(0, 794):
                    line = fl.readline()
                    #print(line)
                    values = re.split(' +', line.strip())
                    depth[dpth]     = float(values[0])
                    gammas[dpth]    = gammas[dpth] + float(values[1])
                    positrons[dpth] = positrons[dpth] + float(values[2])
                    electrons[dpth] = electrons[dpth] + float(values[3])
                    mu_plus[dpth]   = mu_plus[dpth] + float(values[4])
                    mu_minus[dpth]  = mu_minus[dpth] + float(values[5])
                    hadrons[dpth]   = hadrons[dpth] + float(values[6])
                    charged[dpth]   = charged[dpth] + float(values[7])
                    nuclei[dpth]    = nuclei[dpth] + float(values[8])

                log_data['long_dist']['depth']     = depth
                log_data['long_dist']['gammas']    = gammas
                log_data['long_dist']['positrons'] = positrons
                log_data['long_dist']['electrons'] = electrons
                log_data['long_dist']['mu_plus']   = mu_plus
                log_data['long_dist']['mu_minus']  = mu_minus
                log_data['long_dist']['hadrons']   = hadrons
                log_data['long_dist']['charged']   = charged
                log_data['long_dist']['nuclei']    = nuclei

    with open('log_data.pickle', 'wb') as fp:
        pickle.dump(log_data, fp, protocol=pickle.HIGHEST_PROTOCOL)

    #pprint.pprint(log_data)


def main():
    """
    Run the simulation of the reflection of the atmospheric showers
    in the MAGIC telescope
    """
    logFile = sys.argv[1]
    processLogFile(logFile)
    processSetOfCerFiles(sys.argv[2:])


if __name__ == '__main__':
    main()

