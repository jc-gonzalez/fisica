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

def processCerFile(k, fb, newcorex=None, newcorey=None, sz=0):
    """
    Process a single cer* file
    :return:
    """

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
    
    #---- Read Cherenkov photons from file

    wl = 999.
    i = 0

    mnu = 99999.
    mxu = -1.
    mnv = 99999.
    mxv = -1.

    cphotonBufSize = cphotonSize * wordSize

    while True:
        cphotonData = fb.read(cphotonBufSize)

        if len(cphotonData) < cphotonBufSize:
            break

        i = i + 1
        wl, x, y, u, v, t, h = unpack('{}f'.format(cphotonSize), cphotonData)
        w = sqrt(1.0 - u ** 2 - v ** 2)
        
        if h < 1.:
            break

        #wl = wl - 101000.

        r = sqrt(x ** 2 + y ** 2)
        
        phi = atan2(v, u) * 180. / 3.1415926585
        theta = acos(w) * 180 / 3.141592
        
        print('{} {}   {:.2f}    {:.2f} {:.2f}    {:.2f}   {:.6f} {:.6f} {:.6f}    {:.4f} {:.4f}    {:.8f}   {:.2f}'\
              .format(k, i, wl, x, y, r, u, v, w, theta, phi, t, h))


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
            print("## {}".format(f))
            processCerFile(k, fb, sz=sz)

            
def main():
    """
    Run the simulation of the reflection of the atmospheric showers
    in the MAGIC telescope
    """
    processSetOfCerFiles(sys.argv[1:])


if __name__ == '__main__':
    main()

