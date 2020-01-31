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
    
EvtH = { 'words': 273 }

evthSize = EvtH['words']
cphotonSize = 7
wordSize = 4


def rnd():
    return np.random.random_sample()


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

def processCerFile(k, fb, newcorex=None, newcorey=None):
    """
    Process a single cer* file
    :return:
    """

    #---- Read Event Header
    evth = unpack('{}f'.format(evthSize), fb.read(evthSize * wordSize))
    #print(evth)

    thetaEvtH, phiEvtH = evth[10:12]    
    coreX, coreY, coreD = get_core(evth)

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

    print('Shower #{}: Core at ({}, {}) = {}, Orientation: (theta:{}, phi:{}):\n{}\n'
          .format(k, coreX, coreY, coreD, thetaEvtH, phiEvtH, '-'*80))
    
    while wl > 0.5:
        cphotonData = fb.read(cphotonSize * wordSize)
        
        i = i + 1
        wl, x, y, u, v, t, h = unpack('{}f'.format(cphotonSize), cphotonData)
        w = sqrt(1.0 - u ** 2 - v ** 2)
        
        if wl < 1.:
            continue

        wl = wl - 101000.

        print('{} {}   {:.2f}    {:.2f} {:.2f}    {:.6f} {:.6f} {:.6f}    {:.8f} {:.2f}'
              .format(k, i, wl, x, y, u, v, w, t, h))

def processSetOfCerFiles(files):
    """
    Process a list of cer* files throught the reflector simulator
    :return:
    """
    results = {}
    
    k = 0
    for f in files:
        k = k + 1
        with open(f, 'rb') as fb:
            processCerFile(k, fb)

    # k = 0
    # for f in files:
    #     k = k + 10
    #     for j in range(4,5):
    #         w = k + j
    #         with open(f, 'rb') as fb:
    #             result = processCerFile(k=w, fb=fb, newcorex=j*2500.)
    #             results[str(w)] = {"file": f, "data": result}
    # 
    # with open("results.json", "w") as outjson:
    #     json.dump(results, outjson)


def main():
    """
    Run the simulation of the reflection of the atmospheric showers
    in the MAGIC telescope
    """

    files = [
        '/home/jcgonzalez/JC1/fisica.git/detector/run/5/in/cer000001'
    ]

    processSetOfCerFiles(files)
    
    
                
if __name__ == '__main__':
    main()
    
