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

SuperBlockSize = 22932

basicBlockNumWords = 273
cphotonBlockNumWords = 7
wordBytes = 4

basicBlockBytes = basicBlockNumWords * wordBytes

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

def removeBlks(s, blockId, szBytes):
    '''
    Remove block of szBytes that starts with the block identified blockId
    :param s:
    :param blockId:
    :param szBytes:
    :return:
    '''
    found = False
    npos = s.find(blockId, 0)
    while npos > -1:
        found = True
        if len(s) - npos < szBytes:
            print('ERROR: Only {} bytes left instead of {}'.format(len(s) - npos, szBytes))
        s = s[:npos] + s[npos + szBytes:]
        npos = s.find(blockId, npos)

    return found, s

def processCerFile(k, fb, newcorex=None, newcorey=None, sz=0):
    """
    Process a single cer* file
    :return:
    """
    nblk = 0
    bytesWritten = 0

    ncer = 1

    cerfile = 'cer{:06d}'.format(ncer)
    fc = open(cerfile, 'wb')
    print('New file # {}: {}'.format(ncer, cerfile))

    while bytesWritten < sz:

        nblk = nblk + 1
        #- Read block size
        bytesInBlock = unpack('1I', fb.read(1 * wordBytes))[0]

        #- Read block
        #nwords = bytesInBlock / wordSize
        #buff = unpack('{}f'.format(nwords), fb.read(bytesInBlock))
        buff = fb.read(bytesInBlock)

        #- Read again the bytes in the block
        bytesInBlock_again = unpack('1I', fb.read(1 * wordBytes))[0]

        #- Check
        if bytesInBlock != bytesInBlock_again:
            print('ERROR: Bytes in block #{} mismatch ( {} != {} )'.format(nblk, bytesInBlock, bytesInBlock_again))

        #- Look for EVTH or EVTE, and remove them
        found, buff = removeBlks(buff, b'RUNH', basicBlockBytes)
        found, buff = removeBlks(buff, b'EVTE', basicBlockBytes)
        found, buff = removeBlks(buff, b'RUNE', basicBlockBytes)
        #bytesInBlock = len(buff)

        #- In case RUNH is found, start a new file
        npos = buff.find(b'EVTH')
        if npos > 4:
            fc.write(buff[:npos])
            print('Block #{} has {} bytes'.format(nblk, npos))

            buff = buff[npos:]
            fc.close()

            ncer = ncer + 1
            cerfile = 'cer{:06d}'.format(ncer)
            fc = open(cerfile, 'wb')
            print('New file # {}: {}'.format(ncer, cerfile))

        #- Write to file
        fc.write(buff)
        print('Block #{} has {} bytes'.format(nblk, bytesInBlock))

        bytesWritten = bytesWritten + (wordBytes + bytesInBlock + wordBytes)

    fc.close()
    print('{} bytes in {} blocks written to {}'.format(bytesWritten, nblk, cerfile))


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

