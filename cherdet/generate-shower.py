#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
'''

# make print & unicode backwards compatible
from __future__ import print_function
from __future__ import unicode_literals

from tkinter import *

# used to check if functions have a parameter
from inspect import getfullargspec as getArgs

PYTHON2 = False
PY_NAME = "python3"
STRING = str

# import other useful classes
import os, sys
import time
import datetime
import logging
import argparse

from math import *

# details
__author__ = "J C Gonzalez"
__copyright__ = "Copyright 2015-2019, J C Gonzalez"
__license__ = "LGPL 3.0"
__version__ = "0.1"
__maintainer__ = "J C Gonzalez"
__email__ = "jcgonzalez@sciops.esa.int"
__status__ = "Development"
#__url__ = ""

def V(a):
    return ' '.join([V(x) if isinstance(x, list)
                     else '{:.4f}'.format(x)
                     for x in a])

def vector(P, Q):
    return vecsub(Q, P)

def vecsub(a, b):
    return [a[0] - b[0], a[1] - b[1], a[2] - b[2]]

def vecadd(a, b):
    return [a[0] + b[0], a[1] + b[1], a[2] + b[2]]

def vecdot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]

def veccross(a, b):
    return [a[1] * b[2] - a[2] * b[1],
            a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0]]

def vecnorm2(a):
    return vecdot(a, a)

def vecnorm(a):
    return sqrt(vecnorm2(a))

def vecmult(a, z):
    return list(map(lambda x: x * z, a))

def vecdiv(a, z):
    return list(map(lambda x: x / z, a))

def normalize(a):
    n = vecnorm(a)
    anorm = vecdiv(a, n)
    return (n, anorm)

def distanceLinePoint(s, Q):
    P = s.P
    v = s.v
    numer = vecnorm(veccross(vector(P, Q), v))
    denom = vecnorm(v)
    return numer / denom

class Line:
    __slots__ = ["P", "v"]
    def __init__(self, P, v):
        self.P = P
        self.v = v

def line(P, v):
    return Line(P=P, v=v)

import random

def main():

    theta_Cherenkov = 0.6 * pi / 180.0

    height1 = 30000 # m
    height2 = 5000 # m
    height_max = 12000 # m
    height_dev = 5000 # m

    circle = range(0, 360)

    for height in range(height1, height2, -20):
        z = abs(height - height_max) / height_dev
        num_samples = int(100 * exp(- z * z / 2.0))
        if num_samples < 1:
            num_samples = 1
        angles = random.sample(circle, num_samples)

        #theta = 0.601 + 0.4 * (height1 - height) / (height1 - height2)
        #theta = 0.1 + 5.0 / height #0.5 + 3.0 / height
        h = height / 1000
        theta = 30.0 / (h * h)
        theta_Cherenkov = theta * pi / 180.0
        #s_angles = "{} {} : ".format(height, num_samples)
        #for alpha in angles:
        #    s_angles = s_angles + " {}".format(alpha)
        #print(s_angles)

        for alpha in angles:
            r = height * sin(theta_Cherenkov)
            x = r * cos(alpha * pi / 180.0)
            y = r * sin(alpha * pi / 180.0)
            print("{} {} {} {}".format(height, x, y, sqrt(x*x + y*y)))



if __name__ == '__main__':
    main()

