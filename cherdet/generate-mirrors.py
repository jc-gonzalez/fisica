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

class BaseMirror():
    def __init__(self):
        self.R_det = 1.00  # m
        self.R_max = 10.00  # m
        self.phi_1 = 60.00  # deg
        self.phi_2 = 30.00  # deg
        self.H_1 = self.R_max * sin(self.phi_1 * pi / 180.)  # m
        self.H_2 = self.R_max * sin(self.phi_2 * pi / 180.)  # m
        self.R_max2 = self.R_max * self.R_max

        self.C = [0.0, 0.0, self.H_1]  # m

    def get_params(self):
        return {
            'R_det': self.R_det,
            'R_max': self.R_max,
            'phi_1': self.phi_1,
            'phi_2': self.phi_2,
            'H_1': self.H_1,
            'H_2': self.H_2,
        }

    def get_intersections2(self, S, x, y):
        O = [x, y, 0.0]
        oo = vector(S, O)
        lnorm, l = normalize(oo)

        s = Line(P=O, v=l)

        distance = distanceLinePoint(s, self.C)
        if distance >= self.R_max:
            return ([], "-1 {} {} ".format(V(O), V(l)), ())

        a = vecnorm(l)
        b = 2 * (l[0] + l[1] + l[2] * (1.0 - self.R_max))
        c = vecnorm(O) - 2 * O[2] * self.R_max
        Delta = b * b - 4.0 * a * c
        delta = sqrt(Delta)
        lambda_1 = (-b + delta) / (2.0 * a)
        lambda_2 = (-b - delta) / (2.0 * a)
        pts = []
        for lmbda in (lambda_1, lambda_2):
            Q = vecadd(O, vecmult(l, lmbda))
            if 0.0 < Q[2] < (self.H_1 + self.H_2):
                pts.append(Q)

        return (pts,
                "{} {} {} ; {}".format(len(pts), V(O), V(l), V(pts)),
                (O, l))


    def get_intersections(self, S, x, y):
        O = [x, y, 0.0]
        oo = vector(S, O)
        lnorm, l = normalize(oo)

        v = vector(self.C, O)
        w = vecdot(l, v)

        a = 1.0
        b = 2.0 * w
        c = vecnorm2(v) - self.R_max2
        Delta = b * b - 4.0 * a * c

        if Delta < 0.0:
            return ([], "-1 {} {}".format(V(O), V(l)), ())

        if Delta == 0.0:
            return ([], "!!!!!!!!!!!!!!!!!!!!!!!!!!!!", ())

        delta = sqrt(Delta)
        d1 = (- b + delta) / (2.0 * a)
        d2 = (- b - delta) / (2.0 * a)

        pts = []
        for dd in (d1, d2):
            P = vecadd(O, vecmult(l, dd))
            if 0.0 < P[2] <= (self.H_2 + self.H_1):
                pts.append(P)
                # pts.insert(0, P)

        return (pts,
                "{} {} {} ; {}".format(len(pts), V(O), V(l), V(pts)),
                (O, l))

    def get_reflection(self, l, P):
        v = vector(P, self.C)
        nnorm, n = normalize(v)
        nl = vecdot(n, l)
        r = [l[0] - 2.0 * nl * n[0],
             l[1] - 2.0 * nl * n[1],
             l[2] - 2.0 * nl * n[2]]

        return r

class Detector():
    def __init__(self):
        self.R   =  1.00  # m
        self.H_1 =  1.00  # m
        self.H_2 =  5.00  # m

    def get_intersection(self, O, l):
        a = l[0] * l[0] + l[1] * l[1]
        b = 2.0 * (O[0] * l[0] + O[1] * l[1])
        c = O[0] * O[0] + O[1] * O[1] - self.R * self.R

        Delta = b * b - 4 * a * c
        if Delta < 0.0:
            return []

        if Delta == 0.0:
            print("!!!!!!!!!!!!!!!!!!!!!!")
            return []

        delta = sqrt(Delta)
        d1 = (-b + delta) / (2.0 * a)
        d2 = (-b - delta) / (2.0 * a)

        P1 = [O[0] + d1 * l[0], O[1] + d1 * l[1], O[2] + d1 * l[2]]
        P2 = [O[0] + d2 * l[0], O[1] + d2 * l[1], O[2] + d2 * l[2]]
        dist1 = vecnorm2(vector(O,P1))
        dist2 = vecnorm2(vector(O,P2))

        return P1 if dist1 < dist2 else P2


def main():
    mirror = BaseMirror()
    detector = Detector()

    S = [80.0, 80.0, 100.0] # m
    #S = [5, 5, 100.0]

    print("N  ----- O -----  ----- l ----- ; -----pts1---- ----pts2---- " +
          "=> -----refl----- @ -----isec1----- -----isec2-----")

    k = [[120,0.4], [110, 0.3], [100, 0.2], [90, 0.2], [80, 0.3], [70, 0.4]]

    n = [0, 0, 0]
    for height in range(400, 50, -20):
        S[2] = height
        factor = 0.2 * 50 / height
        for i in range(-100, 101):
            x = i * factor
            for j in range(-100, 101):
                y = j * factor

                pts, msg, data = mirror.get_intersections(S, x, y)

                q = []
                rfts = []
                if len(pts) == 1:
                    O, l = data
                    q = mirror.get_reflection(l, pts[0])
                    rfts = detector.get_intersection(pts[0], q)

                print('{}{}{}'.format(msg,
                                      (" => " + V(q) if len(pts) > 0 else ""),
                                      (" @ " + V(rfts) if len(rfts) > 0 else "")))

                nn = len(pts)
                n[nn] = n[nn] + 1

    #print(n)

    #s = Line(P=[0, 1, 0], v=[-1, -1, 0])
    #print(distanceLinePoint(s, [0, 0, 0]))


if __name__ == '__main__':
    main()

