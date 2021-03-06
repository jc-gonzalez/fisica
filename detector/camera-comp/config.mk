##################################################################
#
# config.mk
#
# @file        config.mk
# @title       small configuration file for Makefile
# @author      J C Gonz\'alez
# @email       gonzalez@mppmu.mpg.de
# @date        Time-stamp: "Sat Feb 12 12:00:07 CET 2000"  
#
#_______________________________________________________________
#
# Created: Fri Mar 12 11:51:11 MET 1999
# Author:  Jose Carlos Gonzalez
# Purpose: Makefile for the compilation of the camera program
# Notes:   
#    
#---------------------------------------------------------------
# $RCSfile$
# $Revision$
# $Author$ 
# $Date$
##################################################################
# @maintitle

# @code

##-- program

PROGRAM = camera

##-- system

SYSTEM  = linux

##-- compilers

ifeq ($(SYSTEM),osf)
CC            = cc
CXX           = cxx
F77           = f77
else
CC            = gcc
CXX           = g++
F77           = g77
endif

# OPTIM    = -arch host -ieee -ieee_with_no_inexact
# DEBUG    = 
OPTIM    = -O0 -ieee
DEBUG    = -g3 

##-- paths & applications

DOCUMDIR      = ${HOME}/detector/sus
DOCUMAPP      = ${DOCUMDIR}/sus2

INCLUDE       = ../include
INCLUDE_COR   = ../include-COR
INCLUDE_MC    = ../include-MC
INCLUDE_REFL  = ../reflector

INCLUDE_PATHS = $(HOME)/include \
		$(INCLUDE) $(INCLUDE_COR) $(INCLUDE_MC) $(INCLUDE_REFL)

##-- library paths

USE_CERNLIB = yes
CERNDIR     = /opt/cern/2000
RANLIB_PATH = $(HOME)/lib
CTSLIB_PATH = $(HOME)/lib
CTSLIB_USE  = base hbook 

##-- next line must be uncommented for quiet compilation

.SILENT:

# @endcode
##EOF
