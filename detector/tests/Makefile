##################################################################
#
# Makefile
#
# Author:  Jose Carlos Gonzalez
# Purpose: Makefile for the compilation of the image_errors
# Created: Time-stamp: <Tue Feb 22 08:19:22 CET 2000>
# Notes:   
#
##################################################################

.SILENT:

#----------------------------------------
# This Makefile _needs_ GNUMake !!!
#----------------------------------------

##-- program

PROGRAM = image_errors

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
OPTIM    =  -O4 -ieee
DEBUG    = -Wall

##-- paths & applications

INCLUDE_PATHS = $(HOME)/include 

##-- library paths

USE_CERNLIB = no
CERNDIR     = /CERN
RANLIB_PATH = $(HOME)/lib
CTSLIB_PATH = $(HOME)/lib
CTSLIB_USE  = base hbook 

## CERN libraries flag ----------------------------------------
ifeq ($(USE_CERNLIB),yes)
CERNLIBDIR = ${CERNDIR}/pro/lib/
CERNLIB = -L${CERNLIBDIR} -lmathlib -lpacklib -lkernlib 
CTSLIB = -L$(CTSLIB_PATH) $(addprefix -lcts,$(CTSLIB_USE))
else
CERNLIBDIR = 
CERNLIB = 
CTSLIB = 
endif

## common flags -----------------------------------------------
INCLUDES := $(addprefix -I,$(INCLUDE_PATHS))

## other libraries --------------------------------------------
RANLIB  = -L${RANLIB_PATH} -lranlib

## generic Fortran flags (if needed) --------------------------
# special flags (FORTRAN)
osf_FORLIBS = -lUfor -lfor -lutil -lots -lm
linux_FORLIBS = -lnsl  -lg2c -lm
generic_FORLIBS = -lm 

FORLIBS = ${${SYSTEM}_FORLIBS}

# compilation and linking flags

CXXFLAGS  = -D__${SYSTEM}__ ${INCLUDES} ${OPTIM} ${DEBUG}
CFLAGS    = ${CXXFLAGS}
FFLAGS    = ${CXXFLAGS}
LIBS      = $(CERNLIB) ${CTSLIB} ${RANLIB} ${FORLIBS}

#------------------------------------------------------------------------------

#.SILENT:

.SUFFIXES: .c .cc .cxx .C .c++ .h .hxx .H .h++ .o .so .f

SRCS = image_errors.cc

HEADERS = 

OBJS = $(SRCS:.cc=.o) 

############################################################

all: ${PROGRAM}

depend:
	@makedepend $(SRCS) -fMakefile 2> /dev/null

${PROGRAM}: $(OBJS)
	@echo "Linking..." 
	echo `ls -m $(OBJS)|sed 's/,/ +/g' `" + libraries => " $@
	$(CXX) $(CXXFLAGS) $(OBJS) $(LIBS) -o $@
	@echo "done."

.cxx.o:	
	@echo "Compiling " $<
	$(CXX) $(CXXFLAGS) -c $< -o $@

.cc.o:	
	@echo "Compiling " $<
	$(CXX) $(CXXFLAGS) -c $< -o $@

.c.o:	
	@echo "Compiling " $<
	$(CC) $(CFLAGS) -c $< -o $@

.f.o:	
	@echo "Compiling " $<
	$(F77) $(FFLAGS) -c $< -o $@

clean:
	@echo "Cleanning..."
	@rm -f $(OBJS) core 

mrproper: clean
	@echo "Mr.Proper in action . . ."
	@rm -f *~ $(PROGRAM)

redo: clean all

# DO NOT DELETE THIS LINE -- make depend depends on it.

image_errors.o: /usr/include/math.h /usr/include/features.h
image_errors.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
image_errors.o: /usr/include/bits/huge_val.h /usr/include/bits/mathdef.h
image_errors.o: /usr/include/bits/mathcalls.h
image_errors.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/float.h
image_errors.o: /usr/include/time.h
image_errors.o: /usr/lib/gcc-lib/i486-linux/2.7.2.3/include/stddef.h
image_errors.o: /usr/include/bits/time.h /usr/include/bits/types.h
