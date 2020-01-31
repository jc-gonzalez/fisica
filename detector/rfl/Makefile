##################################################################
#
# makefile
#
# @file        makefile 
# @title       Simulation of the camera and trigger logic
# @author      J C Gonz\'alez
# @email       gonzalez@mppmu.mpg.de
# @date        Fri Mar 12 11:51:11 MET 1999
#
#_______________________________________________________________
#
# Created: Fri Mar 12 11:51:11 MET 1999
# Author:  Jose Carlos Gonzalez
# Purpose: Makefile for the compilation of the camera program
# Notes:   
#    
#---------------------------------------------------------------
#
# $RCSfile$
# $Revision$
# $Author$ 
# $Date$
#
##################################################################
# @maintitle

# @code

#----------------------------------------
# NEEDS GNUMake !!!
#----------------------------------------

PROGRAM = reflector 
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

#OPTIM    = -arch host -ieee -ieee_with_no_inexact
#DEBUG    = 
OPTIM    = -O2
DEBUG    = -g3

##-- paths & applications

DOCUMDIR      = ../sus
DOCUMAPP      = ${DOCUMDIR}/sus2

INCLUDE       = .
INCLUDE_COR   = .
INCLUDE_MC    = .
INCLUDE_REFL  = .

INCLUDE_PATHS = . \
		../ranlib/src \
		$(INCLUDE) $(INCLUDE_COR) $(INCLUDE_MC) $(INCLUDE_REFL)

##-- library paths

USE_CERNLIB = no
CERNDIR     = /CERN
RANLIB_PATH = $(HOME)/lib
CTSLIB_PATH = $(HOME)/lib
CTSLIB_USE  = base hbook 

##-- next line must be uncommented for quiet compilation

#.SILENT:

# @endcode
##EOF

# @endcode

# @code 

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
linux_FORLIBS = -lnsl -lm
#linux_FORLIBS = -lm -lc 
generic_FORLIBS = -lm 

F77 = f77
FORLIBS = ${${SYSTEM}_FORLIBS}

# compilation and linking flags

CXXFLAGS  = -D__${SYSTEM}__ ${INCLUDES} ${OPTIM} ${DEBUG}
CFLAGS    = ${CXXFLAGS}
FFLAGS    = ${CXXFLAGS}
LIBS      = $(CERNLIB) ${CTSLIB} ${RANLIB} ${FORLIBS}

#------------------------------------------------------------------------------

#.SILENT:

.SUFFIXES: .c .cxx .C .c++ .h .hxx .H .h++ .o .so .f

SRCS = \
	${INCLUDE_COR}/COREventHeader.cxx \
	${INCLUDE_COR}/CORParticle.cxx \
	${INCLUDE_COR}/CORStatfile.cxx \
	${INCLUDE_MC}/MCEventHeader.cxx \
	${INCLUDE_MC}/MCCphoton.cxx \
	atm.cxx \
	attenu.f \
	readparam.cxx \
	reflector.cxx      

HEADERS = \
	COREventHeader.hxx \
	CORParticle.hxx \
	CORStatfile.hxx \
	MCEventHeader.hxx \
	MCCphoton.hxx \
	lagrange.h \
	atm.h \
	readparam.h \
	reflector.h

OBJS = \
	${INCLUDE_COR}/COREventHeader.o \
	${INCLUDE_COR}/CORParticle.o \
	${INCLUDE_COR}/CORStatfile.o \
	${INCLUDE_MC}/MCEventHeader.o \
	${INCLUDE_MC}/MCCphoton.o \
	atm.o \
	attenu.o \
	readparam.o \
	reflector.o      

PROGRAM=reflector

############################################################

all: ${PROGRAM}

depend:
	@makedepend $(SRCS) -fMakefile 2> /dev/null

doc: reflector-doc

reflector-doc: 
	@echo "Generating documentation for reflector . . . "
	$(DOCUMAPP) -latex -o reflector.tex \
	reflector.cxx reflector.h \
	readparam.cxx readparam.h \
	atm.cxx atm.h
	TEXINPUTS=${TEXINPUTS}:${DOCUMDIR} \
	latex "\nonstopmode\input{reflector.tex}" && \
	makeindex reflector && \
	TEXINPUTS=${TEXINPUTS}:${DOCUMDIR} \
	latex "\nonstopmode\input{reflector.tex}" && \
	TEXINPUTS=${TEXINPUTS}:${DOCUMDIR} \
	latex "\nonstopmode\input{reflector.tex}"
	@echo "Files reflector.tex and reflector.dvi generated."

rate: 
	@echo "Rating documentation inside code . . . "
	$(RATE) \
	reflector.cxx reflector.h \
	readparam.cxx readparam.h \
	atm.cxx atm.h

${PROGRAM}: $(OBJS)
	@echo "Linking..." $@
	$(CXX) $(CXXFLAGS) $(OBJS) $(LIBS) -o $@
	@echo "done."

.cxx.o:	
	@echo "Compiling " $<
	$(CXX) $(CXXFLAGS) -c $< -o $@

.c.o:	
	@echo "Compiling " $<
	$(CC) $(CFLAGS) -c $< -o $@

.f.o:	
	@echo "Compiling " $<
	$(F77) $(FFLAGS) -c $< -o $@

lclean:
	@echo "Cleanning..."
	@rm -f *.o core 

clean:
	@echo "Cleanning..."
	@rm -f $(OBJS) core 

mrproper: clean
	@echo "Mr.Proper in action . . ."
	@rm -f $(PROGRAM)

ctags:
	@echo "Creating CTAGS file . . ."
	@ctags -txw $(SRCS) $(HEADERS) > CTAGS

etags:
	@echo "Creating TAGS file . . ."
	@etags -C $(SRCS) $(HEADERS)

listsrc:
	@ls -m $(SRCS) $(HEADERS) | sed 's/,//g'

redo: clean all

# @endcode

# DO NOT DELETE THIS LINE -- make depend depends on it.

atm.o: atm.h /usr/include/string.h jcmacros.h jcdebug.h reflector-v.h
COREventHeader.o: COREventHeader.hxx jctypes.h
CORParticle.o: CORParticle.hxx jctypes.h
CORStatfile.o: CORStatfile.hxx jctypes.h
MCCphoton.o: MCCphoton.hxx jctypes.h /usr/include/string.h jcmacros.h
MCEventHeader.o: MCEventHeader.hxx jctypes.h COREventHeader.hxx
readparam.o: readparam.h /usr/include/string.h /usr/include/unistd.h
readparam.o: /usr/include/features.h /usr/include/stdc-predef.h jcmacros.h
readparam.o: jcdebug.h reflector-v.h atm.h
reflector.o: reflector.h /usr/include/string.h /usr/include/dirent.h
reflector.o: /usr/include/features.h /usr/include/stdc-predef.h
reflector.o: /usr/include/unistd.h jcmacros.h jcdebug.h reflector-v.h
reflector.o: readparam.h atm.h lagrange.h COREventHeader.hxx jctypes.h
reflector.o: CORParticle.hxx CORStatfile.hxx MCEventHeader.hxx MCCphoton.hxx
atm.o: /usr/include/string.h jcmacros.h jcdebug.h reflector-v.h
readparam.o: /usr/include/string.h /usr/include/unistd.h
readparam.o: /usr/include/features.h /usr/include/stdc-predef.h jcmacros.h
readparam.o: jcdebug.h reflector-v.h atm.h
reflector.o: /usr/include/string.h /usr/include/dirent.h
reflector.o: /usr/include/features.h /usr/include/stdc-predef.h
reflector.o: /usr/include/unistd.h jcmacros.h jcdebug.h reflector-v.h
reflector.o: readparam.h atm.h lagrange.h COREventHeader.hxx jctypes.h
reflector.o: CORParticle.hxx CORStatfile.hxx MCEventHeader.hxx MCCphoton.hxx
COREventHeader.o: jctypes.h
CORParticle.o: jctypes.h
CORStatfile.o: jctypes.h
MCCphoton.o: jctypes.h /usr/include/string.h jcmacros.h
MCEventHeader.o: jctypes.h COREventHeader.hxx
../ranlib/src/com.o: ../ranlib/src/ranlib.h /usr/include/stdio.h
../ranlib/src/com.o: /usr/include/stdlib.h
../ranlib/src/ranlib.o: ../ranlib/src/ranlib.h /usr/include/stdio.h
../ranlib/src/ranlib.o: /usr/include/math.h /usr/include/stdlib.h
