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

INCLUDEMK = config.mk
include ${INCLUDEMK}

# @endcode

# @code 

## CERN libraries flag ----------------------------------------
ifeq ($(USE_CERNLIB),yes)
CERNLIBDIR = ${CERNDIR}/lib/
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
#linux_FORLIBS = -lm -lc 
generic_FORLIBS = -lm 

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
	${INCLUDE_MC}/MCEventHeader.cxx \
	${INCLUDE_MC}/MCCphoton.cxx \
	moments.cxx \
	creadparam.cxx \
	camera.cxx   

HEADERS = \
	MCEventHeader.hxx \
	MCCphoton.hxx \
	Histogram.h \
	lagrange.h \
	atm.h \
	moments.h \
	creadparam.h \
	camera.h

OBJS = \
	${INCLUDE_MC}/MCEventHeader.o \
	${INCLUDE_MC}/MCCphoton.o \
	moments.o \
	creadparam.o \
	camera.o      

############################################################

all: ${PROGRAM}

depend:
	@makedepend $(SRCS) -fMakefile 2> /dev/null

doc: camera-doc

camera-doc: 
	@echo "Generating documentation for camera . . . "
	@echo '// @T \\newpage' > predoc
	@echo '//# Source code of |trigpatt.h|.' >> predoc
	@echo '//{' >> predoc
	@echo '//}' > postdoc
	cat predoc trigpatt.h postdoc > 'trigpatt.hh'
	$(DOCUMAPP) -latex -o camera.tex camera.cxx camera.h \
	creadparam.cxx creadparam.h \
	moments.cxx moments.h \
	${INCLUDE}/Histogram.h \
	trigpatt.hh
	TEXINPUTS=${TEXINPUTS}:${DOCUMDIR} \
	latex "\nonstopmode\input{camera.tex}" && \
	makeindex camera && \
	TEXINPUTS=${TEXINPUTS}:${DOCUMDIR} \
	latex "\nonstopmode\input{camera.tex}" && \
	TEXINPUTS=${TEXINPUTS}:${DOCUMDIR} \
	latex "\nonstopmode\input{camera.tex}"
	rm trigpatt.hh
	@echo "Files camera.tex and camera.dvi generated."

${PROGRAM}: $(OBJS)
	@echo "Linking..." 
	echo `ls -m $(OBJS)|sed 's/,/ +/g' `" + libraries => " $@
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
	@rm -f *~ $(PROGRAM)

ctags:
	@echo "Creating CTAGS file . . ."
	@ctags -txw $(SRCS) $(HEADERS) > CTAGS

etags:
	@echo "Creating TAGS file . . ."
	@etags -C $(SRCS) $(HEADERS)

listsrc:
	@ls -m $(SRCS) $(HEADERS) | sed 's/,//g'

redo: clean all

cflags: 
	@echo $(INCLUDES) $(CXXFLAGS)

# @endcode

# DO NOT DELETE THIS LINE -- make depend depends on it.

../include-MC/MCEventHeader.o: ../include-MC/MCEventHeader.hxx
../include-MC/MCEventHeader.o: /usr/include/stdlib.h /usr/include/features.h
../include-MC/MCEventHeader.o: /usr/include/sys/cdefs.h
../include-MC/MCEventHeader.o: /usr/include/gnu/stubs.h
../include-MC/MCEventHeader.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stddef.h
../include-MC/MCEventHeader.o: /usr/include/sys/types.h
../include-MC/MCEventHeader.o: /usr/include/bits/types.h /usr/include/time.h
../include-MC/MCEventHeader.o: /usr/include/endian.h
../include-MC/MCEventHeader.o: /usr/include/bits/endian.h
../include-MC/MCEventHeader.o: /usr/include/sys/select.h
../include-MC/MCEventHeader.o: /usr/include/bits/select.h
../include-MC/MCEventHeader.o: /usr/include/bits/sigset.h
../include-MC/MCEventHeader.o: /usr/include/sys/sysmacros.h
../include-MC/MCEventHeader.o: /usr/include/alloca.h /usr/include/math.h
../include-MC/MCEventHeader.o: /usr/include/bits/huge_val.h
../include-MC/MCEventHeader.o: /usr/include/bits/mathdef.h
../include-MC/MCEventHeader.o: /usr/include/bits/mathcalls.h
../include-MC/MCEventHeader.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/float.h
../include-MC/MCCphoton.o: ../include-MC/MCCphoton.hxx /usr/include/stdlib.h
../include-MC/MCCphoton.o: /usr/include/features.h /usr/include/sys/cdefs.h
../include-MC/MCCphoton.o: /usr/include/gnu/stubs.h
../include-MC/MCCphoton.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stddef.h
../include-MC/MCCphoton.o: /usr/include/sys/types.h /usr/include/bits/types.h
../include-MC/MCCphoton.o: /usr/include/time.h /usr/include/endian.h
../include-MC/MCCphoton.o: /usr/include/bits/endian.h
../include-MC/MCCphoton.o: /usr/include/sys/select.h
../include-MC/MCCphoton.o: /usr/include/bits/select.h
../include-MC/MCCphoton.o: /usr/include/bits/sigset.h
../include-MC/MCCphoton.o: /usr/include/sys/sysmacros.h /usr/include/alloca.h
../include-MC/MCCphoton.o: /usr/include/string.h /usr/include/math.h
../include-MC/MCCphoton.o: /usr/include/bits/huge_val.h
../include-MC/MCCphoton.o: /usr/include/bits/mathdef.h
../include-MC/MCCphoton.o: /usr/include/bits/mathcalls.h
../include-MC/MCCphoton.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/float.h
moments.o: moments.h /usr/include/stdlib.h /usr/include/features.h
moments.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
moments.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stddef.h
moments.o: /usr/include/sys/types.h /usr/include/bits/types.h
moments.o: /usr/include/time.h /usr/include/endian.h
moments.o: /usr/include/bits/endian.h /usr/include/sys/select.h
moments.o: /usr/include/bits/select.h /usr/include/bits/sigset.h
moments.o: /usr/include/sys/sysmacros.h /usr/include/alloca.h
moments.o: /usr/include/stdio.h
moments.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stdarg.h
moments.o: /usr/include/libio.h /usr/include/_G_config.h
moments.o: /usr/include/bits/stdio_lim.h /usr/include/string.h
moments.o: /usr/include/math.h /usr/include/bits/huge_val.h
moments.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
moments.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/float.h
moments.o: camera-v.h
creadparam.o: creadparam.h /usr/include/stdlib.h /usr/include/features.h
creadparam.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
creadparam.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stddef.h
creadparam.o: /usr/include/sys/types.h /usr/include/bits/types.h
creadparam.o: /usr/include/time.h /usr/include/endian.h
creadparam.o: /usr/include/bits/endian.h /usr/include/sys/select.h
creadparam.o: /usr/include/bits/select.h /usr/include/bits/sigset.h
creadparam.o: /usr/include/sys/sysmacros.h /usr/include/alloca.h
creadparam.o: /usr/include/stdio.h
creadparam.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stdarg.h
creadparam.o: /usr/include/libio.h /usr/include/_G_config.h
creadparam.o: /usr/include/bits/stdio_lim.h /usr/include/string.h
creadparam.o: /usr/include/math.h /usr/include/bits/huge_val.h
creadparam.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
creadparam.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/float.h
creadparam.o: camera-v.h trigpatt.h
camera.o: camera.h /usr/include/stdlib.h /usr/include/features.h
camera.o: /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h
camera.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stddef.h
camera.o: /usr/include/sys/types.h /usr/include/bits/types.h
camera.o: /usr/include/time.h /usr/include/endian.h
camera.o: /usr/include/bits/endian.h /usr/include/sys/select.h
camera.o: /usr/include/bits/select.h /usr/include/bits/sigset.h
camera.o: /usr/include/sys/sysmacros.h /usr/include/alloca.h
camera.o: /usr/include/stdio.h
camera.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/stdarg.h
camera.o: /usr/include/libio.h /usr/include/_G_config.h
camera.o: /usr/include/bits/stdio_lim.h /usr/include/string.h
camera.o: /usr/include/math.h /usr/include/bits/huge_val.h
camera.o: /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h
camera.o: /usr/lib/gcc-lib/i386-redhat-linux/egcs-2.91.66/include/float.h
camera.o: /usr/include/dirent.h /usr/include/bits/dirent.h
camera.o: /usr/include/bits/posix1_lim.h /usr/include/bits/local_lim.h
camera.o: /usr/include/linux/limits.h /usr/include/unistd.h
camera.o: /usr/include/bits/posix_opt.h /usr/include/bits/confname.h
camera.o: /usr/include/getopt.h /usr/include/libgen.h camera-v.h creadparam.h
camera.o: moments.h ../include-MC/MCEventHeader.hxx
camera.o: ../include-MC/MCCphoton.hxx
