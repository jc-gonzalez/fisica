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
linux_FORLIBS = -lnsl -lg2c -lm
gnu_FORLIBS = -lm -lg2c 
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

all: showopt ${PROGRAM}

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

showopt:
	@echo "==========================================="
	@echo "SYSTEM especified:  " $(SYSTEM)
	@echo "------------------------------------"
	@echo "Compilers:    "$(CC)" , "$(CXX)" , "$(F77)
	@echo "Optim./Debug: ["$(OPTIM)"] , ["$(DEBUG)"]"
	@echo "==========================================="
	@echo ""

redo: clean all

cflags: 
	@echo $(INCLUDES) $(CXXFLAGS)

# @endcode

# DO NOT DELETE THIS LINE -- make depend depends on it.


../include-MC/MCCphoton.c.o: ../include-MC/MCCphoton.hxx
../include-MC/MCCphoton.c.o: /usr/include/_G_config.h /usr/include/alloca.h
../include-MC/MCCphoton.c.o: /usr/include/bits/endian.h
../include-MC/MCCphoton.c.o: /usr/include/bits/huge_val.h
../include-MC/MCCphoton.c.o: /usr/include/bits/mathcalls.h
../include-MC/MCCphoton.c.o: /usr/include/bits/mathdef.h
../include-MC/MCCphoton.c.o: /usr/include/bits/nan.h
../include-MC/MCCphoton.c.o: /usr/include/bits/pthreadtypes.h
../include-MC/MCCphoton.c.o: /usr/include/bits/sched.h
../include-MC/MCCphoton.c.o: /usr/include/bits/select.h
../include-MC/MCCphoton.c.o: /usr/include/bits/sigset.h
../include-MC/MCCphoton.c.o: /usr/include/bits/types.h /usr/include/endian.h
../include-MC/MCCphoton.c.o: /usr/include/features.h
../include-MC/MCCphoton.c.o: /usr/include/g++/fstream.h
../include-MC/MCCphoton.c.o: /usr/include/g++/iomanip.h
../include-MC/MCCphoton.c.o: /usr/include/g++/iostream.h
../include-MC/MCCphoton.c.o: /usr/include/g++/streambuf.h
../include-MC/MCCphoton.c.o: /usr/include/gnu/stubs.h /usr/include/libio.h
../include-MC/MCCphoton.c.o: /usr/include/math.h /usr/include/stdlib.h
../include-MC/MCCphoton.c.o: /usr/include/string.h /usr/include/sys/cdefs.h
../include-MC/MCCphoton.c.o: /usr/include/sys/select.h
../include-MC/MCCphoton.c.o: /usr/include/sys/sysmacros.h
../include-MC/MCCphoton.c.o: /usr/include/sys/types.h /usr/include/time.h
../include-MC/MCCphoton.c.o: /usr/include/xlocale.h
../include-MC/MCCphoton.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
../include-MC/MCCphoton.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
../include-MC/MCCphoton.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
../include-MC/MCEventHeader.c.o: ../include-MC/MCEventHeader.hxx
../include-MC/MCEventHeader.c.o: /usr/include/_G_config.h
../include-MC/MCEventHeader.c.o: /usr/include/alloca.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/endian.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/huge_val.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/mathcalls.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/mathdef.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/nan.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/pthreadtypes.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/sched.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/select.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/sigset.h
../include-MC/MCEventHeader.c.o: /usr/include/bits/types.h
../include-MC/MCEventHeader.c.o: /usr/include/endian.h /usr/include/features.h
../include-MC/MCEventHeader.c.o: /usr/include/g++/fstream.h
../include-MC/MCEventHeader.c.o: /usr/include/g++/iomanip.h
../include-MC/MCEventHeader.c.o: /usr/include/g++/iostream.h
../include-MC/MCEventHeader.c.o: /usr/include/g++/streambuf.h
../include-MC/MCEventHeader.c.o: /usr/include/gnu/stubs.h /usr/include/libio.h
../include-MC/MCEventHeader.c.o: /usr/include/math.h /usr/include/stdlib.h
../include-MC/MCEventHeader.c.o: /usr/include/sys/cdefs.h
../include-MC/MCEventHeader.c.o: /usr/include/sys/select.h
../include-MC/MCEventHeader.c.o: /usr/include/sys/sysmacros.h
../include-MC/MCEventHeader.c.o: /usr/include/sys/types.h /usr/include/time.h
../include-MC/MCEventHeader.c.o: /usr/include/xlocale.h
../include-MC/MCEventHeader.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
../include-MC/MCEventHeader.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
../include-MC/MCEventHeader.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
camera.c.o: /usr/include/_G_config.h /usr/include/alloca.h
camera.c.o: /usr/include/assert.h /usr/include/bits/confname.h
camera.c.o: /usr/include/bits/dirent.h /usr/include/bits/endian.h
camera.c.o: /usr/include/bits/environments.h /usr/include/bits/huge_val.h
camera.c.o: /usr/include/bits/local_lim.h /usr/include/bits/mathcalls.h
camera.c.o: /usr/include/bits/mathdef.h /usr/include/bits/nan.h
camera.c.o: /usr/include/bits/posix1_lim.h /usr/include/bits/posix2_lim.h
camera.c.o: /usr/include/bits/posix_opt.h /usr/include/bits/pthreadtypes.h
camera.c.o: /usr/include/bits/sched.h /usr/include/bits/select.h
camera.c.o: /usr/include/bits/sigset.h /usr/include/bits/sigthread.h
camera.c.o: /usr/include/bits/stdio_lim.h /usr/include/bits/time.h
camera.c.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
camera.c.o: /usr/include/bits/xopen_lim.h /usr/include/dirent.h
camera.c.o: /usr/include/endian.h /usr/include/features.h
camera.c.o: /usr/include/g++/algorithm /usr/include/g++/fstream.h
camera.c.o: /usr/include/g++/iostream.h /usr/include/g++/stl_algo.h
camera.c.o: /usr/include/g++/stl_algobase.h /usr/include/g++/stl_alloc.h
camera.c.o: /usr/include/g++/stl_bvector.h /usr/include/g++/stl_config.h
camera.c.o: /usr/include/g++/stl_construct.h /usr/include/g++/stl_heap.h
camera.c.o: /usr/include/g++/stl_iterator.h /usr/include/g++/stl_pair.h
camera.c.o: /usr/include/g++/stl_relops.h /usr/include/g++/stl_tempbuf.h
camera.c.o: /usr/include/g++/stl_uninitialized.h /usr/include/g++/stl_vector.h
camera.c.o: /usr/include/g++/streambuf.h /usr/include/g++/type_traits.h
camera.c.o: /usr/include/g++/vector /usr/include/getopt.h
camera.c.o: /usr/include/gnu/stubs.h /usr/include/libgen.h
camera.c.o: /usr/include/libio.h /usr/include/limits.h
camera.c.o: /usr/include/linux/limits.h /usr/include/math.h
camera.c.o: /usr/include/pthread.h /usr/include/sched.h /usr/include/signal.h
camera.c.o: /usr/include/stdio.h /usr/include/stdlib.h /usr/include/string.h
camera.c.o: /usr/include/sys/cdefs.h /usr/include/sys/select.h
camera.c.o: /usr/include/sys/sysmacros.h /usr/include/sys/types.h
camera.c.o: /usr/include/time.h /usr/include/unistd.h /usr/include/xlocale.h
camera.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/exception
camera.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
camera.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/limits.h
camera.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/new
camera.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/new.h
camera.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
camera.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
camera.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/syslimits.h
camera.c.o: camera-v.h camera.h creadparam.h moments.h
creadparam.c.o: /usr/include/_G_config.h /usr/include/alloca.h
creadparam.c.o: /usr/include/bits/endian.h /usr/include/bits/huge_val.h
creadparam.c.o: /usr/include/bits/mathcalls.h /usr/include/bits/mathdef.h
creadparam.c.o: /usr/include/bits/nan.h /usr/include/bits/pthreadtypes.h
creadparam.c.o: /usr/include/bits/sched.h /usr/include/bits/select.h
creadparam.c.o: /usr/include/bits/sigset.h /usr/include/bits/stdio_lim.h
creadparam.c.o: /usr/include/bits/types.h /usr/include/endian.h
creadparam.c.o: /usr/include/features.h /usr/include/g++/fstream.h
creadparam.c.o: /usr/include/g++/iostream.h /usr/include/g++/streambuf.h
creadparam.c.o: /usr/include/gnu/stubs.h /usr/include/libio.h
creadparam.c.o: /usr/include/math.h /usr/include/stdio.h /usr/include/stdlib.h
creadparam.c.o: /usr/include/string.h /usr/include/sys/cdefs.h
creadparam.c.o: /usr/include/sys/select.h /usr/include/sys/sysmacros.h
creadparam.c.o: /usr/include/sys/types.h /usr/include/time.h
creadparam.c.o: /usr/include/xlocale.h
creadparam.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
creadparam.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
creadparam.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
creadparam.c.o: camera-v.h creadparam.h trigpatt.h
moments.c.o: /usr/include/_G_config.h /usr/include/alloca.h
moments.c.o: /usr/include/assert.h /usr/include/bits/endian.h
moments.c.o: /usr/include/bits/huge_val.h /usr/include/bits/local_lim.h
moments.c.o: /usr/include/bits/mathcalls.h /usr/include/bits/mathdef.h
moments.c.o: /usr/include/bits/nan.h /usr/include/bits/posix1_lim.h
moments.c.o: /usr/include/bits/posix2_lim.h /usr/include/bits/pthreadtypes.h
moments.c.o: /usr/include/bits/sched.h /usr/include/bits/select.h
moments.c.o: /usr/include/bits/sigset.h /usr/include/bits/sigthread.h
moments.c.o: /usr/include/bits/stdio_lim.h /usr/include/bits/time.h
moments.c.o: /usr/include/bits/types.h /usr/include/bits/xopen_lim.h
moments.c.o: /usr/include/endian.h /usr/include/features.h
moments.c.o: /usr/include/g++/algorithm /usr/include/g++/fstream.h
moments.c.o: /usr/include/g++/iostream.h /usr/include/g++/stl_algo.h
moments.c.o: /usr/include/g++/stl_algobase.h /usr/include/g++/stl_alloc.h
moments.c.o: /usr/include/g++/stl_bvector.h /usr/include/g++/stl_config.h
moments.c.o: /usr/include/g++/stl_construct.h /usr/include/g++/stl_heap.h
moments.c.o: /usr/include/g++/stl_iterator.h /usr/include/g++/stl_pair.h
moments.c.o: /usr/include/g++/stl_relops.h /usr/include/g++/stl_tempbuf.h
moments.c.o: /usr/include/g++/stl_uninitialized.h
moments.c.o: /usr/include/g++/stl_vector.h /usr/include/g++/streambuf.h
moments.c.o: /usr/include/g++/type_traits.h /usr/include/g++/vector
moments.c.o: /usr/include/gnu/stubs.h /usr/include/libio.h
moments.c.o: /usr/include/limits.h /usr/include/linux/limits.h
moments.c.o: /usr/include/math.h /usr/include/pthread.h /usr/include/sched.h
moments.c.o: /usr/include/signal.h /usr/include/stdio.h /usr/include/stdlib.h
moments.c.o: /usr/include/string.h /usr/include/sys/cdefs.h
moments.c.o: /usr/include/sys/select.h /usr/include/sys/sysmacros.h
moments.c.o: /usr/include/sys/types.h /usr/include/time.h
moments.c.o: /usr/include/xlocale.h
moments.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/exception
moments.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
moments.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/limits.h
moments.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/new
moments.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/new.h
moments.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
moments.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
moments.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/syslimits.h
moments.c.o: camera-v.h moments.h
