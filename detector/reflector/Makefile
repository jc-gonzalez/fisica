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


../include-COR/COREventHeader.c.o: ../include-COR/COREventHeader.hxx
../include-COR/COREventHeader.c.o: /usr/include/_G_config.h
../include-COR/COREventHeader.c.o: /usr/include/alloca.h
../include-COR/COREventHeader.c.o: /usr/include/bits/endian.h
../include-COR/COREventHeader.c.o: /usr/include/bits/huge_val.h
../include-COR/COREventHeader.c.o: /usr/include/bits/mathcalls.h
../include-COR/COREventHeader.c.o: /usr/include/bits/mathdef.h
../include-COR/COREventHeader.c.o: /usr/include/bits/nan.h
../include-COR/COREventHeader.c.o: /usr/include/bits/pthreadtypes.h
../include-COR/COREventHeader.c.o: /usr/include/bits/sched.h
../include-COR/COREventHeader.c.o: /usr/include/bits/select.h
../include-COR/COREventHeader.c.o: /usr/include/bits/sigset.h
../include-COR/COREventHeader.c.o: /usr/include/bits/types.h
../include-COR/COREventHeader.c.o: /usr/include/endian.h
../include-COR/COREventHeader.c.o: /usr/include/features.h
../include-COR/COREventHeader.c.o: /usr/include/g++/fstream.h
../include-COR/COREventHeader.c.o: /usr/include/g++/iomanip.h
../include-COR/COREventHeader.c.o: /usr/include/g++/iostream.h
../include-COR/COREventHeader.c.o: /usr/include/g++/streambuf.h
../include-COR/COREventHeader.c.o: /usr/include/gnu/stubs.h
../include-COR/COREventHeader.c.o: /usr/include/libio.h /usr/include/math.h
../include-COR/COREventHeader.c.o: /usr/include/stdlib.h
../include-COR/COREventHeader.c.o: /usr/include/sys/cdefs.h
../include-COR/COREventHeader.c.o: /usr/include/sys/select.h
../include-COR/COREventHeader.c.o: /usr/include/sys/sysmacros.h
../include-COR/COREventHeader.c.o: /usr/include/sys/types.h
../include-COR/COREventHeader.c.o: /usr/include/time.h /usr/include/xlocale.h
../include-COR/COREventHeader.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
../include-COR/COREventHeader.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
../include-COR/COREventHeader.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
../include-COR/CORParticle.c.o: ../include-COR/CORParticle.hxx
../include-COR/CORParticle.c.o: /usr/include/_G_config.h /usr/include/alloca.h
../include-COR/CORParticle.c.o: /usr/include/bits/endian.h
../include-COR/CORParticle.c.o: /usr/include/bits/huge_val.h
../include-COR/CORParticle.c.o: /usr/include/bits/mathcalls.h
../include-COR/CORParticle.c.o: /usr/include/bits/mathdef.h
../include-COR/CORParticle.c.o: /usr/include/bits/nan.h
../include-COR/CORParticle.c.o: /usr/include/bits/pthreadtypes.h
../include-COR/CORParticle.c.o: /usr/include/bits/sched.h
../include-COR/CORParticle.c.o: /usr/include/bits/select.h
../include-COR/CORParticle.c.o: /usr/include/bits/sigset.h
../include-COR/CORParticle.c.o: /usr/include/bits/types.h
../include-COR/CORParticle.c.o: /usr/include/endian.h /usr/include/features.h
../include-COR/CORParticle.c.o: /usr/include/g++/fstream.h
../include-COR/CORParticle.c.o: /usr/include/g++/iomanip.h
../include-COR/CORParticle.c.o: /usr/include/g++/iostream.h
../include-COR/CORParticle.c.o: /usr/include/g++/streambuf.h
../include-COR/CORParticle.c.o: /usr/include/gnu/stubs.h /usr/include/libio.h
../include-COR/CORParticle.c.o: /usr/include/math.h /usr/include/stdlib.h
../include-COR/CORParticle.c.o: /usr/include/sys/cdefs.h
../include-COR/CORParticle.c.o: /usr/include/sys/select.h
../include-COR/CORParticle.c.o: /usr/include/sys/sysmacros.h
../include-COR/CORParticle.c.o: /usr/include/sys/types.h /usr/include/time.h
../include-COR/CORParticle.c.o: /usr/include/xlocale.h
../include-COR/CORParticle.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
../include-COR/CORParticle.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
../include-COR/CORParticle.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
../include-COR/CORStatfile.c.o: ../include-COR/CORStatfile.hxx
../include-COR/CORStatfile.c.o: /usr/include/_G_config.h /usr/include/alloca.h
../include-COR/CORStatfile.c.o: /usr/include/bits/endian.h
../include-COR/CORStatfile.c.o: /usr/include/bits/huge_val.h
../include-COR/CORStatfile.c.o: /usr/include/bits/mathcalls.h
../include-COR/CORStatfile.c.o: /usr/include/bits/mathdef.h
../include-COR/CORStatfile.c.o: /usr/include/bits/nan.h
../include-COR/CORStatfile.c.o: /usr/include/bits/pthreadtypes.h
../include-COR/CORStatfile.c.o: /usr/include/bits/sched.h
../include-COR/CORStatfile.c.o: /usr/include/bits/select.h
../include-COR/CORStatfile.c.o: /usr/include/bits/sigset.h
../include-COR/CORStatfile.c.o: /usr/include/bits/types.h
../include-COR/CORStatfile.c.o: /usr/include/endian.h /usr/include/features.h
../include-COR/CORStatfile.c.o: /usr/include/g++/fstream.h
../include-COR/CORStatfile.c.o: /usr/include/g++/iostream.h
../include-COR/CORStatfile.c.o: /usr/include/g++/streambuf.h
../include-COR/CORStatfile.c.o: /usr/include/gnu/stubs.h /usr/include/libio.h
../include-COR/CORStatfile.c.o: /usr/include/math.h /usr/include/stdlib.h
../include-COR/CORStatfile.c.o: /usr/include/sys/cdefs.h
../include-COR/CORStatfile.c.o: /usr/include/sys/select.h
../include-COR/CORStatfile.c.o: /usr/include/sys/sysmacros.h
../include-COR/CORStatfile.c.o: /usr/include/sys/types.h /usr/include/time.h
../include-COR/CORStatfile.c.o: /usr/include/xlocale.h
../include-COR/CORStatfile.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
../include-COR/CORStatfile.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
../include-COR/CORStatfile.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
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
atm.c.o: /usr/include/_G_config.h /usr/include/alloca.h
atm.c.o: /usr/include/bits/endian.h /usr/include/bits/huge_val.h
atm.c.o: /usr/include/bits/mathcalls.h /usr/include/bits/mathdef.h
atm.c.o: /usr/include/bits/nan.h /usr/include/bits/pthreadtypes.h
atm.c.o: /usr/include/bits/sched.h /usr/include/bits/select.h
atm.c.o: /usr/include/bits/sigset.h /usr/include/bits/stdio_lim.h
atm.c.o: /usr/include/bits/types.h /usr/include/endian.h
atm.c.o: /usr/include/features.h /usr/include/g++/fstream.h
atm.c.o: /usr/include/g++/iostream.h /usr/include/g++/streambuf.h
atm.c.o: /usr/include/gnu/stubs.h /usr/include/libio.h /usr/include/math.h
atm.c.o: /usr/include/stdio.h /usr/include/stdlib.h /usr/include/string.h
atm.c.o: /usr/include/sys/cdefs.h /usr/include/sys/select.h
atm.c.o: /usr/include/sys/sysmacros.h /usr/include/sys/types.h
atm.c.o: /usr/include/time.h /usr/include/xlocale.h
atm.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
atm.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
atm.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h atm.h
atm.c.o: reflector-v.h
readparam.c.o: /usr/include/_G_config.h /usr/include/alloca.h
readparam.c.o: /usr/include/bits/confname.h /usr/include/bits/endian.h
readparam.c.o: /usr/include/bits/environments.h /usr/include/bits/huge_val.h
readparam.c.o: /usr/include/bits/mathcalls.h /usr/include/bits/mathdef.h
readparam.c.o: /usr/include/bits/nan.h /usr/include/bits/posix_opt.h
readparam.c.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/sched.h
readparam.c.o: /usr/include/bits/select.h /usr/include/bits/sigset.h
readparam.c.o: /usr/include/bits/stdio_lim.h /usr/include/bits/types.h
readparam.c.o: /usr/include/bits/wordsize.h /usr/include/endian.h
readparam.c.o: /usr/include/features.h /usr/include/g++/fstream.h
readparam.c.o: /usr/include/g++/iostream.h /usr/include/g++/streambuf.h
readparam.c.o: /usr/include/getopt.h /usr/include/gnu/stubs.h
readparam.c.o: /usr/include/libio.h /usr/include/math.h /usr/include/stdio.h
readparam.c.o: /usr/include/stdlib.h /usr/include/string.h
readparam.c.o: /usr/include/sys/cdefs.h /usr/include/sys/select.h
readparam.c.o: /usr/include/sys/sysmacros.h /usr/include/sys/types.h
readparam.c.o: /usr/include/time.h /usr/include/unistd.h
readparam.c.o: /usr/include/xlocale.h
readparam.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
readparam.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
readparam.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h atm.h
readparam.c.o: readparam.h reflector-v.h
reflector.c.o: /usr/include/_G_config.h /usr/include/alloca.h
reflector.c.o: /usr/include/bits/confname.h /usr/include/bits/dirent.h
reflector.c.o: /usr/include/bits/endian.h /usr/include/bits/environments.h
reflector.c.o: /usr/include/bits/huge_val.h /usr/include/bits/local_lim.h
reflector.c.o: /usr/include/bits/mathcalls.h /usr/include/bits/mathdef.h
reflector.c.o: /usr/include/bits/nan.h /usr/include/bits/posix1_lim.h
reflector.c.o: /usr/include/bits/posix_opt.h /usr/include/bits/pthreadtypes.h
reflector.c.o: /usr/include/bits/sched.h /usr/include/bits/select.h
reflector.c.o: /usr/include/bits/sigset.h /usr/include/bits/stdio_lim.h
reflector.c.o: /usr/include/bits/types.h /usr/include/bits/wordsize.h
reflector.c.o: /usr/include/dirent.h /usr/include/endian.h
reflector.c.o: /usr/include/features.h /usr/include/g++/fstream.h
reflector.c.o: /usr/include/g++/iostream.h /usr/include/g++/streambuf.h
reflector.c.o: /usr/include/getopt.h /usr/include/gnu/stubs.h
reflector.c.o: /usr/include/libio.h /usr/include/linux/limits.h
reflector.c.o: /usr/include/math.h /usr/include/stdio.h /usr/include/stdlib.h
reflector.c.o: /usr/include/string.h /usr/include/sys/cdefs.h
reflector.c.o: /usr/include/sys/select.h /usr/include/sys/sysmacros.h
reflector.c.o: /usr/include/sys/types.h /usr/include/time.h
reflector.c.o: /usr/include/unistd.h /usr/include/xlocale.h
reflector.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
reflector.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
reflector.c.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h atm.h
reflector.c.o: readparam.h reflector-v.h reflector.h
