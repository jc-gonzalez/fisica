#============================================================
# Makefile to compile modified 5.20 version of CORSIKA.
#
# Copyright (c) J C Gonzalez, 1998
#============================================================

DESTDIR = .

F77 = f77
CC = cc
CERNDIR = CERN
ADDFLAGS =

EXE_VERSION = c520

###
#   compiler & linker options section
###

# machine dependencies
SYSTEM = GENERIC

## generic (alpha)
GENERIC_CFLAGS   = -I. -I/usr/include -DJC_UNDERSCORES ${ADDFLAGS}
GENERIC_FFLAGS   = ${GENERIC_CFLAGS}
GENERIC_EXE      = ${EXE_VERSION}-generic
GENERIC_LIBS     = -lm -lc
GENERIC_CC       = cc
GENERIC_F77      = f77

## osf (alpha)
OSF_CFLAGS   = -I. -I/usr/include -DJC_UNDERSCORES ${ADDFLAGS}
OSF_FFLAGS   = ${OSF_CFLAGS}
OSF_EXE      = ${EXE_VERSION}-osf
OSF_LIBS     = -lm -lc
OSF_CC       = cc
OSF_F77      = f77

## linux
LINUX_CFLAGS = -I. -I/usr/include -funderscoring -DJC_UNDERSCORES \
		-m486 -DCPU=686 ${ADDFLAGS}
LINUX_FFLAGS = ${LINUX_CFLAGS}
LINUX_EXE    = ${EXE_VERSION}-linux
LINUX_LIBS   = -lm -lc
LINUX_CC     = gcc
LINUX_F77    = g77

## aix (ibm rs6000)
AIX_CFLAGS   = -I. -I/usr/include -DJC_NO_UNDERSCORES ${ADDFLAGS}
AIX_FFLAGS   = ${AIX_CFLAGS}
AIX_EXE      = ${EXE_VERSION}-aix
AIX_LIBS     = -lm -lc -lbsd
AIX_CC       = cc
AIX_F77      = xlf

# choose this machine values
F77    = ${${SYSTEM}_F77}
CC     = ${${SYSTEM}_CC}
CFLAGS = ${${SYSTEM}_CFLAGS}
FFLAGS = ${${SYSTEM}_FFLAGS}
LIBS   = ${${SYSTEM}_LIBS}

###
#   miscelanea
###

CERNDIR = CERN

###
#   files section
###

HEADERS =

FSRCS = \
	GHEISHA.f VENUS.f addang.f addani.f age.f amoeba.f  \
	amotry.f annih.f ausgab.f ausgb2.f avage.f avept.f  \
	bhabha.f block1.f box2.f box3.f box60.f box61.f  \
	box62.f box63.f box65.f box66.f box67.f box68.f  \
	box69.f box70.f box71.f box72.f box73.f box74.f  \
	brems.f cerenkov.f cghei.f cghini.f cghsig.f  \
	chisq.f compt.f coorin.f datac.f decay1.f decay6.f  \
	difrac.f dpfunc.f egs4.f egs4bd.f egsini.f electr.f  \
	em.f etadec.f flpsor.f fstack.f gam.f garndm.f  \
	gbrsgm.f gprsgm.f granor.f grndm.f hatch.f hdpm.f  \
	heigh.f hmeson.f howfar.f ininkg.f inprm.f isobar.f  \
	istack.f jadach.f kdecay.f leaddf.f ledeny.f  \
	lepacx.f longft.f lowup.f lpm.f main.f mitage.f  \
	mmol4.f mmolie.f moller.f mpoiss.f mpprop.f mscat.f  \
	mubrem.f mucoul.f mudecy.f mupair.f muprpr.f  \
	mutrac.f nihila.f nkg.f nsd.f nucint.f outend.f  \
	output.f pair.f pamaf.f parnum.f parrap.f pcl.f  \
	photo.f photon.f pi0dec.f pigen.f pigen1.f pigen2.f  \
	pparam.f prange.f prtime.f ptram.f ptran.f ptrans.f  \
	rangen.f rannor.f resdec.f rhof.f rmmaq.f rmmar.f  \
	rnegbi.f sdpm.f selcor.f shower.f single.f sobseq.f  \
	staend.f stankg.f start.f strdec.f tarint.f thick.f  \
	tobuf.f tstack.f tstend.f tstini.f tstout.f ucopy.f  \
	update.f uphi.f utqsea.f utqval.f vapor.f vendat.f  \
	venini.f venlnk.f vhmeso.f vstore.f vzero.f

CSRCS = \
	jcio.c jcdate.c

ALLOBJS = ${FSRCS:.f=.o} ${CSRCS:.c=.o}

PROGRAM = ${${SYSTEM}_EXE}

help:
	@echo ''
	@echo 'Usage::'
	@echo ''
	@echo '  make help              shows this help            or' 
	@echo '  make clean             cleans-up garbage          or'
	@echo '  make all [ SYSTEM = { OSF | LINUX | AIX | GENERIC} ]'
	@echo '                         compiles with pre-defined flags for'
	@echo '                         OSF, Linux or AIX flavour of Unix'
	@echo ''
	@echo 'Additionally, you can add to your command line ADDFLAGS=<flags>"
	@echo 'to include any new flag you want.'
	@echo ''
	@echo 'In case of problems, please contact to <gonzalez@mppmu.mpg.de>'
	@echo ''

all: message ${PROGRAM} 

message:
	@echo "============================================================"
	@echo " Compilation of CORSIKA ("${EXE_VERSION}")"
	@echo " Target machine => `uname -a`"
	@echo " System choosed => "${SYSTEM}
	@echo " Date: `date`"
	@echo "============================================================"
	@echo ''

${PROGRAM}: ${ALLOBJS}
	@echo -n "   . . . and linking -> "${PROGRAM}" . . . "
	$(F77) ${FFLAGS} -o ${PROGRAM} ${ALLOBJS} ${LIBS} ${XLIB}
	@echo "done."

.f.o:
	@echo "Compiling "$<" . . ."
	$(F77) -c $(FFLAGS) $< -o $@

.c.o:
	@echo "Compiling "$<" . . ."
	$(CC) -c $(CFLAGS) $< -o $@

script:
	@sh ./c-script.sh

clean: 
	@echo "Cleaning . . ."
	@rm *.o *~ core *.BAK > /dev/null 2>&1 || echo "...already clean."
