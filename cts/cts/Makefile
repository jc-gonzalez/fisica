#
# Makefile for all analysis programs
#
# author: D. Kranich, 15.11.99
#

# set path to needed libs and include files
# libs-dir - path to lib sources
# bin-dir  - install path for binaries
# lib-dir  - install path for libs
# inc-dir  - install path for include files
root-dir := $(shell pwd)
libs-dir := $(root-dir)/libs
misc-dir := $(root-dir)/misc
bin-dir := $(HOME)/bin
lib-dir := $(HOME)/lib
inc-dir := $(lib-dir)/include


export LIBS := $(libs-dir)/libctsbase/libctsbase.so \
	       $(libs-dir)/libctsmath/libctsmath.so \
	       $(libs-dir)/libctshbook/libctshbook.so   \
	       $(libs-dir)/libctshplot/libctshplot.so   \
	       $(libs-dir)/libctscuts/libctscuts.so     \
	       $(libs-dir)/libctsminuit/libctsminuit.so \
	       $(libs-dir)/libcnr/libcnr.so -lm

export 	INCLUDE_DIR := -I$(libs-dir)/libctsbase \
		       -I$(libs-dir)/libctsmath \
		       -I$(libs-dir)/libctsminuit \
		       -I$(libs-dir)/libctshbook  \
		       -I$(libs-dir)/libctshplot  \
		       -I$(libs-dir)/libctscuts   \
		       -I$(libs-dir)/libcnr


# default
#
CFLAGS := -g

# linux
#
ifeq (linux-gnu, $(findstring linux-gnu, $(OSTYPE)))
  CFLAGS := $(CFLAGS) -mcpu=i686 -march=i686
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  CFLAGS := $(CFLAGS)
endif

export CFLAGS



# first all the libs then the progs
# (don't change the order of any lib)
#
all:
	$(MAKE) -w -C $(libs-dir)/libctsbase/
	$(MAKE) -w -C $(libs-dir)/libctsmath/
	$(MAKE) -w -C $(libs-dir)/libctshbook/
	$(MAKE) -w -C $(libs-dir)/libctshplot/
	$(MAKE) -w -C $(libs-dir)/libctscuts/
	$(MAKE) -w -C $(libs-dir)/libctsminuit/
	$(MAKE) -w -C $(libs-dir)/libcnr/

	$(MAKE) -w -C $(root-dir)/calcutta/
	$(MAKE) -w -C $(root-dir)/source2d/
	$(MAKE) -w -C $(root-dir)/powspec/
	$(MAKE) -w -C $(root-dir)/jacuzzi/
	$(MAKE) -w -C $(root-dir)/pixdis/
	$(MAKE) -w -C $(root-dir)/crux/

install: all
	if [ ! -d $(lib-dir) ]; then mkdir $(lib-dir); fi
	cp $(libs-dir)/libctsbase/libctsbase.so     $(lib-dir)
	cp $(libs-dir)/libctsmath/libctsmath.so     $(lib-dir)
	cp $(libs-dir)/libctsminuit/libctsminuit.so $(lib-dir)
	cp $(libs-dir)/libctshbook/libctshbook.so   $(lib-dir)
	cp $(libs-dir)/libctshplot/libctshplot.so   $(lib-dir)
	cp $(libs-dir)/libctscuts/libctscuts.so     $(lib-dir)
	cp $(libs-dir)/libcnr/libcnr.so         $(lib-dir)

	if [ ! -d $(inc-dir) ]; then mkdir $(inc-dir); fi
	cp $(libs-dir)/libctsbase/ctsbase.h     $(inc-dir)
	cp $(libs-dir)/libctsmath/ctsmath.h     $(inc-dir)
	cp $(libs-dir)/libctsminuit/ctsminuit.h $(inc-dir)
	cp $(libs-dir)/libctshbook/ctshbook.h   $(inc-dir)
	cp $(libs-dir)/libctshplot/ctshplot.h   $(inc-dir)
	cp $(libs-dir)/libctscuts/ctscuts.h     $(inc-dir)
	cp $(libs-dir)/libcnr/nr.h          $(inc-dir)

	if [ ! -d $(bin-dir) ]; then mkdir $(bin-dir); fi
	cp $(root-dir)/calcutta/calcutta $(bin-dir)
	cp $(root-dir)/source2d/source2d $(bin-dir)
	cp $(root-dir)/jacuzzi/jacuzzi   $(bin-dir)
	cp $(root-dir)/powspec/powspec   $(bin-dir)
	cp $(root-dir)/pixdis/pixdis     $(bin-dir)
	cp $(root-dir)/crux/crux         $(bin-dir)

# some small progs for special needs
#
misc:
	$(MAKE) -w -C $(misc-dir)/setie/
	$(MAKE) -w -C $(misc-dir)/mkntup/

misc_install:
	if [ ! -d $(bin-dir) ]; then mkdir $(bin-dir); fi
	cp $(misc-dir)/setie/setie $(bin-dir)
	cp $(misc-dir)/mkntup/mkntup $(bin-dir)


clean:
	rm -f *.o *.BAK *~ core
	$(MAKE) -w -C $(libs-dir)/libctsbase/ $@
	$(MAKE) -w -C $(libs-dir)/libctsmath/ $@
	$(MAKE) -w -C $(libs-dir)/libctshbook/ $@
	$(MAKE) -w -C $(libs-dir)/libctshplot/ $@
	$(MAKE) -w -C $(libs-dir)/libctscuts/ $@
	$(MAKE) -w -C $(libs-dir)/libctsminuit/ $@
	$(MAKE) -w -C $(libs-dir)/libcnr/ $@
	$(MAKE) -w -C $(root-dir)/calcutta/ $@
	$(MAKE) -w -C $(root-dir)/source2d/ $@
	$(MAKE) -w -C $(root-dir)/jacuzzi/ $@
	$(MAKE) -w -C $(root-dir)/powspec/ $@
	$(MAKE) -w -C $(root-dir)/pixdis/ $@
	$(MAKE) -w -C $(root-dir)/crux/ $@
	$(MAKE) -w -C $(misc-dir)/setie/ $@
	$(MAKE) -w -C $(misc-dir)/mkntup/ $@
