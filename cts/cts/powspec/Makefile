#
# Makefile for program powspec
#
# author: D. Kranich, 04.07.99
#

PROGRAM = powspec

DESTDIR = .

ifndef LIBS
LIBS := $(HOME)/lib/libctsbase.so \
	$(HOME)/lib/libctsmath.so \
	$(HOME)/lib/libctshbook.so \
	$(HOME)/lib/libctshplot.so \
	$(HOME)/lib/libctsminuit.so \
	$(HOME)/lib/libcnr.so     \
	-lm
endif


#
# set compiler and flags according to OS
#

# default
#
CC = cc
INCLUDE_DIR := $(INCLUDE_DIR) -I$(HOME)/lib/include
CFLAGS := $(CFLAGS) $(INCLUDE_DIR)

# linux
#
ifeq (linux-gnu, $(findstring linux-gnu, $(OSTYPE)))
  CC = gcc
  CFLAGS := $(CFLAGS) -g -pedantic -Wall $(INCLUDE_DIR)
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  CFLAGS := $(CFLAGS) -I/usr/include $(INCLUDE_DIR) -Olimit 1600 \
            -newc -std1 -ieee_with_no_inexact -rpath $(HOME)/lib
endif


OBJS =  get_powspec.o  \
	pow_plots.o  \
	powspec.o  \
	get_pdf.o  \
	sim_lc.o   \
	mle.o


all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)


$(OBJS): powspec.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
