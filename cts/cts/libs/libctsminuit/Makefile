#
# Makefile for library libctsminuit.so
#
# author: D. Kranich, 23.06.99
#

PROGRAM = libctsminuit.so

DESTDIR = .

LIBS =

#
# set compiler and flags according to OS
#

# default
#
FC = f77
CC = cc
INCLUDE_DIR := $(INCLUDE_DIR) -I$(HOME)/lib/include
CFLAGS := $(CFLAGS) -shared $(INCLUDE_DIR)

# linux
#
ifeq (linux-gnu, $(findstring linux-gnu, $(OSTYPE)))
  CC = gcc
  FC = g77
  CFLAGS := $(CFLAGS) -pedantic -Wall $(INCLUDE_DIR) -shared
  CERNLIBS = -L/cern/pro/lib -lmathlib -lpacklib -lkernlib \
             -L/usr/lib -lnsl
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  FC = f77
  CFLAGS := $(CFLAGS) -I/usr/include $(INCLUDE_DIR) -Olimit 1600 -nofor_main \
           -shared -newc -ieee_with_no_inexact
  CERNLIBS = -L/CERN/pro/lib -lmathlib -lpacklib -lkernlib
endif



OBJS = ctsminuit.o

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(FC) $(CFLAGS) -o $@ $(OBJS) $(LIBS) $(CERNLIBS)

$(OBJS): ctsminuit.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
