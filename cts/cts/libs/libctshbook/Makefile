#
# Makefile for library libctshbook.so
#
# author: D. Kranich, 23.06.99
#

PROGRAM = libctshbook.so

DESTDIR = .


#
# set compiler and flags according to OS
#

# default
#
FC = f77
INCLUDE_DIR := $(INCLUDE_DIR) -I$(HOME)/lib/include
CFLAGS := $(CFLAGS) -shared $(INCLUDE_DIR)

# linux
#
ifeq (linux-gnu, $(findstring linux-gnu, $(OSTYPE)))
  CC = gcc
  FC = g77
  CFLAGS := $(CFLAGS) -pedantic -Wall $(INCLUDE_DIR) -shared \
            -DUSE_LINKS
  CERNLIBS = -L/cern/pro/lib -lmathlib -lpacklib -lkernlib \
             -L/usr/lib -lnsl -lg2c
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  FC = f77
  CFLAGS := $(CFLAGS) -I/usr/include $(INCLUDE_DIR) -Olimit 1600 \
            -shared -newc -ieee_with_no_inexact -DUSE_LINKS
  CERNLIBS = -L/CERN/pro/lib -lmathlib -lpacklib -lkernlib -lUfor
endif



OBJS = ctshbook.o

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(FC) $(CFLAGS) -o $@ $(OBJS) $(CERNLIBS)

$(OBJS): ctshbook.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
