#
# Makefile for library libctshbplot.so
#
# author: D. Kranich, 06.08.99
#

PROGRAM = libctshplot.so

DESTDIR = .


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
  CFLAGS := $(CFLAGS) -pedantic -Wall $(INCLUDE_DIR) -shared \
            -DUSE_LINKS
  CERNLIBS = -L/cern/pro/lib -lgraflib -lgrafX11 -lpacklib -lkernlib \
             -L/usr/X11R6/lib -lX11
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  FC = f77
  CFLAGS := $(CFLAGS) -I/usr/include $(INCLUDE_DIR) -Olimit 1600 \
            -shared -newc -ieee_with_no_inexact -DUSE_LINK
  CERNLIBS = -L/CERN/pro/lib -lgraflib -lgrafX11 -lpacklib -lkernlib \
             -L/usr/X11R6/lib -lX11
endif



OBJS = ctshplot.o

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(FC) $(CFLAGS) -o $@ $(OBJS) $(CERNLIBS)

$(OBJS): ctshplot.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
