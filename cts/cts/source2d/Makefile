#
# Makefile for program  source2d
#
# author: D. Kranich, 22.06.99
#

PROGRAM = source2d

DESTDIR = .

ifndef LIBS
LIBS =	$(HOME)/lib/libctshbook.so \
	$(HOME)/lib/libctsbase.so \
	$(HOME)/lib/libctsmath.so \
	$(HOME)/lib/libctscuts.so \
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
  CFLAGS := $(CFLAGS) -pedantic -Wall $(INCLUDE_DIR)
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  CFLAGS := $(CFLAGS) -I/usr/include $(INCLUDE_DIR) -Olimit 1600 \
            -newc -ieee_with_no_inexact -rpath $(HOME)/lib
endif



OBJS = source2d.o

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

$(OBJS): source2d.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
