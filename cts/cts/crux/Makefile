#
# Makefile for program crux
#
# author: D. Kranich, 20.11.99
#

PROGRAM = crux

DESTDIR = .

ifndef LIBS
LIBS := $(HOME)/lib/libctsbase.so \
	$(HOME)/lib/libctsmath.so \
	$(HOME)/lib/libctsminuit.so \
	$(HOME)/lib/libctshbook.so \
	$(HOME)/lib/libctshplot.so \
	$(HOME)/lib/libctscuts.so -lm
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


OBJS =  crux.o \
	get_efun.o \
	get_ifun.o \
	plot_res.o


all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)


$(OBJS): crux.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
