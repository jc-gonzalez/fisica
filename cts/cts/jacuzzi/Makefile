#
# Makefile for program jacuzzi
#
# author: D. Kranich, 28.10.99
#

PROGRAM = jacuzzi

DESTDIR = .

ifndef LIBS
LIBS := $(HOME)/lib/libctsbase.so \
	$(HOME)/lib/libctsmath.so \
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
  LFLAGS := -Xlinker -rpath -Xlinker $(HOME)/lib
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  CFLAGS := $(CFLAGS) -I/usr/include $(INCLUDE_DIR) -Olimit 1600 \
            -newc -std1 -g -ieee_with_no_inexact -rpath $(HOME)/lib
endif


OBJS = jacuzzi.o \
       obs_time.o \
       jacuzzi_plot.o


all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS) $(LFLAGS)


$(OBJS): jacuzzi.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
