#
# Makefile for program pixdis
#
# author: D. Kranich, 05.02.99
#

PROGRAM = pixdis

DESTDIR = .

ifndef LIBS
LIBS := $(HOME)/lib/libctsbase.so \
	$(HOME)/lib/libctshbook.so
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
            -newc -std1 -g -ieee_with_no_inexact -rpath $(HOME)/lib
endif


OBJS = pixdis.o \
       get_pixdis.o


all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)


$(OBJS): pixdis.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
