#
# Makefile for library libctscuts.so
#
# author: D. Kranich, 23.06.99
#

PROGRAM = libctscuts.so

DESTDIR = .


#
# set compiler and flags according to OS
#

# default
#
CC = cc
INCLUDE_DIR := $(INCLUDE_DIR) -I$(HOME)/lib/include
CFLAGS := $(CFLAGS) -shared $(INCLUDE_DIR)

# linux
#
ifeq (linux-gnu, $(findstring linux-gnu, $(OSTYPE)))
  CC = gcc
  CFLAGS := $(CFLAGS) -pedantic -Wall $(INCLUDE_DIR) -shared
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  CFLAGS := $(CFLAGS) -I/usr/include $(INCLUDE_DIR) -Olimit 1600 \
            -shared -newc -ieee_with_no_inexact -rpath $(HOME)/lib
endif



OBJS =	ctscuts.o   \
	sort.o   \
	fill.o   \
	get_zab.o


all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

$(OBJS): ctscuts.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
