#
# Makefile for library libcctsbase.so
#
# author: D. Kranich, 23.06.99
#

PROGRAM = libctsbase.so

DESTDIR = .

LIBS := -lm


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
  CFLAGS := $(CFLAGS) -pedantic -Wall -shared $(INCLUDE_DIR)
endif

# ALPHA OSF
#
ifeq (osf, $(findstring osf, $(OSTYPE)))
  CC = cc
  CFLAGS := $(CFLAGS) -I. -I/usr/include $(INCLUDE_DIR) -Olimit 1600 \
            -newc -ieee_with_no_inexact -shared
endif



OBJS = ctsbase.o

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

$(OBJS): ctsbase.h


clean:
	rm -f *.o *.BAK $(PROGRAM) *~ core
