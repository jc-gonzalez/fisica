# Makefile -- Small makefile for just compiling and installing ranlib 
#
# JCG - Feb 2000

CFLAGS=-I$(HOME)/include -O0 -g3 -ieee

LIBS=libranlib.a

HEADERS=src/ranlib.h
SOURCES=linpack/linpack.c src/com.c src/ranlib.c 
OBJS=linpack/linpack.o src/com.o src/ranlib.o

all: includes preparelib libs clean

includes: 
	echo making include directory . . .
	if [ ! -d $(HOME)/include ];then  mkdir $(HOME)/include;fi
	cp $(HEADERS) $(HOME)/include/ 
	echo done.

preparelib:
	if [ ! -d $(HOME)/lib ];then  mkdir $(HOME)/lib;fi

libs: $(LIBS)

$(LIBS): $(OBJS)
	-ar q $(LIBS) $(OBJS)
	mv $(LIBS) $(HOME)/lib/
	echo done $@ . . .

clean:
	echo cleanning . . .
	find . -name "*.o" -exec rm {} \;



