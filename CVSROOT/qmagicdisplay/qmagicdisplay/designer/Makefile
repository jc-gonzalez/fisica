########################################################################## 
##                                                                          
## Makefile for generation of source code from Designer UIs
##
## Copyright (C) 2001  J C Gonzalez
## gonzalez@gae.ucm.es
##
##------------------------------------------------------------------------
##
## Copyright (C) 2001 J C Gonzalez
##  
## This program is free software;  you can redistribute it and/or  modify
## it under the terms  of the GNU General  Public License as published by
## the Free Software Foundation; either version  2 of the License, or (at
## your option) any later version.
## 
## This piece of code is distributed in the hope  that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS
## FOR A PARTICULAR PURPOSE.
## 
## In no  event shall his author  be liable for  any special, incidental,
## indirect  or  consequential  damages  of any  kind,   or  any  damages
## whatsoever resulting from loss of use, data or profits, whether or not
## advised of the possibility of damage, and on  any theory of liability,
## arising out  of or in connection  with the use  or performance of this
## software. You've been warned.
##
########################################################################## 

# UI=myform

# clear SUFFIXES list
.SUFFIXES:

# define SUFFIXES
.SUFFIXES: .moc.cpp .design.moc.cpp .design.h .design.cpp .h .cpp .ui 

# define variables for build process

UISRC       = $(UI).ui

UICLASS    ?= $(UI)

CLASS       = $(UICLASS)
SUBCLASS    = $(UICLASS)

SOURCE	    = $(UI).design.cpp
HEADER	    = $(UI).design.h

SUBSOURCE   = $(UI).cpp
SUBHEADER   = $(UI).h

MOCSOURCE   = $(UI).moc.cpp
MOCSUBSOURCE= $(UI).design.moc.cpp

ALLSOURCES  = $(SOURCE) $(SUBSOURCE) $(MOCSOURCE) $(MOCSUBSOURCE)
ALLHEADERS  = $(HEADER) $(SUBHEADER)

OBJS        = $(addsuffix .o,$(basename $(ALLSOURCES)))

all: help

design: $(HEADER) $(SOURCE)

impl: $(SUBHEADER) $(SUBSOURCE)

moc: $(MOCSOURCE) $(MOCSUBSOURCE)

help:
	@echo ""
	@echo "Makefile for .ui files created with QT\'s Designer"
	@echo ""
	@echo "Usage:"
	@echo "       make design UI=<uifile> UICLASS=<class> :"
	@echo "                 Generate *.design.{h,cpp} files"
	@echo ""
	@echo "       make impl UI=<uifile> UICLASS=<class> :"
	@echo "                 Generate implementation (subclass) files"
	@echo ""
	@echo "       make moc UI=<uifile> UICLASS=<class> :"
	@echo "                 Generate Meta-Oject-Code files with moc"
	@echo ""
	@echo "       make [help]"
	@echo "                 Show this help "
	@echo ""
	@echo "The <uifile> should not contain de extension .ui."
	@echo "You can use more than one target, but always in the "
	@echo "order specified in this help."
	@echo ""

# create a .design.h file from a .ui file
$(HEADER) : $(UISRC)
	uic -o $@ $<

# create a .design.cpp file from a .ui file
$(SOURCE) : $(UISRC) $(HEADER)
	uic -o $@ -impl $(HEADER) $<

# create a .h file from a .design.h file
$(SUBHEADER) : $(UISRC) $(HEADER)
	uic -o $@ -subdecl $(SUBCLASS) $(HEADER) $(UISRC)

# create a .cpp file from a .h file
$(SUBSOURCE) : $(UISRC) $(SUBHEADER)
	uic -o $@ -subimpl $(SUBCLASS) $(SUBHEADER) $(UISRC)

# create a .moc.h file from a .h file
.h.moc.cpp:
	moc -o $@ $<



#uic -o myform.h myform.ui # generate header file
#uic -o myform.cpp -impl myform.h myform.ui # generate implementation file
#uic -o myformimpl.h -subdecl MyFormImpl myform.h myform.ui # generate subclass header file
#uic -o myformimpl.cpp -subimpl MyFormImpl myformimpl.h myform.ui # generate subclass implementation file
#edit myformimpl.h # edit subclass header file
#edit myformimpl.cpp # edit subclass implementation file
#moc -o moc_myform.cpp myform.h # generate moc code for base class
#moc -o moc_myformimpl.cpp myformimpl.h # generate moc code for subclass
#c++ -c myform.cpp -I$(QTDIR)/include # compile base class, use similar command on Windows
#c++ -c moc_myform.cpp -I$(QTDIR)/include # compile base class moc code, use similar command on Windows
#c++ -c myformimpl.cpp -I$(QTDIR)/include # compile subclass, use similar command on Windows
#c++ -c moc_myformimpl.cpp -I$(QTDIR)/include # compile subclass moc code, use similar command on Windows
# link everything together, use similar command on Windows
#c++ -o myform myform.o moc_myform.o myformimpl.o moc_myformimpl.o -L$(QTDIR)/lib -lqt 


# myform: myform.o moc_myform.o myformimpl.o moc_myformimpl.o
#         c++ -o $@ $+ -L$(QTDIR)/lib -lqt
# 
# myform.o:       myform.cpp myform.h
#         c++ -c $$< -I$(QTDIR)/include
# 
# myformimpl.o: myformimpl.cpp myformimpl.h myform.h
#         c++ -c $$< -I$(QTDIR)/include
# 
# moc_myform.o: moc_myform.cpp myform.h
#         c++ -c $$< -I$(QTDIR)/include
# 
# moc_myformimpl.o: moc_myformimpl.cpp myformimpl.h myform.h
#         c++ -c $$< -I$(QTDIR)/include
#      
# moc_myform.cpp: myform.h
#         moc -o $@ $<
#        
# moc_myformimpl.cpp: myformimpl.h
#         moc -o $@ $<
#      
# myform.h: myform.ui
#         uic -o $@ $<
# 
# myform.cpp: myform.ui myform.h
#         uic -o $@ -impl myform.h $<



# Local Variables:
# mode: makefile
# End:
#EOF
