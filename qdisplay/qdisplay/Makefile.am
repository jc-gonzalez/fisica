####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = qdisplay
qdisplay_SOURCES = qdisplaydoc.cpp qrunhistdata.cpp qrunhist.cpp qprintoptionsdata.cpp qprintoptions.cpp infinitearray.cpp qdisplayview.cpp qdisplay.cpp main.cpp 
qdisplay_LDADD   = -L/home/gonzalez/lib -lqwt -lm -lqt -lXext -lX11 $(LIBSOCKET)

SUBDIRS = docs 

EXTRA_DIST = main.cpp qdisplay.cpp qdisplay.h qdisplayview.cpp qdisplayview.h resource.h qdisplay.xpm mini-qdisplay.xpm filenew.xpm filesave.xpm fileopen.xpm evthdr.h infinitearray.cpp infinitearray.h pixels_simplearray.h printopt.kdevdlg prncolor.xpm qprintoptions.cpp qprintoptions.h qprintoptionsdata.cpp qrunhist.cpp qrunhist.h qrunhistdata.cpp runhist.kdevdlg fileprint.xpm filequit.xpm qdisplaydoc.cpp qdisplaydoc.h Black2White Blues4 Firecode White2Black palette.awk 

####### kdevelop will overwrite this part!!! (end)############


# set the include path for X, qt and KDE
INCLUDES= $(all_includes)

qdisplay_METASOURCES = AUTO

# the library search path. 
qdisplay_LDFLAGS = $(all_libraries) 

# them while "make clean", use CLEANFILES
DISTCLEANFILES = $(qdisplay_METASOURCES)




