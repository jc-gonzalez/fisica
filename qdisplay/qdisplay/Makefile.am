####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = qdisplay
qdisplay_SOURCES = qdisplayview.cpp qdisplaydoc.cpp qdisplay.cpp main.cpp 
qdisplay_LDADD   =  -lqt -lXext -lX11 $(LIBSOCKET)

SUBDIRS = docs 

EXTRA_DIST = main.cpp qdisplay.cpp qdisplay.h qdisplaydoc.cpp qdisplaydoc.h qdisplayview.cpp qdisplayview.h resource.h qdisplay.xpm mini-qdisplay.xpm filenew.xpm filesave.xpm fileopen.xpm 

####### kdevelop will overwrite this part!!! (end)############


# set the include path for X, qt and KDE
INCLUDES= $(all_includes)

qdisplay_METASOURCES = AUTO

# the library search path. 
qdisplay_LDFLAGS = $(all_libraries) 

# them while "make clean", use CLEANFILES
DISTCLEANFILES = $(qdisplay_METASOURCES)




