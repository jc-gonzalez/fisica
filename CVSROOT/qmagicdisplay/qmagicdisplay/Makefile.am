####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = qmagicdisplay
qmagicdisplay_SOURCES = qmagicdisplayview.cpp qmagicdisplaydoc.cpp qmagicdisplay.cpp main.cpp 
qmagicdisplay_LDADD   =  -lqt -lXext -lX11 $(LIBSOCKET)

SUBDIRS = docs 

EXTRA_DIST = main.cpp qmagicdisplay.cpp qmagicdisplay.h qmagicdisplaydoc.cpp qmagicdisplaydoc.h qmagicdisplayview.cpp qmagicdisplayview.h resource.h qmagicdisplay.xpm mini-qmagicdisplay.xpm filenew.xpm filesave.xpm fileopen.xpm 

####### kdevelop will overwrite this part!!! (end)############


# set the include path for X, qt and KDE
INCLUDES= $(all_includes)

qmagicdisplay_METASOURCES = AUTO

# the library search path. 
qmagicdisplay_LDFLAGS = $(all_libraries) 

# them while "make clean", use CLEANFILES
DISTCLEANFILES = $(qmagicdisplay_METASOURCES)




