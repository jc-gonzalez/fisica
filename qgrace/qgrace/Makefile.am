####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = qgrace
qgrace_SOURCES = qgraceview.cpp qgracedoc.cpp qgrace.cpp main.cpp 
SOURCES = qgraceview.cpp qgracedoc.cpp qgrace.cpp main.cpp 
INTERFACES = 
TRANSLATIONS = 
KDE_OPTIONS = qtonly
qgrace_LDADD   =  -lqt -lXext -lX11 $(LIBSOCKET)

SUBDIRS = docs 

EXTRA_DIST = main.cpp qgrace.cpp qgrace.h qgracedoc.cpp qgracedoc.h qgraceview.cpp qgraceview.h qgrace.xpm mini-qgrace.xpm filenew.xpm filesave.xpm fileopen.xpm 

####### kdevelop will overwrite this part!!! (end)############


# set the include path for X, qt and KDE
INCLUDES= $(all_includes)

qgrace_METASOURCES = AUTO

# the library search path. 
qgrace_LDFLAGS = $(all_libraries) 

# them while "make clean", use CLEANFILES
DISTCLEANFILES = $(qgrace_METASOURCES)

# this is to enable Qt behavoir of am_edit
KDE_OPTIONS = qtonly



