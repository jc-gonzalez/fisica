####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = qscanplot
qscanplot_SOURCES = qscanplotview.cpp qscanplotdoc.cpp qscanplot.cpp main.cpp 
qscanplot_LDADD   =  -lqt -lXext -lX11 $(LIBSOCKET)

SUBDIRS = docs 

EXTRA_DIST = main.cpp qscanplot.cpp qscanplot.h qscanplotdoc.cpp qscanplotdoc.h qscanplotview.cpp qscanplotview.h resource.h qscanplot.xpm mini-qscanplot.xpm filenew.xpm filesave.xpm fileopen.xpm 

####### kdevelop will overwrite this part!!! (end)############


# set the include path for X, qt and KDE
INCLUDES= $(all_includes)

qscanplot_METASOURCES = AUTO

# the library search path. 
qscanplot_LDFLAGS = $(all_libraries) 

# them while "make clean", use CLEANFILES
DISTCLEANFILES = $(qscanplot_METASOURCES)




