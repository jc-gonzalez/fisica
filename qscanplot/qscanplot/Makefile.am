####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = qscanplot
qscanplot_SOURCES = dlgCurvesOrder.ui dlgaxispoint.ui daxispoint.cpp axes.cpp pointlist.cpp curve.cpp axispoint.cpp curvepoint.cpp figurepoint.cpp gpoint.cpp point.cpp qsp.cpp qscanplotview.cpp qscanplotdoc.cpp qscanplot.cpp main.cpp 
SOURCES = daxispoint.cpp axes.cpp pointlist.cpp curve.cpp axispoint.cpp curvepoint.cpp figurepoint.cpp gpoint.cpp point.cpp qsp.cpp qscanplotview.cpp qscanplotdoc.cpp qscanplot.cpp main.cpp 
INTERFACES = dlgCurvesOrder.ui dlgaxispoint.ui 
TRANSLATIONS = 
KDE_OPTIONS = qtonly
qscanplot_LDADD   = -lqt -lm -lXext -lX11 $(LIBSOCKET)

SUBDIRS = docs 

EXTRA_DIST = main.cpp qscanplot.cpp qscanplot.h qscanplotdoc.cpp qscanplotdoc.h qscanplotview.cpp qscanplotview.h resource.h qscanplot.xpm mini-qscanplot.xpm filenew.xpm filesave.xpm fileopen.xpm qsp.cpp qsp.h point.cpp point.h gpoint.cpp gpoint.h figurepoint.cpp figurepoint.h curvepoint.cpp curvepoint.h axispoint.cpp axispoint.h curve.cpp curve.h pointlist.cpp pointlist.h axes.cpp axes.h daxispoint.cpp daxispoint.h dlgaxispoint.ui dlgCurvesOrder.ui 

####### kdevelop will overwrite this part!!! (end)############


# set the include path for X, qt and KDE
INCLUDES= $(all_includes)

qscanplot_METASOURCES = AUTO

# the library search path. 
qscanplot_LDFLAGS = $(all_libraries) 

# them while "make clean", use CLEANFILES
DISTCLEANFILES = $(qscanplot_METASOURCES)




