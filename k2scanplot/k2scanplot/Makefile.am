####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = k2scanplot
k2scanplot_SOURCES = k2scanplotview.cpp k2scanplotdoc.cpp k2scanplot.cpp main.cpp 
k2scanplot_LDADD   =  -lkfile -lkdeui -lkdecore $(LIB_QT) $(LIBSOCKET)


EXTRA_DIST = main.cpp k2scanplot.cpp k2scanplot.h k2scanplotdoc.cpp k2scanplotdoc.h k2scanplotview.cpp k2scanplotview.h k2scanplotui.rc k2scanplot.desktop k2scanplot.xpm mini-k2scanplot.xpm 

install-data-local:
	$(mkinstalldirs) $(kde_appsdir)/Applications/
	$(INSTALL_DATA) k2scanplot.desktop $(kde_appsdir)/Applications/k2scanplot.desktop
	$(mkinstalldirs) $(kde_icondir)/medium/locolor/apps/
	$(INSTALL_DATA) k2scanplot.xpm $(kde_icondir)/medium/locolor/apps/k2scanplot.xpm
	$(mkinstalldirs) $(kde_icondir)/small/locolor/apps/
	$(INSTALL_DATA) mini-k2scanplot.xpm $(kde_icondir)/small/locolor/apps/k2scanplot.xpm

uninstall-local:
	-rm -f $(kde_appsdir)/Applications/k2scanplot.desktop
	-rm -f $(kde_icondir)/medium/locolor/apps/k2scanplot.xpm
	-rm -f $(kde_icondir)/small/locolor/apps/k2scanplot.xpm

####### kdevelop will overwrite this part!!! (end)############
# this 10 paths are KDE specific. Use them:
# kde_htmldir       Where your docs should go to. (contains lang subdirs)
# kde_appsdir       Where your application file (.kdelnk) should go to. 
# kde_icondir       Where your icon should go to.
# kde_minidir       Where your mini icon should go to.
# kde_datadir       Where you install application data. (Use a subdir)
# kde_locale        Where translation files should go to.(contains lang subdirs)
# kde_cgidir        Where cgi-bin executables should go to.
# kde_confdir       Where config files should go to.
# kde_mimedir       Where mimetypes should go to.
# kde_toolbardir    Where general toolbar icons should go to.
# kde_wallpaperdir  Where general wallpapers should go to.

# set the include path for X, qt and KDE
INCLUDES= $(all_includes)

METASOURCES = AUTO

# the library search path. 
k2scanplot_LDFLAGS = $(all_libraries) $(KDE_RPATH)

rcdir = $(kde_datadir)/k2scanplot
rc_DATA = k2scanplotui.rc

messages: rc.cpp
	LIST=`find . -name \*.h -o -name \*.hh -o -name \*.H -o -name \*.hxx -o -name \*.hpp -o -name \*.cpp -o -name \*.cc -o -name \*.cxx -o -name \*.ecpp -o -name \*.C`; \
	if test -n "$$LIST"; then \
	 $(XGETTEXT) $$LIST -o $(podir)/k2scanplot.pot; \
	fi

