####### kdevelop will overwrite this part!!! (begin)##########
bin_PROGRAMS = kscanplot
kscanplot_SOURCES = kscanplotview.cpp kscanplotdoc.cpp kscanplot.cpp main.cpp 
kscanplot_LDADD   =  -lkfile -lkdeui -lkdecore -lqt -lXext -lX11 $(LIBSOCKET)


EXTRA_DIST = main.cpp kscanplot.cpp kscanplot.h kscanplotdoc.cpp kscanplotdoc.h kscanplotview.cpp kscanplotview.h resource.h kscanplot.desktop kscanplot.xpm mini-kscanplot.xpm 

install-data-local:
	$(mkinstalldirs) $(kde_appsdir)/Applications/
	$(INSTALL_DATA) kscanplot.desktop $(kde_appsdir)/Applications/kscanplot.desktop
	$(mkinstalldirs) $(kde_icondir)/medium/locolor/apps/
	$(INSTALL_DATA) kscanplot.xpm $(kde_icondir)/medium/locolor/apps/kscanplot.xpm
	$(mkinstalldirs) $(kde_icondir)/small/locolor/apps/
	$(INSTALL_DATA) mini-kscanplot.xpm $(kde_icondir)/small/locolor/apps/kscanplot.xpm

uninstall-local:
	-rm -f $(kde_appsdir)/Applications/kscanplot.desktop
	-rm -f $(kde_icondir)/medium/locolor/apps/kscanplot.xpm
	-rm -f $(kde_icondir)/small/locolor/apps/kscanplot.xpm

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

kscanplot_METASOURCES = AUTO

# the library search path. 
kscanplot_LDFLAGS = $(all_libraries) $(KDE_RPATH)

# them while "make clean", use CLEANFILES
DISTCLEANFILES = $(kscanplot_METASOURCES)

# make messages.po. Move this one to ../po/ and "make merge" in po
# the -x is for skipping messages already translated in kdelibs
messages:
	LIST=`find . -name \*.h -o -name \*.hh -o -name \*.H -o -name \*.hxx -o -name \*.hpp -o -name \*.cpp -o -name \*.cc -o -name \*.cxx -o -name \*.ecpp -o -name \*.C`; \
	if test -n "$$LIST"; then \
	 $(XGETTEXT) -C -ki18n -x $(kde_includes)/kde.pot $$LIST -o ../po/kscanplot.pot; \
	fi

