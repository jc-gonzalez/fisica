
MAKE=gnumake

CC=gcc
F77=g77
GTKCONFIG=/usr/bin/gtk-config
GTK_CFLAGS := $(shell ${GTKCONFIG} --cflags)
GTK_LIBS := $(shell ${GTKCONFIG} --libs)

CFLAGS=${GTK_CFLAGS} -I. -I../include-util -g3
LIBS=${GTK_LIBS} -g3

PROGRAM=ctv

SRCS = ctv.c menuf.c evt.c mcevh.c analysis.c
OBJS := $(subst .c,.o,${SRCS})
HEADERS := $(subst .c,.h,${SRCS}) pixelsinfo.h jcmacros.h

all: ${PROGRAM}

${PROGRAM}: ${OBJS}
	$(CC) ${OBJS} ${CFLAGS} ${LIBS} -o $@ 

.c.o:
	$(CC) -c $< ${CFLAGS} -o $@ 

redo: cleaner all

cleaner: clean
	-rm ${PROGRAM}

clean:
	-rm ${OBJS} core

list:
	@echo "SRCS: " ${SRCS}
	@echo "OBJS: " ${OBJS}
	@echo "HEADERS: " ${HEADERS}

depend:
	makedepend $(SRCS) -fMakefile 2> /dev/null

#------------------------------------------------------------

# DO NOT DELETE THIS LINE -- make depend depends on it.


analysis.o: /usr/include/_G_config.h /usr/include/alloca.h
analysis.o: /usr/include/bits/endian.h /usr/include/bits/huge_val.h
analysis.o: /usr/include/bits/mathcalls.h /usr/include/bits/mathdef.h
analysis.o: /usr/include/bits/nan.h /usr/include/bits/pthreadtypes.h
analysis.o: /usr/include/bits/sched.h /usr/include/bits/select.h
analysis.o: /usr/include/bits/sigset.h /usr/include/bits/stdio_lim.h
analysis.o: /usr/include/bits/types.h /usr/include/endian.h
analysis.o: /usr/include/features.h /usr/include/gnu/stubs.h
analysis.o: /usr/include/libio.h /usr/include/math.h /usr/include/stdio.h
analysis.o: /usr/include/stdlib.h /usr/include/string.h
analysis.o: /usr/include/sys/cdefs.h /usr/include/sys/select.h
analysis.o: /usr/include/sys/sysmacros.h /usr/include/sys/types.h
analysis.o: /usr/include/time.h /usr/include/xlocale.h
analysis.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
analysis.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
analysis.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
analysis.o: analysis.h
ctv.o: /usr/include/_G_config.h /usr/include/alloca.h
ctv.o: /usr/include/bits/endian.h /usr/include/bits/huge_val.h
ctv.o: /usr/include/bits/local_lim.h /usr/include/bits/mathcalls.h
ctv.o: /usr/include/bits/mathdef.h /usr/include/bits/nan.h
ctv.o: /usr/include/bits/posix1_lim.h /usr/include/bits/posix2_lim.h
ctv.o: /usr/include/bits/pthreadtypes.h /usr/include/bits/sched.h
ctv.o: /usr/include/bits/select.h /usr/include/bits/sigset.h
ctv.o: /usr/include/bits/stdio_lim.h /usr/include/bits/types.h
ctv.o: /usr/include/bits/xopen_lim.h /usr/include/endian.h
ctv.o: /usr/include/features.h /usr/include/gdk/gdk.h
ctv.o: /usr/include/gdk/gdkcursors.h /usr/include/gdk/gdkrgb.h
ctv.o: /usr/include/gdk/gdktypes.h /usr/include/glib.h
ctv.o: /usr/include/glibconfig.h /usr/include/gnu/stubs.h
ctv.o: /usr/include/gtk/gtk.h /usr/include/gtk/gtkaccelgroup.h
ctv.o: /usr/include/gtk/gtkaccellabel.h /usr/include/gtk/gtkadjustment.h
ctv.o: /usr/include/gtk/gtkalignment.h /usr/include/gtk/gtkarg.h
ctv.o: /usr/include/gtk/gtkarrow.h /usr/include/gtk/gtkaspectframe.h
ctv.o: /usr/include/gtk/gtkbbox.h /usr/include/gtk/gtkbin.h
ctv.o: /usr/include/gtk/gtkbindings.h /usr/include/gtk/gtkbox.h
ctv.o: /usr/include/gtk/gtkbutton.h /usr/include/gtk/gtkcalendar.h
ctv.o: /usr/include/gtk/gtkcheckbutton.h /usr/include/gtk/gtkcheckmenuitem.h
ctv.o: /usr/include/gtk/gtkclist.h /usr/include/gtk/gtkcolorsel.h
ctv.o: /usr/include/gtk/gtkcombo.h /usr/include/gtk/gtkcompat.h
ctv.o: /usr/include/gtk/gtkcontainer.h /usr/include/gtk/gtkctree.h
ctv.o: /usr/include/gtk/gtkcurve.h /usr/include/gtk/gtkdata.h
ctv.o: /usr/include/gtk/gtkdebug.h /usr/include/gtk/gtkdialog.h
ctv.o: /usr/include/gtk/gtkdnd.h /usr/include/gtk/gtkdrawingarea.h
ctv.o: /usr/include/gtk/gtkeditable.h /usr/include/gtk/gtkentry.h
ctv.o: /usr/include/gtk/gtkenums.h /usr/include/gtk/gtkeventbox.h
ctv.o: /usr/include/gtk/gtkfeatures.h /usr/include/gtk/gtkfilesel.h
ctv.o: /usr/include/gtk/gtkfixed.h /usr/include/gtk/gtkfontsel.h
ctv.o: /usr/include/gtk/gtkframe.h /usr/include/gtk/gtkgamma.h
ctv.o: /usr/include/gtk/gtkgc.h /usr/include/gtk/gtkhandlebox.h
ctv.o: /usr/include/gtk/gtkhbbox.h /usr/include/gtk/gtkhbox.h
ctv.o: /usr/include/gtk/gtkhpaned.h /usr/include/gtk/gtkhruler.h
ctv.o: /usr/include/gtk/gtkhscale.h /usr/include/gtk/gtkhscrollbar.h
ctv.o: /usr/include/gtk/gtkhseparator.h /usr/include/gtk/gtkimage.h
ctv.o: /usr/include/gtk/gtkinputdialog.h /usr/include/gtk/gtkitem.h
ctv.o: /usr/include/gtk/gtkitemfactory.h /usr/include/gtk/gtklabel.h
ctv.o: /usr/include/gtk/gtklayout.h /usr/include/gtk/gtklist.h
ctv.o: /usr/include/gtk/gtklistitem.h /usr/include/gtk/gtkmain.h
ctv.o: /usr/include/gtk/gtkmarshal.h /usr/include/gtk/gtkmenu.h
ctv.o: /usr/include/gtk/gtkmenubar.h /usr/include/gtk/gtkmenufactory.h
ctv.o: /usr/include/gtk/gtkmenuitem.h /usr/include/gtk/gtkmenushell.h
ctv.o: /usr/include/gtk/gtkmisc.h /usr/include/gtk/gtknotebook.h
ctv.o: /usr/include/gtk/gtkobject.h /usr/include/gtk/gtkoptionmenu.h
ctv.o: /usr/include/gtk/gtkpacker.h /usr/include/gtk/gtkpaned.h
ctv.o: /usr/include/gtk/gtkpixmap.h /usr/include/gtk/gtkplug.h
ctv.o: /usr/include/gtk/gtkpreview.h /usr/include/gtk/gtkprogress.h
ctv.o: /usr/include/gtk/gtkprogressbar.h /usr/include/gtk/gtkradiobutton.h
ctv.o: /usr/include/gtk/gtkradiomenuitem.h /usr/include/gtk/gtkrange.h
ctv.o: /usr/include/gtk/gtkrc.h /usr/include/gtk/gtkruler.h
ctv.o: /usr/include/gtk/gtkscale.h /usr/include/gtk/gtkscrollbar.h
ctv.o: /usr/include/gtk/gtkscrolledwindow.h /usr/include/gtk/gtkselection.h
ctv.o: /usr/include/gtk/gtkseparator.h /usr/include/gtk/gtksignal.h
ctv.o: /usr/include/gtk/gtksocket.h /usr/include/gtk/gtkspinbutton.h
ctv.o: /usr/include/gtk/gtkstatusbar.h /usr/include/gtk/gtkstyle.h
ctv.o: /usr/include/gtk/gtktable.h /usr/include/gtk/gtktearoffmenuitem.h
ctv.o: /usr/include/gtk/gtktext.h /usr/include/gtk/gtkthemes.h
ctv.o: /usr/include/gtk/gtktipsquery.h /usr/include/gtk/gtktogglebutton.h
ctv.o: /usr/include/gtk/gtktoolbar.h /usr/include/gtk/gtktooltips.h
ctv.o: /usr/include/gtk/gtktree.h /usr/include/gtk/gtktreeitem.h
ctv.o: /usr/include/gtk/gtktypebuiltins.h /usr/include/gtk/gtktypeutils.h
ctv.o: /usr/include/gtk/gtkvbbox.h /usr/include/gtk/gtkvbox.h
ctv.o: /usr/include/gtk/gtkviewport.h /usr/include/gtk/gtkvpaned.h
ctv.o: /usr/include/gtk/gtkvruler.h /usr/include/gtk/gtkvscale.h
ctv.o: /usr/include/gtk/gtkvscrollbar.h /usr/include/gtk/gtkvseparator.h
ctv.o: /usr/include/gtk/gtkwidget.h /usr/include/gtk/gtkwindow.h
ctv.o: /usr/include/libio.h /usr/include/limits.h /usr/include/linux/limits.h
ctv.o: /usr/include/math.h /usr/include/stdio.h /usr/include/stdlib.h
ctv.o: /usr/include/string.h /usr/include/sys/cdefs.h
ctv.o: /usr/include/sys/select.h /usr/include/sys/sysmacros.h
ctv.o: /usr/include/sys/types.h /usr/include/time.h /usr/include/xlocale.h
ctv.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
ctv.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/limits.h
ctv.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
ctv.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
ctv.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/syslimits.h
ctv.o: arrow_down.xpm arrow_up.xpm ctv.h evt.h mcevh.h menuf.h palette.xpm
ctv.o: pixelsinfo.h
evt.o: /usr/include/_G_config.h /usr/include/alloca.h
evt.o: /usr/include/bits/endian.h /usr/include/bits/huge_val.h
evt.o: /usr/include/bits/mathcalls.h /usr/include/bits/mathdef.h
evt.o: /usr/include/bits/nan.h /usr/include/bits/pthreadtypes.h
evt.o: /usr/include/bits/sched.h /usr/include/bits/select.h
evt.o: /usr/include/bits/sigset.h /usr/include/bits/stdio_lim.h
evt.o: /usr/include/bits/types.h /usr/include/endian.h /usr/include/features.h
evt.o: /usr/include/gnu/stubs.h /usr/include/libio.h /usr/include/math.h
evt.o: /usr/include/stdio.h /usr/include/stdlib.h /usr/include/string.h
evt.o: /usr/include/sys/cdefs.h /usr/include/sys/select.h
evt.o: /usr/include/sys/sysmacros.h /usr/include/sys/types.h
evt.o: /usr/include/time.h /usr/include/xlocale.h
evt.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
evt.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
evt.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h evt.h mcevh.h
mcevh.o: /usr/include/_G_config.h /usr/include/alloca.h
mcevh.o: /usr/include/bits/endian.h /usr/include/bits/huge_val.h
mcevh.o: /usr/include/bits/mathcalls.h /usr/include/bits/mathdef.h
mcevh.o: /usr/include/bits/nan.h /usr/include/bits/pthreadtypes.h
mcevh.o: /usr/include/bits/sched.h /usr/include/bits/select.h
mcevh.o: /usr/include/bits/sigset.h /usr/include/bits/stdio_lim.h
mcevh.o: /usr/include/bits/types.h /usr/include/endian.h
mcevh.o: /usr/include/features.h /usr/include/gnu/stubs.h /usr/include/libio.h
mcevh.o: /usr/include/math.h /usr/include/stdio.h /usr/include/stdlib.h
mcevh.o: /usr/include/sys/cdefs.h /usr/include/sys/select.h
mcevh.o: /usr/include/sys/sysmacros.h /usr/include/sys/types.h
mcevh.o: /usr/include/time.h /usr/include/xlocale.h
mcevh.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
mcevh.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
mcevh.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h mcevh.h
menuf.o: /usr/include/bits/local_lim.h /usr/include/bits/posix1_lim.h
menuf.o: /usr/include/bits/posix2_lim.h /usr/include/bits/stdio_lim.h
menuf.o: /usr/include/bits/xopen_lim.h /usr/include/features.h
menuf.o: /usr/include/gdk/gdk.h /usr/include/gdk/gdkcursors.h
menuf.o: /usr/include/gdk/gdkrgb.h /usr/include/gdk/gdktypes.h
menuf.o: /usr/include/glib.h /usr/include/glibconfig.h
menuf.o: /usr/include/gnu/stubs.h /usr/include/gtk/gtk.h
menuf.o: /usr/include/gtk/gtkaccelgroup.h /usr/include/gtk/gtkaccellabel.h
menuf.o: /usr/include/gtk/gtkadjustment.h /usr/include/gtk/gtkalignment.h
menuf.o: /usr/include/gtk/gtkarg.h /usr/include/gtk/gtkarrow.h
menuf.o: /usr/include/gtk/gtkaspectframe.h /usr/include/gtk/gtkbbox.h
menuf.o: /usr/include/gtk/gtkbin.h /usr/include/gtk/gtkbindings.h
menuf.o: /usr/include/gtk/gtkbox.h /usr/include/gtk/gtkbutton.h
menuf.o: /usr/include/gtk/gtkcalendar.h /usr/include/gtk/gtkcheckbutton.h
menuf.o: /usr/include/gtk/gtkcheckmenuitem.h /usr/include/gtk/gtkclist.h
menuf.o: /usr/include/gtk/gtkcolorsel.h /usr/include/gtk/gtkcombo.h
menuf.o: /usr/include/gtk/gtkcompat.h /usr/include/gtk/gtkcontainer.h
menuf.o: /usr/include/gtk/gtkctree.h /usr/include/gtk/gtkcurve.h
menuf.o: /usr/include/gtk/gtkdata.h /usr/include/gtk/gtkdebug.h
menuf.o: /usr/include/gtk/gtkdialog.h /usr/include/gtk/gtkdnd.h
menuf.o: /usr/include/gtk/gtkdrawingarea.h /usr/include/gtk/gtkeditable.h
menuf.o: /usr/include/gtk/gtkentry.h /usr/include/gtk/gtkenums.h
menuf.o: /usr/include/gtk/gtkeventbox.h /usr/include/gtk/gtkfeatures.h
menuf.o: /usr/include/gtk/gtkfilesel.h /usr/include/gtk/gtkfixed.h
menuf.o: /usr/include/gtk/gtkfontsel.h /usr/include/gtk/gtkframe.h
menuf.o: /usr/include/gtk/gtkgamma.h /usr/include/gtk/gtkgc.h
menuf.o: /usr/include/gtk/gtkhandlebox.h /usr/include/gtk/gtkhbbox.h
menuf.o: /usr/include/gtk/gtkhbox.h /usr/include/gtk/gtkhpaned.h
menuf.o: /usr/include/gtk/gtkhruler.h /usr/include/gtk/gtkhscale.h
menuf.o: /usr/include/gtk/gtkhscrollbar.h /usr/include/gtk/gtkhseparator.h
menuf.o: /usr/include/gtk/gtkimage.h /usr/include/gtk/gtkinputdialog.h
menuf.o: /usr/include/gtk/gtkitem.h /usr/include/gtk/gtkitemfactory.h
menuf.o: /usr/include/gtk/gtklabel.h /usr/include/gtk/gtklayout.h
menuf.o: /usr/include/gtk/gtklist.h /usr/include/gtk/gtklistitem.h
menuf.o: /usr/include/gtk/gtkmain.h /usr/include/gtk/gtkmarshal.h
menuf.o: /usr/include/gtk/gtkmenu.h /usr/include/gtk/gtkmenubar.h
menuf.o: /usr/include/gtk/gtkmenufactory.h /usr/include/gtk/gtkmenuitem.h
menuf.o: /usr/include/gtk/gtkmenushell.h /usr/include/gtk/gtkmisc.h
menuf.o: /usr/include/gtk/gtknotebook.h /usr/include/gtk/gtkobject.h
menuf.o: /usr/include/gtk/gtkoptionmenu.h /usr/include/gtk/gtkpacker.h
menuf.o: /usr/include/gtk/gtkpaned.h /usr/include/gtk/gtkpixmap.h
menuf.o: /usr/include/gtk/gtkplug.h /usr/include/gtk/gtkpreview.h
menuf.o: /usr/include/gtk/gtkprogress.h /usr/include/gtk/gtkprogressbar.h
menuf.o: /usr/include/gtk/gtkradiobutton.h /usr/include/gtk/gtkradiomenuitem.h
menuf.o: /usr/include/gtk/gtkrange.h /usr/include/gtk/gtkrc.h
menuf.o: /usr/include/gtk/gtkruler.h /usr/include/gtk/gtkscale.h
menuf.o: /usr/include/gtk/gtkscrollbar.h /usr/include/gtk/gtkscrolledwindow.h
menuf.o: /usr/include/gtk/gtkselection.h /usr/include/gtk/gtkseparator.h
menuf.o: /usr/include/gtk/gtksignal.h /usr/include/gtk/gtksocket.h
menuf.o: /usr/include/gtk/gtkspinbutton.h /usr/include/gtk/gtkstatusbar.h
menuf.o: /usr/include/gtk/gtkstyle.h /usr/include/gtk/gtktable.h
menuf.o: /usr/include/gtk/gtktearoffmenuitem.h /usr/include/gtk/gtktext.h
menuf.o: /usr/include/gtk/gtkthemes.h /usr/include/gtk/gtktipsquery.h
menuf.o: /usr/include/gtk/gtktogglebutton.h /usr/include/gtk/gtktoolbar.h
menuf.o: /usr/include/gtk/gtktooltips.h /usr/include/gtk/gtktree.h
menuf.o: /usr/include/gtk/gtktreeitem.h /usr/include/gtk/gtktypebuiltins.h
menuf.o: /usr/include/gtk/gtktypeutils.h /usr/include/gtk/gtkvbbox.h
menuf.o: /usr/include/gtk/gtkvbox.h /usr/include/gtk/gtkviewport.h
menuf.o: /usr/include/gtk/gtkvpaned.h /usr/include/gtk/gtkvruler.h
menuf.o: /usr/include/gtk/gtkvscale.h /usr/include/gtk/gtkvscrollbar.h
menuf.o: /usr/include/gtk/gtkvseparator.h /usr/include/gtk/gtkwidget.h
menuf.o: /usr/include/gtk/gtkwindow.h /usr/include/limits.h
menuf.o: /usr/include/linux/limits.h /usr/include/strings.h
menuf.o: /usr/include/sys/cdefs.h
menuf.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/float.h
menuf.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/limits.h
menuf.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stdarg.h
menuf.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/stddef.h
menuf.o: /usr/lib/gcc-lib/i486-suse-linux/2.95.2/include/syslimits.h ctv.h
