/***************************************************************************
                          qdisplay.h  -  description
                             -------------------
    begin                : mié ene 12 22:01:32 CET 2000
    copyright            : (C) 2000 by Jose Carlos Gonzalez
    email                : gonzalez@mppmu.mpg.de
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef QDISPLAY_H
#define QDISPLAY_H

// include files for QT
#include <qstring.h>
#include <qpopupmenu.h>
#include <qmainwindow.h>
#include <qaccel.h>
#include <qmenubar.h>
#include <qtoolbar.h>
#include <qpixmap.h>
#include <qtoolbutton.h>
#include <qapp.h>
#include <qstatusbar.h>
#include <qwhatsthis.h>
#include <qmsgbox.h>
#include <qfiledialog.h>
#include <qprinter.h>
#include <qpainter.h>

// application specific includes
#include "qdisplayview.h"
#include "qdisplaydoc.h"
#include "resource.h"

#include "qprintoptions.h"

/**
  * This Class is the base class for your application. It sets up the main
  * window and providing a menubar, toolbar
  * and statusbar. For the main view, an instance of class QDisplayView is
  * created which creates your view.
  */
class QDisplay : public QMainWindow
{
  Q_OBJECT
  friend class QCamera;

  public:
    /** construtor */
    QDisplay();
    /** destructor */
    ~QDisplay();
    /** initMenuBar creates the menu_bar and inserts the menuitems */
    void initMenuBar();
    /** this creates the toolbars. Change the toobar look and add new toolbars in this
     * function */
    void initToolBar();
    /** setup the statusbar */
    void initStatusBar();
    /** setup the document*/
    void initDoc();
    /** setup the mainview*/
    void initView();
  
    /** overloaded for Message box on last window exit */
    bool queryExit();

  public slots:
  
    /** switch argument for Statusbar help entries on slot selection */
    void statusCallback(int id_);
    /** open a new application window */
  
    /** open a document */
    void slotFileOpen();
    /** save a document */
    void slotFileSaveSelected();
    /** close the actual file */
    void slotFileClose();
    /** print the actual view */
    void slotFilePrint();
    /** exits the application */
    void slotFileQuit();

    /** toggle the toolbar*/
    void slotViewToolBar();
    /** toggle the statusbar*/
    void slotViewStatusBar();
    /** toggle the statusbar*/
    void slotViewPalette();

    /** toggle printing uses color*/
    void slotOptPrintInColor();
    /** set printing options*/
    void slotOptPrintopt();
    /** display Cherenkov Signal*/
    void slotOptDisplaySignal();
    /** display Cherenkov Signal*/
    void slotOptDisplayTimes();

    /** shows an about dlg*/
    void slotHelpAbout();
  
    /** change the status message of the whole statusbar temporary */
    void slotStatusHelpMsg(const QString &text);
  
  private:

    /** view is the main widget which represents your working area. The View
     * class should handle all events of the view widget.  It is kept empty so
     * you can create your view according to your application's needs by
     * changing the view class.
     */
    QDisplayView *view;
    /** doc represents your actual document and is created only once. It keeps
     * information such as filename and does the serialization of your files.
     */
    QDisplayDoc *doc;
  
    /** file_menu contains all items of the menubar entry "File" */
    QPopupMenu *fileMenu;
    /** view_menu contains all items of the menubar entry "View" */
    QPopupMenu *viewMenu;
    /** opt_menu contains all items of the menubar entry "Options" */
    QPopupMenu *optMenu;
    /** view_menu contains all items of the menubar entry "Help" */
    QPopupMenu *helpMenu;

    QToolBar *fileToolbar;

    QToolButton *filePrintColor;
    QPrintOptions *printOpt;

};
#endif 

