/***************************************************************************
                          qdisplay.cpp  -  description
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

#include "qdisplay.h"
#include "filesave.xpm"
#include "fileopen.xpm"
#include "fileprint.xpm"
#include "prncolor.xpm"
#include "filequit.xpm"

#include "qrunhist.h"

QDisplay::QDisplay()
{
  setCaption("QDisplay " VERSION);
  
  ///////////////////////////////////////////////////////////////////
  // call inits to invoke all other construction parts
  initMenuBar();
  initToolBar();
  initStatusBar();

  initDoc();
  initView();  
}

QDisplay::~QDisplay()
{
}

void QDisplay::initMenuBar()
{
  ///////////////////////////////////////////////////////////////////
  // MENUBAR

  ///////////////////////////////////////////////////////////////////
  // menuBar entry fileMenu

  fileMenu=new QPopupMenu();
  fileMenu->insertItem("&Open...", this, SLOT(slotFileOpen()), CTRL+Key_O, ID_FILE_OPEN);
  fileMenu->insertItem("&Save selected...", this, SLOT(slotFileSaveSelected()), CTRL+Key_S, ID_FILE_SAVE_SELECTED);
  fileMenu->insertItem("&Close", this, SLOT(slotFileClose()), CTRL+Key_W, ID_FILE_CLOSE);
  fileMenu->insertSeparator();
  fileMenu->insertItem("&Print...", this, SLOT(slotFilePrint()), CTRL+Key_P, ID_FILE_PRINT);
  fileMenu->insertSeparator();
  fileMenu->insertItem("E&xit", this, SLOT(slotFileQuit()), CTRL+Key_Q, ID_FILE_QUIT);

  ///////////////////////////////////////////////////////////////////
  // menuBar entry viewMenu
  viewMenu=new QPopupMenu();
  viewMenu->setCheckable(true);
  viewMenu->insertItem("Tool&bar", this, SLOT(slotViewToolBar()), 0, ID_VIEW_TOOLBAR);
  viewMenu->insertItem("Stat&usbar", this, SLOT(slotViewStatusBar()), 0, ID_VIEW_STATUSBAR);
  viewMenu->insertItem("&Palette", this, SLOT(slotViewPalette()), 0, ID_VIEW_PALETTE);

  viewMenu->setItemChecked(ID_VIEW_TOOLBAR, true);
  viewMenu->setItemChecked(ID_VIEW_STATUSBAR, true);
  viewMenu->setItemChecked(ID_VIEW_PALETTE, true);

  ///////////////////////////////////////////////////////////////////
  // menuBar entry optMenu
  optMenu=new QPopupMenu();
  optMenu->setCheckable(true);
  optMenu->insertItem("Print in &color", this, SLOT(slotOptPrintInColor()), 0, ID_OPT_PRINT_IN_COLOR);
  optMenu->insertSeparator();
  optMenu->insertItem("Printing op&tions...", this, SLOT(slotOptPrintopt()), 0, ID_OPT_PRINTOPT);
  optMenu->insertSeparator();
  optMenu->insertItem("Display Cherenkov &Signal", this, SLOT(slotOptDisplaySignal()), 0, ID_OPT_DISPLAY_SIGNAL);
  optMenu->insertItem("Display Arrival Times", this, SLOT(slotOptDisplayTimes()), 0, ID_OPT_DISPLAY_TIMES);

  optMenu->setItemChecked(ID_OPT_PRINT_IN_COLOR, false);
  optMenu->setItemChecked(ID_OPT_DISPLAY_SIGNAL, true);
  optMenu->setItemChecked(ID_OPT_DISPLAY_TIMES, false);

  ///////////////////////////////////////////////////////////////////
  // EDIT YOUR APPLICATION SPECIFIC MENUENTRIES HERE
  
  ///////////////////////////////////////////////////////////////////
  // menuBar entry helpMenu
  helpMenu=new QPopupMenu();
  helpMenu->insertItem("About...", this, SLOT(slotHelpAbout()), 0, ID_HELP_ABOUT);


  ///////////////////////////////////////////////////////////////////
  // MENUBAR CONFIGURATION
  // set menuBar() the current menuBar 

  menuBar()->insertItem("&File", fileMenu);
  menuBar()->insertItem("&View", viewMenu);
  menuBar()->insertItem("&Options", optMenu);
  menuBar()->insertSeparator();
  menuBar()->insertItem("&Help", helpMenu);
  
  ///////////////////////////////////////////////////////////////////
  // CONNECT THE SUBMENU SLOTS WITH SIGNALS

  connect(fileMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));
  connect(viewMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));
  connect(optMenu,  SIGNAL(highlighted(int)), SLOT(statusCallback(int)));
  connect(helpMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));
  
}

void QDisplay::initToolBar()
{
  ///////////////////////////////////////////////////////////////////
  // TOOLBAR
  QPixmap openIcon, saveIcon, printIcon, prncolorIcon, quitIcon;

  fileToolbar = new QToolBar(this, "file operations");
 
  openIcon = QPixmap(fileopen);
  QToolButton *fileOpen = new QToolButton(openIcon, "Open File", 0, this,
                                          SLOT(slotFileOpen()), fileToolbar);

  saveIcon = QPixmap(filesave);
  QToolButton *fileSave = new QToolButton(saveIcon, "Save selected events in separate file", 0, this,
                                          SLOT(slotFileSaveSelected()), fileToolbar);
  
  printIcon = QPixmap(fileprint);
  QToolButton *filePrint = new QToolButton(printIcon, "Print this event", 0, this,
                                           SLOT(slotFilePrint()), fileToolbar);


  fileToolbar->addSeparator();

  prncolorIcon = QPixmap(prncolor);
  filePrintColor = new QToolButton(prncolorIcon, "Print this event in color", 0, this,
                                   SLOT(slotOptPrintInColor()), fileToolbar);
  filePrintColor->setToggleButton(TRUE);

  fileToolbar->addSeparator();

  QWhatsThis::whatsThisButton(fileToolbar);
  QWhatsThis::add(fileOpen,"Click this button to open a new file.\n\n"
                  "You can also select the Open command from the File menu.");
  QWhatsThis::add(fileSave,"Click this button to save the file you are "
                  "editing. You will be prompted for a file name.\n\n"
                  "You can also select the Save command from the File menu.");
  QWhatsThis::add(filePrint,"Click this button to print the image and information"
                  "of the current event in the printer.\n\n"
                  "You can also select the Print command from the File menu.");
  QWhatsThis::add(filePrintColor,"Click this button to toggle between using color"
                  " for printing an event, or using gray scale.\n\n"
                  "You can also select the Print in color command from the Options menu.");

  fileToolbar->addSeparator();

  quitIcon = QPixmap(filequit);
  QToolButton *fileQuit = new QToolButton(quitIcon, "Quit the application", 0, this,
                                          SLOT(slotFileQuit()), fileToolbar);

  QWhatsThis::add(fileQuit,"Click this button to end the application.\n\n"
                  "You can also select the Exit command from the File menu.");
}

void QDisplay::initStatusBar()
{
  ///////////////////////////////////////////////////////////////////
  //STATUSBAR
  statusBar()->message(IDS_STATUS_DEFAULT, 2000);
}

void QDisplay::initDoc()
{
   doc=new QDisplayDoc();

   printOpt=new QPrintOptions();
}

void QDisplay::initView()
{ 
  ////////////////////////////////////////////////////////////////////
  // set the main widget here
  view=new QDisplayView(this, doc);
  setCentralWidget(view);
}

bool QDisplay::queryExit()
{
  int exit=QMessageBox::information(this, "Quit...",
                                    "Do your really want to quit?",
                                    QMessageBox::Ok, QMessageBox::Cancel);

  if (exit==1)
  {

  }
  else
  {

  };

  return (exit==1);
}

/////////////////////////////////////////////////////////////////////
// SLOT IMPLEMENTATION
/////////////////////////////////////////////////////////////////////


void QDisplay::slotFileOpen()
{
  statusBar()->message("Opening file...");

  QString fileName = QFileDialog::getOpenFileName(0,0,this);
  if (!fileName.isEmpty())
  {
    doc->load(fileName);
    setCaption("File: "+fileName);
    QString message="Loaded document: "+fileName;
    statusBar()->message(message, 2000);
  }
  else
  {
    statusBar()->message("Opening aborted", 2000);
  }
}

void QDisplay::slotFileSaveSelected()
{
  statusBar()->message("Saving selected events under new filename...");
  QString fn = QFileDialog::getSaveFileName(0, 0, this);
  if (!fn.isEmpty())
  {
    doc->saveSelected(fn);
  }
  else
  {
    statusBar()->message("Saving aborted", 2000);
  }

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotFileClose()
{
  statusBar()->message("Closing file...", 2000);

  doc->close();
  setCaption("File: <none>");

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotFilePrint()
{
  statusBar()->message("Printing...");
  view->print();
  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotFileQuit()
{ 
  statusBar()->message("Exiting application...");
  ///////////////////////////////////////////////////////////////////
  // exits the Application
  if(doc->isModified())
  {
    if(queryExit())
    {
      qApp->quit();
    }
    else
    {

    };
  }
  else
  {
    qApp->quit();
  };

  statusBar()->message(IDS_STATUS_DEFAULT);
}


void QDisplay::slotViewToolBar()
{
  statusBar()->message("Toggle toolbar...");
  ///////////////////////////////////////////////////////////////////
  // turn Toolbar on or off
  
  if (fileToolbar->isVisible())
  {
    fileToolbar->hide();
    viewMenu->setItemChecked(ID_VIEW_TOOLBAR, false);
  } 
  else
  {
    fileToolbar->show();
    viewMenu->setItemChecked(ID_VIEW_TOOLBAR, true);
  };

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotViewStatusBar()
{
  statusBar()->message("Toggle statusbar...");
  ///////////////////////////////////////////////////////////////////
  //turn Statusbar on or off
  
  if (statusBar()->isVisible())
  {
    statusBar()->hide();
    viewMenu->setItemChecked(ID_VIEW_STATUSBAR, false);
  }
  else
  {
    statusBar()->show();
    viewMenu->setItemChecked(ID_VIEW_STATUSBAR, true);
  }
  
  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotViewPalette()
{
  statusBar()->message("Toggle palette...");
  ///////////////////////////////////////////////////////////////////
  //turn Statusbar on or off

  if (view->isPaletteVisible()) {
    view->hidePalette();
    viewMenu->setItemChecked(ID_VIEW_PALETTE, false);
  } else {
    view->showPalette();
    viewMenu->setItemChecked(ID_VIEW_PALETTE, true);
  }

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotOptDisplaySignal()
{
  statusBar()->message("Display Cheremkov Signal...");

  bool newstatus = ! view->isDisplaySignal();

  view->setDisplaySignal(newstatus);
  optMenu->setItemChecked(ID_OPT_DISPLAY_SIGNAL, newstatus);

  view->setDisplayTimes(!newstatus);
  optMenu->setItemChecked(ID_OPT_DISPLAY_TIMES, !newstatus);

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotOptDisplayTimes()
{
  statusBar()->message("Display Cheremkov Pulse Arrival Times...");

  bool newstatus = ! view->isDisplayTimes();

  view->setDisplayTimes(newstatus);
  optMenu->setItemChecked(ID_OPT_DISPLAY_TIMES, newstatus);

  view->setDisplaySignal(!newstatus);
  optMenu->setItemChecked(ID_OPT_DISPLAY_SIGNAL, !newstatus);

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotOptPrintInColor()
{
  statusBar()->message("Toggle printing in color...");
  ///////////////////////////////////////////////////////////////////
  //turn Statusbar on or off
  bool newstatus = ! view->isPrintInColor();
  view->printInColor(newstatus);
  optMenu->setItemChecked(ID_OPT_PRINT_IN_COLOR, newstatus);
  filePrintColor->setOn(newstatus);

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotOptPrintopt()
{
  statusBar()->message("Setting printing options...", 2000);
  ///////////////////////////////////////////////////////////////////
  //turn Statusbar on or off

  QRunHist d;
  d.exec();

  printOpt->set_PrintInColor( view->isPrintInColor() );

  printOpt->show();

  // process dialog results if OK button was pressed
  if ( printOpt->result() ) {

    view->printSetOptions( printOpt->get_PrintInColor(),
                           printOpt->get_PrintImage(),
                           printOpt->get_PrintStat(),
                           printOpt->get_PrintHist(),
                           printOpt->get_PrintCommand(),
                           printOpt->get_PrintFilePrefix(),
                           printOpt->get_PrintFileName(),
                           printOpt->get_PrintOutputTo() );

    optMenu->setItemChecked(ID_OPT_PRINT_IN_COLOR, view->isPrintInColor());
    filePrintColor->setOn(view->isPrintInColor());


  }

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QDisplay::slotHelpAbout()
{
  QMessageBox::about(this,"About...",
                     IDS_APP_ABOUT );
}

void QDisplay::slotStatusHelpMsg(const QString &text)
{
  ///////////////////////////////////////////////////////////////////
  // change status message of whole statusbar temporary (text, msec)
  statusBar()->message(text, 2000);
}

void QDisplay::statusCallback(int id_)
{
  switch (id_)
  {
    case ID_FILE_OPEN:
         slotStatusHelpMsg("Opens a data file");
         break;

    case ID_FILE_SAVE_SELECTED:
         slotStatusHelpMsg("Saves the selected events in a separate file...");
         break;

    case ID_FILE_CLOSE:
         slotStatusHelpMsg("Closes the actual document");
         break;

    case ID_FILE_PRINT:
         slotStatusHelpMsg("Prints out the actual document");
         break;

    case ID_FILE_QUIT:
         slotStatusHelpMsg("Quits the application");
         break;

    case ID_VIEW_TOOLBAR:
         slotStatusHelpMsg("Enables/disables the toolbar");
         break;

    case ID_VIEW_STATUSBAR:
         slotStatusHelpMsg("Enables/disables the statusbar");
         break;

    case ID_VIEW_PALETTE:
         slotStatusHelpMsg("Enables/disables the view of the current palette");
         break;

    case ID_OPT_PRINTOPT:
         slotStatusHelpMsg("Sets the options for printing");
         break;

    case ID_HELP_ABOUT:
         slotStatusHelpMsg("Shows an aboutbox");
         break;
  }
}
