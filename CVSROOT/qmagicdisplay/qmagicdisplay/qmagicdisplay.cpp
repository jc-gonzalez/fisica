/*************************************************************************
 *                                                                          
 * qmagicdisplay.cpp  -  description
 *
 * Copyright (C) 2001  J C Gonzalez
 * gonzalez@gae.ucm.es
 *
 *------------------------------------------------------------------------
 *
 * Copyright (C) 2001 J C Gonzalez
 *  
 * This program is free software;  you can redistribute it and/or  modify
 * it under the terms  of the GNU General  Public License as published by
 * the Free Software Foundation; either version  2 of the License, or (at
 * your option) any later version.
 * 
 * This piece of code is distributed in the hope  that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS
 * FOR A PARTICULAR PURPOSE.
 * 
 * In no  event shall his author  be liable for  any special, incidental,
 * indirect  or  consequential  damages  of any  kind,   or  any  damages
 * whatsoever resulting from loss of use, data or profits, whether or not
 * advised of the possibility of damage, and on  any theory of liability,
 * arising out  of or in connection  with the use  or performance of this
 * software. You've been warned.
 * 
 ************************************************************************/


#include "qmagicdisplay.h"
#include "filesave.xpm"
#include "fileopen.xpm"
#include "filenew.xpm"

QMAGICDisplay::QMAGICDisplay()
{
  setCaption("QMAGICDisplay " VERSION);
  
  ///////////////////////////////////////////////////////////////////
  // call inits to invoke all other construction parts
  initMenuBar();
  initToolBar();
  initStatusBar();

  initDoc();
  initView();  
}

QMAGICDisplay::~QMAGICDisplay()
{
}

void QMAGICDisplay::initMenuBar()
{
  ///////////////////////////////////////////////////////////////////
  // MENUBAR

  ///////////////////////////////////////////////////////////////////
  // menuBar entry fileMenu

  fileMenu=new QPopupMenu();
  fileMenu->insertItem("&New", this, SLOT(slotFileNew()), CTRL+Key_N, ID_FILE_NEW);
  fileMenu->insertItem("&Open...", this, SLOT(slotFileOpen()), CTRL+Key_O, ID_FILE_OPEN);
  fileMenu->insertSeparator();
  fileMenu->insertItem("&Save", this, SLOT(slotFileSave()), CTRL+Key_S, ID_FILE_SAVE);
  fileMenu->insertItem("Save &as...", this, SLOT(slotFileSaveAs()), 0, ID_FILE_SAVE_AS);
  fileMenu->insertItem("&Close", this, SLOT(slotFileClose()), CTRL+Key_W, ID_FILE_CLOSE);
  fileMenu->insertSeparator();
  fileMenu->insertItem("&Print", this, SLOT(slotFilePrint()), CTRL+Key_P, ID_FILE_PRINT);
  fileMenu->insertSeparator();
  fileMenu->insertItem("E&xit", this, SLOT(slotFileQuit()), CTRL+Key_Q, ID_FILE_QUIT);

  ///////////////////////////////////////////////////////////////////
  // menuBar entry editMenu
  editMenu=new QPopupMenu();
  editMenu->insertItem("Cu&t", this, SLOT(slotEditCut()), CTRL+Key_X, ID_EDIT_CUT);
  editMenu->insertItem("&Copy", this, SLOT(slotEditCopy()), CTRL+Key_C, ID_EDIT_COPY);
  editMenu->insertItem("&Paste", this, SLOT(slotEditPaste()), CTRL+Key_V, ID_EDIT_PASTE);
 
  
  ///////////////////////////////////////////////////////////////////
  // menuBar entry viewMenu
  viewMenu=new QPopupMenu();
  viewMenu->setCheckable(true);
  viewMenu->insertItem("Tool&bar", this, SLOT(slotViewToolBar()), 0, ID_VIEW_TOOLBAR);
  viewMenu->insertItem("&Statusbar", this, SLOT(slotViewStatusBar()), 0, ID_VIEW_STATUSBAR);

  viewMenu->setItemChecked(ID_VIEW_TOOLBAR, true);
  viewMenu->setItemChecked(ID_VIEW_STATUSBAR, true);

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
  menuBar()->insertItem("&Edit", editMenu);
  menuBar()->insertItem("&View", viewMenu);
  menuBar()->insertSeparator();
  menuBar()->insertItem("&Help", helpMenu);
  
  ///////////////////////////////////////////////////////////////////
  // CONNECT THE SUBMENU SLOTS WITH SIGNALS

  connect(fileMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));
  connect(editMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));
  connect(viewMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));
  connect(helpMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));
  
}

void QMAGICDisplay::initToolBar()
{
  ///////////////////////////////////////////////////////////////////
  // TOOLBAR
  QPixmap openIcon, saveIcon, newIcon;

  fileToolbar = new QToolBar(this, "file operations");
 
  newIcon = QPixmap(filenew);
  QToolButton *fileNew = new QToolButton(newIcon, "New File", 0, this,
                                         SLOT(slotFileNew()), fileToolbar);

  openIcon = QPixmap(fileopen);
  QToolButton *fileOpen = new QToolButton(openIcon, "Open File", 0, this,
                                          SLOT(slotFileOpen()), fileToolbar);

  saveIcon = QPixmap(filesave);
  QToolButton *fileSave = new QToolButton(saveIcon, "Save File", 0, this,
                                          SLOT(slotFileSave()), fileToolbar);
  
  
  fileToolbar->addSeparator();
  QWhatsThis::whatsThisButton(fileToolbar);
  QWhatsThis::add(fileNew,"Click this button to create a new file.\n\n"
                  "You can also select the New command from the File menu.");
  QWhatsThis::add(fileOpen,"Click this button to open a new file.\n\n"
                  "You can also select the Open command from the File menu.");
  QWhatsThis::add(fileSave,"Click this button to save the file you are "
                  "editing. You will be prompted for a file name.\n\n"
                  "You can also select the Save command from the File menu.");
  
}

void QMAGICDisplay::initStatusBar()
{
  ///////////////////////////////////////////////////////////////////
  //STATUSBAR
  statusBar()->message(IDS_STATUS_DEFAULT, 2000);
}

void QMAGICDisplay::initDoc()
{
   doc=new QMAGICDisplayDoc();
}

void QMAGICDisplay::initView()
{ 
  ////////////////////////////////////////////////////////////////////
  // set the main widget here
  view=new QMAGICDisplayView(this, doc);
  setCentralWidget(view);
}

bool QMAGICDisplay::queryExit()
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


void QMAGICDisplay::slotFileNew()
{
  statusBar()->message("Creating new file...");
  doc->newDoc();
  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QMAGICDisplay::slotFileOpen()
{
  statusBar()->message("Opening file...");

  QString fileName = QFileDialog::getOpenFileName(0,0,this);
  if (!fileName.isEmpty())
  {
    doc->load(fileName);
    setCaption(fileName);
    QString message="Loaded document: "+fileName;
    statusBar()->message(message, 2000);
  }
  else
  {
    statusBar()->message("Opening aborted", 2000);
  }
}


void QMAGICDisplay::slotFileSave()
{
  statusBar()->message("Saving file...");
  doc->save();
  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QMAGICDisplay::slotFileSaveAs()
{
  statusBar()->message("Saving file under new filename...");
  QString fn = QFileDialog::getSaveFileName(0, 0, this);
  if (!fn.isEmpty())
  {
    doc->saveAs(fn);
  }
  else
  {
    statusBar()->message("Saving aborted", 2000);
  }

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QMAGICDisplay::slotFileClose()
{
  statusBar()->message("Closing file...");

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QMAGICDisplay::slotFilePrint()
{
  statusBar()->message("Printing...");
  QPrinter printer;
  if (printer.setup(this))
  {
    QPainter painter;
    painter.begin(&printer);

    ///////////////////////////////////////////////////////////////////
    // TODO: Define printing by using the QPainter methods here

    painter.end();
  };

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QMAGICDisplay::slotFileQuit()
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

void QMAGICDisplay::slotEditCut()
{
  statusBar()->message("Cutting selection...");

  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QMAGICDisplay::slotEditCopy()
{
  statusBar()->message("Copying selection to clipboard...");
  
  statusBar()->message(IDS_STATUS_DEFAULT);
}

void QMAGICDisplay::slotEditPaste()
{
  statusBar()->message("Inserting clipboard contents...");
  
  statusBar()->message(IDS_STATUS_DEFAULT);
}


void QMAGICDisplay::slotViewToolBar()
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

void QMAGICDisplay::slotViewStatusBar()
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

void QMAGICDisplay::slotHelpAbout()
{
  QMessageBox::about(this,"About...",
                     IDS_APP_ABOUT );
}

void QMAGICDisplay::slotStatusHelpMsg(const QString &text)
{
  ///////////////////////////////////////////////////////////////////
  // change status message of whole statusbar temporary (text, msec)
  statusBar()->message(text, 2000);
}

void QMAGICDisplay::statusCallback(int id_)
{
  switch (id_)
  {
    case ID_FILE_NEW:
         slotStatusHelpMsg("Creates a new document");
         break;

    case ID_FILE_OPEN:
         slotStatusHelpMsg("Opens an existing document");
         break;

    case ID_FILE_SAVE:
         slotStatusHelpMsg("Saves the actual document");
         break;

    case ID_FILE_SAVE_AS:
         slotStatusHelpMsg("Saves the actual document as...");
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

    case ID_EDIT_CUT:
         slotStatusHelpMsg("Cuts the selected section and puts it to the clipboard");
         break;

    case ID_EDIT_COPY:
         slotStatusHelpMsg("Copies the selected section to the clipboard");
         break;

    case ID_EDIT_PASTE:
         slotStatusHelpMsg("Pastes the clipboard contents to actual position");
         break;

    case ID_EDIT_SELECT_ALL:
         slotStatusHelpMsg("Selects the whole document contents");
         break;

    case ID_VIEW_TOOLBAR:
         slotStatusHelpMsg("Enables/disables the toolbar");
         break;

    case ID_VIEW_STATUSBAR:
         slotStatusHelpMsg("Enables/disables the statusbar");
         break;

    case ID_HELP_ABOUT:
         slotStatusHelpMsg("Shows an aboutbox");
         break;
  }
}

