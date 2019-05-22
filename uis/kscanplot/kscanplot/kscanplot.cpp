/*************************************************************************
 *                                                                          
 * kscanplot.cpp  -  description
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


// include files for QT
#include <qdir.h>
#include <qprinter.h>
#include <qvbox.h>
#include <qwhatsthis.h>
#include <qtooltip.h>
#include <qtoolbutton.h>

// include files for KDE
#include <kiconloader.h>
#include <kmessagebox.h>
#include <kfiledialog.h>
#include <kmenubar.h>
#include <klocale.h>
#include <kconfig.h>

// application specific includes
#include "kscanplot.h"
#include "kscanplotview.h"
#include "kscanplotdoc.h"
#include "resource.h"


KScanPlotApp::KScanPlotApp()
{
  config=kapp->config();
  printer = new QPrinter;
  untitledCount=0;
  pDocList = new QList<KScanPlotDoc>();
  pDocList->setAutoDelete(true);
  ///////////////////////////////////////////////////////////////////
  // call inits to invoke all other construction parts
  initMenuBar();
  initToolBar();
  initStatusBar();
  initKeyAccel();
  initView();
	
  readOptions();

  ///////////////////////////////////////////////////////////////////
  // disable menu and toolbar items at startup
  disableCommand(ID_EDIT_UNDO);
  disableCommand(ID_EDIT_CUT);
  disableCommand(ID_EDIT_COPY);
  disableCommand(ID_EDIT_PASTE);
}

KScanPlotApp::~KScanPlotApp()
{
  delete printer;
}

void KScanPlotApp::initKeyAccel()
{
  keyAccel = new KAccel(this);
	
  // fileMenu accelerators
  keyAccel->connectItem(KStdAccel::New, this, SLOT(slotFileNew()));
  keyAccel->connectItem(KStdAccel::Open, this, SLOT(slotFileOpen()));
  keyAccel->connectItem(KStdAccel::Save, this, SLOT(slotFileSave()));
  keyAccel->connectItem(KStdAccel::Close, this, SLOT(slotFileClose()));
  keyAccel->connectItem(KStdAccel::Print, this, SLOT(slotFilePrint()));
  keyAccel->connectItem(KStdAccel::Quit, this, SLOT(slotFileQuit()));
  // editMenu accelerators
  keyAccel->connectItem(KStdAccel::Undo, this, SLOT(slotEditUndo()));
  keyAccel->connectItem(KStdAccel::Cut, this, SLOT(slotEditCut()));
  keyAccel->connectItem(KStdAccel::Copy, this, SLOT(slotEditCopy()));
  keyAccel->connectItem(KStdAccel::Paste, this, SLOT(slotEditPaste()));

  keyAccel->connectItem(KStdAccel::Help, this, SLOT(appHelpActivated()));

  // read settings, then change the menus
  keyAccel->readSettings();				
  keyAccel->changeMenuAccel(pFileMenu, ID_FILE_NEW, KStdAccel::New);
  keyAccel->changeMenuAccel(pFileMenu, ID_FILE_OPEN, KStdAccel::Open);
  keyAccel->changeMenuAccel(pFileMenu, ID_FILE_SAVE, KStdAccel::Save);
  keyAccel->changeMenuAccel(pFileMenu, ID_FILE_CLOSE, KStdAccel::Close);
  keyAccel->changeMenuAccel(pFileMenu, ID_FILE_PRINT, KStdAccel::Print);
  keyAccel->changeMenuAccel(pFileMenu, ID_FILE_QUIT, KStdAccel::Quit);

  keyAccel->changeMenuAccel(pEditMenu, ID_EDIT_UNDO, KStdAccel::Undo);
  keyAccel->changeMenuAccel(pEditMenu, ID_EDIT_CUT, KStdAccel::Cut);
  keyAccel->changeMenuAccel(pEditMenu, ID_EDIT_COPY, KStdAccel::Copy);
  keyAccel->changeMenuAccel(pEditMenu, ID_EDIT_PASTE, KStdAccel::Paste);

}

void KScanPlotApp::initMenuBar()
{
  ///////////////////////////////////////////////////////////////////
  // MENUBAR
  pRecentFileMenu = new QPopupMenu(this);
  connect(pRecentFileMenu, SIGNAL(activated(int)), SLOT(slotFileOpenRecent(int)));

  ///////////////////////////////////////////////////////////////////
  // menuBar entry file-Menu
  pFileMenu = new QPopupMenu(this);
  pFileMenu->insertItem(BarIcon("filenew"), i18n("&New"), ID_FILE_NEW);
  pFileMenu->insertItem(BarIcon("fileopen"), i18n("&Open..."), ID_FILE_OPEN);
  pFileMenu->insertItem(i18n("Open &recent"), pRecentFileMenu, ID_FILE_OPEN_RECENT);

  pFileMenu->insertItem(i18n("&Close"), ID_FILE_CLOSE);
  pFileMenu->insertSeparator();
  pFileMenu->insertItem(BarIcon("filefloppy") ,i18n("&Save"), ID_FILE_SAVE);
  pFileMenu->insertItem(i18n("Save &As..."), ID_FILE_SAVE_AS);
  pFileMenu->insertSeparator();
  pFileMenu->insertItem(BarIcon("fileprint"), i18n("&Print..."), ID_FILE_PRINT);
  pFileMenu->insertSeparator();
  pFileMenu->insertItem(i18n("E&xit"), ID_FILE_QUIT);
	
  ///////////////////////////////////////////////////////////////////
  // menuBar entry edit-Menu
  pEditMenu = new QPopupMenu(this);
  pEditMenu->insertItem(BarIcon("undo"), i18n("&Undo"), ID_EDIT_UNDO);
  pEditMenu->insertSeparator();
  pEditMenu->insertItem(BarIcon("editcut"), i18n("Cu&t"), ID_EDIT_CUT);
  pEditMenu->insertItem(BarIcon("editcopy"), i18n("&Copy"), ID_EDIT_COPY);
  pEditMenu->insertItem(BarIcon("editpaste"), i18n("&Paste"), ID_EDIT_PASTE);

  ///////////////////////////////////////////////////////////////////
  // menuBar entry view-Menu
  pViewMenu = new QPopupMenu(this);
  pViewMenu->setCheckable(true);
  pViewMenu->insertItem(i18n("&Toolbar"), ID_VIEW_TOOLBAR);
  pViewMenu->insertItem(i18n("&Statusbar"), ID_VIEW_STATUSBAR);

  ///////////////////////////////////////////////////////////////////
  // menuBar entry window-Menu
  pWindowMenu = new QPopupMenu(this);
  pWindowMenu->setCheckable(true);


  ///////////////////////////////////////////////////////////////////
  // menuBar entry helpMenu
  QPopupMenu* pHelpMenu = helpMenu(i18n("KScanPlot" VERSION "\n\n(c) 2001 by\nJ C Gonzalez\ngonzalez@gae.ucm.es"));

  ///////////////////////////////////////////////////////////////////
  // MENUBAR CONFIGURATION
  // insert your popup menus with the according menubar entries in the order
  // they will appear later from left to right
  menuBar()->insertItem(i18n("&File"), pFileMenu);
  menuBar()->insertItem(i18n("&Edit"), pEditMenu);
  menuBar()->insertItem(i18n("&View"), pViewMenu);
  menuBar()->insertItem(i18n("&Window"), pWindowMenu );
  menuBar()->insertItem(i18n("&Help"), pHelpMenu);

  ///////////////////////////////////////////////////////////////////
  // CONNECT THE MENU SLOTS WITH SIGNALS
  // for execution slots and statusbar messages
  connect(pFileMenu, SIGNAL(activated(int)), SLOT(commandCallback(int)));
  connect(pFileMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));

  connect(pEditMenu, SIGNAL(activated(int)), SLOT(commandCallback(int)));
  connect(pEditMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));

  connect(pViewMenu, SIGNAL(activated(int)), SLOT(commandCallback(int)));
  connect(pViewMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));

  connect(pWindowMenu, SIGNAL(aboutToShow() ), SLOT( windowMenuAboutToShow() ) );
  connect(pWindowMenu, SIGNAL(activated(int)), SLOT(commandCallback(int)));
  connect(pWindowMenu, SIGNAL(highlighted(int)), SLOT(statusCallback(int)));

}

void KScanPlotApp::initToolBar()
{

  ///////////////////////////////////////////////////////////////////
  // TOOLBAR
  toolBar()->insertButton(BarIcon("filenew"), ID_FILE_NEW, true, i18n("New File"));
  toolBar()->insertButton(BarIcon("fileopen"), ID_FILE_OPEN, true, i18n("Open File"));
  toolBar()->insertButton(BarIcon("filefloppy"), ID_FILE_SAVE, true, i18n("Save File"));
  toolBar()->insertSeparator();
  toolBar()->insertButton(BarIcon("editcut"), ID_EDIT_CUT, true, i18n("Cut"));
  toolBar()->insertButton(BarIcon("editcopy"), ID_EDIT_COPY, true, i18n("Copy"));
  toolBar()->insertButton(BarIcon("editpaste"), ID_EDIT_PASTE, true, i18n("Paste"));
  toolBar()->insertSeparator();
  toolBar()->insertButton(BarIcon("fileprint"), ID_FILE_PRINT, true, i18n("Print"));
  toolBar()->insertButton(BarIcon("help"), ID_HELP_CONTENTS, SIGNAL(clicked()),
  				this, SLOT(appHelpActivated()), true,i18n("Help"));

  QToolButton *btnwhat = QWhatsThis::whatsThisButton(toolBar());
  QToolTip::add(btnwhat, i18n("What's this...?"));
  toolBar()->insertWidget(ID_HELP_WHATS_THIS, btnwhat->sizeHint().width(), btnwhat);

  ///////////////////////////////////////////////////////////////////
  // INSERT YOUR APPLICATION SPECIFIC TOOLBARS HERE WITH toolBar(n)


  ///////////////////////////////////////////////////////////////////
  // CONNECT THE TOOLBAR SLOTS WITH SIGNALS - add new created toolbars by their according number
  // connect for invoking the slot actions
  connect(toolBar(), SIGNAL(clicked(int)), SLOT(commandCallback(int)));
  // connect for the status help on holing icons pressed with the mouse button
  connect(toolBar(), SIGNAL(pressed(int)), SLOT(statusCallback(int)));

}

void KScanPlotApp::initStatusBar()
{
  ///////////////////////////////////////////////////////////////////
  // STATUSBAR
  // TODO: add your own items you need for displaying current application status.
  statusBar()->insertItem(i18n("Ready."), ID_STATUS_MSG);
}


void KScanPlotApp::initView()
{ 
  ////////////////////////////////////////////////////////////////////
  // here the main view of the KTMainWindow is created by a background box and
  // the QWorkspace instance for MDI view.
  QVBox* view_back = new QVBox( this );
  view_back->setFrameStyle( QFrame::StyledPanel | QFrame::Sunken );
  pWorkspace = new QWorkspace( view_back );
  connect(pWorkspace, SIGNAL(windowActivated(QWidget*)), this, SLOT(setWndTitle(QWidget*)));
  setView(view_back);
}

void KScanPlotApp::setWndTitle(QWidget*){
  setCaption(pWorkspace->activeWindow()->caption());
}

void KScanPlotApp::enableCommand(int id_)
{
  ///////////////////////////////////////////////////////////////////
  // enable menu and toolbar functions by their ID's
  menuBar()->setItemEnabled(id_, true);
  toolBar()->setItemEnabled(id_, true);
}

void KScanPlotApp::disableCommand(int id_)
{
  ///////////////////////////////////////////////////////////////////
  // disable menu and toolbar functions by their ID's
  menuBar()->setItemEnabled(id_, false);
  toolBar()->setItemEnabled(id_, false);
}

void KScanPlotApp::addRecentFile(const QString &file)
{
  if(recentFiles.contains(file))
    return; // it's already there

  if( recentFiles.count() < 5)
    recentFiles.prepend(file);
  else{
    recentFiles.remove(recentFiles.last());
    recentFiles.prepend(file);
  }

  pRecentFileMenu->clear();

  for ( int i =0 ; i < (int)recentFiles.count(); i++){
    pRecentFileMenu->insertItem(recentFiles.at(i));
  }

}

void KScanPlotApp::createClient(KScanPlotDoc* doc)
{
  KScanPlotView* w = new KScanPlotView(doc, pWorkspace,0,WDestructiveClose);
  w->installEventFilter(this);
  doc->addView(w);
  w->setIcon(kapp->miniIcon());
  if ( pWorkspace->windowList().isEmpty() ) // show the very first window in maximized mode
    w->showMaximized();
  else
    w->show();
}

void KScanPlotApp::openDocumentFile(const char* file)
{
  slotStatusMsg(i18n("Opening file..."));
  KScanPlotDoc* doc;
	// check, if document already open. If yes, set the focus to the first view
  for(doc=pDocList->first(); doc > 0; doc=pDocList->next())
  {
    if(doc->pathName()==file)
    {
      KScanPlotView* view=doc->firstView();	
      view->setFocus();
      return;
    }
  }
  doc = new KScanPlotDoc();
  pDocList->append(doc);
  doc->newDocument();
  // Creates an untitled window if file is 0	
  if(!file)
  {
    untitledCount+=1;
    QString fileName=QString(i18n("Untitled%1")).arg(untitledCount);
    doc->setPathName(fileName);
    doc->setTitle(fileName);
  }
  // Open the file
  else
  {
    if(!doc->openDocument(file))
    {
      KMessageBox::error (this,i18n("Could not open document !"), i18n("Error !"));
      delete doc;
      return;	
    }
    addRecentFile(file);
  }
  // create the window
  createClient(doc);

  slotStatusMsg(i18n("Ready."));
}


void KScanPlotApp::saveOptions()
{	
  config->setGroup("General Options");
  config->writeEntry("Geometry", size());
  config->writeEntry("Show Toolbar", toolBar()->isVisible());
  config->writeEntry("Show Statusbar",statusBar()->isVisible());
  config->writeEntry("ToolBarPos", (int) toolBar()->barPos());
  config->writeEntry("Recent Files", recentFiles);
}


void KScanPlotApp::readOptions()
{
	
  config->setGroup("General Options");

  // bar status settings
  bool bViewToolbar = config->readBoolEntry("Show Toolbar", true);
  menuBar()->setItemChecked(ID_VIEW_TOOLBAR, bViewToolbar);
  if(!bViewToolbar)
  {
    enableToolBar(KToolBar::Hide);
  }
	
  bool bViewStatusbar = config->readBoolEntry("Show Statusbar", true);
  menuBar()->setItemChecked(ID_VIEW_STATUSBAR, bViewStatusbar);
  if(!bViewStatusbar)
  {
    enableStatusBar(KStatusBar::Hide);
  }

  // bar position settings
  KToolBar::BarPosition toolBarPos;
  toolBarPos=(KToolBar::BarPosition) config->readNumEntry("ToolBarPos", KToolBar::Top);
  toolBar()->setBarPos(toolBarPos);
	
  // initialize the recent file list
  config->readListEntry("Recent Files",recentFiles);

  for (int i=0; i < (int) recentFiles.count(); i++)
  {
    pRecentFileMenu->insertItem(recentFiles.at(i));
  }

  QSize size=config->readSizeEntry("Geometry");
  if(!size.isEmpty())
  {
    resize(size);
  }
}

void KScanPlotApp::saveProperties(KConfig *_cfg)
{

}


void KScanPlotApp::readProperties(KConfig* _cfg)
{
}		

bool KScanPlotApp::queryClose()
{

  QStringList saveFiles;
  KScanPlotDoc* doc;
  if(pDocList->isEmpty())
    return true;

  for(doc=pDocList->first(); doc!=0;doc=pDocList->next())
  {
    if(doc->isModified())
      saveFiles.append(doc->title());
  }
  if(saveFiles.isEmpty())
    return true;
			
  switch (KMessageBox::questionYesNoList(this,
          i18n("One or more documents have been modified.\nSave changes before exiting?"),saveFiles)) 		
  {
  case KMessageBox::Yes:
    for(doc=pDocList->first(); doc!=0;doc=pDocList->next())
    {
      if(doc->title().contains(i18n("Untitled")))
        slotFileSaveAs();
      else
      {
        if(!doc->saveDocument(doc->pathName()))
        {
          KMessageBox::error (this,i18n("Could not save the current document !"), i18n("I/O Error !"));
          return false;
         }
       }
     }
     return true;
  case KMessageBox::No:
  default:
  return true;
  }
}

bool KScanPlotApp::queryExit()
{
  saveOptions();
  return true;
}

bool KScanPlotApp::eventFilter(QObject* object, QEvent* event)
{
  if(event->type() == QEvent::Close)
  {
    QCloseEvent* e=(QCloseEvent*)event;
    KScanPlotView* pView=(KScanPlotView*)object;
    KScanPlotDoc* pDoc=pView->getDocument();
    if(pDoc->canCloseFrame(pView))
    {
      pDoc->removeView(pView);
      if(!pDoc->firstView())
        pDocList->remove(pDoc);
      e->accept();
      //////////////	
      if(pWorkspace->windowList().count()==1)
        setPlainCaption(kapp->caption());
      else
        setCaption(pWorkspace->activeWindow()->caption());
      //////////////
    }
    else
      e->ignore();
  }
  return QWidget::eventFilter( object, event );    // standard event processing
}

/////////////////////////////////////////////////////////////////////
// SLOT IMPLEMENTATION
/////////////////////////////////////////////////////////////////////


void KScanPlotApp::slotFileNew()
{
  slotStatusMsg(i18n("Creating new document..."));

  openDocumentFile();

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotFileOpen()
{
  slotStatusMsg(i18n("Opening file..."));
	
  QString fileToOpen=KFileDialog::getOpenFileName(QDir::homeDirPath(),
                                                  i18n("*|All files"), this, i18n("Open File..."));
  if(!fileToOpen.isEmpty())
  {
    openDocumentFile(fileToOpen);		
  }

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotFileOpenRecent(int id_)
{
  slotStatusMsg(i18n("Opening file..."));
  	
  openDocumentFile(pRecentFileMenu->text(id_));
	
  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotFileSave()
{
  slotStatusMsg(i18n("Saving file..."));

  KScanPlotView* m = (KScanPlotView*)pWorkspace->activeWindow();
  if( m )
  {
    KScanPlotDoc* doc = m->getDocument();
    if(doc->title().contains(i18n("Untitled")))
      slotFileSaveAs();
    else
      if(!doc->saveDocument(doc->pathName()))
    KMessageBox::error (this,i18n("Could not save the current document !"), i18n("I/O Error !"));
  }

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotFileSaveAs()
{
  slotStatusMsg(i18n("Saving file with a new filename..."));

  QString newName=KFileDialog::getSaveFileName(QDir::currentDirPath(),
                                               i18n("*|All files"), this, i18n("Save as..."));
  if(!newName.isEmpty())
  {
    KScanPlotView* m = (KScanPlotView*)pWorkspace->activeWindow();
    if( m )
    {
      KScanPlotDoc* doc =	m->getDocument();
      if(!doc->saveDocument(newName))
      {
        KMessageBox::error (this,i18n("Could not save the current document !"), i18n("I/O Error !"));
        return;
      }
      doc->changedViewList();
      setWndTitle(m);
    }	
  }

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotFileClose()
{
  slotStatusMsg(i18n("Closing file..."));
  KScanPlotView* m = (KScanPlotView*)pWorkspace->activeWindow();
  if( m )
  {
    KScanPlotDoc* doc=m->getDocument();
    doc->closeDocument();
  }
	
  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotFilePrint()
{
  slotStatusMsg(i18n("Printing..."));
	
  KScanPlotView* m = (KScanPlotView*) pWorkspace->activeWindow();
  if ( m )
    m->print( printer );

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotFileQuit()
{
  slotStatusMsg(i18n("Exiting..."));
  saveOptions();
  // close the first window, the list makes the next one the first again.
  // This ensures that queryClose() is called on each window to ask for closing
  KTMainWindow* w;
  if(memberList)
  {
    for(w=memberList->first(); w!=0; w=memberList->first())
    {
      // only close the window if the closeEvent is accepted. If the user presses Cancel on the saveModified() dialog,
      // the window and the application stay open.
      if(!w->close())
        break;
    }
  }	
  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotEditUndo()
{
  slotStatusMsg(i18n("Reverting last action..."));
	
  KScanPlotView* m = (KScanPlotView*) pWorkspace->activeWindow();
  if ( m )
//  m->undo();

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotEditCut()
{
  slotStatusMsg(i18n("Cutting selection..."));
	
  KScanPlotView* m = (KScanPlotView*) pWorkspace->activeWindow();
  if ( m )
//    m->cut();	

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotEditCopy()
{
  slotStatusMsg(i18n("Copying selection to clipboard..."));
	
  KScanPlotView* m = (KScanPlotView*) pWorkspace->activeWindow();
  if ( m )
//    m->copy();
		
  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotEditPaste()
{
  slotStatusMsg(i18n("Inserting clipboard contents..."));
	
  KScanPlotView* m = (KScanPlotView*) pWorkspace->activeWindow();
  if ( m )
//    m->paste();
		
  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotViewToolBar()
{
  slotStatusMsg(i18n("Toggle the toolbar..."));
  ///////////////////////////////////////////////////////////////////
  // turn Toolbar on or off
  if( menuBar()->isItemChecked(ID_VIEW_TOOLBAR))
  {
    menuBar()->setItemChecked(ID_VIEW_TOOLBAR, false);
    enableToolBar(KToolBar::Hide);
  }
  else
  {
    menuBar()->setItemChecked(ID_VIEW_TOOLBAR, true);
    enableToolBar(KToolBar::Show);
  }		

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotViewStatusBar()
{
  slotStatusMsg(i18n("Toggle the statusbar..."));
  ///////////////////////////////////////////////////////////////////
  //turn Statusbar on or off
  if( menuBar()->isItemChecked(ID_VIEW_STATUSBAR))
  {
    menuBar()->setItemChecked(ID_VIEW_STATUSBAR, false);
    enableStatusBar(KStatusBar::Hide);
  }
  else
  {
    menuBar()->setItemChecked(ID_VIEW_STATUSBAR, true);
    enableStatusBar(KStatusBar::Show);
  }

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotWindowNewWindow()
{
  slotStatusMsg(i18n("Opening a new application window..."));
	
  KScanPlotView* m = (KScanPlotView*) pWorkspace->activeWindow();
  if ( m )
  {
    KScanPlotDoc* doc = m->getDocument();
    createClient(doc);
  }

  slotStatusMsg(i18n("Ready."));
}

void KScanPlotApp::slotStatusMsg(const QString &text)
{
  ///////////////////////////////////////////////////////////////////
  // change status message permanently
  statusBar()->clear();
  statusBar()->changeItem(text, ID_STATUS_MSG);
}


void KScanPlotApp::slotStatusHelpMsg(const QString &text)
{
  ///////////////////////////////////////////////////////////////////
  // change status message of whole statusbar temporary (text, msec)
  statusBar()->message(text, 2000);
}

void KScanPlotApp::windowMenuAboutToShow()
{
  pWindowMenu->clear();
  pWindowMenu->insertItem(i18n("&New Window"), ID_WINDOW_NEW_WINDOW);
  pWindowMenu->insertItem(i18n("&Cascade"), pWorkspace, SLOT(cascade() ),0 , ID_WINDOW_CASCADE );
  pWindowMenu->insertItem(i18n("&Tile"), pWorkspace, SLOT(tile() ),0 , ID_WINDOW_TILE );

  if ( pWorkspace->windowList().isEmpty() )
  {
    disableCommand(ID_WINDOW_NEW_WINDOW);
    disableCommand(ID_WINDOW_CASCADE);
    disableCommand(ID_WINDOW_TILE);
  }
  pWindowMenu->insertSeparator();

  QWidgetList windows = pWorkspace->windowList();
  for ( int i = 0; i < int(windows.count()); ++i )
  {
    int id = pWindowMenu->insertItem(QString("&%1 ").arg(i+1)+windows.at(i)->caption(), this, SLOT( windowMenuActivated( int ) ) );
    pWindowMenu->setItemParameter( id, i );
    pWindowMenu->setItemChecked( id, pWorkspace->activeWindow() == windows.at(i) );
  }
}

void KScanPlotApp::windowMenuActivated( int id )
{
  QWidget* w = pWorkspace->windowList().at( id );
  if ( w )
    w->setFocus();
}


void KScanPlotApp::commandCallback(int id_)
{
  switch (id_)
  {
    case ID_FILE_NEW:
    	 slotFileNew();
         break;

    case ID_FILE_OPEN:
         slotFileOpen();
         break;

    case ID_FILE_SAVE:
         slotFileSave();
         break;

    case ID_FILE_SAVE_AS:
         slotFileSaveAs();
         break;

    case ID_FILE_CLOSE:
         slotFileClose();
         break;

    case ID_FILE_PRINT:
         slotFilePrint();
         break;

    case ID_FILE_QUIT:
         slotFileQuit();
         break;

    case ID_EDIT_UNDO:
         slotEditUndo();
         break;

    case ID_EDIT_CUT:
         slotEditCut();
         break;

    case ID_EDIT_COPY:
         slotEditCopy();
         break;

    case ID_EDIT_PASTE:
         slotEditPaste();
         break;
  
    case ID_VIEW_TOOLBAR:
         slotViewToolBar();
         break;

    case ID_VIEW_STATUSBAR:
         slotViewStatusBar();
         break;

    case ID_WINDOW_NEW_WINDOW:
         slotWindowNewWindow();
    	 break;

    default:
         break;
  }
}

void KScanPlotApp::statusCallback(int id_)
{
  switch (id_)
  {
    case ID_FILE_NEW:
         slotStatusHelpMsg(i18n("Creates a new document"));
         break;

    case ID_FILE_OPEN:
         slotStatusHelpMsg(i18n("Opens an existing document"));
         break;

    case ID_FILE_OPEN_RECENT:
         slotStatusHelpMsg(i18n("Opens a recently used file"));
         break;

    case ID_FILE_SAVE:
         slotStatusHelpMsg(i18n("Saves the currently active document"));
         break;

    case ID_FILE_SAVE_AS:
         slotStatusHelpMsg(i18n("Saves the currently active document as under a new filename"));
         break;

    case ID_FILE_CLOSE:
         slotStatusHelpMsg(i18n("Closes the currently active document"));
         break;

    case ID_FILE_PRINT:
         slotStatusHelpMsg(i18n("Prints out the actual document"));
         break;

    case ID_FILE_QUIT:
         slotStatusHelpMsg(i18n("Quits the application"));
         break;

    case ID_EDIT_UNDO:
         slotStatusHelpMsg(i18n("Reverts the last editing action"));
         break;

    case ID_EDIT_CUT:
         slotStatusHelpMsg(i18n("Cuts the selected section and puts it to the clipboard"));
         break;

    case ID_EDIT_COPY:
         slotStatusHelpMsg(i18n("Copies the selected section to the clipboard"));
         break;

    case ID_EDIT_PASTE:
         slotStatusHelpMsg(i18n("Pastes the clipboard contents to actual position"));
         break;

    case ID_VIEW_TOOLBAR:
         slotStatusHelpMsg(i18n("Enables/disables the toolbar"));
         break;

    case ID_VIEW_STATUSBAR:
         slotStatusHelpMsg(i18n("Enables/disables the statusbar"));
         break;

    case ID_WINDOW_NEW_WINDOW:
         slotStatusHelpMsg(i18n("Opens a new view for the current document"));
         break;

    case ID_WINDOW_CASCADE:
         slotStatusHelpMsg(i18n("Cascades all windows"));
         break;

    case ID_WINDOW_TILE:
         slotStatusHelpMsg(i18n("Tiles all windows"));
         break;

    default:
         break;
  }
}
