/*************************************************************************
*                                                                          
* qmagicdisplayview.cpp  -  description
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


#include "qmagicdisplayview.h"

#include "wdatgui.h"
#include "camerabase.h"
#include "camerapixels.h"
#include "palette.h"

#include <qlayout.h>
#include <qpushbutton.h>
#include <qtabwidget.h>
#include <qtextview.h>
#include <qlistbox.h>
#include <qpainter.h>
#include <qprinter.h>
#include <qpixmap.h>
#include <qbrush.h>
#include <qfont.h>
#include <qstrlist.h>

#include <iostream.h>
#include <math.h>

QMAGICDisplayView::QMAGICDisplayView(QWidget *parent,
                                     QMAGICDisplayDoc *doc,
                                     EventHeader& eh,
                                     EventData& ed,
                                     EventPostData& epd) :
  QWidget(parent),
  theDoc(doc), eheader(eh), edata(ed), epdata(epd)
{
  qDebug("# QMAGICDisplayView::QMAGICDisplayView() constructor");

  /** generate the interface */
  initView();

  /** connect doc with the view*/
  connect(theDoc, SIGNAL(documentChanged()),
          this, SLOT(slotDocumentChanged()));
}

QMAGICDisplayView::~QMAGICDisplayView()
{
}

void QMAGICDisplayView::initView()
{
  qDebug("# QMAGICDisplayView::initView()");

  palette = new Palette;
  paletteList = new QStrList( TRUE );

  // Generates and initializes the GUI

  QBoxLayout* lh1 = new QHBoxLayout(this);

  QBoxLayout* lv11 = new QVBoxLayout(lh1,-1,"leftside");
  QBoxLayout* lv12 = new QVBoxLayout(lh1,-1,"rightside");

  QTabWidget* tw1 = new QTabWidget(this);

  doctype = theDoc->getDocType();

  QString sdoctype(((doctype == QMAGICDisplayDoc::DATDocType)?"DAT":
                    ((doctype == QMAGICDisplayDoc::PHEDocType)?"PHE":
                     ("RFL"))));

  switch ( doctype ) {

  case QMAGICDisplayDoc::DATDocType :
  case QMAGICDisplayDoc::PHEDocType :
    gui.widget = new wDATGUI(theDoc,this);
    CHECK_PTR( gui.widget );
    tw1->addTab(gui.widget, sdoctype + " file interface");
    theCamera = new CameraPixels(theDoc, theDoc->getEvtD(), *palette, this);
    CHECK_PTR( theCamera );

    break;

  case QMAGICDisplayDoc::RFLDocType :
    tw1->addTab(0, sdoctype + " file interface");
    theCamera = 0;
    
    break;

  }

  theEVTHlog = new QTextView(this);
  CHECK_PTR( theEVTHlog);
  
  theEVTHlog->setTextFormat( QTextView::RichText );
  theEVTHlog->setFont(QFont("helvetica", 12));
  theEVTHlog->resize( gui.widget->size() );

  tw1->addTab(theEVTHlog, "Evt.Header");

  theINFOlist = new QListBox(this);
  CHECK_PTR( theINFOlist);
  
  const char * init[] = {"this","is a","test"};
  theINFOlist->insertStrList (init,3);
  theINFOlist->resize( gui.widget->size() );

  tw1->addTab(theINFOlist, "Image par.");

  // build layout structure
  {
    {
      lv11->addWidget(theCamera);
      lv11->addStretch();
    } lv11->activate();

    //lh1->addLayout(lv11);
    lh1->addStretch();

    {
      lv12->addWidget(tw1);
      lv12->addStretch();
    } lv12->activate();

    //lh1->addLayout(lv12);

  } lh1->activate();

  setFixedSize( childrenRect().size() );

  // Connections
  
  connect( gui.widget, SIGNAL(eventChanged(int)),
           theDoc, SLOT(slotReadEvent(int)));

  connect( theDoc, SIGNAL(eventChanged()),
           this, SLOT(slotEventChanged()));

  connect( theDoc, SIGNAL(eventChanged()),
           theCamera, SLOT(slotDisplayEvent()));

  connect( gui.widget, SIGNAL(addingPalette(const char*)),
           this, SLOT(slotAddedPalette(const char*)));

  connect( gui.widget, SIGNAL(setPalette(int)),
           this, SLOT(slotSetPalette(int)));

  connect( gui.widget, SIGNAL(setPaletteMode(int)),
           this, SLOT(slotSetPaletteMode(int)));

  connect( gui.widget, SIGNAL(setPaletteMin(int)),
           this, SLOT(slotSetPaletteMin(int)));

  connect( gui.widget, SIGNAL(setPaletteMax(int)),
           this, SLOT(slotSetPaletteMax(int)));

  connect( gui.widget, SIGNAL(reDisplayEvent()),
           theCamera, SLOT(slotDisplayEvent()));

  connect( gui.widget, SIGNAL(toggleShowItem(int)),
           theCamera, SLOT(slotToggleShowItem(int)));

  connect( this, SIGNAL(initChildren()),
           gui.widget, SLOT(slotInit()));

  emit initChildren();

  palette->setPaletteMode(0);

  update();
}

void QMAGICDisplayView::slotDocumentChanged()
{
  qDebug("# QMAGICDisplayView::slotDocumentChanged()");

  theCamera->createPixels();

  switch ( doctype ) {
  case QMAGICDisplayDoc::DATDocType :
  case QMAGICDisplayDoc::PHEDocType :
    gui.datgui->setTotalEvents( theDoc->getTotalEvents() );
    gui.datgui->setFilename( theDoc->getDocName() );
    break;
  case QMAGICDisplayDoc::RFLDocType :
    break;
  }
}

void QMAGICDisplayView::slotEventChanged()
{
  qDebug("# QMAGICDisplayView::slotEventChanged()");

  // update the EVTH info tab
  QString t("<big><b>Event Header</b></big><hr><table>");

  int i;
  for (i=0; i<EvtHdr_NUMBER_OF_VARIABLES; i++) {
    t.sprintf("%s<tr><td><i>%s</i>: <td>%f",
              t.latin1(), cEvtHdr_Variables[i], eheader.get(i));
  }
  t = t + "</table>"; 
  
  theEVTHlog->setText( t );

  switch ( doctype ) {
  case QMAGICDisplayDoc::DATDocType :
  case QMAGICDisplayDoc::PHEDocType :
    gui.datgui->setTrigger( eheader.get( EvtHdr_trigger ) );
    theCamera->setImage(eheader.get( EvtHdr_alpha ),
                        eheader.get( EvtHdr_length ),
                        eheader.get( EvtHdr_width ),
                        eheader.get( EvtHdr_xm ),
                        eheader.get( EvtHdr_ym ),
                        eheader.get( EvtHdr_xmax ),
                        eheader.get( EvtHdr_ymax ));
    break;
  case QMAGICDisplayDoc::RFLDocType :
    break;
  }

}

void QMAGICDisplayView::slotAddedPalette(const char* fname)
{
  paletteList->append(fname);
  palette->load(fname);
}

void QMAGICDisplayView::slotSetPalette(int n)
{
  palette->load( paletteList->at(n) );
}

void QMAGICDisplayView::slotSetPaletteMode(int n)
{
  palette->setPaletteMode(n);
}

void QMAGICDisplayView::slotSetPaletteMin(int n)
{
  palette->set_minim(n);
}

void QMAGICDisplayView::slotSetPaletteMax(int n)
{
  palette->set_maxim(n);
}

// Local Variables:
// mode: c++
// End:
//EOF
