/*************************************************************************
 *                                                                          
 * kscanplotview.cpp  -  description
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


// include files for Qt
#include <qprinter.h>
#include <qpainter.h>
#include <qdir.h>
// include files for KDE

// application specific includes
#include "kscanplotview.h"
#include "kscanplotdoc.h"
#include "kscanplot.h"

KScanPlotView::KScanPlotView(KScanPlotDoc* pDoc, QWidget *parent, const char* name, int wflags)
 : QWidget(parent, name, wflags)
{
    doc=pDoc;
}

KScanPlotView::~KScanPlotView()
{
}

KScanPlotDoc *KScanPlotView::getDocument() const
{
	return doc;
}

void KScanPlotView::update(KScanPlotView* pSender){
	if(pSender != this)
		repaint();
}

void KScanPlotView::print(QPrinter *pPrinter)
{
  if (pPrinter->setup(this))
  {
		QPainter p;
		p.begin(pPrinter);
		
		///////////////////////////////
		// TODO: add your printing code here
		///////////////////////////////
		
		p.end();
  }
}

void KScanPlotView::closeEvent(QCloseEvent* e){

// DO NOT CALL QWidget::closeEvent(e) here !!
// This will accept the closing by QCloseEvent::accept() by default.
// The installed eventFilter() in KScanPlotApp takes care for closing the widget
// or ignoring the close event
		
}
