/***************************************************************************
                          qscanplotview.cpp  -  description
                             -------------------
    begin                : Fri Oct 13 19:38:27 CEST 2000
    copyright            : (C) 2000 by J C Gonzalez
    email                : gonzalez@gae.ucm.es
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "qscanplotview.h"

QScanPlotView::QScanPlotView(QWidget *parent, QScanPlotDoc *doc) : QWidget(parent)
{
  /** connect doc with the view*/
  connect(doc, SIGNAL(documentChanged()), this, SLOT(slotDocumentChanged()));
}

QScanPlotView::~QScanPlotView()
{
}

void QScanPlotView::slotDocumentChanged()
{
  //TODO update the view

}
