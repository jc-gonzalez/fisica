/***************************************************************************
                          qscanplotdoc.cpp  -  description
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

#include "qscanplotdoc.h"

QScanPlotDoc::QScanPlotDoc()
{
  modified = false;
}

QScanPlotDoc::~QScanPlotDoc()
{
}

void QScanPlotDoc::newDoc()
{
}

bool QScanPlotDoc::save()
{
  return true;
}

bool QScanPlotDoc::saveAs(const QString &filename)
{
  return true;
}

bool QScanPlotDoc::load(const QString &filename)
{
  emit documentChanged();
  return true;
}

bool QScanPlotDoc::isModified() const
{
  return modified;
}
