/***************************************************************************
                          qdisplaydoc.cpp  -  description
                             -------------------
    begin                : Thu Oct 12 16:57:50 CEST 2000
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

#include "qdisplaydoc.h"

QdisplayDoc::QdisplayDoc()
{
  modified = false;
}

QdisplayDoc::~QdisplayDoc()
{
}

void QdisplayDoc::newDoc()
{
}

bool QdisplayDoc::save()
{
  return true;
}

bool QdisplayDoc::saveAs(const QString &filename)
{
  return true;
}

bool QdisplayDoc::load(const QString &filename)
{
  emit documentChanged();
  return true;
}

bool QdisplayDoc::isModified() const
{
  return modified;
}
