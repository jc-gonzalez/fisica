/***************************************************************************
                          qdisplayview.h  -  description
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

#ifndef QDISPLAYVIEW_H
#define QDISPLAYVIEW_H

// include files for QT
#include <qwidget.h>

// application specific includes
#include "qdisplaydoc.h"

/**
 * This class provides an incomplete base for your application view. 
 */

class QdisplayView : public QWidget
{
  Q_OBJECT
  public:
    QdisplayView(QWidget *parent=0, QdisplayDoc* doc=0);
    ~QdisplayView();
  
  protected slots:
    void slotDocumentChanged();
  
};

#endif
