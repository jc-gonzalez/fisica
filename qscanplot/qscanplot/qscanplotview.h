/***************************************************************************
                          qscanplotview.h  -  description
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

#ifndef QSCANPLOTVIEW_H
#define QSCANPLOTVIEW_H

// include files for QT
#include <qwidget.h>

// application specific includes
#include "qscanplotdoc.h"

/**
 * This class provides an incomplete base for your application view. 
 */

class QScanPlotView : public QWidget
{
  Q_OBJECT
  public:
    QScanPlotView(QWidget *parent=0, QScanPlotDoc* doc=0);
    ~QScanPlotView();
  
  protected slots:
    void slotDocumentChanged();
  
};

#endif
