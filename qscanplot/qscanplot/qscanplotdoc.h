/***************************************************************************
                          qscanplotdoc.h  -  description
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
#ifndef QSCANPLOTDOC_H
#define QSCANPLOTDOC_H

// include files for QT
#include <qobject.h>

// application specific includes

/**
  * the Document Class
  */

class QScanPlotDoc : public QObject
{
  Q_OBJECT

  public:
    QScanPlotDoc();
    ~QScanPlotDoc();
    void newDoc();
    bool save();
    bool saveAs(const QString &filename);
    bool load(const QString &filename);
    bool isModified() const;

  signals:
    void documentChanged();

  protected:
    bool modified;
};

#endif
