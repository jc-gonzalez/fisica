/*************************************************************************
 *                                                                          
 * qscanplotdoc.h  -  description
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

#ifndef QSCANPLOTDOC_H
#define QSCANPLOTDOC_H

// include files for QT
#include <qobject.h>

// application specific includes

#include <qimage.h>
#include <qstring.h>
#include <qtextstream.h>
#include <qintdict.h>

#include "curve.h"
#include "axes.h"

#define SERIALIZE_VERSION  0.1

/**
  * the Document Class
  */

class QScanPlotDoc : public QObject
{
  Q_OBJECT

  friend QTextStream& operator<<(QTextStream &, const QScanPlotDoc &);
  friend QTextStream& operator>>(QTextStream &, QScanPlotDoc &);

  public:
    QScanPlotDoc();
    ~QScanPlotDoc();
    void newDoc();
    bool save();
    bool saveAs(const QString &filename);
    bool load(const QString &filename);
    bool isModified() const;

    /** Loads image file in any of the supported formats */
    bool loadImage(const char* filename);
    /** Returns the pointer to the current image */
    QImage* getImage();
    /** Returns pointer to i-th curve */
    Curve* getCurve(int i);
    /** Returns pointer to i-th axes */
    Axes* getAxes(int i);

  signals:
    void documentChanged();

  protected:
    bool modified;

  private:
    QImage           image;         // the loaded image

    QString          imageName;     // the name of the image

    int              nCurves;
    QIntDict<Curve>  *theCurves;
    
    int              nAxes;
    QIntDict<Axes>   *theAxes;

};

#endif
