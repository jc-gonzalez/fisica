/*************************************************************************
 *                                                                          
 * qscanplotview.cpp  -  description
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


#include "qscanplotview.h"

#include <qpainter.h>
#include <qapplication.h>

#include "curvepoint.h"
#include "axispoint.h"

#include <iostream.h>

QScanPlotView::QScanPlotView(QWidget *parent, QScanPlotDoc *doc) : QCanvasView(0, parent)
{
  theCanvas = new QCanvas();
  theCanvas->setDoubleBuffering( true );
  
  setCanvas(theCanvas);

  theDoc = doc;

  /** connect doc with the view*/
  connect(doc, SIGNAL(documentChanged()), this, SLOT(slotDocumentChanged()));
}

QScanPlotView::~QScanPlotView()
{
}

void QScanPlotView::slotDocumentChanged()
{
  image = theDoc->getImage();
  convertImage();
  showImage();
}

/** converts image to pixmap */
bool QScanPlotView::convertImage()
{
  bool success = false;

  if ( image->isNull() ) return false;

  QApplication::setOverrideCursor( waitCursor ); // this might take time

  if ( pm.convertFromImage(*image, PreferDither) ) {
    pmScaled = QPixmap();
    scale();
    //resize( width(), height() );
    success = true;                         // load successful
  } else {
    pm.resize(0,0);                         // couldn't load image
  }

  QApplication::restoreOverrideCursor();    // restore original cursor

  return success;                           // TRUE if loaded OK

}

/** scales pixmap to fit window */
void QScanPlotView::scale()
{
  int h = height();

  if ( image->isNull() ) return;

  QApplication::setOverrideCursor( waitCursor ); // this might take time

  if ( width() == pm.width() && h == pm.height() ) {                                           // no need to scale if widget

    pmScaled = pm;                          // size equals pixmap size

  } else {

    //if (smooth()) {
    //  pmScaled.convertFromImage(image.smoothScale(width(), h), conversion_flags);
    //} else {
      QWMatrix m;                         // transformation matrix
      //m.scale(double(zoom*width())/pm.width(),// define scale factors
      //        double(zoom*height())/pm.height());
      m.scale(zoom, zoom);
      pmScaled = pm.xForm( m );           // create scaled pixmap
    //}

  }

  QApplication::restoreOverrideCursor();      // restore original cursor
}

/** Changes the zoom to view image */
void QScanPlotView::setInitialZoom(double z)
{
  zoom = z;
}

/** Changes the zoom to view image */
void QScanPlotView::setZoom(double z)
{
  zoom = z;
  scale();
  showImage();
}

/** shows the scaled pixmap */
void QScanPlotView::showImage()
{
  if ( pm.size() != QSize( 0, 0 ) ) {
    theCanvas->setBackgroundPixmap(pmScaled);
    theCanvas->resize(pmScaled.width(), pmScaled.height());
    viewport()->setMaximumSize(pmScaled.size());

    CurvePoint *cp = new CurvePoint(theCanvas);
    cp->moveBy(10, 10);
    cp->show();

    AxisPoint *ap = new AxisPoint(theCanvas);
    ap->moveBy(15, 15);
    ap->show();

    frameChanged();
  }
}

void QScanPlotView::contentsMousePressEvent(QMouseEvent* e)
{
  qDebug("%% (%f, %f)",
         double(e->pos().x()) / pmScaled.width(),
         double(e->pos().y()) / pmScaled.height());

  QCanvasItemList l=canvas()->collisions(e->pos());
  for (QCanvasItemList::Iterator it=l.begin(); it!=l.end(); ++it) {

    if ( (*it)->rtti() == FigurePointRTTI ) {
      qDebug("%% OVER A POINT!!");
	    FigurePoint *item = (FigurePoint*)(*it);
      if ( ! (item->boundingRect()).contains( e->pos() ) ) continue;
    }
    
    moving = *it;
    moving_start = e->pos();

    return;
  }
  
  moving = 0;
}

void QScanPlotView::contentsMouseMoveEvent(QMouseEvent* e)
{
  if ( moving ) {
    moving->moveBy(e->pos().x() - moving_start.x(),
                   e->pos().y() - moving_start.y());
    moving_start = e->pos();
    canvas()->update();
  }
}

