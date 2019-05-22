/*************************************************************************
*                                                                          
* camerapixels.cpp  -  description
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


#include "camerapixels.h"

#include "pixel.h"
#include "magicpar.h"
#include "palette.h"
#include "ellipsoid.h"

#include <qpainter.h>
#include <qtooltip.h>
#include <qfont.h>

#include <iostream.h>

CameraPixels::CameraPixels(QMAGICDisplayDoc* d, EventData& ed, Palette& p,
                           QWidget *parent, const char *name) :
  CameraBase(d, parent, name),
  tips(0), pixelList(0), pixelInfo(0), edata(ed), pal(p), ellipsoid(0)
{
  qDebug(">> CameraPixels() constructor");
}

CameraPixels::~CameraPixels()
{
}

void CameraPixels::createPixels()
{
  offsetx  = width()  / 2;
  offsety  = height() / 2;

  zoomx = width()  / 10000.0;
  zoomy = height() / 10000.0;

  world_transform = new QWMatrix( zoomx,   0,
                                  0,       zoomy,
                                  offsetx, offsety);
  CHECK_PTR( world_transform );

  if ( tips == 0 )
    tips = new QToolTipGroup( this, "tool tip relay" );
  CHECK_PTR(tips);

  if ( pixelList == 0 )
    pixelList = new QArray<Pixel*>((MAGICPar::Instance())->numberOfPixels);
  CHECK_PTR(pixelList);

  if ( pixelInfo == 0 )
    pixelInfo = new QCanvasText("Pixel Information here",
                                QFont("Helvetica", 18), canvas());
  CHECK_PTR(pixelInfo);
  pixelInfo->setX(2);
  pixelInfo->setY(2);
  pixelInfo->setColor( Qt::green );
  
  if ( ellipsoid == 0 )
    ellipsoid = new Ellipsoid(canvas());
  CHECK_PTR(ellipsoid);
  ellipsoid->setWorldTransform(world_transform);
  ellipsoid->setZoom(250.);

  int i;
  Pixel* p;
  QColor c;
  QString s;
  
  for (i=0; i<(MAGICPar::Instance())->numberOfPixels; i++) {
    p = new Pixel(i, canvas());
    CHECK_PTR(p);
    
    p->set_zoom(250.);
    p->setWorldTransform(world_transform);
    p->buildPixel();
    c.setRgb(0, 0, i%256);
    p->setColor( c );

    QToolTip::add(this, p->boundingRect(), s.sprintf("%d", i+1),
                  tips, s.sprintf("This is pixel %d", i+1));

    (*pixelList)[i] = p;
  }

  tips->setEnabled( false );

  pixelInfo->show();

  canvas()->update();
  canvas()->setDoubleBuffering( ! tips->enabled() );
}

void CameraPixels::contentsMousePressEvent(QMouseEvent* e)
{
  QCanvasItemList listhex = canvas()->collisions(e->pos());

  QString s("Oops!");

  if ( ! listhex.isEmpty() ) {
    Pixel *theitem = static_cast<Pixel*>(listhex.last());
    int n = pixelList->find(theitem);
    if (n>-1) {
      int n = theitem->get_id();
      int v = int( edata[n] );
      s.sprintf("Pix#:%3d [%d ph.e-]", n+1, v);
    }
  }

  pixelInfo->setText(s);
  canvas()->update();
}

void CameraPixels::setImage(double a, double l, double w,
                            double x, double y, double mx, double my)
{
  ellipsoid->setAlpha( a );
  ellipsoid->setShape( l, w );
  ellipsoid->setCentroid( x, y );
  ellipsoid->setMaximum( mx, my );
}

void CameraPixels::printIt( QPainter* p )
{
  drawContents( p, 0, 0, canvas()->width(), canvas()->height() );
}

void CameraPixels::slotDisplayEvent()
{
  Pixel* p;

  qDebug(">> CameraPixels::slotDisplayEvent()");

  pal.set_mindata( edata.get_mindata() );
  pal.set_maxdata( edata.get_maxdata() );

  int i;
  for (i=0; i<(MAGICPar::Instance())->numberOfPixels; i++) {
    p = pixelList->at(i);
    p->setColor( pal.getPaletteColor(edata[i], p->get_size()) );
  }
  
  ellipsoid->draw();
  canvas()->update();
}

void CameraPixels::slotToggleShowItem(int i)
{
  qDebug(">> toggle item %d", i);

  switch (i) {
  case 0:
    ellipsoid->toggleEllipse();
    break;

  case 1:
    ellipsoid->toggleAxes();
    break;

  case 2:
    ellipsoid->toggleCentroid();
    break;

  case 3:
    ellipsoid->toggleMaximum();
    break;
  }

  ellipsoid->draw();
  canvas()->update();
}
// Local Variables:
// mode: c++
// End:
//EOF
