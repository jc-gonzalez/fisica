/*************************************************************************
*                                                                          
* pixelbase.cpp  -  description
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


#include "pixelbase.h"
#include "magicpar.h"

#include <qpainter.h>
#include <qstring.h>
#include <qpointarray.h>

#include <iostream.h>
#include <math.h>

inline int    irint(double x) { return int(floor((x)+0.5)); }
inline double frint(double x) { return floor((x)+0.5); }

#define SIN60   0.866025403784439
#define COS60   0.500000000000000
#define COS30   SIN60
#define SIN30   COS60

PixelBase::PixelBase(int n=0, QCanvas *parent) :
  QCanvasPolygon(parent), id(n)
{
  zoom = 250.0;
}

PixelBase::~PixelBase()
{
}

DEFINE_IMPLEMENTATION(PixelBase,int,id);
DEFINE_IMPLEMENTATION(PixelBase,double,zoom);
DEFINE_IMPLEMENTATION(PixelBase,double,charge);
DEFINE_IMPLEMENTATION(PixelBase,double,time);

void PixelBase::buildPixel()
{
  buildPixel(id);
}

#define DBG(a)  debug( #a ); a

void PixelBase::buildPixel(int n)
{
  double cx, cy;
  double ftempsize, fctempsize;
  double fstempsize;

  id = n;

  cx = (MAGICPar::Instance())->getX(id)*zoom;
  cy = (MAGICPar::Instance())->getY(id)*zoom;

  size = (MAGICPar::Instance())->getSize(id);

  ftempsize  = size / sqrt(3.0) * zoom;
  fctempsize = ftempsize * COS30;
  fstempsize = ftempsize * SIN30;

  QPointArray vertex0(6);
  vertex0[0] = QPoint( irint(cx+fctempsize), irint(cy-fstempsize) );
  vertex0[1] = QPoint( irint(cx),            irint(cy-ftempsize) );
  vertex0[2] = QPoint( irint(cx-fctempsize), irint(cy-fstempsize) );
  vertex0[3] = QPoint( irint(cx-fctempsize), irint(cy+fstempsize) );
  vertex0[4] = QPoint( irint(cx),            irint(cy+ftempsize) );
  vertex0[5] = QPoint( irint(cx+fctempsize), irint(cy+fstempsize) );

  QPointArray vertex;
  vertex = inverse_world_transform->map(vertex0);

  setPoints( vertex );
  setBrush( Qt::black );
  setPen( Qt::black );
  setZ( id );

  show();
}

void PixelBase::drawShape( QPainter &p )
{
  p.drawPolygon( areaPoints() );
}

void PixelBase::setWorldTransform(QWMatrix *m)
{
  inverse_world_transform = new QWMatrix(*m);
}

void PixelBase::setColor( QColor &c )
{
  setBrush( c );
}

int PixelBase::get_size()
{
  return size;
}

void PixelBase::mousePressEvent( QMouseEvent * e )
{
  cerr << "this is pixel " << id << endl;
}


// Local Variables:
// mode: c++
// End:
//EOF
